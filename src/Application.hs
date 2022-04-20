module Application (appMain, readModule, readExec, reduce) where

import           ClassyPrelude                 hiding (many, try)
import           Control.Monad.Trans.State
import           Text.Groom
import           Text.ParserCombinators.Parsec hiding ((<|>))

-- * basic data

data Module = Module { moduleName     :: String
                     , moduleSort     :: Map String Sort
                     , moduleVariable :: Map String Variable
                     , moduleOperator :: Map String Operator
                     }
    deriving (Eq, Ord, Show, Read)

data Sort = Sort
    deriving (Eq, Ord, Show, Read)

data Variable = Variable { variableSort :: String }
    deriving (Eq, Ord, Show, Read)

data Operator = Operator { operatorArg    :: [String]
                         , operatorResult :: String
                         , operatorEqual  :: [Equal]
                         }
    deriving (Eq, Ord, Show, Read)

data Equal = Equal { equalLeft  :: Term
                   , equalRight :: Term
                   }
    deriving (Eq, Ord, Show, Read)

data Term = Term { termOperator :: String
                 , termArg      :: [Term]
                 }
    deriving (Eq, Ord, Show, Read)

-- * main

appMain :: IO ()
appMain = error "no impl"

-- * parse

data Statement = StatementSort     String Sort
               | StatementVariable String Variable
               | StatementOperator String Operator
               | StatementEqual    String Equal
    deriving (Eq, Ord, Show, Read)

readModule :: String -> Either ParseError Module
readModule = parse parseModule "sub-maude module"

parseModule :: Parser Module
parseModule = makeModule <$> (string "fmod" *> spaces *> many alphaNum <* spaces <* string "is") <*>
    parseStatements <* string "endfm"

makeModule :: String -> [Statement] -> Module
makeModule mName statements = Module mName mSort mVariable mOperator
  where mSort = mapFromList $ mapMaybe selectSort statements
        selectSort (StatementSort n b) = Just (n, b)
        selectSort _ = Nothing
        mVariable = mapFromList $ mapMaybe selectVariable statements
        selectVariable (StatementVariable n b) = Just (n, b)
        selectVariable _ = Nothing
        mOperator = mapFromList $ mapMaybe selectOperator statements
        selectOperator (StatementOperator n b) =
            Just (n, b { operatorEqual = map snd $ filter ((n ==) . fst) mEqual })
        selectOperator _ = Nothing
        mEqual = mapMaybe selectEqual statements
        selectEqual (StatementEqual n b) = Just (n, b)
        selectEqual _ = Nothing

parseStatements :: Parser [Statement]
parseStatements = parseStatement >>= (\s -> maybe (return []) (\j -> (j :) <$> parseStatements) s)
  where parseStatement = spaces *> (Just . uncurry StatementSort <$> parseSort <|>
                                    Just . uncurry StatementVariable <$> parseVariable <|>
                                    Just . uncurry StatementOperator <$> parseOperator <|>
                                    Just . uncurry StatementEqual <$> try parseEqual <|>
                                    pure Nothing)

parseSort :: Parser (String, Sort)
parseSort = (\n -> (n, Sort)) <$> (string "sort" *> spaces *> many alphaNum <* spaces <* char '.')

parseVariable :: Parser (String, Variable)
parseVariable = (\n s -> (n, Variable s)) <$> (string "var" *> spaces *> many alphaNum) <*>
    (spaces *> char ':' *> spaces *> many alphaNum <* spaces <* char '.')

parseOperator :: Parser (String, Operator)
parseOperator = (\n arg result -> (n, Operator arg result mempty)) <$>
    (string "op" *> spaces *> many1 alphaNum <* skipMany (char '_')
     <* spaces <* char ':' <* spaces) <*>
    (words <$> manyTill anyChar (try (spaces *> string "->" *> spaces))) <*>
    many1 alphaNum <* spaces <* char '.'

parseEqual :: Parser (String, Equal)
parseEqual = (\l r -> ((termOperator l), Equal l r)) <$> (string "eq" *> spaces *> parseTerm) <*>
    (spaces *> char '=' *> spaces *> parseTerm <* spaces <* char '.')

parseTerm :: Parser Term
parseTerm = parseTermPrefix
  where parseTermPrefix = Term <$> many1 alphaNum <*> (spaces *> parseTermPrefixArg)
        parseTermPrefixArg = (parseTermPrefixArgMulti <|>
                               singleton <$> parseTermPrefix <|>
                               pure [])
        parseTermPrefixArgMulti = char '(' *>
            (parseTermPrefix `sepBy` (char ',' *> spaces)) <*
            char ')'

-- * command

data Command = Command { commandModule  :: Module
                       , commandTermLog :: [Term]
                       }
    deriving (Eq, Ord, Show, Read)

type CommandStateIO a = StateT Command IO a

reduce :: Module -> Term -> IO (Term, Command)
reduce m t = runStateT (replace t) (Command m [])

replace :: Term -> CommandStateIO Term
replace t = do
    me <- listToMaybe <$> matchEqual t
    Term uo ua <- case me of
        Nothing -> return t
        Just (Equal{equalLeft = l, equalRight = r}) -> do
            bm <- makeBindMap t l
            replace $ bindVariable bm r
    nua <- mapM replace ua
    let u = Term uo nua
    if t == u
        then return u
        else termLog u >> replace u

matchEqual :: Term -> CommandStateIO [Equal]
matchEqual t = termEqual t >>= filterM (isMatch t . equalLeft)

isMatch :: Term -> Term -> CommandStateIO Bool
isMatch t u = do
    varOr <- (||) <$> (isJust <$> termVariable t) <*> (isJust <$> termVariable u)
    childMatch <- (((length ta == length ua) &&) . and) <$> mapM (uncurry isMatch) (zip ta ua)
    return $ varOr || childMatch
  where ta = termArg t
        ua = termArg u

bindVariable :: (Map Term Term) -> Term -> Term
bindVariable bm r = case lookup r bm of
                        Nothing -> r{termArg = map (bindVariable bm) $ termArg r}
                        Just t  -> t

makeBindMap :: Term -> Term -> CommandStateIO (Map Term Term)
makeBindMap t l = do
    mlv <- termVariable l
    case mlv of
        Nothing -> foldM (\a (ta, la) -> ((a <>) <$>) $ makeBindMap ta la) mempty $
            zip (termArg t) (termArg l)
        Just _  -> return $ singletonMap l t

termLog :: Term -> CommandStateIO ()
termLog t = modify (\c -> c{commandTermLog = t : commandTermLog c})

termEqual :: Term -> CommandStateIO [Equal]
termEqual t = moduleOperatorEqual $ termOperator t

moduleOperatorEqual :: String -> CommandStateIO [Equal]
moduleOperatorEqual o = maybe mempty operatorEqual . lookup o . moduleOperator . commandModule <$>
    get

termVariable :: Term -> CommandStateIO (Maybe Variable)
termVariable Term{termOperator = o} = lookup o . moduleVariable . commandModule <$> get

-- * output

showTerm :: Term -> String
showTerm (Term{termOperator = o, termArg = [] }) = o
showTerm (Term{termOperator = o, termArg = [a]}) = o <> " " <> showTerm a
showTerm (Term{termOperator = o, termArg = r  }) = o <> " " <>
    "(" <> intercalate ", " (map showTerm r) <> ")"

-- * debug

readExec f s = do
    c <- readFile f
    let Right m = readModule c
        Right t = parse parseTerm "term" s
    reduce m t

g :: Show s => s -> IO ()
g = putStrLn . pack . groom

exampleNat :: String
exampleNat = "example/nat.maude"
