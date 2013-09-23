{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module HGVS where

import Text.Parsec
import Control.Applicative hiding ((<|>), optional, many)

data Var = SingleVar Ref RawVar deriving (Read, Show, Eq)
data PtLoc = RealPtLoc (Maybe PtLocSym) Int (Maybe Offset) | Unknown deriving (Read, Show, Eq)
data PtLocSym = FivePrime | ThreePrime deriving (Read, Show, Eq) -- "-" | "*"
data Offset = Offset Direction (Maybe UD) Int (Maybe Int) deriving (Read, Show, Eq)
type UD = Char
type Direction = Char
type Nt = Char
data StrOrNum = Str [Nt] | Numb Int deriving (Read, Show, Eq)
data RangeLoc = Range Extent Bool deriving (Read, Show, Eq)
data Extent = RealExtent PtLoc (Maybe (Bool, AccOrSym)) PtLoc | EXLoc Int (Maybe Int) deriving (Read, Show, Eq)
data AccOrSym = RefSeqAcc GenBankRef | LRG Int (Maybe LRGID) | Sym GeneSymbol deriving (Read, Show, Eq)
data LRGID = LRGTranscriptID Int | LRGProteinID Int deriving (Read, Show, Eq)
data GenBankRef = GenBankRef Acc (Maybe GeneSymbol) deriving (Read, Show, Eq)
data Acc = GI Int | AccNo String deriving (Read, Show, Eq)
type Name = String
data GeneSymbol = GeneSymbol Name (Maybe GeneType) deriving (Read, Show, Eq)
data GeneType = TransVar Int | ProtIso Int deriving (Read, Show, Eq)
data Loc = PtLoc' PtLoc | RangeLoc' RangeLoc deriving (Read, Show, Eq)
type Nest = [ExtendedRawVar]
data ExtendedRawVar = RawVar' RawVar | Equal | UnknownVar deriving (Read, Show, Eq)
data InsSeq = InsStr [Nt] | InsNum Int | InsRange RangeLoc | InsFar FarLoc deriving (Read, Show, Eq)
type RefType = Char
data Ref = Ref (Maybe AccOrSym) (Maybe RefType) deriving (Read, Show, Eq)
data FarLoc = FarLoc AccOrSym (Maybe (Extent, Maybe RefType)) deriving (Read, Show, Eq)

parseVar = (SingleVar (Ref Nothing Nothing) <$> parseRawVar) <|>> (SingleVar <$> parseRef <*> parseRawVar)
parseRef = Ref <$> (optionMaybe $ parseAccOrSym <* char ':') <*> (optionMaybe $ parseRefType)
parseRefType = oneOf "cgmnr" <* char '.'
parseNum = (read <$> many1 digit)
parseLoc = (PtLoc' <$> parsePtLoc) <|>> (RangeLoc' <$> parseRangeLoc)
parsePtLoc = (RealPtLoc <$> (optionMaybe parsePtLocSym) <*> parseNum <*> (optionMaybe parseOffset)) <|>>
    pure Unknown
parsePtLocSym = (FivePrime <$ char '-') <|> (ThreePrime <$ char '*')
parseDirection = oneOf "+-"
parseAccOrSym = (RefSeqAcc <$> parseGenBankRef) <|>>
    (LRG <$> (string "LRG" *> parseNum) <*> (optionMaybe $ char '_' *> parseLRGID))
parseLRGID = (LRGTranscriptID <$> (char 't' *> parseNum)) <|> (LRGProteinID <$> (char 'p' *> parseNum))
parseOffset = Offset <$> parseDirection <*> (optionMaybe $ oneOf "ud") <*> parseNum <*> 
    ((Just <$> parseNum) <|>> (Nothing <$ char '?'))
parseRangeLoc = ((\a -> Range a True) <$> parseExtent) <|>>
    ((\a -> Range a False) <$> (char '(' *> parseExtent <* char ')'))
parseExtent = (RealExtent <$> (parsePtLoc <* char '_') <*>
    (optionMaybe $ (,) <$> (option False $ (True <$ char 'o')) <*> parseAccOrSym) <*> parsePtLoc) <|>>
    (EXLoc <$> (string "EX" *> parseNum) <*> (optionMaybe $ char '-' *> parseNum))
parseNest = [] <$ (char '{' *> (many $ noneOf "}") <* char '}')
parseExtRawVar = (RawVar' <$> parseRawVar) <|>> (Equal <$ char '=') <|> (UnknownVar <$ char '?')
parseFarLoc = FarLoc <$> (parseAccOrSym <* char ':') <*> (optionMaybe $ ((\a b -> (b, Just a)) <$> parseRefType <*>
    parseExtent) <|> ((\a -> (a, Nothing)) <$> parseExtent))
parseInsSeq = (InsStr <$> parseNts) <|>> (InsRange <$> parseRangeLoc) <|>> (InsFar <$> parseFarLoc) <|>> (InsNum <$> parseNum)
parseStrOrNum = (Str <$> parseNts) <|> (Numb <$> parseNum)
data RawVar = Subst PtLoc Nt Nt
            | Del Loc (Maybe StrOrNum)
            | Dup Loc (Maybe StrOrNum) Nest
            | VarPtSSR PtLoc [Nt] Int
            | VarRangeSSR RangeLoc Int
            | AbrSSR PtLoc [Nt] Int Int
            | Ins RangeLoc InsSeq Nest
            | Indel RangeLoc (Maybe StrOrNum) InsSeq Nest
            | Inv RangeLoc (Maybe StrOrNum) Nest
            | Conv RangeLoc FarLoc Nest deriving (Read, Show, Eq)

--parseRef :: Stream s m Char => ParsecT s u m GenBankRef
parseGenBankRef = GenBankRef <$>
    ((GI . read <$> (string "GI" *> (optional $ char ':') *> many1 digit)) <|> 
        (AccNo <$> ((optional $ string ".ish") *> (many1 (letter <|> digit <|> (oneOf "._")))))) <*>
    (optionMaybe $ GeneSymbol <$> (char '(' *> many1 (letter <|> digit)) <*> (optionMaybe parseGType))

--parseGType :: Stream s m Char => ParsecT s u m GeneType
parseGType = char '_' *> ((TransVar . read <$> (char 'v' *> many1 digit)) <|> (ProtIso . read <$> (char 'i' *> many1 digit)))

--parseNts :: Stream s m Char => ParsecT s u m [Nt]
parseNts = many1 parseNt

--parseNt :: Stream s m Char => ParsecT s u m Nt
parseNt = oneOf "acgtuACGTU"

p0 <|>> p1 = (try p0) <|> p1

--parseRawVar :: Stream s m Char => ParsecT s u m RawVar
parseRawVar = (Subst <$> parsePtLoc <*> (parseNt <* char '>') <*> parseNt) <|>>
    (Del <$> (parseLoc <* string "del") <*> (optionMaybe parseStrOrNum)) <|>>
    (Dup <$> (parseLoc <* string "dup") <*> (optionMaybe parseStrOrNum) <*> parseNest) <|>>
    (VarPtSSR <$> parsePtLoc <*> parseNts <*> (read <$> (char '[' *> many1 digit <* char ']'))) <|>>
    (VarRangeSSR <$> parseRangeLoc <*> (read <$> (char '[' *> many1 digit <* char ']'))) <|>>
    (AbrSSR <$> parsePtLoc <*> parseNts <*> (read <$> (char '(' *> many1 digit <* char '_')) <*>
        (read <$> (many1 digit <* char ')'))) <|>>
    (Ins <$> (parseRangeLoc <* string "ins") <*> parseInsSeq <*> parseNest) <|>>
    (Indel <$> (parseRangeLoc <* string "del") <*> (optionMaybe parseStrOrNum) <*> (string "ins" *> parseInsSeq) <*>
        parseNest) <|>>
    (Inv <$> (parseRangeLoc <* string "inv") <*> (optionMaybe parseStrOrNum) <*> parseNest) <|>>
    (Conv <$> (parseRangeLoc <* string "con") <*> parseFarLoc <*> parseNest)
