import Control.Monad.IO.Class
import Data.Char
import Data.Maybe
import Data.List
import System.IO

data BFCompressed = BFCTokens BFTokens Int | BFCLoop [BFCompressed]
	deriving Show
data BFTokens = BFLeft | BFRight | BFInc | BFDec | BFPrint | BFInput | BFLoop [BFTokens] | BFError String
	deriving Show

instance Eq BFTokens where
	t1 == t2 = case t1 of
		BFLeft -> case t2 of {BFLeft -> True; _ -> False}
		BFRight -> case t2 of {BFRight -> True; _ -> False}
		BFInc -> case t2 of {BFInc -> True; _ -> False}
		BFDec -> case t2 of {BFDec -> True; _ -> False}
		_ -> False

data BFState = BFState [Int] Int [Int]
	deriving Show

-- fja koja bi trebala uzastopne operacije zdrobiti u jednu
bfCompress :: [BFTokens] -> [BFCompressed]
-- TODO ruzno je i pretpostavlja da prvi token nije loop, fail
bfCompress tokens = reverse $ foldl func [BFCTokens (head tokens) 1] $ tail tokens
	where
		func = (\(l:ls) t -> case t of
			BFLoop ts -> BFCLoop (bfCompress ts) : l : ls
			_ -> case l of
				BFCTokens tt num ->
					if tt == t then (BFCTokens t (num+1)) : ls else (BFCTokens t 1) : l : ls
				_ -> BFCTokens t 1 : l : ls)


-- pocetno stanje defaultno -- TODO je dodati mogucnost dodavanja pocetnog stanja
bfInit = BFState [] 0 []

-- interpreter
bfRun [] s = return s
bfRun (t:ts) s@(BFState ls p rs) = do
		--putStrLn $ (show t) ++ " " ++ (show s)
		x <- case t of
			BFLeft -> return $ case ls of
				[] -> BFState [] 0 (p:rs)
				_ -> BFState (tail ls) (head ls) (p:rs)
			BFRight -> return $ case rs of
				[] -> BFState (p:ls) 0 []
				_ -> BFState (p:ls) (head rs) (tail rs)
			BFInc -> return $ BFState ls (p+1) rs
			BFDec -> return $ BFState ls (p-1) rs
			BFPrint -> do {putStrLn (show (chr p)); return s}
			BFInput -> do {x <- getChar; getChar; return $ BFState ls (ord x) rs}
			BFLoop tkns -> if p == 0 then return s
									 else bfRun (tkns ++ [BFLoop tkns]) s
			_ -> error "Ovo se nesme dogoditi!"
		bfRun ts x


bfParse [] = []
bfParse (s:ss) = case s of
	'<' -> BFLeft : bfParse ss
	'>' -> BFRight : bfParse ss
	'+' -> BFInc : bfParse ss
	'-' -> BFDec : bfParse ss
	'.' -> BFPrint : bfParse ss
	',' -> BFInput : bfParse ss
	'[' -> let idx = findClosingBracket ss
		   (beg, end) = splitAt (idx) ss
		   in (BFLoop (bfParse beg)) : bfParse (tail end)
	']' -> [BFError (s:ss)]
	_	-> bfParse ss
	where
		findClosingBracket ss = fcs 0 1 ss
			where fcs idx i (s:ss) = case s of
				']' -> if i == 1 then idx else fcs (idx+1) (i-1) ss
				'[' -> fcs (idx+1) (i+1) ss
				_ -> fcs (idx+1) i ss

r str = do
		 -- TODO: prvo splittam na ! tak da mogu inicijalizirati memoriju
	   case bfCheck str of
		 False -> print "fakof"
		 _ -> putStr ""
	   let tokens = bfParse str
	   print $ bfCompress tokens
	   x <- bfRun tokens bfInit
	   print x

main = do
		 -- TODO: omoguciti citanje iz fajlova
	   string <- getLine
	   r string

-- provjeravamo dal smo zatvorili zagradu prerano ili dal je na kraju izbalansirano
bfCheck str = if any (<0) list || last list /= 0
							then False
							else True
	where list = scanl (+) 0 $ map (\x -> case x of
		'[' -> 1
		']' -> -1
		_ -> 0) str
