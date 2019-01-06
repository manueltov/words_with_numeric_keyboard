-- Manuel Tovar
-- FC49522

import Data.List.Split

--Funcao que converte uma string de numeros e espacos numa string de letras
teclasParaPalavra :: [Char] -> [Char]
teclasParaPalavra [] = []
teclasParaPalavra xs = numerosParaLetras (splitOn " " (meteEspacos xs))

--Funcao auxiliar recursiva que chama a funcao que converte cada numero numa letra
numerosParaLetras :: [[Char]] -> [Char]
numerosParaLetras [] = []
numerosParaLetras (x:xs) = (umNumParaUmLetra x)++(numerosParaLetras xs)

--Funcao aulixiar que vai buscar o carracter correspondente ao numero inserido
umNumParaUmLetra :: [Char] -> [Char]
umNumParaUmLetra n = [ b | (a,b) <- possibilidades , n==a ]

--Possibilidade de conversao de letras possiveis
--Caso se queira adicionar o numero 1 e o 0 assim eh mais facil
possibilidades = [("2",'A'), ("22",'B'), ("222",'C'), ("3",'D'),
				("33", 'E'), ("333",'F'), ("4",'G'), ("44",'H'),
				("444",'I'), ("5",'J'), ("55",'K'), ("555",'L'),
				("6",'M'), ("66",'N'), ("666",'O'), ("7",'P'),
				("77",'Q'), ("777",'R'), ("7777",'S'), ("8",'T'),
				("88",'U'), ("888",'V'), ("9",'W'), ("99",'X'),
				("999",'X'), ("9999",'Z')]

--Funcao auxiliar que coloca um espaco entre algarismos diferentes
meteEspacos :: [Char] -> [Char]
meteEspacos [] = []
meteEspacos (a:[]) = [a]
meteEspacos (a:b:xs) =
	if b /= a
	then a:' ':b:meteEspacos xs
	else a:b:meteEspacos xs
