(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE sets3 ;

FROM DynamicStrings IMPORT String, InitString, ConCat, Mark, ConCatChar ;
FROM M2Printf IMPORT printf1, printf0 ;
FROM M2LexBuf IMPORT GetToken ;
(* FROM M2Reserved IMPORT toktype ; *)


CONST
   Debugging = TRUE ;


TYPE
   toktype = (eoftok, plustok, minustok, timestok, dividetok, becomestok, ambersandtok, periodtok, commatok, semicolontok, lparatok, rparatok, lsbratok, rsbratok, lcbratok, rcbratok, uparrowtok, singlequotetok, equaltok, hashtok, lesstok, greatertok, lessgreatertok, lessequaltok, greaterequaltok, periodperiodtok, colontok, doublequotestok, bartok, andtok, arraytok, begintok, bytok, casetok, consttok, definitiontok, divtok, dotok, elsetok, elsiftok, endtok, exittok, exporttok, fortok, fromtok, iftok, implementationtok, importtok, intok, looptok, modtok, moduletok, nottok, oftok, ortok, pointertok, proceduretok, qualifiedtok, unqualifiedtok, recordtok, repeattok, returntok, settok, thentok, totok, typetok, untiltok, vartok, whiletok, withtok, asmtok, volatiletok, periodperiodperiodtok, datetok, linetok, filetok, integertok, identtok, realtok, stringtok) ;


   SetOfStop0 = SET OF [eoftok..begintok] ;
   SetOfStop1 = SET OF [bytok..thentok] ;
   SetOfStop2 = SET OF [totok..stringtok] ;

VAR
   currenttoken: toktype ;


PROCEDURE ErrorString (s: String) ;
BEGIN
END ErrorString ;

PROCEDURE DescribeError (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
VAR
   str: String ;
BEGIN
   str := InitString('') ;
   CASE currenttoken OF
   
   stringtok: str := ConCat(InitString("syntax error, found `string'"), Mark(str)) |
   realtok: str := ConCat(InitString("syntax error, found `real number'"), Mark(str)) |
   identtok: str := ConCat(InitString("syntax error, found `identifier'"), Mark(str)) |
   integertok: str := ConCat(InitString("syntax error, found `integer number'"), Mark(str)) |
   filetok: str := ConCat(InitString("syntax error, found `__FILE__'"), Mark(str)) |
   linetok: str := ConCat(InitString("syntax error, found `__LINE__'"), Mark(str)) |
   datetok: str := ConCat(InitString("syntax error, found `__DATE__'"), Mark(str)) |
   periodperiodperiodtok: str := ConCat(InitString("syntax error, found `...'"), Mark(str)) |
   volatiletok: str := ConCat(InitString("syntax error, found `VOLATILE'"), Mark(str)) |
   asmtok: str := ConCat(InitString("syntax error, found `ASM'"), Mark(str)) |
   withtok: str := ConCat(InitString("syntax error, found `WITH'"), Mark(str)) |
   whiletok: str := ConCat(InitString("syntax error, found `WHILE'"), Mark(str)) |
   vartok: str := ConCat(InitString("syntax error, found `VAR'"), Mark(str)) |
   untiltok: str := ConCat(InitString("syntax error, found `UNTIL'"), Mark(str)) |
   typetok: str := ConCat(InitString("syntax error, found `TYPE'"), Mark(str)) |
   totok: str := ConCat(InitString("syntax error, found `TO'"), Mark(str)) |
   thentok: str := ConCat(InitString("syntax error, found `THEN'"), Mark(str)) |
   settok: str := ConCat(InitString("syntax error, found `SET'"), Mark(str)) |
   returntok: str := ConCat(InitString("syntax error, found `RETURN'"), Mark(str)) |
   repeattok: str := ConCat(InitString("syntax error, found `REPEAT'"), Mark(str)) |
   recordtok: str := ConCat(InitString("syntax error, found `RECORD'"), Mark(str)) |
   unqualifiedtok: str := ConCat(InitString("syntax error, found `UNQUALIFIED'"), Mark(str)) |
   qualifiedtok: str := ConCat(InitString("syntax error, found `QUALIFIED'"), Mark(str)) |
   proceduretok: str := ConCat(InitString("syntax error, found `PROCEDURE'"), Mark(str)) |
   pointertok: str := ConCat(InitString("syntax error, found `POINTER'"), Mark(str)) |
   ortok: str := ConCat(InitString("syntax error, found `OR'"), Mark(str)) |
   oftok: str := ConCat(InitString("syntax error, found `OF'"), Mark(str)) |
   nottok: str := ConCat(InitString("syntax error, found `NOT'"), Mark(str)) |
   moduletok: str := ConCat(InitString("syntax error, found `MODULE'"), Mark(str)) |
   modtok: str := ConCat(InitString("syntax error, found `MOD'"), Mark(str)) |
   looptok: str := ConCat(InitString("syntax error, found `LOOP'"), Mark(str)) |
   intok: str := ConCat(InitString("syntax error, found `IN'"), Mark(str)) |
   importtok: str := ConCat(InitString("syntax error, found `IMPORT'"), Mark(str)) |
   implementationtok: str := ConCat(InitString("syntax error, found `IMPLEMENTATION'"), Mark(str)) |
   iftok: str := ConCat(InitString("syntax error, found `IF'"), Mark(str)) |
   fromtok: str := ConCat(InitString("syntax error, found `FROM'"), Mark(str)) |
   fortok: str := ConCat(InitString("syntax error, found `FOR'"), Mark(str)) |
   exporttok: str := ConCat(InitString("syntax error, found `EXPORT'"), Mark(str)) |
   exittok: str := ConCat(InitString("syntax error, found `EXIT'"), Mark(str)) |
   endtok: str := ConCat(InitString("syntax error, found `END'"), Mark(str)) |
   elsiftok: str := ConCat(InitString("syntax error, found `ELSIF'"), Mark(str)) |
   elsetok: str := ConCat(InitString("syntax error, found `ELSE'"), Mark(str)) |
   dotok: str := ConCat(InitString("syntax error, found `DO'"), Mark(str)) |
   divtok: str := ConCat(InitString("syntax error, found `DIV'"), Mark(str)) |
   definitiontok: str := ConCat(InitString("syntax error, found `DEFINITION'"), Mark(str)) |
   consttok: str := ConCat(InitString("syntax error, found `CONST'"), Mark(str)) |
   casetok: str := ConCat(InitString("syntax error, found `CASE'"), Mark(str)) |
   bytok: str := ConCat(InitString("syntax error, found `BY'"), Mark(str)) |
   begintok: str := ConCat(InitString("syntax error, found `BEGIN'"), Mark(str)) |
   arraytok: str := ConCat(InitString("syntax error, found `ARRAY'"), Mark(str)) |
   andtok: str := ConCat(InitString("syntax error, found `AND'"), Mark(str)) |
   colontok: str := ConCat(InitString("syntax error, found `:'"), Mark(str)) |
   periodperiodtok: str := ConCat(InitString("syntax error, found `..'"), Mark(str)) |
   greaterequaltok: str := ConCat(InitString("syntax error, found `>='"), Mark(str)) |
   lessequaltok: str := ConCat(InitString("syntax error, found `<='"), Mark(str)) |
   lessgreatertok: str := ConCat(InitString("syntax error, found `<>'"), Mark(str)) |
   hashtok: str := ConCat(InitString("syntax error, found `#'"), Mark(str)) |
   equaltok: str := ConCat(InitString("syntax error, found `='"), Mark(str)) |
   uparrowtok: str := ConCat(InitString("syntax error, found `^'"), Mark(str)) |
   semicolontok: str := ConCat(InitString("syntax error, found `;'"), Mark(str)) |
   commatok: str := ConCat(InitString("syntax error, found `,'"), Mark(str)) |
   periodtok: str := ConCat(InitString("syntax error, found `.'"), Mark(str)) |
   ambersandtok: str := ConCat(InitString("syntax error, found `&'"), Mark(str)) |
   dividetok: str := ConCat(InitString("syntax error, found `/'"), Mark(str)) |
   timestok: str := ConCat(InitString("syntax error, found `*'"), Mark(str)) |
   minustok: str := ConCat(InitString("syntax error, found `-'"), Mark(str)) |
   plustok: str := ConCat(InitString("syntax error, found `+'"), Mark(str)) |
   doublequotestok: str := ConCat(ConCatChar(ConCatChar(InitString("syntax error, found '"), '"'), "'"), Mark(str)) |
   singlequotetok: str := ConCat(ConCatChar(ConCatChar(InitString('syntax error, found "'), "'"), '"'), Mark(str)) |
   greatertok: str := ConCat(InitString("syntax error, found `>'"), Mark(str)) |
   lesstok: str := ConCat(InitString("syntax error, found `<'"), Mark(str)) |
   rparatok: str := ConCat(InitString("syntax error, found `)'"), Mark(str)) |
   lparatok: str := ConCat(InitString("syntax error, found `('"), Mark(str)) |
   rcbratok: str := ConCat(InitString("syntax error, found `}'"), Mark(str)) |
   lcbratok: str := ConCat(InitString("syntax error, found `{'"), Mark(str)) |
   rsbratok: str := ConCat(InitString("syntax error, found `]'"), Mark(str)) |
   lsbratok: str := ConCat(InitString("syntax error, found `['"), Mark(str)) |
   bartok: str := ConCat(InitString("syntax error, found `|'"), Mark(str)) |
   becomestok: str := ConCat(InitString("syntax error, found `:='"), Mark(str)) |
   eoftok: str := ConCat(InitString("syntax error, found `'"), Mark(str))
   ELSE
   END ;
   ErrorString(str) ;
END DescribeError ;


(*
   SyntaxError - after a syntax error we skip all tokens up until we reach
                 a stop symbol.
*)
(*
PROCEDURE SyntaxError (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   DescribeError(stopset0, stopset1, stopset2) ;
   IF Debugging
   THEN
      printf0('\nskipping token *** ')
   END ;
   (*
      yes the ORD(currenttoken) looks ugly, but it is *much* safer than
      using currenttoken<sometok as a change to the ordering of the
      token declarations below would cause this to break. Using ORD() we are
      immune from such changes
   *)
   WHILE NOT (((ORD(currenttoken)<32)  AND (currenttoken IN stopset0)) OR
              ((ORD(currenttoken)>=32) AND (ORD(currenttoken)<64) AND (currenttoken IN stopset1)) OR
              ((ORD(currenttoken)>=64) AND (currenttoken IN stopset2)))
   DO
      GetToken
   END ;
   IF Debugging
   THEN
      printf0(' ***\n')
   END
END SyntaxError ;
*)


BEGIN
END sets3.