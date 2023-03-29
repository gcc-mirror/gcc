(* Copyright (C) 2005 Free Software Foundation, Inc. *)
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

MODULE setcritical ;

FROM libc IMPORT printf, exit ;

TYPE
   tokenum = (eoftok, plustok, minustok, timestok, dividetok, becomestok, ambersandtok, periodtok, commatok, semicolontok, lparatok, rparatok, lsbratok, rsbratok, lcbratok, rcbratok, uparrowtok, singlequotetok, equaltok, hashtok, lesstok, greatertok, lessgreatertok, lessequaltok, greaterequaltok, periodperiodtok, colontok, doublequotestok, bartok, andtok, arraytok, begintok, bytok, casetok, consttok, definitiontok, divtok, dotok, elsetok, elsiftok, endtok, exittok, exporttok, fortok, fromtok, iftok, implementationtok, importtok, intok, looptok, modtok, moduletok, nottok, oftok, ortok, pointertok, proceduretok, qualifiedtok, unqualifiedtok, recordtok, remtok, repeattok, returntok, settok, thentok, totok, typetok, untiltok, vartok, whiletok, withtok, asmtok, volatiletok, periodperiodperiodtok, datetok, linetok, filetok, attributetok, builtintok, integertok, identtok, realtok, stringtok) ;

   SetOfStop0 = SET OF [eoftok..begintok] ;
   SetOfStop1 = SET OF [bytok..settok] ;
   SetOfStop2 = SET OF [thentok..stringtok] ;

VAR
   e: INTEGER ;


(*
   Assert - 
*)

PROCEDURE Assert (b: BOOLEAN; c: CARDINAL) ;
VAR
   r: INTEGER ;
BEGIN
   IF NOT b
   THEN
      r := printf("assert failed in sets, with bit %d\n", c) ;
      e := 1
   END
END Assert ;

PROCEDURE only0 (s: SetOfStop0; enumValue: tokenum) : BOOLEAN ;
VAR
   i: tokenum ;
   c: CARDINAL ;
BEGIN
   (* just see if one bit has been set *)
   i := eoftok ;
   c := 0 ;
   REPEAT
      IF i IN s
      THEN
         INC(c)
      END ;
      INC(i)
   UNTIL i>begintok ;
   RETURN (c=1) AND (enumValue IN s)
END only0 ;

PROCEDURE only1 (s: SetOfStop1; enumValue: tokenum) : BOOLEAN ;
VAR
   i: tokenum ;
   c: CARDINAL ;
BEGIN
   (* just see if one bit has been set *)
   i := bytok ;
   c := 0 ;
   REPEAT
      IF i IN s
      THEN
         INC(c)
      END ;
      INC(i)
   UNTIL i>settok ;
   RETURN (c=1) AND (enumValue IN s)
END only1 ;

PROCEDURE only2 (s: SetOfStop2; enumValue: tokenum) : BOOLEAN ;
VAR
   i: tokenum ;
   c: CARDINAL ;
BEGIN
   (* just see if one bit has been set *)
   i := thentok ;
   c := 0 ;
   FOR i := thentok TO stringtok DO
      IF i IN s
      THEN
         INC(c)
      END
   END ;
   RETURN (c=1) AND (enumValue IN s)
END only2 ;

VAR
   i : tokenum ;
   s0: SetOfStop0 ;
   s1: SetOfStop1 ;
   s2: SetOfStop2 ;
BEGIN
   e := 0 ;
   s0 := SetOfStop0{} ;
   s1 := SetOfStop1{} ;
   s2 := SetOfStop2{} ;
   FOR i := eoftok TO stringtok DO
      IF ORD(i)<ORD(bytok)
      THEN
         INCL(s0, i) ;
         Assert(only0(s0, i), ORD(i)) ;
         EXCL(s0, i)
      ELSIF ORD(i)<ORD(thentok)
      THEN
         INCL(s1, i) ;
         Assert(only1(s1, i), ORD(i)) ;
         EXCL(s1, i)
      ELSE
         INCL(s2, i) ;
         Assert(only2(s2, i), ORD(i)) ;
         EXCL(s2, i)
      END
   END ;
   exit(e)
END setcritical.
