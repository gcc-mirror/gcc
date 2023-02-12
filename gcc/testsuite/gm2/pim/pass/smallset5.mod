(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE smallset5 ;

FROM libc IMPORT exit ;


TYPE
   tokens = (eoftok, plustok, minustok, timestok, dividetok, becomestok, ambersandtok, periodtok, commatok, semicolontok, lparatok, rparatok, lsbratok, rsbratok, lcbratok, rcbratok, uparrowtok, singlequotetok, equaltok, hashtok, lesstok, greatertok, lessgreatertok, lessequaltok, greaterequaltok, periodperiodtok, colontok, doublequotestok, bartok, andtok, arraytok, begintok, bytok, casetok, consttok, definitiontok, divtok, dotok, elsetok, elsiftok, endtok, exittok, exporttok, fortok, fromtok, iftok, implementationtok, importtok, intok, looptok, modtok, moduletok, nottok, oftok, ortok, pointertok, proceduretok, qualifiedtok, unqualifiedtok, recordtok, repeattok, returntok, settok, thentok, totok, typetok, untiltok, vartok, whiletok, withtok, asmtok, volatiletok, periodperiodperiodtok, datetok, linetok, filetok, integertok, identtok, realtok, stringtok) ;

   SetOfStop0 = SET OF [eoftok..begintok] ;
   SetOfStop1 = SET OF [bytok..thentok] ;
   SetOfStop2 = SET OF [totok..stringtok] ;
   
VAR
   s0, s4: SetOfStop0 ;
   s1: SetOfStop1 ;
   s2: SetOfStop2 ;
BEGIN
   s0 := SetOfStop0{eoftok} ;
   s0 := s0 + SetOfStop0{plustok} ;

   IF s0#SetOfStop0{eoftok, plustok}
   THEN
      exit(1)
   END ;

   s4 := SetOfStop0{timestok, becomestok} ;
   s0 := s0 + s4 ;
   IF s0#SetOfStop0{eoftok, plustok, timestok, becomestok}
   THEN
      exit(2)
   END
END smallset5.
