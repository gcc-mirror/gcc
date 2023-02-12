(* Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE program ;


TYPE
   tokenset = (eoftok, plustok, minustok, timestok, dividetok,
becomestok, ambersandtok, periodtok, commatok, semicolontok, lparatok,
rparatok, lsbratok, rsbratok, lcbratok, rcbratok, uparrowtok,
singlequotetok, equaltok, hashtok, lesstok, greatertok,
lessgreatertok, lessequaltok, greaterequaltok, periodperiodtok,
colontok, doublequotestok, bartok, andtok, arraytok, begintok, bytok,
casetok, consttok, definitiontok, divtok, dotok, elsetok, elsiftok,
endtok, exittok, exporttok, fortok, fromtok, iftok, implementationtok,
importtok, intok, looptok, modtok, moduletok, nottok, oftok, ortok,
pointertok, proceduretok, qualifiedtok, unqualifiedtok, recordtok,
repeattok, returntok, settok, thentok, totok, typetok, untiltok,
vartok, whiletok, withtok, asmtok, volatiletok, periodperiodperiodtok,
datetok, linetok, filetok, attributetok, builtintok, integertok,
identtok, realtok, stringtok) ;

TYPE
   SetOfStop0 = SET OF [eoftok..begintok] ;
   SetOfStop1 = SET OF [bytok..thentok] ;
   SetOfStop2 = SET OF [totok..stringtok] ;

VAR
   currenttoken: tokenset ;


PROCEDURE ProgramModule (stopset0: SetOfStop0; stopset1: SetOfStop1; stopset2: SetOfStop2) ;
BEGIN
   Expect(moduletok, stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
(*
   PushAutoOn  ;
   Ident(stopset0 + SetOfStop0{semicolontok, lsbratok}, stopset1, stopset2) ;
   P3StartBuildProgModule  ;
   PushAutoOff  ;
   IF currenttoken=lsbratok
   THEN
      Priority(stopset0 + SetOfStop0{semicolontok}, stopset1, stopset2) ;
   END ;
   Expect(semicolontok, stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{importtok, fromtok, endtok, consttok, proceduretok, moduletok}, stopset2 + SetOfStop2{typetok, vartok}) ;
   WHILE ((currenttoken>=bytok) AND (currenttoken<totok) AND (currenttoken IN SetOfStop1 {fromtok, importtok})) DO
      Import(stopset0 + SetOfStop0{begintok}, stopset1 + SetOfStop1{endtok, fromtok, importtok, consttok, moduletok, proceduretok}, stopset2 + SetOfStop2{vartok, typetok}) ;
   END (* while *) ;
   StartBuildInit  ;
   Block(stopset0, stopset1, stopset2 + SetOfStop2{identtok}) ;
   PushAutoOn  ;
   Ident(stopset0 + SetOfStop0{periodtok}, stopset1, stopset2) ;
   EndBuildFile  ;
   P3EndBuildProgModule  ;
   Expect(periodtok, stopset0, stopset1, stopset2) ;
   PopAuto ;
   EndBuildInit ;
   PopAuto
*)
END ProgramModule ;


PROCEDURE PushAutoOn ; BEGIN END PushAutoOn ;
PROCEDURE PushAutoOff ; BEGIN END PushAutoOff ;
PROCEDURE PopAuto ; BEGIN END PopAuto ;
PROCEDURE P3StartBuildProgModule ; BEGIN END P3StartBuildProgModule ;
PROCEDURE P3EndBuildProgModule ; BEGIN END P3EndBuildProgModule ;
PROCEDURE StartBuildInit ; BEGIN END StartBuildInit ;
PROCEDURE EndBuildInit ; BEGIN END EndBuildInit ;
PROCEDURE EndBuildFile ; BEGIN END EndBuildFile ;


PROCEDURE Priority (s0: SetOfStop0; s1: SetOfStop1; s2: SetOfStop2) ;
BEGIN
END Priority ;

PROCEDURE Import (s0: SetOfStop0; s1: SetOfStop1; s2: SetOfStop2) ;
BEGIN
END Import ;

PROCEDURE Ident (s0: SetOfStop0; s1: SetOfStop1; s2: SetOfStop2) ;
BEGIN
END Ident ;

PROCEDURE Expect (t: tokenset; s0: SetOfStop0; s1: SetOfStop1; s2: SetOfStop2) ;
BEGIN
END Expect ;

PROCEDURE Block (s0: SetOfStop0; s1: SetOfStop1; s2: SetOfStop2) ;
BEGIN
END Block ;

BEGIN
   ProgramModule(SetOfStop0{}, SetOfStop1{}, SetOfStop2{})
END program.
