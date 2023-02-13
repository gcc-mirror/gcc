(* Copyright (C) 2015 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE Options ;  (*!m2pim+gm2*)

FROM DynamicStrings IMPORT String, InitString, KillString ;
FROM StringConvert IMPORT stoc ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT Write ;
FROM ASCII IMPORT nul ;
FROM GetOpt IMPORT GetOpt ;
FROM libc IMPORT printf, exit ;
FROM Chance IMPORT GetSeed, SetSeed ;
FROM WriteMap IMPORT SetOutputFile ;

FROM BoxMap IMPORT MaxX, MaxY,
                   MinRoomLength, MaxRoomLength,
		   MinCorridorLength, MaxCorridorLength, TotalCorridorLength ;

IMPORT UnixArgs ;


CONST
   programName = "rndpen" ;


(*
   Max -
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a > b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END Max ;


(*
   Min -
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a < b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END Min ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   userX := MaxX ;
   userY := MaxY ;
   userMinRoomLength := MinRoomLength ;
   userMaxRoomLength := MaxRoomLength ;
   userMinCorridorLength := MinCorridorLength ;
   userMaxCorridorLength := MaxCorridorLength ;
   userTotalCorridorLength := TotalCorridorLength
END Init ;


(*
   CheckLimits -
*)

PROCEDURE CheckLimits ;
VAR
   l: CARDINAL ;
BEGIN
   l := Min (userX, userY) ;
   userMaxRoomLength := Min (l, userMaxRoomLength) ;
   userMinRoomLength := Min (userMinRoomLength, userMaxRoomLength) ;
   userMaxCorridorLength := Min (l, userMaxCorridorLength) ;
   userMinCorridorLength := Min (userMinCorridorLength, userMaxCorridorLength)
END CheckLimits ;


(*
   help -
*)

PROCEDURE help (code: INTEGER) ;
BEGIN
   printf ("Usage %s [-a minroomsize] [-b maxroomsize] [-c mincorridorlength] [-d maxcorridorlength] [-e totalcorridorlength] [-h] [-o outputfile] [-s seed] [-x maxx] [-y maxy]\n", programName) ;
   printf ("  -a minroomsize            (default is %d)\n", MinRoomLength) ;
   printf ("  -b maxroomsize            (default is %d)\n", MaxRoomLength) ;
   printf ("  -c mincorridorsize        (default is %d)\n", MinCorridorLength) ;
   printf ("  -d maxcorridorsize        (default is %d)\n", MaxCorridorLength) ;
   printf ("  -e totalcorridorlength    (default is %d)\n", TotalCorridorLength) ;
   printf ("  -o outputfile             (default is stdout)\n") ;
   printf ("  -s seed                   (default is %d)\n", GetSeed ()) ;
   printf ("  -x minx for whole map     (default is %d)\n", MaxX) ;
   printf ("  -y maxy for whole map     (default is %d)\n", MaxY) ;
   exit (code)
END help ;


(*
   HandleOptions -
*)

PROCEDURE HandleOptions ;
VAR
   optind,
   opterr,
   optopt: INTEGER ;
   arg,
   s, l  : String ;
   ch    : CHAR ;
BEGIN
   l := InitString (':a:b:c:d:e:o:s:hx:y:') ;
   s := NIL ;
   arg := NIL ;
   ch := GetOpt (UnixArgs.GetArgC (), UnixArgs.GetArgV (), l,
                 arg, optind, opterr, optopt) ;
   WHILE ch # nul DO
      CASE ch OF

      'a':  userMinRoomLength := stoc (arg) |
      'b':  userMaxRoomLength := stoc (arg) |
      'c':  userMinCorridorLength := stoc (arg) |
      'd':  userMaxCorridorLength := stoc (arg) |
      'e':  userTotalCorridorLength := stoc (arg) |
      'h':  help (0) |
      'o':  SetOutputFile(arg) |
      's':  SetSeed (stoc (arg)) |
      'x':  userX := stoc (arg) |
      'y':  userY := stoc (arg) |
      '?':  printf ("illegal option\n") ; help (1)

      ELSE
         WriteString ("unrecognised option '-") ; Write (ch) ; WriteString ('"') ; WriteLn ;
         exit (1)
      END ;
      arg := KillString (arg) ;
      ch := GetOpt (UnixArgs.GetArgC (), UnixArgs.GetArgV (), l,
                    arg, optind, opterr, optopt)
   END ;
   CheckLimits
END HandleOptions ;


BEGIN
   Init
END Options.
