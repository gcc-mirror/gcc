(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
(* This file is part of Chisel.

Chisel is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

Chisel is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE WriteMap ;


FROM DynamicStrings IMPORT String, EqualArray ;
FROM RoomMap IMPORT MaxNoOfRooms, Rooms, DoorStatus ;
FROM NumberIO IMPORT CardToStr ;

IMPORT FIO ;
IMPORT SFIO ;


VAR
   outfile: FIO.File ;


(*
   SetOutputFile - set the output file to, name.
*)

PROCEDURE SetOutputFile (name: String) ;
BEGIN
   IF EqualArray (name, '-')
   THEN
      outfile := FIO.StdOut
   ELSE
      outfile := SFIO.OpenToWrite(name)
   END
END SetOutputFile ;


(*
   WriteCard -
*)

PROCEDURE WriteCard (c, n: CARDINAL) ;
VAR
   a: ARRAY [0..50] OF CHAR ;
BEGIN
   CardToStr(c, n, a) ;
   WriteString(a)
END WriteCard ;


(*
   WriteString -
*)

PROCEDURE WriteString (a: ARRAY OF CHAR) ;
BEGIN
   FIO.WriteString(outfile, a)
END WriteString ;


(*
   WriteLn -
*)

PROCEDURE WriteLn ;
BEGIN
   FIO.WriteLine(outfile)
END WriteLn ;


(*
   WriteMapText - writes out the map in textual form.
*)

PROCEDURE WriteMapText ;
VAR
   i: CARDINAL ;
BEGIN
   MakeRoomNumbers ;
   FOR i := 1 TO MaxNoOfRooms DO
      IF RoomExists(i)
      THEN
         WriteRoom(i)
      END
   END ;
   WriteString('END.') ; WriteLn ; FIO.Close(outfile)
END WriteMapText ;


(*
   MakeRoomNumbers - makes room numbers for the rooms that exist.
*)

PROCEDURE MakeRoomNumbers ;
VAR
   i, j: CARDINAL ;
BEGIN
   j := 1 ;
   FOR i := 1 TO MaxNoOfRooms DO
      IF RoomExists(i)
      THEN
         Rooms[i].RoomNo := j ;
         INC(j)
      END
   END
END MakeRoomNumbers ;


(*
   WriteRoom - writes out the room coordinates.
*)

PROCEDURE WriteRoom (r: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   WITH Rooms[r] DO
      WriteString('ROOM') ; WriteCard(RoomNo, 4) ; WriteLn ;
      WriteString('WALL') ; WriteLn ;
      FOR i := 1 TO NoOfWalls DO
         WITH Walls[i] DO
            WriteCard(X1, 8) ; WriteCard(Y1, 4) ;
            WriteCard(X2, 4) ; WriteCard(Y2, 4) ; WriteLn
         END
      END ;
      FOR i := 1 TO NoOfDoors DO
         WriteString('DOOR') ;
         WITH Doors[i] DO
            WITH Position DO
               WriteCard(X1, 4) ; WriteCard(Y1, 4) ;
               WriteCard(X2, 4) ; WriteCard(Y2, 4)
            END ;
            WriteString('  STATUS ') ;
            WriteStatus(StateOfDoor) ;
            WriteString(' LEADS TO') ;
            WriteCard(Rooms[LeadsTo].RoomNo, 4) ; WriteLn
         END
      END ;
      WriteString('END') ; WriteLn
   END
END WriteRoom ;


(*
   WriteStatus - writes the status of a door.
*)

PROCEDURE WriteStatus (s: DoorStatus) ;
BEGIN
   CASE s OF

   Open   : WriteString('OPEN  ') |
   Closed : WriteString('CLOSED') |
   Secret : WriteString('SECRET')

   ELSE
      HALT
   END
END WriteStatus ;


(*
   RoomExists - returns true if a room exists.
*)

PROCEDURE RoomExists (r: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Rooms[r].RoomNo#0 )
END RoomExists ;


BEGIN
   outfile := FIO.StdOut
END WriteMap.
