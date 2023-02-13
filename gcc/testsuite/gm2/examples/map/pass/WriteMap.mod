(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)
IMPLEMENTATION MODULE WriteMap ;



IMPORT Break ;

FROM StrIO IMPORT WriteLn, WriteString ;

FROM NumberIO IMPORT WriteCard ;

FROM RoomMap IMPORT NoOfRooms, Rooms, DoorStatus ;


(*
   WriteMapText - writes out the map in textual form.
*)

PROCEDURE WriteMapText ;
VAR
   i: CARDINAL ;
BEGIN
   MakeRoomNumbers ;
   FOR i := 1 TO NoOfRooms DO
      IF RoomExists(i)
      THEN
         WriteRoom(i)
      END
   END ;
   WriteString('END.') ; WriteLn
END WriteMapText ;


(*
   MakeRoomNumbers - makes room numbers for the rooms that exist.
*)

PROCEDURE MakeRoomNumbers ;
VAR
   i, j: CARDINAL ;
BEGIN
   j := 1 ;
   FOR i := 1 TO NoOfRooms DO
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


END WriteMap.
