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

MODULE multtypes ;


CONST
   MaxNoOfRooms           = 350 ;  (* An upper limit *)
   WallsPerRoom           =   8 ;  (* An upper limit *)
   DoorsPerRoom           =   6 ;  (* An upper limit *)
   MaxNoOfTreasures       =  15 ;  (* An upper limit *)

TYPE
   Line         = RECORD
                     X1 : CARDINAL ;
                     Y1 : CARDINAL ;
                     X2 : CARDINAL ;
                     Y2 : CARDINAL
                  END ;

   DoorStatus   = (Open, Closed, Secret) ;

   Door         = RECORD
                     Position    : Line ;
                     StateOfDoor : DoorStatus ;
                     LeadsTo     : CARDINAL
                  END ;

   TreasureInfo = RECORD
                     Xpos         : CARDINAL ;
                     Ypos         : CARDINAL ;
                     Rm           : CARDINAL ;
                     Tweight      : CARDINAL ;
                     TreasureName : ARRAY [0..12] OF CHAR
                  END ;

   Room         = RECORD
                     RoomNo      : CARDINAL ;
                     NoOfWalls   : CARDINAL ;
                     NoOfDoors   : CARDINAL ;
                     Walls       : ARRAY [1..WallsPerRoom] OF Line ;
                     Doors       : ARRAY [1..DoorsPerRoom] OF Door ;
                     (* Treasures   : BITSET ; *)
                  END ;



VAR
   NoOfRooms       : CARDINAL ;
   Treasure        : ARRAY [1..MaxNoOfTreasures] OF TreasureInfo ;
   Rooms           : ARRAY [1..MaxNoOfRooms] OF Room ;

BEGIN
END multtypes.
