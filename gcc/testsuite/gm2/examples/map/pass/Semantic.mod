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
MODULE Semantic ;


FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT exit, system ;
FROM StrLib IMPORT StrCopy, StrConCat ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM Args IMPORT GetArg ;
FROM libc IMPORT system ;
(*
FROM FIO IMPORT File, OpenToWrite, Close, Exists, ReportError, WriteShort,
                WriteChar, IsNoError ;
*)
FROM FIO IMPORT File, OpenToWrite, Close, Exists, WriteChar, IsNoError ;

FROM AdvMap IMPORT ReadAdvMap, Rooms, DoorStatus, ActualNoOfRooms,
                   MaxNoOfTreasures, Treasure ;

CONST
   MaxFileName = 4096 ;

VAR
   ErrorInRoom: BOOLEAN ;


PROCEDURE GetOppositeDoor (r, x1, y1, x2, y2: CARDINAL ;
                           VAR doorno: CARDINAL ; VAR ok: BOOLEAN) ;
VAR
   xok, yok: BOOLEAN ;
BEGIN
   ok := FALSE ;
   doorno := 1 ;
   WITH Rooms[r] DO
      WHILE (NOT ok) AND (doorno<=NoOfDoors) DO
         xok :=  (x1=Doors[doorno].Position.X1) AND
                 (x2=Doors[doorno].Position.X2) ;
         yok :=  (y1=Doors[doorno].Position.Y1) AND
                 (y2=Doors[doorno].Position.Y2) ;
         IF xok AND yok
         THEN
            ok := TRUE
         ELSE
            INC( doorno )
         END
      END
   END
END GetOppositeDoor ;


PROCEDURE GetWallOnDoor (r, x1, y1, x2, y2: CARDINAL ;
                         VAR ok: BOOLEAN) ;
VAR
   wallno: CARDINAL ;
BEGIN
   ok := FALSE ;
   wallno := 1 ;
   WITH Rooms[r] DO
      WHILE (NOT ok) AND (wallno<=NoOfWalls) DO
         WITH Walls[wallno] DO
            IF (Walls[wallno].X1=x1) AND (Walls[wallno].X2=x2)
            THEN
               IF (Walls[wallno].Y1<=y1) AND (Walls[wallno].Y2>=y2)
               THEN
                  ok := TRUE
               END
            END ;
            IF (Walls[wallno].Y1=y1) AND (Walls[wallno].Y2=y2)
            THEN
               IF (Walls[wallno].X1<=x1) AND (Walls[wallno].X2>=x2)
               THEN
                  ok := TRUE
               END
            END ;
            INC( wallno )
         END
      END
   END
END GetWallOnDoor ;


PROCEDURE HorizWallOnDoor (r, x1, y1: CARDINAL ;
                           VAR ok: BOOLEAN) ;
VAR
   wallno: CARDINAL ;
BEGIN
   ok := FALSE ;
   wallno := 1 ;
   WITH Rooms[r] DO
      WHILE (NOT ok) AND (wallno<=NoOfWalls) DO
         WITH Walls[wallno] DO
            IF (Walls[wallno].X1=Walls[wallno].X2) AND (x1=Walls[wallno].X1)
            THEN
               IF (Walls[wallno].Y1<=y1) AND (Walls[wallno].Y2>=y1)
               THEN
                  ok := TRUE
               END
            END
         END ;
         INC( wallno )
      END
   END
END HorizWallOnDoor ;


PROCEDURE VertWallOnDoor (r, x1, y1: CARDINAL ;
                          VAR ok: BOOLEAN) ;
VAR
   wallno: CARDINAL ;
BEGIN
   ok := FALSE ;
   wallno := 1 ;
   WITH Rooms[r] DO
      WHILE (NOT ok) AND (wallno<=NoOfWalls) DO
         WITH Walls[wallno] DO
            IF (Walls[wallno].Y1=Walls[wallno].Y2) AND (y1=Walls[wallno].Y1)
            THEN
               IF (Walls[wallno].X1<=x1) AND (Walls[wallno].X2>=x1)
               THEN
                  ok := TRUE
               END
            END
         END ;
         INC( wallno )
      END
   END
END VertWallOnDoor ;


(*
   AnalyzeSemantic - 
*)

PROCEDURE AnalyzeSemantic ;
VAR
   room: CARDINAL ;
BEGIN
   FOR room := 1 TO ActualNoOfRooms DO
      AnalyzeRoom(room)
   END
END AnalyzeSemantic ;


(*
   AnalyzeRoom - 
*)

PROCEDURE AnalyzeRoom (room: CARDINAL) ;
VAR
   door: CARDINAL ;
BEGIN
   WITH Rooms[room] DO
      IF NoOfDoors#0
      THEN
         FOR door := 1 TO NoOfDoors DO
            AnalyzeDoor(room, door)
         END
      END
   END
END AnalyzeRoom ;


(*
   AnalyzeDoor - 
*)

PROCEDURE AnalyzeDoor (room, door: CARDINAL) ;
VAR
   OtherDoor,
   i        : CARDINAL ;
   ok       : BOOLEAN ;
BEGIN
   WITH Rooms[room] DO
      WITH Doors[door] DO
         IF LeadsTo#0
         THEN
            GetOppositeDoor( LeadsTo, Position.X1, Position.Y1,
                             Position.X2, Position.Y2 ,OtherDoor, ok ) ;
            IF ok
            THEN
               IF StateOfDoor#Rooms[LeadsTo].Doors[OtherDoor].StateOfDoor
               THEN
                  WriteString('Inconsistant Door STATUS in room') ;
                  WriteCard( room, 6 ) ; WriteString('Door NO.') ;
                  WriteCard( door, 6 ) ; WriteLn ;
                  ErrorInRoom := TRUE
               END
            ELSE
               WriteString('Inconsistant Door LEADSTO in room') ;
               WriteCard( room, 6 ) ; WriteString('  Door NO.') ;
               WriteCard( door, 6 ) ; WriteString('  - OR -') ;WriteLn ;
               WriteString('Inconsistant Door COORDS in room') ;
               WriteCard( room, 6 ) ; WriteString('  Door NO.') ;
               WriteCard( door, 6 ) ; WriteLn ;
               ErrorInRoom := TRUE
            END ;
            GetWallOnDoor( room, Position.X1, Position.Y1,
                           Position.X2, Position.Y2, ok ) ;
            IF NOT ok
            THEN
               WriteString('Door NOT ON WALL in room') ;
               WriteCard( room, 6 ) ; WriteString('  Door NO.') ;
               WriteCard( door, 6 ) ; WriteLn ;
               ErrorInRoom := TRUE
            END ;
            IF Position.X1=Position.X2
            THEN
               i := Position.Y1 ;
               REPEAT
                  VertWallOnDoor( LeadsTo, Position.X1, i, ok ) ;
                  INC( i ) ;
               UNTIL ok OR (i>Position.Y2)
            ELSE
               i := Position.X1 ;
               REPEAT
                  HorizWallOnDoor( LeadsTo, i, Position.Y1, ok ) ;
                  INC( i )
               UNTIL ok OR (i>Position.X2)
            END ;
            IF ok
            THEN
               WriteString('Adjacent Room CONFLICT with DOOR in ROOM') ;
               WriteCard( room, 6 ) ; WriteString('  Door NO.') ;
               WriteCard( door, 6 ) ; WriteLn ;
               WriteString('Adjacent Room is') ; WriteCard( LeadsTo, 6 ) ;
               WriteLn ;
               ErrorInRoom := TRUE
            END
         END
      END
   END
END AnalyzeDoor ;


(*
(*
   CrunchRooms - 
*)

PROCEDURE CrunchRooms (f: File) ;
VAR
   room: CARDINAL ;
BEGIN
   WriteShort(f, ActualNoOfRooms) ;
   FOR room := 1 TO ActualNoOfRooms DO
      CrunchRoom(f, room)
   END
END CrunchRooms ;


(*
   CrunchRoom - 
*)

PROCEDURE CrunchRoom (f: File; room: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   WITH Rooms[room] DO
      WriteShort(f, NoOfWalls) ;
      FOR i := 1 TO NoOfWalls DO
         CrunchWall(f, room, i)
      END ;
      WriteShort(f, NoOfDoors) ;
      FOR i := 1 TO NoOfDoors DO
         CrunchDoor(f, room, i)
      END
   END
END CrunchRoom ;


(*
   CrunchDoor - 
*)

PROCEDURE CrunchDoor (f: File; room: CARDINAL; doorno: CARDINAL) ;
BEGIN
   WITH Rooms[room].Doors[doorno] DO
      WriteShort(f, Position.X1) ;
      WriteShort(f, Position.Y1) ;
      WriteShort(f, Position.X2) ;
      WriteShort(f, Position.Y2) ;
      WriteShort(f, LeadsTo) ;
      WriteChar(f, VAL(CHAR, StateOfDoor))
   END
END CrunchDoor ;


(*
   CrunchWall - 
*)

PROCEDURE CrunchWall (f: File; room: CARDINAL; wallno: CARDINAL) ;
BEGIN
   WITH Rooms[room].Walls[wallno] DO
      WriteShort(f, X1) ;
      WriteShort(f, Y1) ;
      WriteShort(f, X2) ;
      WriteShort(f, Y2)
   END
END CrunchWall ;


(*
   CrunchTreasures - 
*)

PROCEDURE CrunchTreasures (f: File) ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 1 TO MaxNoOfTreasures DO
      WITH Treasure[i] DO
         WriteShort(f, Xpos) ;
         WriteShort(f, Ypos) ;
         WriteShort(f, Rm)
      END
   END
END CrunchTreasures ;


(*
   CrunchMap - 
*)

PROCEDURE CrunchMap (a: ARRAY OF CHAR) ;
VAR
   f: File ;
   c: ARRAY [0..MaxFileName] OF CHAR ;
BEGIN
   StrConCat(a, '.bin', a) ;
   IF Exists(a)
   THEN
      StrCopy('/bin/rm -f ', c) ;
      StrConCat(c, a, c) ;
      IF system(ADR(c))#0
      THEN
         WriteString('failed to ') ; WriteString(c) ; WriteLn ;
         exit(1)
      END
   END ;
   f := OpenToWrite(a) ;
   IF IsNoError(f)
   THEN
      CrunchRooms(f) ;
      CrunchTreasures(f) ;
      Close(f)
   ELSE
      WriteString('error when opening ') ; WriteString(a) ;
      WriteString(' for writing: ') ; ReportError(f) ; WriteLn
   END
END CrunchMap ;
*)

VAR
   FileName: ARRAY [0..MaxFileName] OF CHAR ;
BEGIN
   IF GetArg(FileName, 1)
   THEN
      IF ReadAdvMap(FileName)
      THEN
         ErrorInRoom := FALSE ;
         AnalyzeSemantic ;
(*
         IF NOT ErrorInRoom
         THEN
            CrunchMap(FileName)
         END
*)
      END
   END
END Semantic.
