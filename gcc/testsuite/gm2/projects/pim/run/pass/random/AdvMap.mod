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

IMPLEMENTATION MODULE AdvMap ;

IMPORT StdIO ;

FROM Scan IMPORT WriteError, GetNextSymbol, OpenSource, CloseSource ;
FROM StrIO IMPORT ReadString, WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM StrLib IMPORT StrEqual, StrLen, StrCopy ;
FROM ASCII IMPORT cr, lf, nul, EOL ;


VAR
   CurrentRoom  : CARDINAL ;
   CurrentSymbol: ARRAY [0..20] OF CHAR ;
   FatalError   : BOOLEAN ;


(* IncPosition increments the x,y coordinates according  *)
(* the Direction sent.                                   *)

PROCEDURE IncPosition (VAR x, y: CARDINAL ; Dir: CARDINAL) ;
BEGIN
   IF (Dir=0) AND (y>0)
   THEN
      DEC(y)
   ELSIF Dir=3
   THEN
      INC(x)
   ELSIF Dir=2
   THEN
      INC(y)
   ELSIF x>0
   THEN
      DEC(x)
   END
END IncPosition ;



(* Adjacent tests whether two rooms R1 & R2 are adjacent *)
(* Assume that access to map has been granted.           *)

PROCEDURE Adjacent (R1, R2: CARDINAL) : BOOLEAN ;
VAR
   i, r1, r2 : CARDINAL ;
   ok: BOOLEAN ;
BEGIN
   WITH Rooms[R1] DO
      i := NoOfDoors ;
      ok := FALSE ;
      WHILE (i>0) AND (NOT ok) DO
         IF Doors[i].LeadsTo=R2
         THEN
            ok := TRUE
         ELSE
            DEC(i)
         END
      END
   END ;
   RETURN( ok )
END Adjacent ;


(* The following procedures test and read the syntax marking out the  *)
(* map of the adventure game. Displaying syntactic errors if occurred *)

(*
   ReadAdvMap - read map, Name, into memory.
                TRUE is returned if the operation was successful.
*)

PROCEDURE ReadAdvMap (Name: ARRAY OF CHAR) : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   Success := OpenSource(Name) ;
   IF Success
   THEN
      GetNextSymbol(CurrentSymbol) ;
      WHILE (NOT StrEqual( CurrentSymbol, 'END.' )) AND (NOT FatalError) DO
         ReadRoom ;
         GetNextSymbol(CurrentSymbol)
      END ;
      CloseSource ;
      Success := NOT FatalError
   ELSE
      WriteString('cannot open: ') ; WriteString(Name) ; WriteLn
   END ;
   RETURN( Success )
END ReadAdvMap ;


PROCEDURE ReadRoom ;
BEGIN
   IF NOT FatalError
   THEN
      IF NOT StrEqual( CurrentSymbol, 'ROOM' )
      THEN
         WriteError('ROOM --- Expected') ;
         FatalError := TRUE
      ELSE
         GetNextSymbol(CurrentSymbol) ;
         ReadRoomNo ;
         IF (CurrentRoom<1) OR (CurrentRoom>MaxNoOfRooms)
         THEN
            WriteError('Out Of Range Error - Room No.') ;
            FatalError := TRUE ;
            WriteString('Non Recoverable Error') ;
            WriteLn
         ELSE
            WITH Rooms[CurrentRoom] DO
               Treasures := {} ;
               NoOfWalls := 0 ;
               NoOfDoors := 0 ;
            END ;
            GetNextSymbol(CurrentSymbol) ;

            WHILE (NOT StrEqual( CurrentSymbol, 'END' )) AND
                  (NOT FatalError) DO
               IF StrEqual( CurrentSymbol, 'WALL' )
               THEN
                  ReadWall
               ELSIF StrEqual( CurrentSymbol, 'DOOR' )
               THEN
                  ReadDoor
               ELSIF StrEqual( CurrentSymbol, 'TREASURE' )
               THEN
                  ReadTreasure
               ELSE
                  WriteError('WALL, DOOR, TREASURE, END --- Expected') ;
                  FatalError := TRUE ;
                  GetNextSymbol(CurrentSymbol)
               END
            END
         END
      END
   END
END ReadRoom ;


PROCEDURE ReadWall ;
VAR
   x1, y1,
   x2, y2: CARDINAL ;
BEGIN
   IF NOT FatalError
   THEN
      GetNextSymbol(CurrentSymbol) ;
      WITH Rooms[CurrentRoom] DO
         REPEAT
            INC( NoOfWalls ) ;
            IF NoOfWalls>MaxWallsPerRoom
            THEN
               WriteError('MaxWallsPerRoom needs to be increased') ;
               FatalError := TRUE ;
               WriteString('Non Recoverable Error') ;
               WriteLn
            ELSE
               ReadCard( x1 ) ;
               GetNextSymbol(CurrentSymbol) ;
               ReadCard( y1 ) ;
               GetNextSymbol(CurrentSymbol) ;
               ReadCard( x2 ) ;
               GetNextSymbol(CurrentSymbol) ;
               ReadCard( y2 ) ;

               IF (x1#x2) AND (y1#y2)
               THEN
                  WriteError('Diagonal Wall --- Not Allowed') ;
                  FatalError := TRUE
               END ;

               (* Always have the lowest value of x in x1 OR y in y1 *)

               IF (x1<x2) OR (y1<y2)
               THEN
                  Walls[NoOfWalls].X1 := x1 ;
                  Walls[NoOfWalls].Y1 := y1 ;
                  Walls[NoOfWalls].X2 := x2 ;
                  Walls[NoOfWalls].Y2 := y2
               ELSE
                  Walls[NoOfWalls].X1 := x2 ;
                  Walls[NoOfWalls].Y1 := y2 ;
                  Walls[NoOfWalls].X2 := x1 ;
                  Walls[NoOfWalls].Y2 := y1
               END
            END ;
            GetNextSymbol(CurrentSymbol) ;
         UNTIL StrEqual( CurrentSymbol, 'WALL' ) OR
               StrEqual( CurrentSymbol, 'DOOR' ) OR
               StrEqual( CurrentSymbol, 'TREASURE' ) OR
               StrEqual( CurrentSymbol, 'END' ) OR
               FatalError ;
      END ;
   END
END ReadWall ;


PROCEDURE ReadDoor ;
VAR
   x1, y1,
   x2, y2: CARDINAL ;
BEGIN
   IF NOT FatalError
   THEN
      GetNextSymbol(CurrentSymbol) ;
      WITH Rooms[CurrentRoom] DO
         REPEAT
            INC( NoOfDoors ) ;
            IF NoOfDoors>MaxDoorsPerRoom
            THEN
               WriteError('Out Of Range Error - Too Many Doors') ;
               FatalError := TRUE ;
               WriteString('Non Recoverable Error') ;
               WriteLn
            ELSE
               ReadCard( x1 ) ;
               GetNextSymbol(CurrentSymbol) ;
               ReadCard( y1 ) ;
               GetNextSymbol(CurrentSymbol) ;
               ReadCard( x2 ) ;
               GetNextSymbol(CurrentSymbol) ;
               ReadCard( y2 ) ;

               IF (x1#x2) AND (y1#y2)
               THEN
                  WriteError('Diagonal Door --- Not Allowed') ;
                  FatalError := TRUE
               END ;

               (* Always have the lowest value of x in x1 OR y in y1 *)

               IF (x1<x2) OR (y1<y2)
               THEN
                  Doors[NoOfDoors].Position.X1 := x1 ;
                  Doors[NoOfDoors].Position.Y1 := y1 ;
                  Doors[NoOfDoors].Position.X2 := x2 ;
                  Doors[NoOfDoors].Position.Y2 := y2
               ELSE
                  Doors[NoOfDoors].Position.X1 := x2 ;
                  Doors[NoOfDoors].Position.Y1 := y2 ;
                  Doors[NoOfDoors].Position.X2 := x1 ;
                  Doors[NoOfDoors].Position.Y2 := y1
               END ;
               GetNextSymbol(CurrentSymbol) ;
               IF NOT StrEqual( CurrentSymbol, 'STATUS' )
               THEN
                  WriteError('STATUS --- Expected') ;
                  FatalError := TRUE
               END ;
               GetNextSymbol(CurrentSymbol) ;
               IF StrEqual( CurrentSymbol, 'CLOSED' )
               THEN
                  Doors[NoOfDoors].StateOfDoor := Closed
               ELSIF StrEqual( CurrentSymbol, 'SECRET' )
               THEN
                  Doors[NoOfDoors].StateOfDoor := Secret
               ELSIF StrEqual( CurrentSymbol, 'OPEN' )
               THEN
                  Doors[NoOfDoors].StateOfDoor := Open
               ELSE
                  WriteError('Illegal Door Status')
               END ;
               GetNextSymbol(CurrentSymbol) ;
               IF NOT StrEqual( CurrentSymbol, 'LEADS' )
               THEN
                  WriteError('LEADS --- Expected') ;
                  FatalError := TRUE
               END ;
               GetNextSymbol(CurrentSymbol) ;
               IF NOT StrEqual( CurrentSymbol, 'TO' )
               THEN
                  WriteError('TO --- Expected') ;
                  FatalError := TRUE
               END ;
               GetNextSymbol(CurrentSymbol) ;
               ReadCard( x1 ) ;
               IF x1>MaxNoOfRooms
               THEN
                  WriteError('Out Of Range Error - Room No.') ;
                  FatalError := TRUE
               ELSE
                  Doors[NoOfDoors].LeadsTo := x1
               END
            END ;
            GetNextSymbol(CurrentSymbol) ;
         UNTIL StrEqual( CurrentSymbol, 'DOOR' ) OR
               StrEqual( CurrentSymbol, 'WALL' ) OR
               StrEqual( CurrentSymbol, 'TREASURE' ) OR
               StrEqual( CurrentSymbol, 'END' ) OR
               FatalError ;
      END
   END
END ReadDoor ;


PROCEDURE ReadTreasure ;
VAR
   x, y, TreasureNo: CARDINAL ;
BEGIN
   IF NOT FatalError
   THEN
      GetNextSymbol(CurrentSymbol) ;
      REPEAT
         WITH Rooms[CurrentRoom] DO
            IF NOT StrEqual( CurrentSymbol, 'AT' )
            THEN
               WriteError('AT --- Expected') ;
               FatalError := TRUE
            END ;
            GetNextSymbol(CurrentSymbol) ;
            ReadCard( x ) ;
            GetNextSymbol(CurrentSymbol) ;
            ReadCard( y ) ;
            GetNextSymbol(CurrentSymbol) ;
            IF NOT StrEqual( CurrentSymbol, 'IS' )
            THEN
               WriteError('IS --- Expected') ;
               FatalError := TRUE
            END ;
            GetNextSymbol(CurrentSymbol) ;
            ReadCard( TreasureNo ) ;

            IF (TreasureNo<=MaxNoOfTreasures) AND (TreasureNo>0)
            THEN
               (* Tell Room about treasures *)

               INCL( Treasures, TreasureNo ) ;

               (* Tell Treasures about Treasures! and Room *)

               Treasure[TreasureNo].Xpos := x ;
               Treasure[TreasureNo].Ypos := y ;
               Treasure[TreasureNo].Rm := CurrentRoom ;
            ELSE
               WriteError('Out Of Range Error - Treasure No.') ;
               FatalError := TRUE
            END
         END ;
         GetNextSymbol(CurrentSymbol) ;
      UNTIL StrEqual( CurrentSymbol, 'WALL' ) OR
            StrEqual( CurrentSymbol, 'DOOR' ) OR
            StrEqual( CurrentSymbol, 'TREASURE' ) OR
            StrEqual( CurrentSymbol, 'END' ) OR
            FatalError ;
   END
END ReadTreasure ;


PROCEDURE ReadRoomNo ;
BEGIN
   IF NOT FatalError
   THEN
      ReadCard( CurrentRoom ) ;
      IF (CurrentRoom>0) AND (CurrentRoom<=MaxNoOfRooms)
      THEN
         IF CurrentRoom>ActualNoOfRooms
         THEN
            ActualNoOfRooms := CurrentRoom
         END
      END
   END
END ReadRoomNo ;


PROCEDURE ReadCard (VAR c: CARDINAL) ;
VAR
   i    : CARDINAL ;
   High : CARDINAL ;
   ch   : CHAR ;
BEGIN
   IF NOT FatalError
   THEN
      i := 0 ;
      c := 0 ;
      High := HIGH(CurrentSymbol) ;
      REPEAT
         ch := CurrentSymbol[i] ;
         IF (ch>='0') AND (ch<='9')
         THEN
            c := c*10+ORD(ch)-ORD('0')
         ELSIF ch#nul
         THEN
            WriteError('Cardinal Number Expected') ;
            FatalError := TRUE
         END ;
         INC( i ) ;
      UNTIL (i>High) OR (ch=nul) ;
   END
END ReadCard ;


PROCEDURE Init ;
BEGIN
   ActualNoOfRooms := 0 ;
   FatalError := FALSE
END Init ;


BEGIN
   Init
END AdvMap.
