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

IMPLEMENTATION MODULE RoomMap ;


FROM MapOptions IMPORT isVerbose, isDebugging, isStatistics ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteLn, WriteString ;
FROM Assertion IMPORT Assert ;

FROM Geometry IMPORT IsSubLine, IsSubRange, Swap, IntersectionLength,
                     Abs, Max, Min, IsPointOnLine, IsIntersectingRange ;

FROM Chance IMPORT GetRand, InitRandom, KillRandom, GetAndDeleteRandom,
                   AddRandom ;

FROM StoreCoords IMPORT InitCoords, KillCoords, GetAndDeleteRandomCoord,
                        AddCoord ;

FROM BoxMap IMPORT Boxes, NoOfBoxes, NoOfCorridorBoxes, CreateBoxMap,
                   MinDoorLength, MaxDoorLength ;

CONST
   MaxLineStack = 20 ;
   CorridorDoorLength = 2 ;

VAR
   NoOfCorridors: CARDINAL ;
   NoOfLines    : CARDINAL ;
   Lines        : ARRAY [1..MaxLineStack] OF Line ;


(*
   CreateRoomMap - copy boxes into rooms and amalgamate boxes into rooms.
*)

PROCEDURE CreateRoomMap ;
BEGIN
   RETURN ;  (* Remove this line if you really want to generate a map.  *)
   (* WriteString('Creating Boxes') ; WriteLn ; *)
   CreateBoxMap ;
   (* WriteString('Creating Rooms') ; WriteLn ; *)
   InitRooms ;
   CreateCorridors ;
   CreateRooms ;
END CreateRoomMap ;


(*
   InitRooms - copies the box array from Module BoxMap into the Room array.
*)

PROCEDURE InitRooms ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=NoOfBoxes DO
      WITH Rooms[i] DO
         RoomNo := i ;
         NoOfDoors := 0 ;
         NoOfWalls := 4 ;
         (* Treasures := {} ; *)
         Walls[1].X1 := Boxes[i].x1 ;   (* Lower y=c *)
         Walls[1].Y1 := Boxes[i].y1 ;
         Walls[1].X2 := Boxes[i].x2 ;
         Walls[1].Y2 := Boxes[i].y1 ;

         Walls[2].X1 := Boxes[i].x2 ;   (* Right x=c *)
         Walls[2].Y1 := Boxes[i].y1 ;
         Walls[2].X2 := Boxes[i].x2 ;
         Walls[2].Y2 := Boxes[i].y2 ;

         Walls[3].X1 := Boxes[i].x1 ;   (* Top y=c   *)
         Walls[3].Y1 := Boxes[i].y2 ;
         Walls[3].X2 := Boxes[i].x2 ;
         Walls[3].Y2 := Boxes[i].y2 ;

         Walls[4].X1 := Boxes[i].x1 ;   (* Left x=c  *)
         Walls[4].Y1 := Boxes[i].y1 ;
         Walls[4].X2 := Boxes[i].x1 ;
         Walls[4].Y2 := Boxes[i].y2
      END ;
      INC(i)
   END ;
   GenNoOfRooms := NoOfBoxes ;
   NoOfCorridors := NoOfCorridorBoxes ;
   (* Now set all other rooms to void *)
   i := GenNoOfRooms+1 ;
   WHILE i<=MaxNoOfRooms DO
      Rooms[i].RoomNo := 0 ;
      INC(i)
   END ;
   IF isStatistics ()
   THEN
      WriteString('Corridors') ; WriteCard(NoOfCorridors, 4) ; WriteLn ;
      WriteString('Rooms    ') ; WriteCard(GenNoOfRooms, 4) ; WriteLn
   END
END InitRooms ;


(*
   CreateCorridors - creates corridors from the corridor boxes.
*)

PROCEDURE CreateCorridors ;
BEGIN
   AmalgamateCorridors ;
   CreateCorridorDoors
END CreateCorridors ;


(*
   CreateRooms - creates rooms from the room boxes.
*)

PROCEDURE CreateRooms ;
BEGIN
   AmalgamateRooms ;
   IF isVerbose ()
   THEN
      WriteString('Creating RoomDoors') ; WriteLn
   END ;
   CreateMinRoomDoors ;
   CreateRoomDoors
END CreateRooms ;


(*
   CreateCorridorDoors - places corridors doors along the corridors.
*)

PROCEDURE CreateCorridorDoors ;
VAR
   i, j: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=NoOfCorridors DO
      j := 1 ;
      WHILE j<=NoOfCorridors DO
         IF (i#j) AND RoomExists(i) AND RoomExists(j)
         THEN
            MakeCorridorDoors(i, j)
         END ;
         INC(j)
      END ;
      INC(i)
   END
END CreateCorridorDoors ;


(*
   CreateMinRoomDoors - create minimum doors arround map. Thus allowing
                        an entrance into every room.
*)

PROCEDURE CreateMinRoomDoors ;
VAR
   Done: BOOLEAN ;
   i   : CARDINAL ;
BEGIN
   REPEAT
      Done := FALSE ;
      i := 1 ;
      WHILE (NOT Done) AND (i<=GenNoOfRooms) DO
         IF RoomExists(i)
         THEN
            IF Rooms[i].NoOfDoors=0
            THEN
               IF MakeDoorInRoomToSafty(i)
               THEN
               END ;
               Done := (Rooms[i].NoOfDoors#0)
            END
         END ;
         INC(i)
      END
   UNTIL NOT Done ;
   (* Now remove all rooms that have no doors *)
   IF isStatistics ()
   THEN
      WriteString('Number of rooms') ; WriteCard(GenNoOfRooms, 4) ; WriteLn ;
   END ;
   i := 1 ;
   WHILE i<=GenNoOfRooms DO
      IF RoomExists(i) AND (Rooms[i].NoOfDoors=0)
      THEN
         RemoveRoom(i) ;
	 IF isStatistics ()
         THEN
            WriteString('Removing room') ; WriteCard(i, 4) ; WriteLn
         END
      END ;
      INC(i)
   END
END CreateMinRoomDoors ;


(*
   CreateRoomDoors - places room doors in rooms.
*)

PROCEDURE CreateRoomDoors ;
VAR
   i, n: CARDINAL ;
BEGIN
   i := NoOfCorridors+1 ;
   WHILE i<=GenNoOfRooms DO
      IF RoomExists(i)
      THEN
         n := GetRand(MaxDoorsPerRoom-1 DIV 2)+1 ;
         WHILE (Rooms[i].NoOfDoors<MaxDoorsPerRoom) AND (n>0) DO
            IF MakeDoorInRoom(i)
            THEN
            END ;
            DEC(n)
         END
      END ;
      INC(i)
   END
END CreateRoomDoors ;


(*
   MakeCorridorDoors - checks for corridor doors thoughout the corridor rooms.
*)

PROCEDURE MakeCorridorDoors (r1, r2: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=Rooms[r1].NoOfWalls DO
      j := 1 ;
      WHILE j<=Rooms[r2].NoOfWalls DO
         IF IsIntersection( Rooms[r1].Walls[i].X1, Rooms[r1].Walls[i].Y1,
                            Rooms[r1].Walls[i].X2, Rooms[r1].Walls[i].Y2,
                            Rooms[r2].Walls[j].X1, Rooms[r2].Walls[j].Y1,
                            Rooms[r2].Walls[j].X2, Rooms[r2].Walls[j].Y2 )
         THEN
            CheckForCorridorDoor(r1, i, r2, j)
         END ;
         INC(j)
      END ;
      INC(i)
   END
END MakeCorridorDoors ;


(*
   MakeDoorInRoomToSafty - true is returned if a door is made in room r
                           to a room which already has a door.
                           Thus room, r, is reachable by the whole map.
*)

PROCEDURE MakeDoorInRoomToSafty (r: CARDINAL) : BOOLEAN ;
VAR
   RoomList,
   Neighbour: CARDINAL ;
   Done     : BOOLEAN ;
BEGIN
   Done := FALSE ;
   RoomList := InitRandom() ;
   AddRandom(RoomList, GenNoOfRooms) ;  (* 1..GenNoOfRooms *)
   Neighbour := GetAndDeleteRandom(RoomList) ;
   WHILE (Neighbour#0) AND (NOT Done) DO
      IF RoomExists(Neighbour) AND (r#Neighbour) AND
         IsTouching(r, Neighbour) AND (Rooms[Neighbour].NoOfDoors#0)
      THEN
         Done := MakeDoorBetweenRooms(r, Neighbour)
      END ;
      Neighbour := GetAndDeleteRandom(RoomList)
   END ;
   KillRandom(RoomList) ;
   RETURN( Done )
END MakeDoorInRoomToSafty ;


(*
   MakeDoorInRoom - true is returned if a door is made in room r.
*)

PROCEDURE MakeDoorInRoom (r: CARDINAL) : BOOLEAN ;
VAR
   RoomList,
   Neighbour: CARDINAL ;
   Done     : BOOLEAN ;
BEGIN
   Done := FALSE ;
   RoomList := InitRandom() ;
   AddRandom(RoomList, GenNoOfRooms) ;  (* 1..GenNoOfRooms *)
   Neighbour := GetAndDeleteRandom(RoomList) ;
   WHILE (Neighbour#0) AND (NOT Done) DO
      IF RoomExists(Neighbour) AND (r#Neighbour) AND IsTouching(r, Neighbour)
      THEN
         Done := MakeDoorBetweenRooms(r, Neighbour)
      END ;
      Neighbour := GetAndDeleteRandom(RoomList)
   END ;
   KillRandom(RoomList) ;
   RETURN( Done )
END MakeDoorInRoom ;


(*
   MakeDoorBetweenRooms - returns true if it makes a door between
                          rooms r1 and r2.
*)

PROCEDURE MakeDoorBetweenRooms (r1, r2: CARDINAL) : BOOLEAN ;
VAR
   CoordList: CARDINAL ;
   Success  : BOOLEAN ;
BEGIN
   Success := FALSE ;
   CoordList := InitCoords() ;
   PushPossibleDoorCoords(CoordList, r1, r2) ;
   Success := ChooseDoor(CoordList, r1, r2) ;
   KillCoords(CoordList) ;
   RETURN( Success )
END MakeDoorBetweenRooms ;


(*
   PushPossibleDoorCoords - pushes the possible door coordinates on the
                            coordinate stack CoordList.
                            The door links rooms r1 and r2 together.
*)

PROCEDURE PushPossibleDoorCoords (CoordList: CARDINAL; r1, r2: CARDINAL) ;
VAR
   i, j     : CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=Rooms[r1].NoOfWalls DO
      j := 1 ;
      WHILE j<=Rooms[r2].NoOfWalls DO
         IF IsIntersection( Rooms[r1].Walls[i].X1, Rooms[r1].Walls[i].Y1,
                            Rooms[r1].Walls[i].X2, Rooms[r1].Walls[i].Y2,
                            Rooms[r2].Walls[j].X1, Rooms[r2].Walls[j].Y1,
                            Rooms[r2].Walls[j].X2, Rooms[r2].Walls[j].Y2 )
         THEN
            IF PossibleDoorLength(r1, i, r2, j)>=MinDoorLength
            THEN
               PushPossibleDoorCoordsOnWall(CoordList, r1, i, r2, j)
            END
         END ;
         INC(j)
      END ;
      INC(i)
   END
END PushPossibleDoorCoords ;


(*
   PushPossibleDoorCoordsOnWall - pushes the coordinates which can take a door
                                  between rooms r1 and r2 on walls w1 and w2
                                  onto the coordinate stack CoordList.
*)

PROCEDURE PushPossibleDoorCoordsOnWall (CoordList: CARDINAL ;
                                        r1, w1, r2, w2: CARDINAL) ;
VAR
   s, e,
   x1, y1, x2, y2,
   x3, y3, x4, y4: CARDINAL ;
BEGIN
   IF isVerbose ()
   THEN
      WriteString('Pushing walls') ; WriteCard(w1, 4) ; WriteCard(w2, 4) ; WriteLn
   END ;
   WITH Rooms[r1].Walls[w1] DO
      x1 := X1 ;
      y1 := Y1 ;
      x2 := X2 ;
      y2 := Y2
   END ;
   WITH Rooms[r2].Walls[w2] DO
      x3 := X1 ;
      y3 := Y1 ;
      x4 := X2 ;
      y4 := Y2
   END ;
   IF IsVertical(x1, y1, x2, y2) AND IsVertical(x3, y3, x4, y4)
   THEN
      Assert(x1=x3) ; Assert(x2=x4) ;
      IF IsSubRange(y1, y2, y3)
      THEN
         s := y3
      ELSE
         Assert(IsSubRange(y3, y4, y1)) ;
         s := y1
      END ;
      e := Min(y2, y4) ;
      INC(s) ;
      DEC(e) ;
      WHILE s<=e DO
         IF IsDoorAllowed(r1, r2, x1, s, x1, e) AND
            IsDoorAllowed(r2, r1, x1, s, x1, e)
         THEN
            AddCoord(CoordList, x1, s) ;
	    IF isDebugging ()
            THEN
               WriteString('Point') ; WriteCard(x1, 4) ; WriteCard(s, 4) ; WriteLn
            END
         END ;
         INC(s)
      END
   ELSE
      Assert(IsHorizontal(x1, y1, x2, y2) AND IsHorizontal(x3, y3, x4, y4)) ;
      Assert(y1=y3) ; Assert(y2=y4) ;
      IF IsSubRange(x1, x2, x3)
      THEN
         s := x3
      ELSE
         Assert(IsSubRange(x3, x4, x1)) ;
         s := x1
      END ;
      e := Min(x2, x4) ;
      INC(s) ;
      DEC(e) ;
      WHILE s<=e DO
         IF IsDoorAllowed(r1, r2, s, y1, e, y1) AND
            IsDoorAllowed(r2, r1, s, y1, e, y1)
         THEN
            AddCoord(CoordList, s, y1) ;
	    IF isDebugging ()
            THEN
               WriteString('Point') ; WriteCard(s, 4) ; WriteCard(y1, 4) ; WriteLn
            END
         END ;
         INC(s)
      END
   END
END PushPossibleDoorCoordsOnWall ;


(*
   ChooseDoor - chooses a door from the CoordList which connects rooms
                r1 and r2.
*)

PROCEDURE ChooseDoor (CoordList: CARDINAL; r1, r2: CARDINAL) : BOOLEAN ;
VAR
   x, y  : CARDINAL ;
   w1, w2: CARDINAL ;
   ok    : BOOLEAN ;
BEGIN
   GetAndDeleteRandomCoord(CoordList, x, y) ;
   ok := (x#0) AND (y#0) ;
   IF ok
   THEN
      w1 := FindWall(r1, x, y) ;
      w2 := FindWall(r2, x, y) ;
      MakeRoomDoor(r1, w1, r2, w2, x, y)
   END ;
   RETURN( ok )
END ChooseDoor ;


(*
   FindWall - returns the wall number of a room r1 which has the point x, y
              on it. A corner point will return a wall of zero.
*)

PROCEDURE FindWall (r: CARDINAL; x, y: CARDINAL) : CARDINAL ;
VAR
   Found: BOOLEAN ;
   i    : CARDINAL ;
BEGIN
   i := 1 ;
   Found := FALSE ;
   WITH Rooms[r] DO
      WHILE (i<=NoOfWalls) AND (NOT Found) DO
         WITH Walls[i] DO
            IF ((x=X1) AND (y=Y1)) OR ((x=X2) AND (y=Y2))
            THEN
               (* Corner has been found *)
               Found := TRUE ;
               i := 0
            ELSIF IsPointOnLine(x, y, X1, Y1, X2, Y2)
            THEN
               Found := TRUE
            ELSE
               INC(i)
            END
         END
      END
   END ;
   IF Found
   THEN
      RETURN( i )
   ELSE
      RETURN( 0 )
   END
END FindWall ;


(*
   MakeRoomDoor - makes a door between rooms r1 and r2 on walls w1 and w2
                  at position x, y.
*)

PROCEDURE MakeRoomDoor (r1: CARDINAL; w1: CARDINAL; r2: CARDINAL; w2: CARDINAL;
                        x, y: CARDINAL) ;
VAR
   l, h, List,
   x1, y1, x2, y2,
   x3, y3, x4, y4: CARDINAL ;
   Done          : BOOLEAN ;
BEGIN
   IF isVerbose ()
   THEN
      WriteString('Making walls') ; WriteCard(w1, 4) ; WriteCard(w2, 4) ; WriteLn
   END ;
   WITH Rooms[r1].Walls[w1] DO
      x1 := X1 ;
      y1 := Y1 ;
      x2 := X2 ;
      y2 := Y2
   END ;
   WITH Rooms[r2].Walls[w2] DO
      x3 := X1 ;
      y3 := Y1 ;
      x4 := X2 ;
      y4 := Y2
   END ;
   IF IsVertical(x1, y1, x2, y2) AND IsVertical(x3, y3, x4, y4)
   THEN
      Assert(x1=x3) ; Assert(x2=x4) ;
      Done := FALSE ;
      List := InitRandom() ;
      AddRandom(List, MaxDoorLength) ;
      REPEAT
         l := GetAndDeleteRandom(List) ;
         IF l>=MinDoorLength
         THEN
            h := Min(y+l-1, Min(y2, y4)-1) ;
            Done := IsDoorAllowed(r1, r2, x, y, x, h) AND
                    IsDoorAllowed(r2, r1, x, y, x, h)
         END
      UNTIL (l=0) OR Done ;
      KillRandom(List) ;
      Assert(l#0) ;
      FixRoomDoor(r2, r1, x, y, x, h)
   ELSIF IsHorizontal(x1, y1, x2, y2) AND IsHorizontal(x3, y3, x4, y4)
   THEN
      Assert(y1=y3) ; Assert(y2=y4) ;
      Done := FALSE ;
      List := InitRandom() ;
      AddRandom(List, MaxDoorLength) ;
      REPEAT
         l := GetAndDeleteRandom(List) ;
         IF l>=MinDoorLength
         THEN
            h := Min(x+l-1, Min(x2, x4)-1) ;
            Done := IsDoorAllowed(r1, r2, x, y, h, y) AND
                    IsDoorAllowed(r2, r1, x, y, h, y)
         END
      UNTIL (l=0) OR Done ;
      KillRandom(List) ;
      Assert(l#0) ;
      FixRoomDoor(r2, r1, x, y, h, y)
   ELSE
      HALT
   END
END MakeRoomDoor ;


(*
   IsDoorAllowed - checks whether a door can be built in r1 leading to r2
                   the coordinates of the door are x1, y1, x2, y2.
*)

PROCEDURE IsDoorAllowed (r1, r2: CARDINAL; x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   ok  : BOOLEAN ;
   i, w: CARDINAL ;
BEGIN
   ok := (NOT DoorsClash(r1, x1, y1, x2, y2)) AND
         (Rooms[r1].NoOfDoors<MaxDoorsPerRoom) AND
         (Max(Abs(x1, x2)+1, Abs(y1, y2)+1)>=MinDoorLength) ;
   IF ok
   THEN
      w := FindWall(r1, x1, y1) ;
      IF IsVertical(x1, y1, x2, y2)
      THEN
         i := y1 ;
         WHILE (i<=y2) AND ok DO
            ok := (w=FindWall(r1, x1, i)) ;
            INC(i)
         END
      ELSIF IsHorizontal(x1, y1, x2, y2)
      THEN
         i := x1 ;
         WHILE (i<=x2) AND ok DO
            ok := (w=FindWall(r1, i, y1)) ;
            INC(i)
         END
      END
   END ;
   RETURN( ok )
END IsDoorAllowed ;


(*
   FixRoomDoor - places a door between rooms r1 and r2 with coordinates
                 x1, y1, x2, y2.
*)

PROCEDURE FixRoomDoor (r1, r2: CARDINAL; x1, y1, x2, y2: CARDINAL) ;
BEGIN
   IF IsConnectionSecret(r1, r2)
   THEN
      AddDoor(r1, r2, x1, y1, x2, y2, Secret) ;
      AddDoor(r2, r1, x1, y1, x2, y2, Secret)
   ELSIF (NOT Adjacent(r1, r2)) AND (GetRand(100)>49)
   THEN
      AddDoor(r1, r2, x1, y1, x2, y2, Secret) ;
      AddDoor(r2, r1, x1, y1, x2, y2, Secret)
   ELSE
      AddDoor(r1, r2, x1, y1, x2, y2, Closed) ;
      AddDoor(r2, r1, x1, y1, x2, y2, Closed)
   END
END FixRoomDoor ;


(*
   IsConnectionSecret - returns true if the rooms, r1 and r2, are
                        connected via a secret door.
*)

PROCEDURE IsConnectionSecret (r1, r2: CARDINAL) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WITH Rooms[r1] DO
      WHILE i<=NoOfDoors DO
         WITH Doors[i] DO
            IF (LeadsTo=r1) AND (StateOfDoor=Secret)
            THEN
               RETURN( TRUE )
            ELSE
               INC(i)
            END
         END
      END
   END ;
   RETURN( FALSE )
END IsConnectionSecret ;


(*
   CheckForCorridorDoor - checks whether a door can be built on walls
                          w1 and w2 of rooms r1 and r2.
*)

PROCEDURE CheckForCorridorDoor (r1: CARDINAL; w1: CARDINAL;
                                r2: CARDINAL; w2: CARDINAL) ;
VAR
   l: CARDINAL ;
BEGIN
   l := PossibleDoorLength(r1, w1, r2, w2) ;
   (* WriteString('Intersection length') ; WriteCard(l, 4) ; WriteLn ; *)
   IF l>=CorridorDoorLength
   THEN
      BuildCorridorDoor(r1, w1, r2, w2)
   END
END CheckForCorridorDoor ;


(*
   BuildCorridorDoor - will build a door on walls w1 and w2.
                       BuildCorridorDoor works out the coordinates
                       for the corridor doors.
*)

PROCEDURE BuildCorridorDoor (r1: CARDINAL; w1: CARDINAL;
                             r2: CARDINAL; w2: CARDINAL) ;
VAR
   x1, y1, x2, y2,
   x3, y3, x4, y4: CARDINAL ;
BEGIN
   WITH Rooms[r1].Walls[w1] DO
      x1 := X1 ;
      y1 := Y1 ;
      x2 := X2 ;
      y2 := Y2
   END ;
   WITH Rooms[r2].Walls[w2] DO
      x3 := X1 ;
      y3 := Y1 ;
      x4 := X2 ;
      y4 := Y2
   END ;
   IF IsVertical(x1, y1, x2, y2) AND IsVertical(x3, y3, x4, y4)
   THEN
      Assert(x1=x3) ; Assert(x2=x4) ;
      IF Abs(y1, y2)<Abs(y3, y4)
      THEN
         AttemptBuildCorridorDoor(r1, r2, x1, y1+1, x2, y2-1)
      ELSE
         AttemptBuildCorridorDoor(r1, r2, x1, y3+1, x2, y4-1)
      END
   ELSIF IsHorizontal(x1, y1, x2, y2) AND IsHorizontal(x3, y3, x4, y4)
   THEN
      Assert(y1=y3) ; Assert(y2=y4) ;
      IF Abs(x1, x2)<Abs(x3, x4)
      THEN
         AttemptBuildCorridorDoor(r1, r2, x1+1, y1, x2-1, y2)
      ELSE
         AttemptBuildCorridorDoor(r1, r2, x3+1, y1, x4-1, y4)
      END
   END
END BuildCorridorDoor ;


(*
   AttemptBuildCorridorDoor - attempts to make a corridor door
                              between rooms r1 and r2 with
                              coordinates x1, y1, x2, y2.
*)

PROCEDURE AttemptBuildCorridorDoor (r1, r2: CARDINAL ;
                                    x1, y1, x2, y2: CARDINAL) ;
BEGIN
   IF IsDoorAllowed(r1, r2, x1, y1, x2, y2) AND
      IsDoorAllowed(r2, r1, x1, y1, x2, y2)
   THEN
      IF IsConnectionSecret(r1, r2)
      THEN
         AddDoor(r1, r2, x1, y1, x2, y2, Secret) ;
         AddDoor(r2, r1, x1, y1, x2, y2, Secret)
      ELSIF (NOT Adjacent(r1, r2)) AND (GetRand(100)>65)
      THEN
         AddDoor(r1, r2, x1, y1, x2, y2, Secret) ;
         AddDoor(r2, r1, x1, y1, x2, y2, Secret)
      ELSE
         AddDoor(r1, r2, x1, y1, x2, y2, Open) ;
         AddDoor(r2, r1, x1, y1, x2, y2, Open)
      END
   ELSE
      IF isVerbose ()
      THEN
         WriteString('Not allowing corridor door!') ; WriteLn
      END
   END
END AttemptBuildCorridorDoor ;


(*
   DoorsClash - returns true if there does exist a door which clashes with
                x1, y1, x2, y2 in room r1.
*)

PROCEDURE DoorsClash (r: CARDINAL; x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   Clash: BOOLEAN ;
   i    : CARDINAL ;
BEGIN
   IF IsVertical(x1, y1, x2, y2)
   THEN
      INC(y2) ;
      IF y1>1
      THEN
         DEC(y1)
      END
   ELSE
      INC(x2) ;
      IF x1>1
      THEN
         DEC(x1)
      END
   END ;
   Clash := FALSE ;
   WITH Rooms[r] DO
      i := 1 ;
      WHILE (i<=NoOfDoors) AND (NOT Clash) DO
         WITH Doors[i].Position DO
            IF IsIntersection(x1, y1, x2, y2, X1, Y1, X2, Y2)
            THEN
               Clash := TRUE
            END
         END ;
         INC(i)
      END
   END ;
   RETURN( Clash )
END DoorsClash ;


(*
   AddDoor - adds a door in room r1 that leads to a door in r2.
             The coordinates of the door are x1, y1, x2, y2 and the
             door type is in Status.
*)

PROCEDURE AddDoor (r1, r2: CARDINAL;
                   x1, y1, x2, y2: CARDINAL; Status: DoorStatus) ;
BEGIN
  ; Assert(IsTouching(r1, r2))
  ; Assert(IsDoorAllowed(r1, r2, x1, y1, x2, y2)) ;
  ; Assert(r1#r2) ;
  ; Assert(RoomExists(r1) AND RoomExists(r2)) ;
   WITH Rooms[r1] DO
      Assert(NoOfDoors<MaxDoorsPerRoom) ;
      INC(NoOfDoors) ;
      WITH Doors[NoOfDoors] DO
         Position.X1 := x1 ;
         Position.Y1 := y1 ;
         Position.X2 := x2 ;
         Position.Y2 := y2 ;
         StateOfDoor := Status ;
         LeadsTo := r2
      END
   END
END AddDoor ;


(*
   PossibleDoorLength - returns the possible door length between rooms
                        r1 and r2 on wall w1 and w2.
*)

PROCEDURE PossibleDoorLength (r1: CARDINAL; w1: CARDINAL;
                              r2: CARDINAL; w2: CARDINAL) : CARDINAL ;
VAR
   l,
   x1, x2, x3, x4,
   y1, y2, y3, y4: CARDINAL ;
BEGIN
   WITH Rooms[r1].Walls[w1] DO
      x1 := X1 ;
      y1 := Y1 ;
      x2 := X2 ;
      y2 := Y2
   END ;
   WITH Rooms[r2].Walls[w2] DO
      x3 := X1 ;
      y3 := Y1 ;
      x4 := X2 ;
      y4 := Y2
   END ;
   IF IsVertical(x1, y1, x2, y2) AND IsVertical(x3, y3, x4, y4)
   THEN
      l := IntersectionLength(y1, y2, y3, y4)+1 ;
      IF (IsSubRange(y1, y2, y3) OR IsSubRange(y3, y4, y1)) AND (l>0)
      THEN
         DEC(l)
      END ;
      IF (IsSubRange(y1, y2, y4) OR IsSubRange(y3, y4, y2)) AND (l>0)
      THEN
         DEC(l)
      END
   ELSIF IsHorizontal(x1, y1, x2, y2) AND IsHorizontal(x3, y3, x4, y4)
   THEN
      l := IntersectionLength(x1, x2, x3, x4)+1 ;
      IF (IsSubRange(x1, x2, x3) OR IsSubRange(x3, x4, x1)) AND (l>0)
      THEN
         DEC(l)
      END ;
      IF (IsSubRange(x1, x2, x4) OR IsSubRange(x3, x4, x2)) AND (l>0)
      THEN
         DEC(l)
      END
   ELSE
      l := 0
   END ;
   RETURN( l )
END PossibleDoorLength ;


(*
   AmalgamateCorridors - joins corridors together.
*)

PROCEDURE AmalgamateCorridors ;
VAR
   i, j: CARDINAL ;
   Done: BOOLEAN ;
BEGIN
   REPEAT
      Done := FALSE ;
      i := 1 ;
      WHILE (NOT Done) AND (i<=NoOfCorridors) DO
         j := 1 ;
         WHILE (NOT Done) AND (j<=NoOfCorridors) DO
            IF (i#j) AND RoomExists(i) AND RoomExists(j)
            THEN
               IF Amalgamate(i, j)
               THEN
                  Done := TRUE
               END
            END ;
            INC(j)
         END ;
         INC(i)
      END
   UNTIL NOT Done
END AmalgamateCorridors ;


(*
   AmalgamateRooms - joins rooms together.
*)

PROCEDURE AmalgamateRooms ;
VAR
   i, j: CARDINAL ;
   Done: BOOLEAN ;
BEGIN
   REPEAT
      Done := FALSE ;
      i := NoOfCorridors+1 ;
      WHILE (NOT Done) AND (i<=GenNoOfRooms) DO
         j := NoOfCorridors+1 ;
         WHILE (NOT Done) AND (j<=GenNoOfRooms) DO
            IF (i#j) AND RoomExists(i) AND RoomExists(j)
            THEN
               IF Amalgamate(i, j)
               THEN
                  Done := TRUE
               END
            END ;
            INC(j)
         END ;
         INC(i)
      END
   UNTIL NOT Done
END AmalgamateRooms ;


(*
   Amalgamate - returns true if it can join two rooms r1 and r2 together.
*)

PROCEDURE Amalgamate (r1, r2: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsTouching(r1, r2)
   THEN
      NoOfLines := 0 ;
      CopyWallsToLines(r1) ;
      CopyWallsToLines(r2) ;
      RemoveRoom(r1) ;
      RemoveRoom(r2) ;
      IF CompactLines() AND IsLineRoomSatisfied()
      THEN
         CopyLinesToWalls(r1) ;
         InsertRoom(r1) ;
         RETURN( TRUE )
      ELSE
         InsertRoom(r1) ;
         InsertRoom(r2) ;
         RETURN( FALSE )
      END
   ELSE
      RETURN( FALSE )
   END
END Amalgamate ;


(*
   CompactLines - returns true if the lines in the line buffer are
                  reduced by forming a bigger room.
*)

PROCEDURE CompactLines () : BOOLEAN ;
VAR
   Done,
   Compacted: BOOLEAN ;
   i, j     : CARDINAL ;
BEGIN
   Compacted := FALSE ;
   REPEAT
      Done := FALSE ;
      i := 1 ;
      WHILE (i<=NoOfLines) AND (NOT Done) DO
         j := 1 ;
         WHILE (j<=NoOfLines) AND (NOT Done) DO
            IF (i#j) AND (NOT IsNulLine(i)) AND (NOT IsNulLine(j))
            THEN
               IF IsIntersectionLine(i, j)
               THEN
                  DeleteIntersectionLine(i, j) ;
                  LinkUpLines ;
                  Done := TRUE ;
                  Compacted := TRUE
               END
            END ;
            INC(j)
         END ;
         INC(i)
      END
   UNTIL NOT Done ;
   RETURN( Compacted )
END CompactLines ;


(*
   LinkUpLines - attempts to join lines that naturally run in to each other.
*)

PROCEDURE LinkUpLines ;
VAR
   Joined: BOOLEAN ;
   i, j  : CARDINAL ;
BEGIN
   Joined := FALSE ;
   i := 1 ;
   WHILE (i<=NoOfLines) AND (NOT Joined) DO
      j := 1 ;
      WHILE (j<=NoOfLines) AND (NOT Joined) DO
         Joined := JoinedLines(i, j) ;
         INC(j)
      END ;
      INC(i)
   END
END LinkUpLines ;


(*
   JoinedLines - returns true if it can join lines i and j.
*)

PROCEDURE JoinedLines (i, j: CARDINAL) : BOOLEAN ;
VAR
   Joined        : BOOLEAN ;
   x1, x2, x3, x4,
   y1, y2, y3, y4: CARDINAL ;
BEGIN
   WITH Lines[i] DO
      x1 := X1 ;
      y1 := Y1 ;
      x2 := X2 ;
      y2 := Y2
   END ;
   WITH Lines[j] DO
      x3 := X1 ;
      y3 := Y1 ;
      x4 := X2 ;
      y4 := Y2
   END ;
   Joined := FALSE ;
   (* X1 <= X2  - always *)
   WITH Lines[i] DO
      IF IsVertical(x1, y1, x2, y2) AND IsVertical(x3, y3, x4, y4) AND (x1=x3)
      THEN
         IF y4=y1
         THEN
            AddLine(x3, y3, x2, y2) ;
            DeleteLine(i) ;
            DeleteLine(j) ;
            Joined := TRUE
         ELSIF y2=y3
         THEN
            AddLine(x1, y1, x4, y4) ;
            DeleteLine(i) ;
            DeleteLine(j) ;
            Joined := TRUE
         END
      ELSIF IsHorizontal(x1, y1, x2, y2) AND IsHorizontal(x3, y3, x4, y4) AND
            (y1=y3)
      THEN
         IF x4=x1
         THEN
            AddLine(x3, y3, x2, y2) ;
            DeleteLine(i) ;
            DeleteLine(j) ;
            Joined := TRUE
         ELSIF x2=x3
         THEN
            AddLine(x1, y1, x4, y4) ;
            DeleteLine(i) ;
            DeleteLine(j) ;
            Joined := TRUE
         END
      END
   END ;
   RETURN( Joined )
END JoinedLines ;


(*
   IsIntersectionLine - returns true if lines i and j intersect.
*)

PROCEDURE IsIntersectionLine (i, j: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
            IsIntersection(
               Lines[i].X1, Lines[i].Y1, Lines[i].X2, Lines[i].Y2,
               Lines[j].X1, Lines[j].Y1, Lines[j].X2, Lines[j].Y2)
            )
END IsIntersectionLine ;


(*
   DeleteIntersectionLine - joins two lines together, i and j, and
                            removes the intersection.
*)

PROCEDURE DeleteIntersectionLine (i, j: CARDINAL) ;
VAR
   x1, x2, x3, x4,
   y1, y2, y3, y4: CARDINAL ;
BEGIN
   WITH Lines[i] DO
      x1 := X1 ;
      y1 := Y1 ;
      x2 := X2 ;
      y2 := Y2
   END ;
   WITH Lines[j] DO
      x3 := X1 ;
      y3 := Y1 ;
      x4 := X2 ;
      y4 := Y2
   END ;
   IF IsVertical(x1, y1, x2, y2) AND IsVertical(x3, y3, x4, y4)
   THEN
      DeleteLine(i) ;
      DeleteLine(j) ;
      SubtractIntersection(y1, y2, y3, y4) ;
      AddLine(x1, y1, x2, y2) ;
      AddLine(x3, y3, x4, y4)
   ELSIF IsHorizontal(x1, y1, x2, y2) AND IsHorizontal(x3, y3, x4, y4)
   THEN
      DeleteLine(i) ;
      DeleteLine(j) ;
      SubtractIntersection(x1, x2, x3, x4) ;
      AddLine(x1, y1, x2, y2) ;
      AddLine(x3, y3, x4, y4)
   END
END DeleteIntersectionLine ;


(*
   SubtractIntersection - deletes the intersecting entities of the range
                          i1..i2  j1..j2.
*)

PROCEDURE SubtractIntersection (VAR i1, i2, j1, j2: CARDINAL) ;
VAR
   k1, k2,
   l1, l2: CARDINAL ;
BEGIN
   Assert(i1<=i2) ;
   Assert(j1<=j2) ;
   IF IsSubRange(i1, i2, j1)
   THEN
      k1 := i1 ;
      k2 := j1
   ELSIF IsSubRange(j1, j2, i1)
   THEN
      k1 := j1 ;
      k2 := i1
   END ;
   IF IsSubRange(i1, i2, j2)
   THEN
      l1 := j2 ;
      l2 := i2
   ELSIF IsSubRange(j1, j2, i2)
   THEN
      l1 := i2 ;
      l2 := j2
   END ;
   i1 := k1 ;
   i2 := k2 ;
   j1 := l1 ;
   j2 := l2
END SubtractIntersection ;


(*
   AddLine - adds a line to the lines buffer.
*)

PROCEDURE AddLine (x1, y1, x2, y2: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   IF (x1>x2) OR (y1>y2)
   THEN
      Swap(x1, x2) ;
      Swap(y1, y2)
   ELSIF (x1#x2) OR (y1#y2)
   THEN
      (* Do not store points *)
      IF NOT InsertLine(x1, y1, x2, y2)
      THEN
         INC(NoOfLines) ;
         WITH Lines[NoOfLines] DO
            X1 := x1 ;
            Y1 := y1 ;
            X2 := x2 ;
            Y2 := y2
         END
      END
    (* ; DisplayLines *)
   END
END AddLine ;


(*
   InsertLine - attempts to insert a line in a free slot,
                true is returned if successfull.
*)

PROCEDURE InsertLine (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   i      : CARDINAL ;
BEGIN
   i := 1 ;
   Success := FALSE ;
   WHILE (NOT Success) AND (i<=NoOfLines) DO
      IF IsNulLine(i)
      THEN
         WITH Lines[i] DO
            X1 := x1 ;
            Y1 := y1 ;
            X2 := x2 ;
            Y2 := y2
         END ;
         Success := TRUE
      ELSE
         INC(i)
      END
   END ;
   RETURN( Success )
END InsertLine ;


(*
   DeleteLine - deletes a line from the lines buffer.
*)

PROCEDURE DeleteLine (l: CARDINAL) ;
BEGIN
   WITH Lines[l] DO
      X1 := 0 ;
      Y1 := 0 ;
      X2 := 0 ;
      Y2 := 0
   END
 (* ; DisplayLines *)
END DeleteLine ;


PROCEDURE DisplayLines ;
VAR
   i: CARDINAL ;
BEGIN
   WriteString('Lines') ; WriteLn ;
   i := 1 ;
   WHILE i<=NoOfLines DO
      WITH Lines[i] DO
         WriteCard(X1, 4) ; WriteCard(Y1, 4) ;
         WriteCard(X2, 4) ; WriteCard(Y2, 4) ; WriteLn
      END ;
      INC(i)
   END
END DisplayLines ;


(*
   IsNulLine - returns true if line l is a nul line.
*)

PROCEDURE IsNulLine (l: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Lines[l] DO
      RETURN( (X1=0) AND (Y1=0) AND (X2=0) AND (Y2=0) )
   END
END IsNulLine ;


(*
   RemoveRoom - removes a room, r, from the room list.
*)

PROCEDURE RemoveRoom (r: CARDINAL) ;
BEGIN
   Rooms[r].RoomNo := 0   (* No longer exists *)
END RemoveRoom ;


(*
   InsertRoom - inserts a room, r, back into the room list.
*)

PROCEDURE InsertRoom (r: CARDINAL) ;
BEGIN
   Rooms[r].RoomNo := r
END InsertRoom ;


(*
   RoomExist - returns true if a room, r, exists.
*)

PROCEDURE RoomExists (r: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( Rooms[r].RoomNo#0 )
END RoomExists ;


(*
   IsLineRoomSatisfied - returns true if the line room meets the requirements
                         of a room.
*)

PROCEDURE IsLineRoomSatisfied () : BOOLEAN ;
VAR
   Count, i: CARDINAL ;
BEGIN
   Count := 0 ;
   i := 1 ;
   WHILE i<=NoOfLines DO
      IF NOT IsNulLine(i)
      THEN
         INC(Count)
      END ;
      INC(i)
   END ;
   RETURN( Count<=MaxWallsPerRoom )
   (* Must also check for a door into another room *)
END IsLineRoomSatisfied ;


(*
   CopyWallsToLines - copies walls from room r into the lines buffer.
*)

PROCEDURE CopyWallsToLines (r: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WITH Rooms[r] DO
      WHILE i<=NoOfWalls DO
         INC(NoOfLines) ;
         WITH Lines[NoOfLines] DO
            X1 := Walls[i].X1 ;
            Y1 := Walls[i].Y1 ;
            X2 := Walls[i].X2 ;
            Y2 := Walls[i].Y2
         END ;
         INC(i)
      END
   END
END CopyWallsToLines ;


(*
   CopyLinesToWalls - copies the lines buffer into the walls of room r.
*)

PROCEDURE CopyLinesToWalls (r: CARDINAL) ;
BEGIN
   WITH Rooms[r] DO
      NoOfWalls := 0 ;
      WHILE NoOfLines>0 DO
         IF NOT IsNulLine(NoOfLines)
         THEN
            INC(NoOfWalls) ;
            WITH Lines[NoOfLines] DO
               Walls[NoOfWalls].X1 := X1 ;
               Walls[NoOfWalls].Y1 := Y1 ;
               Walls[NoOfWalls].X2 := X2 ;
               Walls[NoOfWalls].Y2 := Y2
            END
         END ;
         DEC(NoOfLines)
      END
   END
END CopyLinesToWalls ;


(*
   IsTouching - returns true if room r1 and r2 touch each other.
*)

PROCEDURE IsTouching (r1, r2: CARDINAL) : BOOLEAN ;
VAR
   i, j: CARDINAL ;
   ok  : BOOLEAN ;
BEGIN
   ok := FALSE ;
   i := 1 ;
   WHILE (NOT ok) AND (i<=Rooms[r1].NoOfWalls) DO
      j := 1 ;
      WHILE (NOT ok) AND (j<=Rooms[r2].NoOfWalls) DO
         ok := IsIntersection(Rooms[r1].Walls[i].X1, Rooms[r1].Walls[i].Y1,
                              Rooms[r1].Walls[i].X2, Rooms[r1].Walls[i].Y2,
                              Rooms[r2].Walls[j].X1, Rooms[r2].Walls[j].Y1,
                              Rooms[r2].Walls[j].X2, Rooms[r2].Walls[j].Y2) ;
         INC(j)
      END ;
      INC(i)
   END ;
   RETURN( ok )
END IsTouching ;


(*
   IsIntersection - returns true if the line x1, y1, x2, y2 touches
                    line X1, Y1, X2, Y2.
                    This routine does not consider perpendicular
                    intersections.
*)

PROCEDURE IsIntersection (x1, y1, x2, y2, X1, Y1, X2, Y2: CARDINAL) : BOOLEAN ;
BEGIN
   Assert(x1#0) ;
   Assert(X1#0) ;
   IF IsVertical(x1, y1, x2, y2) AND IsVertical(X1, Y1, X2, Y2) AND
      (x1=X1)
   THEN
      RETURN( IsIntersectingRange(y1, y2, Y1, Y2) )
   ELSIF IsHorizontal(x1, y1, x2, y2) AND IsHorizontal(X1, Y1, X2, Y2) AND
         (y1=Y1)
   THEN
      RETURN( IsIntersectingRange(x1, x2, X1, X2) )
   ELSE
      RETURN( FALSE )
   END
END IsIntersection ;


(*
   IsVertical - returns true if line x1, y1, x2, y2 is vertical.
*)

PROCEDURE IsVertical (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (x1=x2) AND (y1#y2) )
END IsVertical ;


(*
   IsHorizontal - returns true if line x1, y1, x2, y2 is horizontal.
*)

PROCEDURE IsHorizontal (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (y1=y2) AND (x1#x2) )
END IsHorizontal ;


(*
   Adjacent - tests whether two rooms r1 & r2 are adjacent.
*)

PROCEDURE Adjacent (r1, r2: CARDINAL) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   WITH Rooms[r1] DO
      i := NoOfDoors ;
      WHILE i>0 DO
         IF Doors[i].LeadsTo=r2
         THEN
            RETURN( TRUE )
         ELSE
            DEC(i)
         END
      END
   END ;
   RETURN( FALSE )
END Adjacent ;


PROCEDURE Stop ;
BEGIN
   HALT
END Stop ;


END RoomMap.
