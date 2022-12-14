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
IMPLEMENTATION MODULE BoxMap ;

(*
   Title      : MakeMap
   Author     : Gaius Mulley
   Date       : 18/7/88
   LastEdit   : 18/7/88
   System     : LOGITECH MODULA-2/86
   Description: Generates a simple random box map for Dungeon
*)

IMPORT Break ;
FROM StdIO IMPORT Write, Read ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard, ReadCard ;
FROM Assertion IMPORT Assert ;
FROM Geometry IMPORT IsSubLine, IsSubRange, IsIntersectingRange,
                     IntersectionLength, IsPointOnLine, Abs, Min, Max ;
FROM MakeBoxes IMPORT InitBoxes, KillBoxes,
                      AddBoxes, GetAndDeleteRandomBox ;
FROM StoreCoords IMPORT InitCoords, KillCoords,
                        GetAndDeleteRandomCoord, AddCoord, CoordsExist ;
FROM Chance IMPORT InitRandom, KillRandom,
                   GetAndDeleteRandom, AddRandom,
                   GetRand ;

CONST
   MaxCard  = 65535 ;

   MaxStack =   500 ;

TYPE
   Square = RECORD
               Contents    : (Empty, Secret, Door, Wall, Treasure) ;
               RoomOfSquare: CARDINAL ;
            END ;

   Map      = ARRAY [1..MaxX], [1..MaxY] OF Square ;

   StackEntity = RECORD
                    PerimeterIndex  : CARDINAL ;  (* Untried Coords   *)
                    BoxIndex        : CARDINAL ;  (* Untried boxes    *)
                    OrientationIndex: CARDINAL ;  (* Untried orient's *)
                 END ;

VAR
   CurrentMap       : Map ;
   Stack            : ARRAY [1..MaxStack] OF StackEntity ;
   StackPtr         : CARDINAL ;


(*
   InitializeMap - Initializes CurrentMap.
                   CurrentMap has its boarder set to a Wall and middle
                   is set to Empty.
*)

PROCEDURE InitializeMap ;
VAR
   i, j: CARDINAL ;
BEGIN
   FOR i := 1 TO MaxX DO
      FOR j := 1 TO MaxY DO
         WITH CurrentMap[i, j] DO
            Contents := Empty ;
            RoomOfSquare := 0
         END
      END
   END
END InitializeMap ;


(*
   Init - Initialize the module and start the generation of a map.
*)

PROCEDURE Init ;
BEGIN
   NoOfBoxes := 0 ;
   (* Initialize box 0 the edge of the map *)
   WITH Boxes[0] DO
      x1 := 1 ;
      x2 := MaxX ;
      y1 := 1 ;
      y2 := MaxY
   END ;
   StackPtr := 0
END Init ;


(*
   CreateBoxMap - builds a map with central corridors and ajoining rooms.
*)

PROCEDURE CreateBoxMap ;
BEGIN
   Init ;
   CorridorMap ;
   RoomMap
END CreateBoxMap ;


(*
   CorridorMap - makes a map based arround central corridors.
*)

PROCEDURE CorridorMap ;
BEGIN
   CreateCorridors ;
   NoOfCorridorBoxes := NoOfBoxes
END CorridorMap ;


(*
   CleanUpStack - cleans up the temporary stack where alternative rooms were
                  stored but are no longer needed.
*)

PROCEDURE CleanUpStack ;
BEGIN
   WHILE StackPtr>0 DO
      WITH Stack[StackPtr] DO
         KillBoxes(BoxIndex) ;
         KillCoords(PerimeterIndex) ;
         KillRandom(OrientationIndex)
      END ;
      DEC(StackPtr)
   END
END CleanUpStack ;


(*
   RoomMap - creates the rooms on the map which fill in space left by the
             corridors.
*)

PROCEDURE RoomMap ;
BEGIN
   WriteString('Starting Room building') ; WriteLn ;
   CreateRooms
END RoomMap ;


(*
   CreateCorridors - creates a length of corridor on the map.
*)

PROCEDURE CreateCorridors ;
VAR
   Length,
   LengthLeft: CARDINAL ;
BEGIN
   LengthLeft := TotalCorridorLength ;
   InitBoxCorridor ;  (* Place new Box on the stack *)
   REPEAT
      IF MakeCorridor()
      THEN
         WITH Boxes[NoOfBoxes] DO
            Length := Max(Abs(x1, x2), Abs(y1, y2))
         END ;
         IF LengthLeft>Length
         THEN
            DEC(LengthLeft, Length) ;
            InitBoxCorridor   (* Place new corridors on the stack *)
         ELSE
            LengthLeft := 0   (* All done *)
         END
      ELSE
         IF StackPtr>0
         THEN
            (* Retract last corridor and try another *)
            WriteString('Backtracking') ; WriteLn ;
            WriteString('HALTing - quicker than backtracking') ; WriteLn ;
            HALT ;
            WITH Boxes[NoOfBoxes] DO
               INC(LengthLeft, Max(Abs(x1, x2), Abs(y1, y2)))
            END ;
            KillBox ;
            UnMakeBox
         ELSE
            WriteString('Run out of ideas! MaxCorridorLength too large!') ;
            WriteLn ;
            LengthLeft := 0    (* Fail safe exit *)
         END
      END
   UNTIL LengthLeft=0
END CreateCorridors ;


(*
   CreateRooms - places rooms inbetween the corridors on the map.
*)

PROCEDURE CreateRooms ;
VAR
   Finished: BOOLEAN ;
BEGIN
   InitBoxRoom ;
   Finished := FALSE ;
   REPEAT
      IF MakeRoom()
      THEN
         InitBoxRoom ;
         Finished := NOT CoordsExist(Stack[StackPtr].PerimeterIndex)
      ELSE
         Finished := TRUE ;
(*
         IF StackPtr>0
         THEN
            (* Retract last room and try another *)
            WriteString('Backtracking room') ; WriteLn ;
            KillBox ;
            UnMakeBox
         ELSE
            WriteString('Run out of ideas! Trying to create rooms!') ;
            WriteLn ;
         END
*)
      END
   UNTIL Finished ;
END CreateRooms ;


(*
   MakeCorridor - returns true if a corridor was legally placed
                  onto the map.
*)

PROCEDURE MakeCorridor () : BOOLEAN ;
VAR
   Success   : BOOLEAN ;
   x, y      : CARDINAL ;
BEGIN
   WITH Stack[StackPtr] DO
      (*
         Perimeter has been previously pushed.
         We now try to place a piece of corridor
         on a selected perimeter coordinate.
      *)
      Success := FALSE ;
      REPEAT
         GetAndDeleteRandomCoord(PerimeterIndex, x, y) ;
         x := Min(x, MaxX) ;
         y := Min(y, MaxY) ;
         IF x#0  (* x=0 means no more coordinates to fetch *)
         THEN
            Success := PutCorridorOntoMap(x, y)
         END
      UNTIL Success OR (x=0)    (* x=0 and y=0 means no coordinates left *)
                                (* when x=0  y is also 0.                *)
   END ;
   RETURN( Success )
END MakeCorridor ;


(*
   MakeRoom - returns true if a room was legally placed
              onto the map.
*)

PROCEDURE MakeRoom () : BOOLEAN ;
VAR
   Success   : BOOLEAN ;
   x, y      : CARDINAL ;
BEGIN
   WITH Stack[StackPtr] DO
      (*
         Perimeter has been previously pushed.
         We now try to place a piece of corridor
         on a selected perimeter coordinate.
      *)
      Success := FALSE ;
      REPEAT
         GetAndDeleteRandomCoord(PerimeterIndex, x, y) ;
         IF x#0  (* x=0 means no more coordinates to fetch *)
         THEN
            Success := PutRoomOntoMap(x, y)
         END
      UNTIL Success OR (x=0)    (* x=0 and y=0 means no coordinates left *)
                                (* when x=0  y is also 0.                *)
   END ;
   RETURN( Success )
END MakeRoom ;


(*
   UnMakeBox - deletes the last box placed in the Box list.
*)

PROCEDURE UnMakeBox ;
BEGIN
(*
   IF NoOfCorridorBoxes>0
   THEN
      FindSpaceNextToRoom
   END ;
*)
   DEC(NoOfBoxes)
END UnMakeBox ;


(*
   KillSurroundingBoxes - finds a pocket of space on the map and deletes
                          all neighbouring boxes.
*)

(*
PROCEDURE KillSurroundingBoxes ;
VAR
   x, y,
   i, j,
   Swap, b: CARDINAL ;
BEGIN
   GetFreeSpace(x, y) ;
   i := x ;
   j := y ;
   Swap := NoOfBoxes ;
   REPEAT
      b := 1 ;
      WHILE b<=Swap DO
         IF IsPointOnBox(b, i, j)
         THEN
            SwapBox(b, Swap) ;
            DEC(Swap)
         END ;
         INC(b)
      END ;
      WalkClockWise(i, j)
   UNTIL (x=i) AND (y=j) ;
   RenewBoxes(Swap, Swap)
END KillSurroundingBoxes ;
*)


(*
   SwapBox - swaps two boxes, i and j, arround on the stack.
*)

PROCEDURE SwapBox (i, j: CARDINAL) ;
VAR
   s: StackEntity ;
   b: Box ;
BEGIN
   b := Boxes[i] ;
   Boxes[i] := Boxes[j] ;
   Boxes[j] := b ;
   s := Stack[i] ;
   Stack[i] := Stack[j] ;
   Stack[j] := s
END SwapBox ;


(*
   FindSpaceNextToRoom - finds a pocket of space on the map and places
                         a room near this onto the top of the box stack.
*)

PROCEDURE FindSpaceNextToRoom ;
VAR
   t: Box ;
   x, y, b, d,
   Nearest,
   Distance  : CARDINAL ;
BEGIN
   GetSpaceCoord(x, y) ;
   Nearest := 1 ;
   Distance := DistanceAppartPoint(1, x, y) ;
   b := NoOfBoxes-1 ;
   WHILE b>1 DO
      d := DistanceAppartPoint(b, x, y) ;
      IF d<Distance
      THEN
         Distance := d ;
         Nearest := b
      END ;
      DEC(b)
   END ;
   SwapBox(Nearest, NoOfBoxes)
END FindSpaceNextToRoom ;


(*
   GetSpaceCoord - Sets x and y to a coordinate which has no room on it.
*)

PROCEDURE GetSpaceCoord (VAR x, y: CARDINAL) ;
VAR
   Space: BOOLEAN ;
BEGIN
   Space := FALSE ;
   x := 1 ;
   WHILE (NOT Space) AND (x<=MaxX) DO
      y := 1 ;
      WHILE (NOT Space) AND (y<=MaxY) DO
         IF NOT IsSpace(x, y)
         THEN
            INC(y)
         ELSE
            Space := TRUE
         END
      END ;
      IF NOT Space
      THEN
         INC(x)
      END
   END
END GetSpaceCoord ;


(*
   Reschedule - reorders boxes on the stack, all boxes that touch the
                top box are placed n-1 n-2 etc on the stack,
                efficient recursive backtracking!
*)

PROCEDURE Reschedule (Lowest: CARDINAL) ;
VAR
   b,
   Swap: CARDINAL ;
   t   : Box ;
BEGIN
   Swap := NoOfBoxes-1 ;
   b := Lowest+1 ;
   WITH Boxes[NoOfBoxes] DO
      WHILE Swap>b DO
         IF IsTouchingBox(b, x1, y1, x2, y2)
         THEN
            SwapBox(b, Swap) ;
            INC(b)
         END ;
         DEC(Swap)
      END
   END
END Reschedule ;


(*
   InitBoxCorridor - initializes a new corridor on the Stack,
                     the perimeter of the map is also pushed.
*)

PROCEDURE InitBoxCorridor ;
BEGIN
   INC(StackPtr) ;
   WITH Stack[StackPtr] DO
      PerimeterIndex := InitCoords() ;
      PushPerimeterOfBoxes(PerimeterIndex, FALSE) ;
      OrientationIndex := 0 ;
      BoxIndex := 0
   END
END InitBoxCorridor ;


(*
   InitBoxRoom - initializes a new corridor on the Stack,
                 the perimeter of the map is also pushed.
*)

PROCEDURE InitBoxRoom ;
BEGIN
   (*
      This is a really nasty kludge - because of memory space limitations
      the StoreCoords module is pushed for space when creating large size
      maps.
      The kludge to get arround this is to kill all perimeter coordinates of the
      previous box. This can be done since we never invoke backtracking
      when creating boxrooms - but we may when we come up with a suitable
      reliable algorithm, however, until then we can take advantage of
      no backtracking and delete all perimeter coords of the last box.
   *)
   IF StackPtr>1
   THEN
      (* Ok delete perimeter coord *)
      KillCoords(Stack[StackPtr].PerimeterIndex) ;
      KillBoxes(Stack[StackPtr].BoxIndex)
   END ;
   (* All done - kludge over *)
   INC(StackPtr) ;
   WITH Stack[StackPtr] DO
      PerimeterIndex := InitCoords() ;
      PushPerimeterOfBoxes(PerimeterIndex, TRUE) ;
      OrientationIndex := 0 ;
      BoxIndex := 0
   END
END InitBoxRoom ;


(*
   KillBox - pops the last Box from the stack.
*)

PROCEDURE KillBox ;
BEGIN
   WITH Stack[StackPtr] DO
      KillCoords(PerimeterIndex)
   END ;
   DEC(StackPtr)
END KillBox ;


(*
   PutCorridorOntoMap - returns true if it has placed a corridor
                        onto a map.
                        Otherwise no corridor has been placed onto
                        this map.
*)

PROCEDURE PutCorridorOntoMap (x, y: CARDINAL) : BOOLEAN ;
VAR
   LenX,
   LenY    : CARDINAL ;
   Success : BOOLEAN ;
BEGIN
   CheckInitBoxCorridorIndex ;
   WITH Stack[StackPtr] DO
      Success := FALSE ;
      REPEAT
         IF GetBox(LenX, LenY)
         THEN
            Success := PlaceCorridorBox(x, y, LenX-1, LenY-1)
         END
      UNTIL Success OR (LenX=0) ;
   END ;
   CheckKillBoxIndex(LenX=0) ;
   RETURN( Success )
END PutCorridorOntoMap ;


(*
   PutRoomOntoMap - returns true if it has placed a room
                    onto a map.
                    Otherwise no room has been placed onto
                    this map.
*)

PROCEDURE PutRoomOntoMap (x, y: CARDINAL) : BOOLEAN ;
VAR
   LenX,
   LenY    : CARDINAL ;
   Success : BOOLEAN ;
BEGIN
   CheckInitBoxRoomIndex ;
   WITH Stack[StackPtr] DO
      Success := FALSE ;
      REPEAT
         IF GetBox(LenX, LenY)
         THEN
            Success := PlaceRoomBox(x, y, LenX-1, LenY-1)
         END
      UNTIL Success OR (LenX=0) ;
   END ;
   CheckKillBoxIndex(LenX=0) ;
   RETURN( Success )
END PutRoomOntoMap ;


(*
   GetBox - returns true if a box can be returned.
            It chooses one box from the box index,
            from the stack.
            The lengths of the Box are returned
            in LengthX and LengthY.
*)

PROCEDURE GetBox (VAR LengthX, LengthY: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Stack[StackPtr] DO
      GetAndDeleteRandomBox(BoxIndex, LengthX, LengthY)
   END ;
   RETURN(LengthX#0)        (* LengthX#0 means found legal size box *)
END GetBox ;


(*
   CheckInitBoxCorridorIndex - checks to see whether the current
                               stacked box needs
                               a list of legal corridor sizes stacked.
*)

PROCEDURE CheckInitBoxCorridorIndex ;
BEGIN
   WITH Stack[StackPtr] DO
      IF BoxIndex=0
      THEN
         (* Without stacked box list of legal sized corridors *)
         BoxIndex := InitBoxes() ;
         AddBoxes(BoxIndex, MinCorridorLength, CorridorWidth,
                            MaxCorridorLength, CorridorWidth) ;
         AddBoxes(BoxIndex, CorridorWidth, MinCorridorLength,
                            CorridorWidth, MaxCorridorLength)
      END
   END
END CheckInitBoxCorridorIndex ;


(*
   CheckInitBoxRoomIndex - checks to see whether the current stack box
                           needs a list of legal corridor sizes stacked.
*)

PROCEDURE CheckInitBoxRoomIndex ;
BEGIN
   WITH Stack[StackPtr] DO
      IF BoxIndex=0
      THEN
         (* Without stacked box list of legal sized rooms *)
         BoxIndex := InitBoxes() ;
         AddBoxes(BoxIndex, MinRoomLength, MinRoomLength,
                            MaxRoomLength, MaxRoomLength)
      END
   END
END CheckInitBoxRoomIndex ;


(*
   CheckKillBoxIndex - if NeedToKill is set then the list of boxes
                       on the stack is killed.
                       Ideally this procedure should be a macro.
*)

PROCEDURE CheckKillBoxIndex (NeedToKill: BOOLEAN) ;
BEGIN
   IF NeedToKill
   THEN
      WITH Stack[StackPtr] DO
         KillBoxes(BoxIndex) ;
         BoxIndex := 0
      END
   END
END CheckKillBoxIndex ;


(*
   PlaceCorridorBox - returns true if a box can make a corridor at
                      position x, y.
                      All 4 orientations are tried.


                              2  1
                              4  3

                      Ie  1:  (x, y) (x+LenX, y+LenY)
                          2:  (x, y) (x-LenX, y+LenY)
                          3:  (x, y) (x+LenX, y-LenY)
                          4:  (x, y) (x-LenX, y-LenY)
*)

PROCEDURE PlaceCorridorBox (x, y: CARDINAL; LenX, LenY: CARDINAL) : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   i      : CARDINAL ;
BEGIN
   CheckInitOrientationIndex ;
   WITH Stack[StackPtr] DO
      Success := FALSE ;
      REPEAT
         i := GetAndDeleteRandom(OrientationIndex) ;
         CASE i OF

         1:  Success := AttemptToPlaceCorridor(x, y, x+LenX, y+LenY) |

         2:  IF x>LenX
             THEN
                Success := AttemptToPlaceCorridor(x-LenX, y, x, y+LenY)
             END |

         3:  IF y>LenY
             THEN
                Success := AttemptToPlaceCorridor(x, y-LenY, x+LenX, y)
             END |

         4:  IF (x>LenX) AND (y>LenY)
             THEN
                Success := AttemptToPlaceCorridor(x-LenX, y-LenY, x, y)
             END

         ELSE
         END
      UNTIL Success OR (i=0) ;
   END ;
   CheckKillOrientationIndex(i=0) ;
   RETURN( Success )
END PlaceCorridorBox ;


(*
   PlaceRoomBox - returns true if a box can make a corridor at
                  position x, y.
                  All 4 orientations are tried.


                          2  1
                          4  3

                  Ie  1:  (x, y) (x+LenX, y+LenY)
                      2:  (x, y) (x-LenX, y+LenY)
                      3:  (x, y) (x+LenX, y-LenY)
                      4:  (x, y) (x-LenX, y-LenY)
*)

PROCEDURE PlaceRoomBox (x, y: CARDINAL; LenX, LenY: CARDINAL) : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   i      : CARDINAL ;
BEGIN
   CheckInitOrientationIndex ;
   WITH Stack[StackPtr] DO
      Success := FALSE ;
      REPEAT
         i := GetAndDeleteRandom(OrientationIndex) ;
         CASE i OF

         1:  Success := AttemptToPlaceRoom(x, y, x+LenX, y+LenY) |

         2:  IF x>LenX
             THEN
                Success := AttemptToPlaceRoom(x-LenX, y, x, y+LenY)
             END |

         3:  IF y>LenY
             THEN
                Success := AttemptToPlaceRoom(x, y-LenY, x+LenX, y)
             END |

         4:  IF (x>LenX) AND (y>LenY)
             THEN
                Success := AttemptToPlaceRoom(x-LenX, y-LenY, x, y)
             END

         ELSE
         END
      UNTIL Success OR (i=0) ;
   END ;
   CheckKillOrientationIndex(i=0) ;
   RETURN( Success )
END PlaceRoomBox ;


(*
   CheckInitOrientationIndex - checks to see whether the current stacked
                               entity needs a new orientation index to also
                               be stacked.
*)

PROCEDURE CheckInitOrientationIndex ;
BEGIN
   WITH Stack[StackPtr] DO
      IF OrientationIndex=0
      THEN
         OrientationIndex := InitRandom() ;
         AddRandom(OrientationIndex, 4)
      END
   END
END CheckInitOrientationIndex ;


(*
   CheckKillOrientationIndex - checks to see whether the current stacked
                               entities orientation index needs to be
                               deleted.
                               This procedure ideally should be a macro..
*)

PROCEDURE CheckKillOrientationIndex (NeedToKill: BOOLEAN) ;
BEGIN
   IF NeedToKill
   THEN
      WITH Stack[StackPtr] DO
         KillRandom(OrientationIndex) ;
         OrientationIndex := 0
      END
   END
END CheckKillOrientationIndex ;


(*
   PushPerimeterOfBoxes - pushes all the current perimeter of the box map onto
                          the perimeter stack.
*)

PROCEDURE PushPerimeterOfBoxes (CoordIndex: CARDINAL; NoOpt: BOOLEAN) ;
VAR
   i: CARDINAL ;
BEGIN
   IF NoOfBoxes=0
   THEN
      (* Perimeter is center square in map *)
      AddCoord(CoordIndex, MaxX DIV 2, MaxY DIV 2)
   ELSE
      i := 1 ;
      WHILE i<=NoOfBoxes DO
         PushPerimeterOfWalls(CoordIndex, i, NoOpt) ;
         INC(i)
      END
   END
END PushPerimeterOfBoxes ;


(*
   PushPerimeterOfWalls - pushes all coordinates of a box wall which are
                          external to the group of boxes.
                          Ie any wall which does is not shared by an
                          adjacent box MUST be external.
                          NoOpt determines whether optimization should be
                          applied to the restricting of perimeter coords.
                          Optimiztion tests for the minimum size of a room
                          to any wall, if this fails the coord is not added
                          to the perimeter list.
                          However this should not be used when pushing the
                          room perimeter since optimization is too restrictive.
                          (Corridor restrictions etc).
*)

PROCEDURE PushPerimeterOfWalls (CoordIndex: CARDINAL; b: CARDINAL;
                                NoOpt: BOOLEAN) ;
VAR
   i, j: CARDINAL ;
BEGIN
   WITH Boxes[b] DO
      FOR i := x1 TO x2 DO
         IF IsExternalHorizWallPerimeter(b, i, y1) AND
            (NoOpt OR IsEnoughSpacePointToBox(i, y1))
         THEN
            AddCoord(CoordIndex, i, y1)
         END ;
         IF IsExternalHorizWallPerimeter(b, i, y2) AND
            (NoOpt OR IsEnoughSpacePointToBox(i, y2))
         THEN
            AddCoord(CoordIndex, i, y2)
         END
      END ;
      FOR j := y1 TO y2 DO
         IF IsExternalVertWallPerimeter(b, x1, j) AND
            (NoOpt OR IsEnoughSpacePointToBox(x1, j))
         THEN
            AddCoord(CoordIndex, x1, j)
         END ;
         IF IsExternalVertWallPerimeter(b, x2, j) AND
            (NoOpt OR IsEnoughSpacePointToBox(x2, j))
         THEN
            AddCoord(CoordIndex, x2, j)
         END
      END
   END
END PushPerimeterOfWalls ;


(*
   IsExternalHorizWallPerimeter - returns true if coordinates,
                                  x and y are not on any Horiz
                                  wall of any box except b.
                                  This routine allows point z, y to be
                                  on a Vertical wall, but NOT on another
                                  Horizontal wall.
*)

PROCEDURE IsExternalHorizWallPerimeter (b: CARDINAL;
                                        x, y: CARDINAL) : BOOLEAN ;
VAR
   i    : CARDINAL ;
   Found: BOOLEAN ;
BEGIN
   Found := FALSE ;
   IF NOT IsCornerPerimeter(b, x, y)
   THEN
      i := 0 ;
      WHILE (i<=NoOfBoxes) AND (NOT Found) DO
         IF i#b
         THEN
            WITH Boxes[i] DO
               IF IsPointOnLine(x, y, x1, y1, x2, y1)
               THEN
                  Found := TRUE
               ELSIF IsPointOnLine(x, y, x1, y2, x2, y2)
               THEN
                  Found := TRUE
               END
            END
         END ;
         INC(i)
      END
   END ;
   RETURN( NOT Found )
END IsExternalHorizWallPerimeter ;


(*
   IsExternalVertWallPerimeter - returns true if coordinates,
                                 x and y are not on any Vertical
                                 wall of any box except b.
                                 This routine allows point z, y to be
                                 on a Horizontal wall, but NOT on another
                                 Vertical wall.
*)

PROCEDURE IsExternalVertWallPerimeter (b: CARDINAL;
                                       x, y: CARDINAL) : BOOLEAN ;
VAR
   i    : CARDINAL ;
   Found: BOOLEAN ;
BEGIN
   Found := FALSE ;
   IF NOT IsCornerPerimeter(b, x, y)
   THEN
      i := 0 ;
      WHILE (i<=NoOfBoxes) AND (NOT Found) DO
         IF i#b
         THEN
            WITH Boxes[i] DO
               IF IsPointOnLine(x, y, x1, y1, x1, y2)
               THEN
                  Found := TRUE
               ELSIF IsPointOnLine(x, y, x2, y1, x2, y2)
               THEN
                  Found := TRUE
               END
            END
         END ;
         INC(i)
      END
   END ;
   RETURN( NOT Found )
END IsExternalVertWallPerimeter ;


(*
   AttemptToPlaceCorridor - attempts to place a corridor x1, y1  x2, y2
                            onto the map.
                            If it succeeds it returns true
                            otherwise false
*)

PROCEDURE AttemptToPlaceCorridor (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF IsCorridorSatisfied(x1, y1, x2, y2)
   THEN
      AddBox(x1, y1, x2, y2) ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END AttemptToPlaceCorridor ;


(*
   AttemptToPlaceRoom - attempts to place a room x1, y1  x2, y2
                        onto the map.
                        If it succeeds it returns true
                        otherwise false
*)

PROCEDURE AttemptToPlaceRoom (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   IF IsRoomSatisfied(x1, y1, x2, y2)
   THEN
      AddBox(x1, y1, x2, y2) ;
      Success := TRUE
   ELSE
      Success := FALSE
   END ;
   RETURN( Success )
END AttemptToPlaceRoom ;


(*
   IsCorridorSatisfied - returns true if a Corridor x1, y1  x2, y2
                         may be placed onto the map without
                         contraveining the various rules.
*)

PROCEDURE IsCorridorSatisfied (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   (* Put(x1, y1, x2, y2) ; *)
   IF (x2>MaxX) OR (y2>MaxY)
   THEN
      (* WriteString('Failed SIZE') ; WriteLn *)
      Success := FALSE
   ELSIF NOT DistanceAppartEdge(x1, y1, x2, y2)
   THEN
      Success := FALSE
   ELSIF IsOverLappingBox(x1, y1, x2, y2)
   THEN
      (* WriteString('Failed OVERLAP') ; *)
      Success := FALSE
   ELSIF NOT IsCorridorJoin(x1, y1, x2, y2)
   THEN
      (* WriteString('Failed CORRIDOR JOIN') ; *)
      Success := FALSE
   ELSIF NOT IsEnoughSpaceBetweenCorridors(x1, y1, x2, y2)
   THEN
      (* WriteString('Failed SPACE') ; *)
      Success := FALSE
   ELSE
      Success := TRUE
   END ;
   RETURN( Success )
END IsCorridorSatisfied ;


(*
   IsRoomSatisfied - returns true if a box x1, y1  x2, y2
                     may be placed onto the map without
                     contraveining the various rules.
*)

PROCEDURE IsRoomSatisfied (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   (* Put(x1, y1, x2, y2) ; *)
   IF (x2>MaxX) OR (y2>MaxY)
   THEN
      (* WriteString('Failed SIZE') ; WriteLn ; *)
      Success := FALSE
   ELSIF IsOverLappingBox(x1, y1, x2, y2)
   THEN
      (* WriteString('Failed OVERLAP') ; *)
      Success := FALSE
   ELSIF NOT DistanceAppartEdge(x1, y1, x2, y2)
   THEN
      Success := FALSE
   ELSIF NOT IsBoxRoomLegal(x1, y1, x2, y2)
   THEN
      (* WriteString('Failed Legal') ; *)
      Success := FALSE
   ELSIF NOT IsRoomJoin(x1, y1, x2, y2)
   THEN
      (* WriteString('Failed ROOM JOIN') ; *)
      Success := FALSE
   ELSIF NOT IsEnoughSpaceBetweenRooms(x1, y1, x2, y2)
   THEN
      (* WriteString('Failed SPACE') ; *)
      Success := FALSE
   ELSE
      Success := TRUE
      (* ; WriteString('SUCCESS') ; *)
   END ;
   RETURN( Success )
END IsRoomSatisfied ;


(*
   IsEnoughSpacePointToBox - returns true if there is enough space
                             between a point, x, y and all the boxes.
                             This routine is called before perimeter
                             coordinates are pushed, therefore coordinates
                             pushed are not doomed to failure due to lack
                             of space.
                             This routine consists of a reduced
                             IsEnoughSpaceBetweenBoxes procedure.
*)

PROCEDURE IsEnoughSpacePointToBox (x, y: CARDINAL) : BOOLEAN ;
VAR
   ok      : BOOLEAN ;
   i       : CARDINAL ;
   Distance: CARDINAL ;
BEGIN
   ok := TRUE ;
   i := 0 ;  (* 0 = Perimeter of map *)
   WHILE ok AND (i<=NoOfBoxes) DO
      Distance := DistanceAppartPoint(i, x, y) ;
      IF Distance#0
      THEN
         ok := (Distance>=MinDistanceBetweenRooms)
      END ;
      INC(i)
   END ;
   RETURN( ok )
END IsEnoughSpacePointToBox ;


(*
   IsEnoughSpaceBetweenCorridors - returns true if there is enough
                                   space between box x1, y1  x2, y2
                                   and the other boxes.
                                   Also tests for right angle connection.
*)

PROCEDURE IsEnoughSpaceBetweenCorridors (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   ok      : BOOLEAN ;
   i       : CARDINAL ;
   Distance: CARDINAL ;
BEGIN
   ok := TRUE ;
   i := 1 ;
   WHILE ok AND (i<=NoOfBoxes) DO
      IF IsTouchingBox(i, x1, y1, x2, y2)
      THEN
         (* Check for a box that is not at right angles to new box. *)
         (* We are only allowed to touch a box at right angles.     *)
         IF NOT IsDifferentOrientationBox(i, x1, y1, x2, y2)
         THEN
            (* touching a box which is not at right angles *)
            ok := FALSE
         END
      ELSIF FreeSpace(i, x1, y1, x2, y2)
      THEN
         Distance := DistanceAppartBox(i, x1, y1, x2, y2) ;
(*
      Distance := Min(
                       DistanceAppartBox(i, x1, y1, x2, y2),
                       DistanceAppartDiagonal(i, x1, y1, x2, y2)
                     ) ;
*)
         IF Distance=0
         THEN
         ELSE
            ok := (Distance>=MinDistanceBetweenCorridors)
         END
      END ;
      INC(i)
   END ;
   RETURN( ok )
END IsEnoughSpaceBetweenCorridors ;


(*
   IsBoxRoomLegal - returns true if a box x1, y1, x2, y2 does not
                    have a wall which is next to but not sharing
                    another wall.
*)

PROCEDURE IsBoxRoomLegal (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   ok      : BOOLEAN ;
   i       : CARDINAL ;
   Distance: CARDINAL ;
   CoveredN,
   CoveredS,
   CoveredE,
   CoveredW: BOOLEAN ;
BEGIN
   CoveredN := IsFullyCovered(x1, y2, x2, y2) ;
   CoveredS := IsFullyCovered(x1, y1, x2, y1) ;
   CoveredE := IsFullyCovered(x2, y1, x2, y2) ;
   CoveredW := IsFullyCovered(x1, y1, x1, y2) ;
   ok := TRUE ;
   i := 1 ;
   WHILE ok AND (i<=NoOfBoxes) DO
      IF NOT IsTouchingBox(i, x1, y1, x2, y2)
      THEN
         IF (x1>1) AND (NOT CoveredW)
         THEN
            ok := NOT IsTouchingBox(i, x1-1, y1, x2, y2)
         END ;
         IF ok AND (y1>1) AND (NOT CoveredS)
         THEN
            ok := NOT IsTouchingBox(i, x1, y1-1, x2, y2)
         END ;
         IF ok AND (x2<MaxX) AND (NOT CoveredE)
         THEN
            ok := NOT IsTouchingBox(i, x1, y1, x2+1, y2)
         END ;
         IF ok AND (y2<MaxY) AND (NOT CoveredN)
         THEN
            ok := NOT IsTouchingBox(i, x1, y1, x2, y2+1)
         END
      END ;
      INC(i)
   END ;
   RETURN( ok )
END IsBoxRoomLegal ;


(*
   IsFullyCovered - returns true if every point on the line
                    x1, y1, x2, y2 is covered. The line must
                    either be horizontal or vertical.
*)

PROCEDURE IsFullyCovered (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   i      : CARDINAL ;
   Covered: BOOLEAN ;
BEGIN
   Covered := TRUE ;
   IF x1=x2
   THEN
      (* Vertical *)
      i := y1 ;
      WHILE Covered AND (i<=y2) DO
         Covered := IsSpace(x1, i) ;
         INC(i)
      END
   ELSIF y1=y2
   THEN
      (* Horizontal *)
      i := x1 ;
      WHILE Covered AND (i<=x2) DO
         Covered := IsSpace(i, y1) ;
         INC(i)
      END
   END ;
   RETURN( Covered )
END IsFullyCovered ;


(*
   IsEnoughSpaceBetweenRooms - returns true if there is enough
                               space between box x1, y1  x2, y2
                               and the other boxes.
*)

PROCEDURE IsEnoughSpaceBetweenRooms (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   ok      : BOOLEAN ;
   i       : CARDINAL ;
   Distance: CARDINAL ;
BEGIN
   ok := TRUE ;
   i := 1 ;
   WHILE ok AND (i<=NoOfBoxes) DO
      IF NOT IsTouchingBox(i, x1, y1, x2, y2)
      THEN
         (* Dont test the walls of the box for contraveining the space rule *)
(*
         IF (x1+1<x2-1) AND (y1+1<y2-1) AND FreeSpace(i, x1+1, y1+1, x2-1, y2-1)
         THEN
            Distance := DistanceAppartDiagonal(i, x1+1, y1+1, x2-1, y2-1) ;
*)
         IF FreeSpace(i, x1, y1, x2, y2)
         THEN
(*
            Distance := DistanceAppartDiagonal(i, x1, y1, x2, y2) ;
*)
            Distance := Max( DistanceAppartDiagonal(i, x1, y1, x2, y2),
                             DistanceAppartBox(i, x1, y1, x2, y2) ) ;
            (* WriteString('Dist') ; WriteCard(Distance, 6) ; WriteLn ; *)
            IF Distance=0
            THEN
               (* touching a box *)
            ELSE
               Assert(NOT IsTouchingBox(i, x1, y1, x2, y2)) ;
               ok := (Distance>=MinDistanceBetweenRooms)
            END
         END
      END ;
      INC(i)
   END ;
   RETURN( ok )
END IsEnoughSpaceBetweenRooms ;


(*
   FreeSpace - returns true if there exists free space between box
               X1, Y1, X2, Y2 and box b.
               Should not be called if box b touches X1, Y1, X2, Y2.
*)

PROCEDURE FreeSpace (b: CARDINAL; X1, Y1, X2, Y2: CARDINAL) : BOOLEAN ;
VAR
   Free  : BOOLEAN ;
   xs, xe,
   ys, ye,
   i, j  : CARDINAL ;
BEGIN
   WITH Boxes[b] DO
      IF Abs(X1, x2)<Abs(X2, x1)
      THEN
         xs := Min(X1, x2) ;
         xe := Max(X1, x2)
      ELSE
         xs := Min(X2, x1) ;
         xe := Max(X2, x1)
      END ;
      IF Abs(Y1, y2)<Abs(Y2, y1)
      THEN
         ys := Min(Y1, y2) ;
         ye := Max(Y1, y2)
      ELSE
         ys := Min(Y2, y1) ;
         ye := Max(Y2, y1)
      END ;
      Free := FALSE ;
      i := xs ;
      WHILE (NOT Free) AND (i<=xe) DO
         j := ys ;
         WHILE (NOT Free) AND (j<=ye) DO
            Free := IsSpace(i, j) ;
            INC(j)
         END ;
         INC(i)
      END
   END ;
   (* IF Free THEN WriteString('FREE') END ; *)
   RETURN( Free )
END FreeSpace ;


(*
   IsSpace - returns true if point x, y is not in any box.
             A wall is counted as in the box.
*)

PROCEDURE IsSpace (x, y: CARDINAL) : BOOLEAN ;
VAR
   b    : CARDINAL ;
   InBox: BOOLEAN ;
BEGIN
   InBox := FALSE ;
   b := 1 ;   (* Not zero of course !! *)
   WHILE (NOT InBox) AND (b<=NoOfBoxes) DO
      WITH Boxes[b] DO
         InBox := IsSubRange(x1, x2, x) AND IsSubRange(y1, y2, y)
      END ;
      INC(b)
   END ;
   RETURN( NOT InBox )
END IsSpace ;


(*
   DistanceAppartEdge - returns true if the box, x1, y1, x2, y2, is a
                        required distance away from the edge of the
                        map.
                        Cannot use room zero for this test as we are inside
                        it and may touch one wall but be too near another!
*)

PROCEDURE DistanceAppartEdge (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   ok      : BOOLEAN ;
   Distance: CARDINAL ;
BEGIN
    ok := TRUE ;
    Distance := Abs(x1, 1) ;
    IF (Distance>0) AND ok
    THEN
       ok := (Distance>=MinDistanceBetweenRooms)
    END ;
    Distance := Abs(x2, MaxX) ;
    IF (Distance>0) AND ok
    THEN
       ok := (Distance>=MinDistanceBetweenRooms)
    END ;
    Distance := Abs(y1, 1) ;
    IF (Distance>0) AND ok
    THEN
       ok := (Distance>=MinDistanceBetweenRooms)
    END ;
    Distance := Abs(y2, MaxY) ;
    IF (Distance>0) AND ok
    THEN
       ok := (Distance>=MinDistanceBetweenRooms)
    END ;
    RETURN( ok )
END DistanceAppartEdge ;


(*
   DistanceAppartPoint - returns the distance appart between box, b,
                         and point X, Y.
*)

PROCEDURE DistanceAppartPoint (b: CARDINAL; X, Y: CARDINAL) : CARDINAL ;
VAR
   Xmin,
   Ymin: CARDINAL ;
BEGIN
   WITH Boxes[b] DO
      IF IsSubRange(x1, x2, X)
      THEN
         Ymin := Min( Abs(y1, Y), Abs(y2, Y) )
      ELSE
         Ymin := MaxCard
      END ;
      IF IsSubRange(y1, y2, Y)
      THEN
         Xmin := Min( Abs(x1, X), Abs(x2, X) )
      ELSE
         Xmin := MaxCard
      END
   END ;
   RETURN( Min(Xmin, Ymin) )
END DistanceAppartPoint ;


(*
   DistanceAppartBox - returns the distance appart between box, b,
                       and box X1, Y1, X2, Y2
*)

PROCEDURE DistanceAppartBox (b: CARDINAL; X1, Y1, X2, Y2: CARDINAL) : CARDINAL ;
VAR
   Xmin,
   Ymin: CARDINAL ;
BEGIN
   WITH Boxes[b] DO
      IF IsIntersectingRange(x1, x2, X1, X2)
      THEN
         Ymin := Min(
                      Min( Abs(y1, Y1), Abs(y2, Y2) ),
                      Min( Abs(y1, Y2), Abs(Y1, y2) )
                    )
      ELSE
         Ymin := MaxCard
      END ;
      IF IsIntersectingRange(y1, y2, Y1, Y2)
      THEN
         Xmin := Min(
                      Min( Abs(x1, X1), Abs(x2, X2) ),
                      Min( Abs(x1, X2), Abs(X1, x2) )
                    )
      ELSE
         Xmin := MaxCard
      END
   END ;
   RETURN( Min(Xmin, Ymin) )
END DistanceAppartBox ;


(*
   DistanceAppartDiagonal - returns the diagonal
                            distance appart between X1, Y1, X2, Y2
                            and box b.
*)

PROCEDURE DistanceAppartDiagonal (b: CARDINAL;
                                  X1, Y1, X2, Y2: CARDINAL) : CARDINAL ;
BEGIN
   WITH Boxes[b] DO
      RETURN(
              Min(
                   Min( Abs(x1, X2), Abs(x2, X1) ),
                   Min( Abs(y1, Y2), Abs(y2, Y1) )
                 )
            )
   END
END DistanceAppartDiagonal ;


(*
   IsCorridorJoin - returns true if a box corridor x1, y1  x2, y2
                    joins another corridor at right angles without
                    cutting off the potential corridor door.

                    A corridor is thought of as

                    ##########################
                    |                        |
                    |                        |
                    ##########################

                    and may only be placed together in a way such that
                    they meet  -|  or  -  etc
                                       |

                    False is returned if this box corridor does not
                    correctly form a T junction with another.
*)

PROCEDURE IsCorridorJoin (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   ok,
   DoorFound: BOOLEAN ;
   b        : CARDINAL ;
BEGIN
   ok := TRUE ;
   IF NoOfBoxes=0
   THEN
      DoorFound := TRUE
   ELSE
      DoorFound := FALSE ;
      b := 1 ;
      WHILE ok AND (b<=NoOfBoxes) DO
         (* WriteString('Box') ; WriteCard(b, 2) ; *)
         IF IsTouchingBox(b, x1, y1, x2, y2)
         THEN
            (* WriteString('TouchingBox') ; *)
            IF IsDifferentOrientationBox(b, x1, y1, x2, y2)
            THEN
               (* WriteString('Different Orientation') ; *)
               IF NOT DoorFound
               THEN
                  DoorFound := IsCorridorWallJoinBox(b, x1, y1, x2, y2)
               END
               (* ; IF ok THEN WriteString('WallJoin') END ; *)
            ELSE
               ok := FALSE  (* Dont allow parallel corridors to touch *)
            END
         END ;
         INC(b)
      END
   END ;
   RETURN( ok AND DoorFound )
END IsCorridorJoin ;


(*
   IsRoomJoin - returns true if a box room x1, y1  x2, y2
                joins another room with enough space for a door.
*)

PROCEDURE IsRoomJoin (x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
VAR
   DoorFound: BOOLEAN ;
   b        : CARDINAL ;
BEGIN
   IF NoOfBoxes=0
   THEN
      DoorFound := TRUE
   ELSE
      DoorFound := FALSE ;
      b := 1 ;
      WHILE (NOT DoorFound) AND (b<=NoOfBoxes) DO
         (* WriteString('Box') ; WriteCard(b, 2) ; *)
         IF IsTouchingBox(b, x1, y1, x2, y2)
         THEN
            IF NOT DoorFound
            THEN
               DoorFound := IsRoomWallJoinBox(b, x1, y1, x2, y2)
            END
         END ;
         INC(b)
      END
   END ;
   RETURN( DoorFound )
END IsRoomJoin ;


(*
   IsCorridorWallJoinBox - returns true if box, b, and box X1, Y1  X2, Y2
                           form a correct join ie covering the potential
                           door.
*)

PROCEDURE IsCorridorWallJoinBox (b: CARDINAL;
                                 X1, Y1, X2, Y2: CARDINAL) : BOOLEAN ;
VAR
   Success: BOOLEAN ;
BEGIN
   WITH Boxes[b] DO
      Success := (((X1=x1) OR (X1=x2) OR (X2=x1) OR (X2=x2))
                              AND IsSubLine(Y1, Y2, y1, y2)) OR
                 (((Y1=y1) OR (Y1=y2) OR (Y2=y1) OR (Y2=y2))
                              AND IsSubLine(X1, X2, x1, x2)) ;
      RETURN( Success )
   END
END IsCorridorWallJoinBox ;


(*
   IsRoomWallJoinBox - returns true if box, b, and box X1, Y1  X2, Y2
                       form a correct join ie covering the potential
                       door.
*)

PROCEDURE IsRoomWallJoinBox (b: CARDINAL;
                             X1, Y1, X2, Y2: CARDINAL) : BOOLEAN ;
VAR
   DoorWidth: CARDINAL ;
BEGIN
   DoorWidth := 0 ;
   WITH Boxes[b] DO
      IF (X1=x2) OR (x1=X2)
      THEN
         DoorWidth := IntersectionLength(Y1, Y2, y1, y2) ;
         IF (IsSubRange(Y1, Y2, y1) OR IsSubRange(y1, y2, Y1)) AND (DoorWidth>1)
         THEN
            DEC(DoorWidth)
         END ;
         IF (IsSubRange(Y1, Y2, y2) OR IsSubRange(y1, y2, Y2)) AND (DoorWidth>1)
         THEN
            DEC(DoorWidth)
         END
      ELSIF (Y1=y2) OR (y1=Y2)
      THEN
         DoorWidth := IntersectionLength(X1, X2, x1, x2) ;
         IF (IsSubRange(X1, X2, x1) OR IsSubRange(x1, x2, X1)) AND (DoorWidth>1)
         THEN
            DEC(DoorWidth)
         END ;
         IF (IsSubRange(X1, X2, x2) OR IsSubRange(x1, x2, X2)) AND (DoorWidth>1)
         THEN
            DEC(DoorWidth)
         END
      END ;
      RETURN( DoorWidth>=MinDoorLength )
   END
END IsRoomWallJoinBox ;


(*
   IsDifferentOrientationBox - returns true if box b has a different
                               orientation to box X1, Y1  X2, Y2.
*)

PROCEDURE IsDifferentOrientationBox (b: CARDINAL;
                                     X1, Y1, X2, Y2: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Boxes[b] DO
      IF Abs(X1, X2) = Abs(Y1, Y2)
      THEN
         RETURN( TRUE )                        (* Square X1, Y1, X2, Y2 *)
      ELSIF Abs(X1, X2) > Abs(Y1, Y2)
      THEN
         RETURN( Abs(x1, x2) <= Abs(y1, y2) )
      ELSE
         RETURN( Abs(x1, x2) >= Abs(y1, y2) )
      END
   END
END IsDifferentOrientationBox ;


(*
   IsTouchingBox - returns true if a box X1, Y1  X2, Y2 touches box b
                   or if it intersects with this box.
*)

PROCEDURE IsTouchingBox (b: CARDINAL; X1, Y1, X2, Y2: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Boxes[b] DO
      RETURN( NOT ((X2<x1) OR (X1>x2) OR (Y2<y1) OR (Y1>y2)) )
   END
END IsTouchingBox ;


(*
   IsCornerPerimeter - returns true if box, b, has a corner x, y which
                       is a perimeter.
*)

PROCEDURE IsCornerPerimeter (b: CARDINAL; x, y: CARDINAL) : BOOLEAN ;
VAR
   Perimeter: BOOLEAN ;
   i, j     : CARDINAL ;
BEGIN
   IF IsCorner(b, x, y)
   THEN
      Perimeter := FALSE ;
      i := x-1 ;
      j := y-1 ;
      WHILE (NOT Perimeter) AND (i<=x+1) DO
         j := y-1 ;
         WHILE (NOT Perimeter) AND (j<=y+1) DO
            IF IsSubRange(1, MaxX, i) AND IsSubRange(1, MaxY, j)
            THEN
               Perimeter := IsSpace(i, j)
            END ;
            INC(j, 2)
         END ;
         INC(i, 2)
      END ;
      RETURN( Perimeter )
   ELSE
      RETURN( FALSE )
   END
END IsCornerPerimeter ;


(*
   IsCorner - returns true if box, b, has a corner x, y.
*)

PROCEDURE IsCorner (b: CARDINAL; x, y: CARDINAL) : BOOLEAN ;
BEGIN
   WITH Boxes[b] DO
      RETURN( ((x1=x) OR (x2=x)) AND ((y1=y) OR (y2=y)) )
   END
END IsCorner ;


(*
   IsOverLappingBox - returns true if box X1, Y1  X2, Y2 overlaps
                      with another box NOT including edges touching.
*)

PROCEDURE IsOverLappingBox (X1, Y1, X2, Y2: CARDINAL) : BOOLEAN ;
VAR
   b    : CARDINAL ;
   Found: BOOLEAN ;
BEGIN
   Found := FALSE ;
   b := 1 ;
   WHILE (NOT Found) AND (b<=NoOfBoxes) DO
      WITH Boxes[b] DO
         Found := IsIntersection(x1, y1, x2, y2, X1, Y1, X2, Y2)
      END ;
      INC(b)
   END ;
   RETURN( Found )
END IsOverLappingBox ;


(*
   IsIntersection - returns true if two boxes x1, y1  x2, y2 intersects
                    with X1, Y1  X2, Y2. Wall touching is allowed.
*)

PROCEDURE IsIntersection (x1, y1, x2, y2,
                          X1, Y1, X2, Y2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( NOT ( (x2<=X1) OR (x1>=X2) OR (y2<=Y1) OR (y1>=Y2) ) )
END IsIntersection ;


(*
   AddBox - adds a box to the list of boxes and
            adds a box to the Map.
*)

PROCEDURE AddBox (X1, Y1, X2, Y2: CARDINAL) ;
BEGIN
   IF NoOfBoxes=MaxBoxes
   THEN
      WriteString('Too many boxes in Module MakeMap') ; WriteLn ;
      HALT
   ELSE
      INC(NoOfBoxes) ;
      WITH Boxes[NoOfBoxes] DO
         x1 := X1 ;
         y1 := Y1 ;
         x2 := X2 ;
         y2 := Y2
      END
   END
END AddBox ;


(*
   GetCh - waits for a character to be pressed.
*)

PROCEDURE GetCh ;
VAR
   ch: CHAR ;
BEGIN
   Read(ch)
END GetCh ;


BEGIN
   Init
END BoxMap.
