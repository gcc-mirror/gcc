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

IMPLEMENTATION MODULE StoreCoords ;


FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM Chance IMPORT GetRand ;


CONST
   MaxCoord = 15000 ;
   MaxIndex =   500 ;

TYPE
   Coord  = RECORD
               X,
               Y: CARDINAL ;
            END ;

   Index  = RECORD
               Start,               (* Start of the Coord list *)
               End  : CARDINAL ;    (* End of the Coord list   *)
            END ;

VAR
   CoordIndex : ARRAY [0..MaxIndex] OF Index ;
   Coords     : ARRAY [1..MaxCoord] OF Coord ;
   NoOfCoords : CARDINAL ;   (* Number of coordinates in array Coords *)
   NoOfIndices: CARDINAL ;   (* Number of indices in CoordIndex       *)


(*
   InitCoords - Initializes a potential list of coordinates.
                An index to this potential coordinate list is returned.
*)

PROCEDURE InitCoords () : CARDINAL ;
BEGIN
   IF NoOfIndices=MaxIndex
   THEN
      WriteString('Too many coordinate list indices in Module StoreCoords') ;
      WriteLn ;
      WriteString('Increase MaxIndex') ;
      WriteLn ;
      HALT
   ELSE
      INC(NoOfIndices) ;
      WITH CoordIndex[NoOfIndices] DO
         Start := NoOfCoords+1 ;
         End := 0
      END ;
      AddCoord(NoOfIndices, 0, 0) ;  (* Dummy coordinate that we keep *)
      RETURN(NoOfIndices)            (* for the life of this list.    *)
   END
END InitCoords ;


(*
   KillCoords - Kills a complete coordinate list.
*)

PROCEDURE KillCoords (CoordListIndex: CARDINAL) ;
BEGIN
   IF NoOfIndices>0
   THEN
      (* Destroy index to Coord list *)
      WITH CoordIndex[CoordListIndex] DO
         WriteString('No of coords') ; WriteCard(End-Start+1, 4) ; WriteLn ;
         Start := 0 ;
         End := 0
      END ;
      (*
         If killed last Coord list see if we can garbage collect
         previously killed middle indices.
      *)
      IF NoOfIndices=CoordListIndex
      THEN
         REPEAT
            DEC(NoOfIndices)
         UNTIL (NoOfIndices=0) OR (CoordIndex[NoOfIndices].Start#0)
      END ;
      NoOfCoords := CoordIndex[NoOfIndices].End
   ELSE
      WriteString('All Coordinate lists have been killed - Module StoreCoords') ;
      WriteLn ;
      HALT
   END
END KillCoords ;



(*
   AddCoord - places a coordinate into the specified list.
*)

PROCEDURE AddCoord (CoordListIndex: CARDINAL; x, y: CARDINAL) ;
BEGIN
   IF NoOfCoords=MaxCoord
   THEN
      WriteString('Too many coordinates in a list in Module StoreCoords') ;
      WriteLn ;
      WriteString('Increase MaxCoord') ;
      WriteLn ;
      HALT
   ELSIF UniqueCoord(CoordListIndex, x, y)
   THEN
      INC(NoOfCoords) ;
      WITH Coords[NoOfCoords] DO
         X := x ;
         Y := y
      END ;
      WITH CoordIndex[CoordListIndex] DO
         End := NoOfCoords
      END
   END
END AddCoord ;


(*
   UniqueCoord - returns true if x and y are unique in the coord list.
*)

PROCEDURE UniqueCoord (CoordListIndex: CARDINAL;
                       x, y: CARDINAL) : BOOLEAN ;
VAR
   i    : CARDINAL ;
   Found: BOOLEAN ;
BEGIN
   WITH CoordIndex[CoordListIndex] DO
      i := Start ;
      Found := FALSE ;
      WHILE (NOT Found) AND (i<=End) DO
         WITH Coords[i] DO
            Found := (X=x) AND (Y=y)
         END ;
         INC(i)
      END
   END ;
   RETURN( NOT Found )
END UniqueCoord ;


(*
   GetAndDeleteRandomCoord - Returns a random coordinate from the coordinate
                             list and then it is deleted from the list.
*)

PROCEDURE GetAndDeleteRandomCoord (CoordListIndex: CARDINAL;
                                   VAR x, y: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   WITH CoordIndex[CoordListIndex] DO
      i := Start+GetRand(End-Start+1) ;  (* +1 for GetRand *)
      j := i ;
      REPEAT
         IF Coords[j].X=0
         THEN
            INC(j) ;
            IF j>End
            THEN
               j := Start
            END
         END
      UNTIL (j=i) OR (Coords[j].X#0) ;
      WITH Coords[j] DO
         x := X ;
         y := Y ;
         X := 0 ;      (* Now delete this box *)
         Y := 0
      END
   END
END GetAndDeleteRandomCoord ;


(*
   CoordsExist - returns true if a coordinate exists
                 within the CoordListIndex.
*)

PROCEDURE CoordsExist (CoordListIndex: CARDINAL) : BOOLEAN ;
VAR
   i : CARDINAL ;
   ok: BOOLEAN ;
BEGIN
   ok := FALSE ;
   WITH CoordIndex[CoordListIndex] DO
      IF End>0
      THEN
         (* Was at least one coordinate *)
         i := Start ;
         WHILE (NOT ok) AND (i<=End) DO
            ok := (Coords[i].X#0) ;   (* #0 means coordinate still exists *)
            INC(i)
         END
      END
   END ;
   RETURN( ok )
END CoordsExist ;


PROCEDURE Init ;
BEGIN
   NoOfCoords := 0 ;
   NoOfIndices := 0 ;
   WITH CoordIndex[NoOfIndices] DO
      End := 0
   END
END Init ;


BEGIN
   Init
END StoreCoords.
