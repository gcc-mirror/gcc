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
IMPLEMENTATION MODULE MakeBoxes ;


FROM StrIO IMPORT WriteString, WriteLn ;

FROM Chance IMPORT GetRand ;


CONST
   MaxBox   = 15000 ;
   MaxIndex =   500 ;

TYPE
   Box    = RECORD
               LengthX,
               LengthY: CARDINAL ;
            END ;

   Index  = RECORD
               Start,               (* Start of the Box list *)
               End  : CARDINAL ;    (* End of the Box list   *)
            END ;

VAR
   BoxIndex   : ARRAY [0..MaxIndex] OF Index ;
   Boxes      : ARRAY [1..MaxBox] OF Box ;
   NoOfBoxes  : CARDINAL ;   (* Number of boxes in array Boxes *)
   NoOfIndices: CARDINAL ;   (* Number of indices in BoxIndex  *)


(*
   InitBoxes - Initializes a list of boxes.
               An index to this box list is returned.
*)

PROCEDURE InitBoxes () : CARDINAL ;
BEGIN
   IF NoOfIndices=MaxIndex
   THEN
      WriteString('Too many box list indices in Module MakeBoxes') ;
      WriteLn ;
      WriteString('Increase MaxIndex') ;
      WriteLn ;
      HALT
   ELSE
      INC(NoOfIndices) ;
      WITH BoxIndex[NoOfIndices] DO
         Start := NoOfBoxes+1 ;
         End := NoOfBoxes
      END ;
      RETURN(NoOfIndices)
   END
END InitBoxes ;


(*
   AddBoxes - Adds a list of boxes MinX..MaxX, MinY..MaxY
              to a box list BoxListIndex.
*)

PROCEDURE AddBoxes (BoxListIndex: CARDINAL;
                    MinX, MinY, MaxX, MaxY: CARDINAL) ;
BEGIN
   WITH BoxIndex[BoxListIndex] DO
      Expand(BoxListIndex, MinX, MinY, MaxX, MaxY) ;
      End := NoOfBoxes
   END
END AddBoxes ;


(*
   Expand - expands the box limitations MinX..MaxX, MinY..MaxY for all
            possibilities of boxes.
*)

PROCEDURE Expand (BoxListIndex: CARDINAL;
                  MinX, MinY, MaxX, MaxY: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   i := MinX ;
   WHILE i<=MaxX DO
      j := MinY ;
      WHILE j<=MaxY DO
         AddBox(BoxListIndex, i, j) ;
         INC(j)
      END ;
      INC(i)
   END
END Expand ;


(*
   AddBox - adds a box of Width, Height to a list of boxes specified by
            BoxListIndex.
*)

PROCEDURE AddBox (BoxListIndex: CARDINAL;
                  Width, Height: CARDINAL) ;
BEGIN
   IF NoOfBoxes=MaxBox
   THEN
      WriteString('Too many boxes in a list in Module MakeBoxes') ;
      WriteLn ;
      WriteString('Increase MaxBox') ;
      WriteLn ;
      HALT
   ELSIF UniqueBox(BoxListIndex, Width, Height)
   THEN
      INC(NoOfBoxes) ;
      WITH Boxes[NoOfBoxes] DO
         LengthX := Width ;
         LengthY := Height
      END
   END
END AddBox ;


(*
   UniqueBox - returns true if a box Width, Height is unique in the
               box list BoxListIndex.
*)

PROCEDURE UniqueBox (BoxListIndex: CARDINAL;
                     Width, Height: CARDINAL) : BOOLEAN ;
VAR
   i    : CARDINAL ;
   Found: BOOLEAN ;
BEGIN
   WITH BoxIndex[BoxListIndex] DO
      i := Start ;
      Found := FALSE ;
      WHILE (NOT Found) AND (i<=End) DO
         WITH Boxes[i] DO
            Found := (LengthX=Width) AND (LengthY=Height)
         END ;
         INC(i)
      END
   END ;
   RETURN( NOT Found )
END UniqueBox ;


(*
   KillBoxes - Kills a complete box list.
*)

PROCEDURE KillBoxes (BoxListIndex: CARDINAL) ;
BEGIN
   IF NoOfIndices>0
   THEN
      (* Destroy index to box list *)
      WITH BoxIndex[BoxListIndex] DO
         Start := 0 ;
         End := 0
      END ;
      (*
         If killed last box list see if we can garbage collect
         previously killed middle indices.
      *)
      IF NoOfIndices=BoxListIndex
      THEN
         REPEAT
            DEC(NoOfIndices)
         UNTIL (NoOfIndices=0) OR (BoxIndex[NoOfIndices].Start#0)
      END ;
      NoOfBoxes := BoxIndex[NoOfIndices].End
   ELSE
      WriteString('All boxes have been killed - Module MakeBoxes') ;
      WriteLn ;
      HALT
   END
END KillBoxes ;


(*
   GetAndDeleteRandomBox - Returns a random box from the box list and
                           this box is then deleted from the list.
*)

PROCEDURE GetAndDeleteRandomBox (BoxListIndex: CARDINAL;
                                 VAR SizeX, SizeY: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   WITH BoxIndex[BoxListIndex] DO
      i := Start+GetRand(End-Start+1) ;  (* +1 for GetRand *)
      j := i ;
      REPEAT
         IF Boxes[j].LengthX=0
         THEN
            INC(j) ;
            IF j>End
            THEN
               j := Start
            END
         END
      UNTIL (j=i) OR (Boxes[j].LengthX#0) ;
      WITH Boxes[j] DO
         SizeX := LengthX ;
         SizeY := LengthY ;
         LengthX := 0 ;      (* Now delete this box *)
         LengthY := 0
      END
   END
END GetAndDeleteRandomBox ;


PROCEDURE Init ;
BEGIN
   NoOfBoxes := 0 ;
   NoOfIndices := 0 ;
   WITH BoxIndex[NoOfIndices] DO
      End := 0
   END
END Init ;


BEGIN
   Init
END MakeBoxes.
