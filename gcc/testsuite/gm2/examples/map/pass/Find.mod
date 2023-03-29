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
IMPLEMENTATION MODULE Find ;


FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;


CONST
   MaxX     =         10 ;
   MaxY     =         20 ;
   Infinity =      10000 ;
   Wall     = Infinity   ;
   Door     = Infinity-1 ;

VAR
   MapDist: ARRAY [0..MaxX], [0..MaxY] OF CARDINAL ;
   Xoffset,
   Yoffset: CARDINAL ;

(*
   FindOptimumRoute - finds the optimum route between two points,
                      x1, y1, x2, y2. The directions are returned
                      in a string, Commands. A boolean is returned
                      if any commands were entered.
*)

PROCEDURE FindOptimumRoute (x1, y1, x2, y2: INTEGER;
      	       	     	    VAR Commands: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   IF IsOptimumRoutePossible(x1, y1, x2, y2)
   THEN
      InitMapDist(x1, y1, x2, y2) ;
      RETURN( CalculateOptimumRoute(x1, y1, x2, y2, Commands) )
   ELSE
      RETURN( FALSE )
   END
END FindOptimumRoute ;


(*
   IsOptimumRoutePossible - returns true if we can use the optimum
                            route procedure to work out how to get
      	       	     	    to position, x2, y2 from x1, y1.
*)

PROCEDURE IsOptimumRoutePossible (x1, y1, x2, y2: INTEGER) : BOOLEAN ;
BEGIN
   RETURN( (Abs(x1, x2) <= MaxX) AND (Abs(y1, y2) <= MaxY) )
END IsOptimumRoutePossible ;


(*
   Min - returns the minimum of two cardinals.
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( b )
   ELSE
      RETURN( a )
   END
END Min ;


(*
   Abs - returns the absolute difference between two INTEGERs.
*)

PROCEDURE Abs (a, b: INTEGER) : INTEGER ;
BEGIN
   IF a>b
   THEN
      RETURN( a-b )
   ELSE
      RETURN( b-a )
   END
END Abs ;


(*
   CalculateOptimumRoute - calculates the optimum route between
      	       	     	   two points and putting the route into
      	       	     	   the string Commands. TRUE is returned
                           if any commands are placed into Commands.
*)

PROCEDURE CalculateOptimumRoute (x1, y1, x2, y2: INTEGER;
      	       	     	      	 VAR Commands: ARRAY OF CHAR) : BOOLEAN ;
VAR
   Distance: CARDINAL ;
BEGIN
   Distance := ScanBackwards(x1, y1, x2, y2, 0) ;
   RETURN( FALSE )
END CalculateOptimumRoute ;


(*
   ScanBackwards - fills in the DistMap with the distance values
      	       	   from x2, y2 to x1, y1.
*)

PROCEDURE ScanBackwards (x1, y1, x2, y2: INTEGER;
      	       	     	 Distance: CARDINAL) : CARDINAL ;
VAR
   ShortDist: CARDINAL ;
BEGIN
   IF ((MapDist[x2, y2]=0) OR (Distance < MapDist[x2, y2]))
   THEN
      ShortDist := Infinity ;
      MapDist[x2, y2] := Distance ;
      ShortDist := MoveVector(x1, y1, x2, y2,  0,  1, Distance) ;
      ShortDist := MoveVector(x1, y1, x2, y2,  1,  0, Distance) ;
      ShortDist := MoveVector(x1, y1, x2, y2,  0, -1, Distance) ;
      ShortDist := MoveVector(x1, y1, x2, y2, -1,  0, Distance) ;
      RETURN( ShortDist )
   ELSE
      RETURN( MapDist[x2, y2]+Distance )
   END
END ScanBackwards ;


(*
   MoveVector - returns a CARDINAL value indicating the distance between
      	        x2, y2 and x1, y1. It moves one step from x2, y2 along
      	        vector xv, yv. If this vector hits upon a door then
      	        we increment the vector - we dont increment the
      	        distance as one goes through a door using 1 unit
      	        of distance (actually covering 2 units of space).
*)

PROCEDURE MoveVector (x1, y1, x2, y2: INTEGER ;
      	       	      xv, yv  : INTEGER ;
      	       	      Distance: CARDINAL) : CARDINAL ;
VAR
   ShortDist: CARDINAL ;
BEGIN
   IF IsDoor(x2+xv, y2+yv)
   THEN
      xv := 2*xv ;
      yv := 2*yv
   END ;
   INC(Distance) ;
   y2 := y2+yv ;
   x2 := x2+xv ;
   IF CheckSquare(x2, y2)
   THEN
      ShortDist := ScanBackwards(x1, y1, x2, y2, Distance)
   ELSE
      ShortDist := Infinity
   END ;
   RETURN( ShortDist )
END MoveVector ;


(*
   CheckSquare - checks to see whether square, x, y, is free.
      	       	 If this square is free then true is returned.
*)

PROCEDURE CheckSquare (x, y: INTEGER) : BOOLEAN ;
BEGIN
   IF InRange(x, y) AND (MapDist[x, y] # Wall)
   THEN
      RETURN( TRUE )
   END ;
   RETURN( FALSE )
END CheckSquare ;


(*
   IsDoor - returns true if point, x, y, is legal and is
      	    on a door.
*)

PROCEDURE IsDoor (x, y: INTEGER) : BOOLEAN ;
BEGIN
   RETURN( InRange(x, y) AND (MapDist[x, y] = Door) )
END IsDoor ;


(*
   InRange - returns a boolean result determining whether, x, y,
      	     is a legal index for the MapDist array.
*)

PROCEDURE InRange (x, y: INTEGER) : BOOLEAN ;
BEGIN
   RETURN( (x <= MaxX) AND (y <= MaxY) AND
      	   (x >= 0)    AND (y >= 0) )
END InRange ;


(*
   InitMapDist - initializes the MapDist matrix.
*)

PROCEDURE InitMapDist (x1, y1, x2, y2: INTEGER) ;
VAR
   i, j: CARDINAL ;
BEGIN
   FOR i := 0 TO MaxX DO
      FOR j := 0 TO MaxY DO
      	 MapDist[i, j] := 0  (* Empty *)
      END
   END ;
   (*
      Work out the Xoffset and Yoffset. We require that
      our x1, y1 and x2, y2 are as central as possible.
   *)
   Xoffset := (MaxX DIV 2) - (Abs(x1, x2) DIV 2) ;
   Yoffset := (MaxY DIV 2) - (Abs(y1, y2) DIV 2) ;
   OverlayAdvMap
END InitMapDist ;


PROCEDURE OverlayAdvMap ;
VAR
   r, w, d: CARDINAL ;
BEGIN
END OverlayAdvMap ;


(*
   DisplayMap - display the contents of the DistMap.
*)

PROCEDURE DisplayMap ;
VAR
   x, y: CARDINAL ;
BEGIN
   WriteString('------------------------------------------------------') ;
   WriteLn ;
   FOR y := 0 TO MaxY DO
      FOR x := 0 TO MaxX DO
      	 IF MapDist[x, y] = Wall
      	 THEN
      	    WriteString('######')
      	 ELSIF MapDist[x, y] = Door
      	 THEN
      	    WriteString('      ')
      	 ELSE
      	    WriteCard(MapDist[x, y], 3) ; WriteString('   ')
      	 END
      END ;
      WriteLn
   END
END DisplayMap ;


(*
   TestDistance - tests to see whether the ScanBackwards procedure
      	       	  works.
*)

PROCEDURE TestDistance ;
VAR
   MyDist: CARDINAL ;
   x, y  : CARDINAL ;
BEGIN
   InitMapDist(0, 0, MaxX, MaxY) ;
   FOR y := 0 TO MaxY DO
      MapDist[4, y] := Wall
   END ;
   MapDist[4, 3] := Door ;

   FOR y := 0 TO MaxY DO
      MapDist[7, y] := Wall
   END ;
   MapDist[7, 7] := Door ;

   FOR x := 0 TO MaxX DO
      MapDist[x, 2] := Wall
   END ;
   MapDist[2, 2] := Door ;
   DisplayMap ;
   WriteString('Creating distance map between points 0,0 and MaxX, MaxY') ;
   WriteLn ;
   MyDist := ScanBackwards(0, 0, MaxX, MaxY, 1) ;

   WriteString('Distance is') ;

   WriteCard(MapDist[0,0], 6) ;
   WriteLn ;
   DisplayMap
END TestDistance ;


BEGIN
   TestDistance
END Find.
