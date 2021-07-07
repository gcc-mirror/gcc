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
IMPLEMENTATION MODULE Geometry ;


FROM Assertion IMPORT Assert ;


(*
   IsSubLine - returns true if the range i1..i2 or j1..j2 are ranges
               of each other.
*)

PROCEDURE IsSubLine (i1, i2, j1, j2: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( ((i1<=j1) AND (i2>=j2)) OR ((j1<=i1) AND (j2>=i2)) )
END IsSubLine ;


(*
   IsIntersectingRange - returns true if the ranges i1..i2  j1..j2
                         overlap.
*)

PROCEDURE IsIntersectingRange (i1, i2, j1, j2: CARDINAL) : BOOLEAN ;
BEGIN
   (* Easier to prove NOT outside limits!! *)
   RETURN( NOT ((i1>j2) OR (i2<j1)) )
END IsIntersectingRange ;


(*
   IntersectionLength - returns the intersection length
                        of the overlapping ranges i1..i2  j1..j2.
*)

PROCEDURE IntersectionLength (i1, i2, j1, j2: CARDINAL) : CARDINAL ;
BEGIN
   IF IsSubRange(i1, i2, j1)
   THEN
      RETURN( Abs(j1, Min(i2, j2)) )
   ELSIF IsSubRange(i1, i2, j2)
   THEN
      RETURN( Abs(Max(i1, j1), j2) )
   ELSE
      RETURN( 0 )
   END
END IntersectionLength ;


(*
   IsPointOnLine - returns true if point x, y is on line (x1, y1) , (x2, y2)
*)

PROCEDURE IsPointOnLine (x, y: CARDINAL; x1, y1, x2, y2: CARDINAL) : BOOLEAN ;
BEGIN
   IF (x1=x2) AND (x=x1)
   THEN
      RETURN( IsSubRange(y1, y2, y) )
   ELSIF (y1=y2) AND (y=y1)
   THEN
      RETURN( IsSubRange(x1, x2, x) )
   ELSE
      RETURN( FALSE )
   END
END IsPointOnLine ;


(*
   IsSubRange - returns true if i lies inbetween High and Low.
*)

PROCEDURE IsSubRange (Low, High, i: CARDINAL) : BOOLEAN ;
BEGIN
   Assert(High>=Low) ;
   RETURN( (i>=Low) AND (i<=High) )
END IsSubRange ;


(*
   Max - returns the largest cardinal number from i and j.
*)

PROCEDURE Max (i, j: CARDINAL) : CARDINAL ;
BEGIN
   IF i>j
   THEN
      RETURN( i )
   ELSE
      RETURN( j )
   END
END Max ;


(*
   Min - returns the smallest cardinal number from i and j.
*)

PROCEDURE Min (i, j: CARDINAL) : CARDINAL ;
BEGIN
   IF i<j
   THEN
      RETURN( i )
   ELSE
      RETURN( j )
   END
END Min ;


(*
   Abs - returns the difference between i and j.
*)

PROCEDURE Abs (i, j: CARDINAL) : CARDINAL ;
BEGIN
   IF i>j
   THEN
      RETURN( i-j )
   ELSE
      RETURN( j-i )
   END
END Abs ;


(*
   Swap - swaps two cardinal numbers i and j.
*)

PROCEDURE Swap (VAR i, j: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   t := i ;
   i := j ;
   j := t
END Swap ;


END Geometry.
