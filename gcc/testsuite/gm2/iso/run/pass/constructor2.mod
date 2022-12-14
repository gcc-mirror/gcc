(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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

MODULE constructor2 ;

FROM libc IMPORT exit ;

TYPE
   Vector = ARRAY [0..2] OF REAL ;


PROCEDURE ScalarMultiplication (Vector: ARRAY OF REAL; Scalar: REAL;
                                VAR Result : ARRAY OF REAL) ;
VAR
   h: CARDINAL ;
BEGIN
   FOR h := 0 TO HIGH(Vector) DO
      Result[h] := Vector[h] * Scalar
   END
END ScalarMultiplication ;


VAR
   a, r, s: Vector ;
BEGIN
   a := Vector {1.0, 2.0, 3.0} ;
   ScalarMultiplication (a, 5.0, r) ;

   ScalarMultiplication (Vector {1.0, 2.0, 3.0}, 5.0, s) ;
   IF (r[0]=s[0]) AND (r[1]=s[1]) AND (r[2]=s[2])
   THEN
      exit(0)
   ELSE
      exit(1)
   END
END constructor2.