(* Copyright (C) 2018 Free Software Foundation, Inc.  *)
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
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

MODULE adrunbounded3 ;

FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT ADDRESS, ADR ;


CONST
   MaxDim = 3 ;

TYPE
   Vector = ARRAY [1..MaxDim] OF REAL;
   Matrix = ARRAY [1..MaxDim] OF Vector;


(*
   assert -
*)

PROCEDURE assert (b: BOOLEAN; file: ARRAY OF CHAR; line: CARDINAL; message: ARRAY OF CHAR) ;
BEGIN
   IF NOT b
   THEN
      printf ("%s:%d: assert failed: %s\n", file, line, message) ;
      exit (1)
   END
END assert ;


(*
   inner -
*)

PROCEDURE inner (VAR m: ARRAY OF REAL) ;
VAR
   b: ADDRESS ;
BEGIN
   b := ADR (m) ;
   assert (a = b, __FILE__, __LINE__, "a = b")
END inner ;


(*
   test -
*)

PROCEDURE test (VAR m: ARRAY OF ARRAY OF REAL) ;
VAR
   u0, u1, u2: ADDRESS ;
   i         : CARDINAL ;
BEGIN
   u0 := ADR (m) ;
   assert (a0 = u0, __FILE__, __LINE__, "a0 = u0") ;
   u1 := ADR (m[0]) ;
   assert (a0 = u1, __FILE__, __LINE__, "a0 = u1") ;
   u2 := ADR (m[0][0]) ;
   assert (a0 = u2, __FILE__, __LINE__, "a0 = u2") ;
   FOR i := 0 TO MaxDim-1 DO
      a := ADR (m[i]) ;
      printf ("a = %p\n", a) ;
      inner (m[i])
   END
END test ;


VAR
   m            : Matrix ;
   a, a0, a1, a2: ADDRESS ;
BEGIN
   a0 := ADR (m) ;
   a1 := ADR (m[1]) ;
   a2 := ADR (m[1][1]) ;
   assert (a0 = a1, __FILE__, __LINE__, "a0 = a1") ;
   assert (a0 = a2, __FILE__, __LINE__, "a0 = a2") ;
   test (m)
END adrunbounded3.
