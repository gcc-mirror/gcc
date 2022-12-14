(* Copyright (C) 2016, 2017
   Free Software Foundation, Inc. *)
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

MODULE modulus2 ;

FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;

VAR
   res: INTEGER ;


PROCEDURE Assert (i, v: INTEGER; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF i=v
   THEN
      r := printf("successfully evaluated %s = %d\n", ADR(e), i)
   ELSE
      r := printf("%s:%d assertion failed when evaluating %s as %d whereas it should be %d\n", ADR(f), l, ADR(e), i, v) ;
      res := 1
   END
END Assert ;


(*
   Consistency - run the PIM4 P28 consistency test for DIV and MOD.
*)

PROCEDURE Consistency (t, q, r, left, right: INTEGER) ;
BEGIN
   printf ("Test %d\n", t) ;
   printf ("======\n") ;
   IF (left = q * right + r) AND (r >= 0)
   THEN
      printf (" satisfies PIM4 consistency test\n")
   ELSE
      printf (" fails PIM4 consistency test") ;
      IF r<0
      THEN
         printf ("  (the remainder must be >= 0)  (not %d)", r)
      END ;
      printf ("\n") ;

      printf ("   q = %d,   r = %d,  left = %d,  right = %d\n", q, r, left, right) ;
      res := 1
   END
END Consistency ;


VAR
   q, r: INTEGER ;
BEGIN
   res := 0 ;

   (* see P29 of PIM4 and using the examples in ISO M2 P201
      and the GM2 documentation.  *)

   (* test 1:   31 and 10 *)

   q := 31 DIV 10 ;
   Assert (q, 3, __FILE__, __LINE__, "31 DIV 10") ;
   r := 31 MOD 10 ;
   Assert (r, 1, __FILE__, __LINE__, "31 MOD 10") ;
   Consistency (1, q, r, 31, 10) ;

   (* test 2:  -31 and 10 *)

   q := (-31) DIV 10 ;
   Assert (q, -4, __FILE__, __LINE__, "(-31) DIV 10") ;
   r := (-31) MOD 10 ;
   Assert (r, 9, __FILE__, __LINE__, "(-31) MOD 10") ;
   Consistency (2, q, r, -31, 10) ;

   (* test 3:   31 and -10 *)

   q := 31 DIV (-10) ;
   Assert (q, -3, __FILE__, __LINE__, "31 DIV (-10)") ;
   r := 31 MOD (-10) ;
   Assert (r, 1, __FILE__, __LINE__, "31 MOD (-10)") ;
   Consistency (3, q, r, 31, -10) ;

   (* test 4:  -31 and -10 *)

   q := (-31) DIV (-10) ;
   Assert (q, 4, __FILE__, __LINE__, "(-31) DIV (-10)") ;
   r := (-31) MOD (-10) ;
   Assert (r, 9, __FILE__, __LINE__, "(-31) MOD (-10)") ;
   Consistency (4, q, r, -31, -10) ;

   exit(res)
END modulus2.
