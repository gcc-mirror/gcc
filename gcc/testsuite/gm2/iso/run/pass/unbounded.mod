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

MODULE unbounded ;

FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;


PROCEDURE Assert (b: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf ("%s:%d:failure\n", ADR (f), l) ;
      exit (1)
   END
END Assert ;


PROCEDURE test (VAR a: ARRAY OF ARRAY OF CHAR) ;
VAR
   m, n: CARDINAL ;
BEGIN
   m := HIGH (a) ;
   n := HIGH (a[0]) ;
   printf ("m = %d, n = %d\n", m, n);
   a[1, 2] := 'a' ;
   a[2, 1] := 'c'
END test ;


VAR
   b   : ARRAY [0..4], [0..5] OF CHAR ;
   i, j: CARDINAL ;
BEGIN
   FOR i := 0 TO 4 DO
      FOR j := 0 TO 5 DO
         b[i, j] := 'z'
      END
   END ;
   test (b) ;
   FOR i := 0 TO 4 DO
      FOR j := 0 TO 5 DO
         IF (i = 1) AND (j = 2)
         THEN
            Assert (b[1, 2] = 'a', __FILE__, __LINE__)
         ELSIF (i = 2) AND (j = 1)
         THEN
            Assert (b[2, 1] = 'c', __FILE__, __LINE__)
         ELSE
            Assert (b[i, j] = 'z', __FILE__, __LINE__)
         END
      END
   END
END unbounded.
