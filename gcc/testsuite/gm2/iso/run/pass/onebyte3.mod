(* onebyte3.mod stress bit changes on arrays of bytes.

Copyright (C) 2020 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

MODULE onebyte3 ;

FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT SIZE, ADR, BYTE ;


PROCEDURE assert (b: BOOLEAN; file: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf("%s:%d:assert failed\n", ADR (file), line) ;
      r := 1
   END
END assert ;


TYPE
   byte = PACKEDSET OF [0..7] ;
   array = ARRAY [0..10] OF byte ;
VAR
   a      : array ;
   r      : INTEGER ;
   i, j, k: CARDINAL ;
BEGIN
   r := 0 ;
   FOR i := 0 TO 10 DO
      (* zap all bytes to zero.  *)
      FOR k := 0 TO 10 DO
         a[k] := byte {}
      END ;
      (* assign all bits to 1 in byte i.  *)
      FOR j := 0 TO 7 DO
         INCL (a[i], j)
      END ;
      assert (ORD (a[i]) = 255, __FILE__, __LINE__) ;
      (* check all other bytes unaffected.  *)
      FOR k := 0 TO 10 DO
         IF k # i
         THEN
            assert (ORD (a[k]) = 0, __FILE__, __LINE__);
         END
      END
   END ;
   exit (r)
END onebyte3.
