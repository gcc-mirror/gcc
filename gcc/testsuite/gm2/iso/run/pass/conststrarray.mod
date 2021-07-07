(* conststrarray.mod provides a test to access a constant array.

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

MODULE conststrarray ;

FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT SIZE, ADR ;

TYPE
   array = ARRAY [0..26] OF CHAR ;

CONST
   str = array { "A", "B", "C", "D", "E", "F", "G", "H",
                 "I", "J", "K", "L", "M", "N", "O", "P",
                 "Q", "R", "S", "T", "U", "V", "W", "X",
                 "Y", "Z" } ;

PROCEDURE assert (b: BOOLEAN; file: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf ("%s:%d:assert failed\n", ADR (file), line) ;
      r := 1
   END
END assert ;


VAR
   s: array ;
   c: CARDINAL ;
   r: INTEGER ;
BEGIN
   r := 0 ;
   s := "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
   FOR c := 0 TO 25 DO
      printf ("c = %d, s[c] = %c, str[c] = %c, %c\n", c, s[c], str[c], CHR (ORD ('A')+c)) ;
      assert (s[c] = CHR (ORD ('A')+c), __FILE__, __LINE__) ;
      assert (s[c] = str[c], __FILE__, __LINE__)
   END ;
   exit (r)
END conststrarray.
