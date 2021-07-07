(* tinyconst2.mod minimal array access test.

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

MODULE tinyconst2 ;

FROM libc IMPORT printf, exit ;

TYPE
   array = ARRAY [0..25] OF CHAR ;

CONST
   str = array { "A", "B", "C", "D", "E", "F", "G", "H",
                 "I", "J", "K", "L", "M", "N", "O", "P",
                 "Q", "R", "S", "T", "U", "V", "W", "X",
                 "Y", "Z" } ;

VAR
   z: CHAR ;
   s: array ;
   r: INTEGER ;
BEGIN
   s := str ;
   z := s[25] ;
   printf ("z should be z = %c\n", z) ;
   IF z = 'Z'
   THEN
      r := 0
   ELSE
      r := 1
   END ;
   exit (r)
END tinyconst2.
