(* onebyte2.mod provides an include on a packed byte.

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

MODULE onebyte2 ;

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
   color = (red, blue, green, yellow, cyan, purple, black) ;
   smallInt = PACKEDSET OF [0..7] ;
   smallEnum = PACKEDSET OF color ;

VAR
   r: INTEGER ;
   si: smallInt ;
   se: smallEnum ;
BEGIN
   r := 0 ;
   assert (SIZE (smallInt) = SIZE (BYTE), __FILE__, __LINE__) ;
   assert (SIZE (smallEnum) = SIZE (BYTE), __FILE__, __LINE__) ;
   assert (SIZE (si) = SIZE (BYTE), __FILE__, __LINE__) ;
   assert (SIZE (se) = SIZE (BYTE), __FILE__, __LINE__) ;
   se := smallEnum {} ;
   si := smallInt {} ;
   INCL (si, 1) ;
   INCL (se, purple) ;
   assert (ORD (si) = 2, __FILE__, __LINE__) ;
   assert (ORD (se) = 32, __FILE__, __LINE__) ;
   exit (r)
END onebyte2.
