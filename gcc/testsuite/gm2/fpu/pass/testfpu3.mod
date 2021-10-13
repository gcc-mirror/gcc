(* testfpu3.mod basic floating point test.

Copyright (C) 2001-2019 Free Software Foundation, Inc.
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

MODULE testfpu3 ;

TYPE
   Stack = RECORD
              Left, Right: REAL ;
           END ;

VAR
   Stacked: ARRAY [0..10] OF Stack ;
   i      : CARDINAL ;
BEGIN
   FOR i := 0 TO 10 DO
      WITH Stacked[i] DO
         Left  := FLOAT(i) ;
         Right := 10.0
      END
   END ;
   FOR i := 0 TO 10 DO
      WITH Stacked[i] DO
         Left := Right * Left
      END
   END ;
   LOOP
   END
END testfpu3.
