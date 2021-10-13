(* Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE nestedwith2 ;

FROM libc IMPORT exit ;

TYPE
   record = RECORD
               x, y, z: CARDINAL ;
            END ;

VAR
   a: ARRAY [0..1] OF record ;
BEGIN
   a[0].x := 1 ;
   a[0].y := 2 ;
   a[0].z := 3 ;
   a[1].x := 20 ;
   a[1].y := 40 ;
   a[1].z := 60 ;
   WITH a[0] DO
      WITH a[1] DO
         IF x#20
         THEN
            exit(1)
         END
      END
   END ;
   exit(0)
END nestedwith2.
