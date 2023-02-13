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

MODULE shift ;

FROM libc IMPORT exit ;
FROM SYSTEM IMPORT SHIFT ;

VAR
   b: BITSET ;
   i: INTEGER ;
BEGIN
   b := BITSET{1, 2, 3} ;
   b := SHIFT(b, 1) ;
   IF b#BITSET{2, 3, 4}
   THEN
      exit(1)
   END ;
   b := BITSET{1, 2, 3} ;
   b := SHIFT(b, -1) ;
   IF b#BITSET{0, 1, 2}
   THEN
      exit(2)
   END ;
   i := -1 ;
   b := BITSET{1, 2, 3} ;
   b := SHIFT(b, i) ;
   IF b#BITSET{0, 1, 2}
   THEN
      exit(3)
   END
END shift.
