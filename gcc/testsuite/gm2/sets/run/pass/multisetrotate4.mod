(* Copyright (C) 2019 Free Software Foundation, Inc. *)
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

MODULE multisetrotate4 ;

FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT ROTATE, WORD, BITSPERLOC ;

TYPE
   multi = SET OF [0..SIZE (WORD) * 2 * BITSPERLOC-1] ;

VAR
   set : multi ;
   bits: INTEGER ;
BEGIN
   set := multi {1} ;
   bits := SIZE (multi) * BITSPERLOC ;
   IF ROTATE (set, bits-1) # multi {0}
   THEN
      exit (1)
   END ;
   IF ROTATE (set, -(bits - 1)) # multi {2}
   THEN
      exit (2)
   END ;
   exit (0)
END multisetrotate4.
