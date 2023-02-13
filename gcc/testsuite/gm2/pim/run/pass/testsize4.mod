(* Copyright (C) 2022 Free Software Foundation, Inc. *)
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

MODULE testsize4 ;

FROM SYSTEM IMPORT TSIZE ;
FROM libc IMPORT printf ;

TYPE
   ptr = POINTER TO CARDINAL ;

VAR
   foo: ptr ;
   bar: CARDINAL ;
BEGIN
   foo := NIL ;
   (* should be able to dereference an initialized pointer using TSIZE.  *)
   bar := TSIZE (foo^) ;
   printf ("result of TSIZE (foo^) is %d\n", bar) ;
   (* and SIZE.  *)
   bar := SIZE (foo^) ;
   printf ("result of SIZE (foo^) is %d\n", bar)
END testsize4.
