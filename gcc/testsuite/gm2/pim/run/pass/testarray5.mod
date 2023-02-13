(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

MODULE testarray5 ;

FROM libc IMPORT exit ;

TYPE
   array = ARRAY [1..100] OF RECORD
                                x, y: CARDINAL ;
                             END ;


PROCEDURE foo (VAR a: array; VAR i: CARDINAL) ;
BEGIN
   WITH a[i] DO
      x := 98 ;
      y := 99 ;
   END ;
   IF (a[i].x#98) OR (a[i].y#99)
   THEN
      exit(2)
   END
END foo ;


VAR
   a: array ;
   i: CARDINAL ;
BEGIN
   i := 50 ;
   foo(a, i) ;
   IF (a[i].x#98) OR (a[i].y#99)
   THEN
      exit(1)
   END
END testarray5.
