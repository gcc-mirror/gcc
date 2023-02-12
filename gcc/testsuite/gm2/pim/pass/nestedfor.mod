(* Copyright (C) 2010 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

MODULE nestedfor ;

FROM SYSTEM IMPORT INTEGER32 ;

VAR
   a: ARRAY [1..9] OF INTEGER32 ;


PROCEDURE one (q: INTEGER32) ;
   PROCEDURE two (p: INTEGER32) ;
   VAR
      i: INTEGER32 ;
   BEGIN
      FOR i := 1 TO p-1 DO
         INC(s, a[i])
      END
   END two ;
VAR
   j: INTEGER32 ;
BEGIN
   RETURN ;
   FOR j := 1 TO q-1 DO
      INC(s, a[j])
   END ;
   two(q)
END one ;

PROCEDURE three (q: INTEGER32) ;
   PROCEDURE four (p: INTEGER32) ;
   VAR
      i: INTEGER32 ;
   BEGIN
      FOR i := 1 TO p-1 DO
         INC(s, a[i])
      END
   END four ;
VAR
   j: INTEGER32 ;
BEGIN
   RETURN ;
   FOR j := 1 TO q-1 DO
      INC(s, a[j])
   END ;
   four(q)
END three ;

VAR
   s: INTEGER32 ;
BEGIN
   s := 0 ;
   one(1) ;
   three(1)
END nestedfor.
