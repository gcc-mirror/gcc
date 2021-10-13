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

MODULE localproc ;

FROM M2RTS IMPORT HALT ;

(* (outer (a, (b (c)))) *)
PROCEDURE outer ;
VAR
   k, l: INTEGER ;
   
   PROCEDURE a (VAR x: INTEGER) ;
   VAR
      j: CARDINAL ;
   BEGIN
      j := 1 ;
      k := x ;
      x := j
   END a ;

   PROCEDURE b (VAR x: INTEGER) ;
   VAR
      j: CARDINAL ;

      PROCEDURE c (x: INTEGER) ;
      BEGIN
         k := x
      END c ;

   BEGIN
      j := 1 ;
      k := x ;
      x := j
   END b ;

BEGIN
   k := 0 ;
   l := 2 ;
   a(l) ;
   IF k#2
   THEN
      HALT
   END ;
   IF l#1
   THEN
      HALT
   END
END outer ;

BEGIN
   outer
END localproc.
