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

MODULE pimimp ;

   MODULE M1 ;
   IMPORT a ;
   EXPORT w, x ;
   VAR
      u, v, w: CARDINAL ;

      MODULE M2 ;
      IMPORT u ;
      EXPORT x, y;
      VAR
         x, y, z: CARDINAL ;
      BEGIN
         x := 11 ;
         y := 22 ;
         z := 33 ;
         u := 44
      END M2 ;

   BEGIN
      a := 55 ;
      u := 66 ;
      v := 77 ;
      w := 88 ;
      x := 99 ;
      y := 111
   END M1 ;

VAR
   a, b: CARDINAL ;
BEGIN
   a := 222 ;
   b := 333 ;
   w := 444 ;
   x := 555
END pimimp.
