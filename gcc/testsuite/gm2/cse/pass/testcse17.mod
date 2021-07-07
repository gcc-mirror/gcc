(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
MODULE testcse17 ;


TYPE
   r = RECORD
          a, b, c: CARDINAL ;
          array  : ARRAY [0..10] OF CARDINAL ;
       END ;


PROCEDURE testcase (i: CARDINAL) : CARDINAL ;
BEGIN
   WITH t[i] DO
      CASE i OF

      1:  a := i ; b := i ; c := i |
      2:  a := i ; b := i ; c := i |
      3:  a := i ; b := i ; c := i |
      4:  a := i ; b := i ; c := i |
      5:  a := i ; b := i ; c := i

      END ;
      RETURN( t[i].a )
   END
END testcase ;



VAR
   t: ARRAY [1..10] OF r ;
   i: CARDINAL ;
BEGIN
   FOR i := 1 TO 5 DO
      IF testcase(i)#i
      THEN
         HALT
      END
   END
END testcse17.
