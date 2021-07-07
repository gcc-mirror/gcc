(* Copyright (C) 2018 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

MODULE callassignment4 ;  (*!m2iso+gm2*)

FROM libc IMPORT printf ;


PROCEDURE a (x: INTEGER) : INTEGER ;
BEGIN
   DEC (x) ;
   RETURN x
END a ;


PROCEDURE b (x: INTEGER) : INTEGER ;
BEGIN
   DEC (x) ;
   RETURN x
END b ;


PROCEDURE c (x: INTEGER) : INTEGER ;
BEGIN
   DEC (x) ;
   RETURN x
END c ;


VAR
   y: CARDINAL ;
BEGIN
   printf ("can the plugin detect the range error after these calls?\n");
   printf ("call 1\n");
   printf ("call 2\n");
   printf ("call 3\n");
   printf ("call 4\n");
   y := 1 ;
   IF y = 1
   THEN

   ELSE

   END ;
   y := c(b(a(2)))
END callassignment4.
