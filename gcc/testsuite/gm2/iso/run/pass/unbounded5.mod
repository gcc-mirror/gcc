(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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

MODULE unbounded5 ;

FROM libc IMPORT exit, printf ;

PROCEDURE assign (VAR a: arrayType) ;
BEGIN
   a := arrayType{1, 2, 3, 4, 5, 6, 7, 8, 9}
END assign ;

TYPE
   arrayType = ARRAY [1..9] OF CARDINAL ;

VAR
   a: arrayType ;
BEGIN
   assign(a) ;
   IF (a[1]#1) AND (a[2]#2) AND (a[3]#3)
   THEN
      printf("assignment failed\n") ;
      exit(1)
   END ;
   IF (a[4]#4) AND (a[5]#5) AND (a[6]#6)
   THEN
      printf("assignment failed\n") ;
      exit(2)
   END ;
   IF (a[7]#7) AND (a[8]#8) AND (a[9]#9)
   THEN
      printf("assignment failed\n") ;
      exit(3)
   END
END unbounded5.
