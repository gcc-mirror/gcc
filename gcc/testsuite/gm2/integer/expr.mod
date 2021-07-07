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
MODULE expr ;

FROM libc IMPORT exit ;

VAR
   i, j: INTEGER ;
BEGIN
   IF 6 DIV 2#3
   THEN
      exit(1)
   END ;
   IF 6 MOD 2#0
   THEN
      exit(2)
   END ;
   IF 1 DIV 3#0
   THEN
      exit(3)
   END ;
   IF 1 MOD 3#1
   THEN
      exit(4)
   END ;
   IF -3 DIV 2#-1
   THEN
      exit(5)
   END ;
   IF 3 DIV (-2)#-1    (* we must surround the -2 in parenthenesis as `DIV' `-' is illegal *)
   THEN
      exit(6)
   END ;

   (* and the same for variables *)

   i := 6 ;
   j := 2 ;
   IF i DIV j#3
   THEN
      exit(7)
   END ;
   i := 6 ;
   j := 2 ;
   IF i MOD j#0
   THEN
      exit(8)
   END ;
   i := 1 ;
   j := 3 ;
   IF i DIV j#0
   THEN
      exit(9)
   END ;
   i := 1 ;
   j := 3 ;
   IF i MOD j#1
   THEN
      exit(10)
   END ;
   i := -3 ;
   j := 2 ;
   IF i DIV j#-1
   THEN
      exit(11)
   END ;
 
   i := 3 ;
   j := -2 ;
   IF i DIV j#-1
   THEN
      exit(12)
   END ;
END expr. 
