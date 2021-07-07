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
MODULE procindirect ;


FROM libc IMPORT exit ;
TYPE
   sec = PROCEDURE (CARDINAL) : CHAR ;

PROCEDURE first ;
BEGIN
END first ;

PROCEDURE second (c: CARDINAL) : CHAR ;
BEGIN
   RETURN( 'a' )
END second ;

PROCEDURE fetch () : CARDINAL ;
BEGIN
   RETURN( 2 )
END fetch ;

VAR
   f: PROC ;
   s: sec ;
   b: CARDINAL ;
   array: ARRAY [1..10] OF sec ;
   i: CARDINAL ;
BEGIN
   f := first ;
   f ;
   s := second ;
   IF s(b)#'a'
   THEN
      exit(1)
   END ;
   i := fetch() ;
   array[i] := second ;
   i := fetch() ;
   IF array[i](b)#'a'
   THEN
      exit(1)
   END
END procindirect.
