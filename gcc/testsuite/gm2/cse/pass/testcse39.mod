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
MODULE testcse39 ;


TYPE
   Ptr    = POINTER TO String ;
   String = ARRAY [0..10] OF CHAR ;


PROCEDURE Read (VAR a, b: Ptr) ;
BEGIN
   a^[0] := b^[0]
END Read ;


VAR
   p, q: POINTER TO CHAR ;
   c, d: CHAR ;
   a, b: Ptr ;
   i   : CARDINAL ;
BEGIN
   a := b ;
   b^[i] := b^[3] ;
   c := d ;
   Read(a, b) ;
END testcse39.
