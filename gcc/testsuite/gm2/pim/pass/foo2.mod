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
MODULE foo2 ;


PROCEDURE foobar (param, more: CHAR) : CARDINAL ;
VAR
(*
   seven, eight,
   five, six,
   isit, now,
*)
   working: CARDINAL;
   then,
   now : CHAR ;
BEGIN
   working := 1 ;
   IF param='a'
   THEN
      RETURN( 0 )
   ELSE
      RETURN( 1 )
   END
END foobar ;

VAR
   i1    : CARDINAL ;
   global: CHAR ;
   c1    : CHAR ;
   a     : ARRAY [1..200] OF CHAR ;
BEGIN
   i1 := foobar(global, c1)
END foo2.
