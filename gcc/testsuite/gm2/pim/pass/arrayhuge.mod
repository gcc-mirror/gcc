(* Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc. *)
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

MODULE arrayhuge ;

VAR
   a: ARRAY [MAX(CARDINAL)-4..MAX(CARDINAL)] OF CHAR ;
   i: CARDINAL ;
BEGIN
   a[MAX(CARDINAL)-1] := 'd' ;
(*
   a[MAX(CARDINAL)-4] := 'a' ;
   FOR i := MAX(CARDINAL)-4 TO MAX(CARDINAL) DO
      a[i] := 'z'
   END
*)
END arrayhuge.
