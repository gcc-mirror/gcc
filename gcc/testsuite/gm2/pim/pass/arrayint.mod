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

MODULE arrayint ;


(*
   sum - 
*)

PROCEDURE sum (x: ARRAY OF INTEGER) : INTEGER ;
VAR
   i: CARDINAL ;
   s: INTEGER ;
BEGIN
   s := 0 ;
   FOR i := 0 TO HIGH(x) DO
      INC(s, x[i])
   END ;
   RETURN s
END sum ;

VAR
   a: ARRAY [0..10] OF INTEGER ;
   c: INTEGER ;
BEGIN
   a[0] := 0 ;
   a[1] := 1 ;
   a[2] := 2 ;
   a[3] := 3 ;
   a[4] := 4 ;
   a[5] := 5 ;
   a[6] := 6 ;
   a[7] := 7 ;
   a[8] := 8 ;
   a[9] := 9 ;
   a[10] := 10 ;
   c := sum(a)
END arrayint.
