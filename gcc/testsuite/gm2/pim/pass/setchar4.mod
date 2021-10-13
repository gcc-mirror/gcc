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

MODULE setchar4 ;

FROM SYSTEM IMPORT ADR ;

TYPE
   charset = SET OF CHAR ;

VAR
   s: charset ;
   p: POINTER TO ARRAY [0..7] OF INTEGER ;
BEGIN
   p := ADR(s) ;
   p^[0] := -1 ;
   p^[1] := -1 ;
   p^[2] := -1 ;
   p^[3] := -1 ;
   p^[4] := -1 ;
   p^[5] := -1 ;
   p^[6] := 255 ;
   p^[7] := -1 ;
   p^[7] := -1 ;
END setchar4.
