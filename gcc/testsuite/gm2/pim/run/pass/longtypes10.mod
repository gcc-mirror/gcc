(* Copyright (C) 2005 Free Software Foundation, Inc. *)
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

MODULE longtypes10 ;

FROM libc IMPORT exit ;

PROCEDURE x (r: LONGCARD) : LONGCARD ;
BEGIN
   RETURN r
END x ;

VAR
   y: LONGCARD ;
BEGIN
   y := 5 ;
   IF x(y+1+1)#7
   THEN
      exit(1)
   END ;
   IF x(1+1+y)#7
   THEN
      exit(1)
   END
END longtypes10.
