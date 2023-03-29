(* Copyright (C) 2015 Free Software Foundation, Inc.  *)
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

MODULE tinyvarient4 ;   (*!m2pim*)

FROM Storage IMPORT ALLOCATE ;

TYPE
   decl = POINTER TO RECORD
                        a: CHAR ;
                        CASE b: BOOLEAN OF

                        TRUE:  c: CARDINAL |
			FALSE: d: decl

                        END ;
			e: INTEGER ;
                     END ;

VAR
   d: decl ;
BEGIN
   NEW (d) ;
   d^.a := 'a' ;
   d^.b := TRUE ;
   d^.c := 1 ;
   d^.d := d ;
   d^.e := 2
END tinyvarient4.
