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

MODULE testaddr ;

FROM SYSTEM IMPORT ADDRESS, ADR ;

PROCEDURE foo (a: ADDRESS) ;
VAR
   i: CARDINAL ;
   p: POINTER TO CHAR ;
BEGIN
   i := 0 ;
   p := a ;
   WHILE p^#0C DO
      INC(i) ;
      INC(p)
   END ;
   IF i#11
   THEN
      HALT
   END
END foo ;


BEGIN
   foo(ADR("hello world"))
END testaddr.
