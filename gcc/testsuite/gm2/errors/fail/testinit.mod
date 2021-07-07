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

MODULE testinit ;


PROCEDURE ToThePower (i, n: CARDINAL) : CARDINAL ;
BEGIN
   IF n=0
   THEN
      RETURN( 1 )
   ELSE
      WHILE n>1 DO
         i := i * i ;
         DEC(n)
      END ;
      RETURN( i )
   END
END ToThePower ;


PROCEDURE foo ;
VAR
   n: CARDINAL ;
BEGIN
   n := ToThePower(n, 2)
END foo ;

BEGIN
   foo
END testinit.
