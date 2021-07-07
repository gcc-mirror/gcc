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

MODULE For3 ;

FROM libc IMPORT exit ;

PROCEDURE arithp (a, n, d: CARDINAL) : CARDINAL ;
BEGIN
   RETURN( a+(n-1)*d )
END arithp ;

PROCEDURE sumarithp (a, n, d: CARDINAL) : CARDINAL ;
BEGIN
   RETURN( (n DIV 2) * (2*a+(n-1)*d) )
END sumarithp ;


VAR
   c, i: CARDINAL ;
BEGIN
   c := 0 ;
   FOR i := 3 TO 96 BY 4 DO
      INC(c, i)
   END ;
   IF i#arithp(3, 1+(96-3) DIV 4, 4)
   THEN
      exit(1)
   END ;
   IF c#sumarithp(3, 1+(96-3) DIV 4, 4)
   THEN
      exit(2)
   END
END For3.
