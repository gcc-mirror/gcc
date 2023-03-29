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

MODULE unbounded8 ;

FROM SYSTEM IMPORT BYTE, WORD ;
FROM libc IMPORT exit, printf ;

PROCEDURE lowlevel (VAR b: ARRAY OF BYTE) ;
BEGIN
   c := SIZE(b)
END lowlevel ;


PROCEDURE assign (a: ARRAY OF ARRAY OF CARDINAL) ;
BEGIN
   d := SIZE(a[0]) ;
   lowlevel(a[0])
END assign ;

VAR
   e   : ARRAY [1..5] OF ARRAY [0..29] OF CARDINAL ;
   c, d: CARDINAL ;
BEGIN
   assign(e) ;
   printf("c = %d, d = %d\n", c, d) ;
   IF c#d
   THEN
      exit(1)
   END
END unbounded8.
