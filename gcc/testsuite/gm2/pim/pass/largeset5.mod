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

MODULE largeset5 ;

FROM libc IMPORT exit ;

TYPE
   LargeBitset = SET OF [0..127] ;

VAR
   a, b, c, d: LargeBitset ;
BEGIN
   a := LargeBitset{1, 2, 3, 4, 5, 100, 101, 102, 103, 104} ;
   b := LargeBitset{1, 2, 3, 4, 5,      101, 102, 103, 104} ;
   c := LargeBitset{1, 2, 3, 4, 5, 100, 101, 102, 103, 104, 105} ;
   d := LargeBitset{1, 2, 3, 4, 5, 100, 101, 102, 103, 104, 105} ;
   IF a<b
   THEN
      exit(1)
   END ;
   IF b>c
   THEN
      exit(1)
   END ;
   IF d>=c
   THEN
      exit(0)
   END ;
   exit(1)
END largeset5.
