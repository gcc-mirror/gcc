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

MODULE unbounded ;

FROM SYSTEM IMPORT ADR, ADDRESS ;
FROM libc IMPORT exit ;

PROCEDURE foo (f: ARRAY OF CHAR) ;
BEGIN
   f[2] := 'a'
END foo ;

PROCEDURE bar (b: ARRAY OF CHAR) ;
TYPE
   PtrToChar = POINTER TO CHAR ;
VAR
   a: PtrToChar ;
BEGIN
   a := ADR(b) ;
   INC(a, VAL(PtrToChar, 2)) ;
   a^ := 'a'
END bar ;

VAR
   a: ARRAY [0..10] OF CHAR ;
BEGIN
   a := '01234567890' ;
   foo(a) ;
   IF a[2]='a'
   THEN
      exit(1)
   END ;
   bar(a) ;
   IF a[2]='a'
   THEN
      exit(2)
   END
END unbounded.
