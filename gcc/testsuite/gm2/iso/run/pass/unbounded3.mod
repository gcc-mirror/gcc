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

MODULE unbounded3 ;

FROM libc IMPORT exit, printf ;

VAR
   z: CARDINAL ;

PROCEDURE testSize (a: ARRAY OF ARRAY OF ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
BEGIN
   IF s1#SIZE(a)
   THEN
      printf("SIZE(a) = %d\n", SIZE(a)) ;
      exit(1)
   ELSIF s2#SIZE(a[0])
   THEN
      z := SIZE(a[0]) ;
      printf("SIZE(a[0]) = %d\n", SIZE(a[0])) ;
      exit(2)
   ELSIF s3#SIZE(a[0][0])
   THEN
      printf("SIZE(a[0][0]) = %d\n", SIZE(a[0][0])) ;
      exit(3)
   ELSIF s3#SIZE(a[0,0])
   THEN
      printf("SIZE(a[0,0]) = %d\n", SIZE(a[0,0])) ;
      exit(4)
   END
END testSize ;

VAR
   b: ARRAY [0..4] OF ARRAY [0..5] OF ARRAY [0..1] OF CHAR ;
BEGIN
   testSize(b, 60, 12, 2)
END unbounded3.
