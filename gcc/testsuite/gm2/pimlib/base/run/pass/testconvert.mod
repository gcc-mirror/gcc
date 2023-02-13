(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

MODULE testconvert ;

FROM SYSTEM IMPORT ADR ;
FROM StringConvert IMPORT LongrealToString ;
FROM DynamicStrings IMPORT String, EqualArray, string ;
FROM libc IMPORT exit, printf ;

PROCEDURE Assert (b: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL;
                  message: ARRAY OF CHAR) ;
BEGIN
   IF NOT b
   THEN
      printf("%s:%d:  assert failed:  %s\n", ADR(f), l, ADR(message)) ;
      exit(1)
   END
END Assert ;


VAR
   s: String ;
BEGIN
   s := LongrealToString(-123.0, 8, 3) ;
   printf("returned value '%s'\n", string(s)) ;
   Assert(EqualArray(LongrealToString(-123.0, 8, 3), '-123.000'), __FILE__, __LINE__, '-123.000') ;
   s := LongrealToString(1.0, 4, 2) ;
   printf("returned value '%s'\n", string(s)) ;
   Assert(EqualArray(LongrealToString(1.0, 4, 2), '1.00'), __FILE__, __LINE__, '1.00') ;
   s := LongrealToString(1.0, 4, 3) ;
   printf("returned value '%s'\n", string(s)) ;
   Assert(EqualArray(LongrealToString(1.0, 4, 3), '1.00'), __FILE__, __LINE__, '1.00') ;
   s := LongrealToString(1.0, 6, 3) ;
   printf("returned value '%s'\n", string(s)) ;
   Assert(EqualArray(LongrealToString(1.0, 6, 3), ' 1.000'), __FILE__, __LINE__, ' 1.000') ;
   s := LongrealToString(123.0, 8, 3) ;
   printf("returned value '%s'\n", string(s)) ;
   Assert(EqualArray(LongrealToString(123.0, 8, 3), ' 123.000'), __FILE__, __LINE__, ' 123.000') ;
   s := LongrealToString(-123.0, 6, 3) ;
   printf("returned value '%s'\n", string(s)) ;
   Assert(EqualArray(LongrealToString(-123.0, 6, 3), '-123.0'), __FILE__, __LINE__, '-123.0') ;
END testconvert.
