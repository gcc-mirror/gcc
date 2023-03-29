(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE EnumTest ;

FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;

VAR
   res: INTEGER ;

PROCEDURE Assert (v: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF v
   THEN
      r := printf("successfully evaluated assertion (%s)\n", ADR(e))
   ELSE
      r := printf("%s:%d assertion failed when evaluating %s\n", ADR(f), l, ADR(e)) ;
      res := 1
   END
END Assert ;

TYPE
   enumType = (zero, one, two, three, four) ;
   arrayType = ARRAY enumType OF BOOLEAN ;

CONST
   lastEnum = four ;

VAR
   e: enumType ;
   i: CARDINAL ;
   a: arrayType ;
BEGIN
   res := 0 ;
   FOR e := zero TO lastEnum DO
      a[e] := FALSE ;
   END ;
   FOR e := zero TO lastEnum DO
      Assert(NOT a[e], __FILE__, __LINE__, 'testing array against FALSE')
   END ;
   FOR e := zero TO lastEnum DO
      a[e] := TRUE ;
   END ;
   FOR e := zero TO lastEnum DO
      Assert(a[e], __FILE__, __LINE__, 'testing array against TRUE')
   END ;
   i := 1 ;
   FOR e := one TO lastEnum DO
      Assert (ORD (e) = i, __FILE__, __LINE__, 'enum against a value') ;
      INC (i)
   END ;
   exit(res)
END EnumTest.
