(* Copyright (C) 2005, 2006, 2007, 2008  Free Software Foundation, Inc. *)
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

MODULE realconv ;

FROM RealConversions IMPORT RealToString, StringToReal ;
FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;
FROM StrLib IMPORT StrEqual ;


PROCEDURE Assert (v: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
BEGIN
   IF v
   THEN
      printf("successfully evaluated assertion (%s)\n", ADR(e))
   ELSE
      printf("%s:%d assertion failed when evaluating %s\n", ADR(f), l, ADR(e)) ;
      res := 1 ;
      exit(res)
   END
END Assert ;


VAR
   d  : REAL ;
   l  : LONGREAL ;
   res: INTEGER ;
   a  : ARRAY [0..100] OF CHAR ;
   ok : BOOLEAN ;
BEGIN
   res := 0 ;
   RealToString(100.0, 10, 10, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   printf("value returned is '%s'\n", ADR(a)) ;
   Assert(StrEqual('100.000000', a), __FILE__, __LINE__, 'testing return value of "100.000000"') ;
   RealToString(100.0, -5, 12, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   printf("value returned is '%s'\n", ADR(a)) ;
   Assert(StrEqual('  1.00000E+2', a), __FILE__, __LINE__, 'testing return value of "  1.00000E+2"') ;

   RealToString(123.456789, 10, 10, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   printf("value returned is '%s'\n", ADR(a)) ;
   Assert(StrEqual('123.456789', a), __FILE__, __LINE__, 'testing return value of "123.456789"') ;
   RealToString(123.456789, -5, 13, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   printf("value returned is '%s'\n", ADR(a)) ;
   Assert(StrEqual('   1.23456E+2', a), __FILE__, __LINE__, 'testing return value of "   1.23456E+2"') ;

   RealToString(123.456789, -2, 15, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   printf("value returned is '%s'\n", ADR(a)) ;
   Assert(StrEqual('        1.23E+2', a), __FILE__, __LINE__, 'testing return value of "        1.23E+2"') ;

   StringToReal('  1234567.89E-4', d, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   printf('value returned is %f\n', d) ;

   RealToString(3.14159268, -6, 13, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   printf("value returned is '%s'\n", ADR(a)) ;
   Assert(StrEqual('  3.141592E+0', a), __FILE__, __LINE__, 'testing return value of "  3.141592E+0"') ;

   RealToString(12345.6789, 5, 20, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   printf("value returned is '%s'\n", ADR(a)) ;
   Assert(StrEqual('         12345.67890', a), __FILE__, __LINE__, 'testing return value of "         12345.67890"') ;

   exit(res)
END realconv.
