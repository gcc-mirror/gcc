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

MODULE testreal ;

FROM FpuIO IMPORT WriteLongReal, LongRealToStr ;
FROM StrLib IMPORT StrEqual ;
FROM libc IMPORT exit, printf ;
FROM StrIO IMPORT WriteLn ;
FROM FIO IMPORT FlushBuffer, StdOut ;

PROCEDURE Assert (b: BOOLEAN; l: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      FlushBuffer(StdOut) ;
      printf("%s:%d:regression test failed during execution\n",
              __FILE__, l) ;
      exit(1)
   END
END Assert ;

VAR
   a: LONGREAL;
   s: ARRAY [0..20] OF CHAR ;
BEGIN
   a := 0.1 ;
   WriteLongReal(a,15,11) ; WriteLn ;
   LongRealToStr(a, 15, 11, s) ;
   Assert(StrEqual(s, '  0.10000000000'), __LINE__) ;

   a := 0.25 ;
   WriteLongReal(a,15,11) ; WriteLn ;
   LongRealToStr(a, 15, 11, s) ;
   Assert(StrEqual(s, '  0.25000000000'), __LINE__)
END testreal.
