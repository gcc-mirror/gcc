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

MODULE NumberIOBug ;

FROM NumberIO IMPORT IntToStr ;
FROM StrLib IMPORT StrEqual ;
FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;


PROCEDURE Assert (v: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF v
   THEN
      r := printf("successfully evaluated %s\n", ADR(e))
   ELSE
      r := printf("%s:%d assertion failed when evaluating %s\n", ADR(f), l, ADR(e)) ;
      res := 1
   END
END Assert ;


VAR
   a     : ARRAY [0..100] OF CHAR ;
   r, res: INTEGER ;
BEGIN
   res := 0 ;

   IntToStr(MAX(INTEGER), 0, a) ;
   r := printf('result of MAX(INTEGER) = %s\n', ADR(a)) ;
   Assert(StrEqual(a, '2147483647'), __FILE__, __LINE__, 'MAX(INTEGER) in IntToStr') ;

   IntToStr(MIN(INTEGER), 0, a) ;
   r := printf('result of MIN(INTEGER) = %s\n', ADR(a)) ;
   Assert(StrEqual(a, '-2147483648'), __FILE__, __LINE__, 'MIN(INTEGER) in IntToStr') ;

   IntToStr(MIN(INTEGER)+1, 0, a) ;
   r := printf('result of MIN(INTEGER)+1 = %s\n', ADR(a)) ;
   Assert(StrEqual(a, '-2147483647'), __FILE__, __LINE__, 'MIN(INTEGER)+1 in IntToStr') ;
   exit(res)
END NumberIOBug.
