(* Copyright (C) 2005 Free Software Foundation, Inc. *)
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

MODULE concat ;

FROM StrLib IMPORT StrConCat, StrLen, StrEqual ;
FROM NumberIO IMPORT CardToStr ;
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


PROCEDURE foo ;
VAR
   line: ARRAY [0..79] OF CHAR ;
BEGIN
   CardToStr(10000, 6, line) ;
   Assert(StrEqual(line, ' 10000'), __FILE__, __LINE__, 'StrEqual of CardToStr') ;
   StrConCat(' ', line, line) ;
   Assert(StrLen(line)=7, __FILE__, __LINE__, 'StrLen of StrConCat') ;
   Assert(StrEqual(line, '  10000'), __FILE__, __LINE__, 'StrEqual')
END foo ;


BEGIN
   foo
END concat.
