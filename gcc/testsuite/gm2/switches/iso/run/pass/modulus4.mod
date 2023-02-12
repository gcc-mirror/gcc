(* Copyright (C) 2017 Free Software Foundation, Inc.  *)
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

MODULE modulus4 ;

FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;

VAR
   res: INTEGER ;


PROCEDURE Assert (r: INTEGER; v: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
VAR
   c: INTEGER ;
BEGIN
   IF v
   THEN
      c := printf("successfully evaluated %s as %d\n", ADR(e), r)
   ELSE
      c := printf("%s:%d assertion failed when evaluating %s as %d\n", ADR(f), l, ADR(e), r) ;
      res := 1
   END
END Assert ;


VAR
   i: INTEGER ;
BEGIN
   res := 0 ;

   (* example from ISO Standard 6-7 *)
   i := 31 MOD 10 ;
   Assert(i, i=1, __FILE__, __LINE__, "31 MOD 10") ;

   (* example from PIM 4th edition *)
   i := (-15) MOD 4 ;
   Assert(i, i=1, __FILE__, __LINE__, "(-15) DIV 4") ;

   (* example from ISO Standard 6-7 *)
   i := (-31) MOD 10 ;
   Assert(i, i=9, __FILE__, __LINE__, "(-31) MOD 9") ;

   (* example from ISO Standard 6-7 *)
   i := (-31) DIV 10 ;
   Assert(i, i=-4, __FILE__, __LINE__, "(-31) DIV 10") ;

   (* example from ISO Standard 6-7 *)
   i := 31 REM 10 ;
   Assert(i, i=1, __FILE__, __LINE__, "31 REM 10") ;
   i := 31 REM (-10) ;
   Assert(i, i=1, __FILE__, __LINE__, "31 REM (-10)") ;
   i := (-31) REM 10 ;
   Assert(i, i=-1, __FILE__, __LINE__, "(-31) REM 10") ;
   i := (-31) REM (-10) ;
   Assert(i, i=-1, __FILE__, __LINE__, "(-31) REM (-10)") ;

   (* example from ISO Standard 6-7 *)
   i := (-31) / 10 ;
   Assert(i, i=-3, __FILE__, __LINE__, "(-31) / 10") ;
   exit(res)
END modulus4.
