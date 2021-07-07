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

MODULE modulus ;

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
      r := printf("successfully evaluated %s\n", ADR(e))
   ELSE
      r := printf("%s:%d assertion failed when evaluating %s\n", ADR(f), l, ADR(e)) ;
      res := 1
   END
END Assert ;


VAR
   i: INTEGER ;
BEGIN
   res := 0 ;

   (* see P29 of PIM4 *)
   i := 31 MOD 10 ;
   Assert(i=1, __FILE__, __LINE__, "31 MOD 10") ;

   (* example from PIM 4th edition *)
   i := (-15) MOD 4 ;
   Assert(i=1, __FILE__, __LINE__, "(-15) DIV 4") ;

   i := (-31) MOD 10 ;
   Assert(i=9, __FILE__, __LINE__, "(-31) MOD 9") ;

   i := (-31) DIV 10 ;
   Assert(i=-4, __FILE__, __LINE__, "(-31) DIV 10") ;

   (* and we allow ISO compatability *)
   i := (-31) / 10 ;
   Assert(i=-3, __FILE__, __LINE__, "(-31) / 10") ;
   exit(res)
END modulus.
