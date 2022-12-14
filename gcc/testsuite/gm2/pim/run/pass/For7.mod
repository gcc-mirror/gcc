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

MODULE For7 ;

FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;
(* FROM EndFor IMPORT CheckLongintInteger ; *)

CONST
   PseudoCheck = FALSE ;

VAR
   res: INTEGER ;

PROCEDURE Assert (v: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF v
   THEN
      r := printf("successfully evaluated assertion\n")
   ELSE
      r := printf("%s:%d assertion failed when evaluating %s\n", ADR(f), l, ADR(e)) ;
      res := 1
   END
END Assert ;

VAR
   c      : INTEGER ;
   n, i, j: LONGINT ;
BEGIN
   res := 0 ;
   n := 4 ;
   c := 0 ;
   FOR i := -256 TO 256 BY 64 DO
      printf("i = %d\n", i);
      j := i ;
      Assert(c<10, __FILE__,  __LINE__, "for loop executed too many times") ;
      INC(c) ;
      (* CheckLongintInteger(i, 64) *)
   END ;
   Assert(c=9, __FILE__,  __LINE__, "for loop executed too few times") ;
   Assert(i=256, __FILE__,  __LINE__, "for loop index") ;
   exit(res)
END For7.
