(* RawIO.mod implement the ISO RawIO specification.

Copyright (C) 2008-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE RawIO ;

FROM IOChan IMPORT RawWrite, RawRead, ReadResult ;
FROM IOConsts IMPORT ReadResults ;
FROM libc IMPORT printf ;
FROM FIO IMPORT FlushOutErr ;


(* Reading and writing data over specified channels using raw
   operations, that is, with no conversion or interpretation.
   The read result is of the type IOConsts.ReadResults.
*)

(*
   Read - storage units from cid, and assigns them to successive
          components of to.  The read result is set to the value
          allRight, wrongFormat, or endOfInput.
*)

PROCEDURE Read (cid: IOChan.ChanId; VAR to: ARRAY OF SYSTEM.LOC) ;
VAR
   i, n: CARDINAL ;
   a   : SYSTEM.ADDRESS ;
BEGIN
   FlushOutErr ;
   a := SYSTEM.ADR(to) ;
   n := HIGH(to)+1 ;
   LOOP
      RawRead(cid, a, n, i) ;
      IF (n=0) OR
         (ReadResult(cid)=wrongFormat) OR
         (ReadResult(cid)=endOfInput)
      THEN
         EXIT
      ELSE
         INC(a, i) ;
         DEC(n, i)
      END
   END
END Read ;


(*
   memDump - 
*)

PROCEDURE memDump (a: SYSTEM.ADDRESS; len: CARDINAL) ;
VAR
   i, j: CARDINAL ;
   p   : POINTER TO SYSTEM.LOC ;
BEGIN
   p := a ;
   j := 0 ;
   FOR i := 0 TO len DO
      IF j MOD 16 = 0
      THEN
         printf ("\n%p  %02x", p, VAL(CARDINAL, p^))
      ELSE
         printf (" %02x", VAL(CARDINAL, p^))
      END ;
      INC(p) ;
      INC(j)
   END ;
   printf ("\n")
END memDump ;


(*
   Write - storage units to cid from successive components of from.
*)

PROCEDURE Write (cid: IOChan.ChanId; from: ARRAY OF SYSTEM.LOC);
BEGIN
(*
   printf ("in RawIO.mod ");
   memDump (SYSTEM.ADR(from), HIGH(from)+1) ;
*)
   RawWrite(cid, SYSTEM.ADR(from), HIGH(from)+1)
END Write ;


END RawIO.
