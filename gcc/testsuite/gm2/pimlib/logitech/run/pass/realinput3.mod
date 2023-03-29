(* Copyright (C) 2009-2020 Free Software Foundation, Inc. *)
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

MODULE realinput3 ;

FROM RealInOut IMPORT ReadReal ;
FROM FIO IMPORT FlushBuffer, StdOut, StdIn ;
FROM PushBackInput IMPORT PutString, PutCh, GetCh ;
FROM ASCII IMPORT lf ;
FROM Termbase IMPORT ReadProcedure, AssignRead ;
FROM libc IMPORT exit, printf ;


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


(*
   MyRead -
*)

PROCEDURE MyRead (VAR ch: CHAR) ;
BEGIN
   ch := GetCh(StdIn)
END MyRead ;


PROCEDURE Status () : BOOLEAN ;
BEGIN
   RETURN( TRUE )
END Status ;

VAR
   x : REAL ;
   ch: CHAR ;
   ok: BOOLEAN ;
BEGIN
   AssignRead(MyRead, Status, ok) ;
   ch := PutCh(lf) ;
   PutString(" 0.0123") ;
   ReadReal(x) ;
   Assert(x=0.0123, __LINE__)
END realinput3.
