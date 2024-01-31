(* Copyright (C) 2015 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

MODULE teststdio ;   (*!m2pim*)

CONST
   MaxStack = 40 ;

TYPE
   ProcWrite = PROCEDURE (CHAR) ;

VAR
   StackW   : ARRAY [0..MaxStack] OF ProcWrite ;
   StackWPtr: CARDINAL ;


PROCEDURE write (ch: CHAR) ;
BEGIN

END write ;


PROCEDURE PushOutput (p: ProcWrite) ;
BEGIN
   IF StackWPtr=MaxStack
   THEN
      HALT
   ELSE
      INC(StackWPtr) ;
      StackW[StackWPtr] := p
   END
END PushOutput ;


BEGIN
   StackWPtr := 0 ;
   PushOutput(write)
END teststdio.
