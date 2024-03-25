(* StdIO.mod provides general Read and Write procedures.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE StdIO ;

IMPORT IO ;


CONST
   MaxStack = 40 ;

VAR
   StackW   : ARRAY [0..MaxStack] OF ProcWrite ;
   StackWPtr: CARDINAL ;
   StackR   : ARRAY [0..MaxStack] OF ProcRead ;
   StackRPtr: CARDINAL ;


(*
   Read - is the generic procedure that all higher application layers
          should use to receive a character.
*)

PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   StackR[StackRPtr](ch)
END Read ;


(*
   Write - is the generic procedure that all higher application layers
           should use to emit a character.
*)

PROCEDURE Write (ch: CHAR) ;
BEGIN
   StackW[StackWPtr](ch)
END Write ;


(*
   PushOutput - pushes the current Write procedure onto a stack,
                any future references to Write will actually invoke
                procedure, p.
*)

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


(*
   PopOutput - restores Write to use the previous output procedure.
*)

PROCEDURE PopOutput ;
BEGIN
   IF StackWPtr=1
   THEN
      HALT
   ELSE
      DEC(StackWPtr)
   END
END PopOutput ;


(*
   GetCurrentOutput - returns the current output procedure.
*)

PROCEDURE GetCurrentOutput () : ProcWrite ;
BEGIN
   IF StackWPtr>0
   THEN
      RETURN( StackW[StackWPtr] )
   ELSE
      HALT
   END
END GetCurrentOutput ;


(*
   PushInput - pushes the current Read procedure onto a stack,
               any future references to Read will actually invoke
               procedure, p.
*)

PROCEDURE PushInput (p: ProcRead) ;
BEGIN
   IF StackRPtr=MaxStack
   THEN
      HALT
   ELSE
      INC(StackRPtr) ;
      StackR[StackRPtr] := p
   END
END PushInput ;


(*
   PopInput - restores Write to use the previous output procedure.
*)

PROCEDURE PopInput ;
BEGIN
   IF StackRPtr=1
   THEN
      HALT
   ELSE
      DEC(StackRPtr)
   END
END PopInput ;


(*
   GetCurrentInput - returns the current input procedure.
*)

PROCEDURE GetCurrentInput () : ProcRead ;
BEGIN
   IF StackRPtr>0
   THEN
      RETURN( StackR[StackRPtr] )
   ELSE
      HALT
   END
END GetCurrentInput ;


BEGIN
   StackWPtr := 0 ;
   StackRPtr := 0 ;
   PushOutput(IO.Write) ;
   PushInput(IO.Read)
END StdIO.
