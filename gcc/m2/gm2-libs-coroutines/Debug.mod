(* Debug.mod provides some simple debugging routines.

Copyright (C) 2002-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE Debug ;


FROM ASCII IMPORT cr, nul, lf ;
FROM NumberIO IMPORT CardToStr ;
FROM libc IMPORT exit, write ;
FROM SYSTEM IMPORT ADR ;


CONST
   MaxStack = 10 ;

VAR
   Stack: ARRAY [1..MaxStack] OF WriteP ;
   Ptr  : CARDINAL ;


PROCEDURE Write (ch: CHAR) ;
BEGIN
   IF Ptr>0
   THEN
      Stack[Ptr](ch)
   ELSE
      LocalWrite(ch)
   END
END Write ;


PROCEDURE LocalWrite (ch: CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   r := write(2, ADR(ch), 1)
END LocalWrite ;


(*
   PushOutput - pushes the output procedure, p, which is used Debug.
*)

PROCEDURE PushOutput (p: WriteP) ;
BEGIN
   IF Ptr=MaxStack
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'stack exceeded')
   ELSE
      INC(Ptr) ;
      Stack[Ptr] := p
   END
END PushOutput ;


(*
   PopOutput - pops the current output procedure from the stack.
*)

PROCEDURE PopOutput ;
BEGIN
   IF Ptr>1
   THEN
      DEC(Ptr)
   END
END PopOutput ;


(*
   Halt - writes a message in the format:
          Module:Line:Message

          It then terminates by calling HALT.
*)

PROCEDURE Halt (File    : ARRAY OF CHAR;
                LineNo  : CARDINAL;
                Function,
                Message : ARRAY OF CHAR) ;
CONST
   MaxNoOfDigits = 12 ;  (* should be large enough for most source files.. *)
VAR
   No               : ARRAY [0..MaxNoOfDigits] OF CHAR ;
BEGIN
   DebugString(File) ;
   CardToStr(LineNo, 0, No) ;
   DebugString(':') ;
   DebugString(No) ;
   DebugString(':') ;
   DebugString(Function) ;
   DebugString(':') ;
   DebugString(Message) ;
   DebugString('\n') ;
   HALT
END Halt ;


(*
   DebugString - writes a string to the debugging device (Scn.Write).
                 It interprets \n as carriage return, linefeed.
*)

PROCEDURE DebugString (a: ARRAY OF CHAR) ;
VAR
   n, high: CARDINAL ;
BEGIN
   high := HIGH( a ) ;
   n := 0 ;
   WHILE (n <= high) AND (a[n] # nul) DO
      IF a[n]='\'
      THEN
         IF n+1<=high
         THEN
            IF a[n+1]='n'
            THEN
               WriteLn ;
               INC(n)
            ELSIF a[n+1]='\'
            THEN
               Write('\') ;
               INC(n)
            END
         END
      ELSE
         Write( a[n] )
      END ;
      INC( n )
   END
END DebugString ;


(*
   WriteLn - writes a carriage return and a newline
             character.
*)

PROCEDURE WriteLn ;
BEGIN
   Write(cr) ;
   Write(lf)
END WriteLn ;


(*
   Init - initialises this module.
*)

PROCEDURE Init ;
BEGIN
   Ptr := 0 ;
   PushOutput(LocalWrite)
END Init ;


BEGIN
   Init
END Debug.
