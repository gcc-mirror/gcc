(* PushBackInput.mod provides a method for pushing back and consuming input.

Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE PushBackInput ;


FROM FIO IMPORT ReadChar, IsNoError, EOF, OpenToRead, WriteChar, StdErr ;
FROM DynamicStrings IMPORT string, Length, char ;
FROM ASCII IMPORT nul, cr, lf ;
FROM Debug IMPORT Halt ;
FROM StrLib IMPORT StrCopy, StrLen ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT Write, PushOutput, PopOutput ;
FROM libc IMPORT exit ;

IMPORT FIO ;


CONST
   MaxPushBackStack =  8192 ;
   MaxFileName      =  4096 ;

VAR
   FileName  : ARRAY [0..MaxFileName] OF CHAR ;
   CharStack : ARRAY [0..MaxPushBackStack] OF CHAR ;
   ExitStatus: CARDINAL ;
   Column,
   StackPtr,
   LineNo    : CARDINAL ;
   Debugging : BOOLEAN ;


(*
   GetCh - gets a character from either the push back stack or
           from file, f.
*)

PROCEDURE GetCh (f: File) : CHAR ;
VAR
   ch: CHAR ;
BEGIN
   IF StackPtr>0
   THEN
      DEC(StackPtr) ;
      IF Debugging
      THEN
         Write(CharStack[StackPtr])
      END ;
      RETURN( CharStack[StackPtr] )
   ELSE
      IF EOF(f) OR (NOT IsNoError(f))
      THEN
         ch := nul
      ELSE
         REPEAT
            ch := ReadChar(f)
         UNTIL (ch#cr) OR EOF(f) OR (NOT IsNoError(f)) ;
         IF ch=lf
         THEN
            Column := 0 ;
            INC(LineNo)
         ELSE
            INC(Column)
         END
      END ;
      IF Debugging
      THEN
         Write(ch)
      END ;
      RETURN( ch )
   END
END GetCh ;


(*
   PutStr - pushes a dynamic string onto the push back stack.
            The string, s, is not deallocated.
*)

PROCEDURE PutStr (s: String) ;
VAR
   i: CARDINAL ;
BEGIN
   i := Length (s) ;
   WHILE i > 0 DO
      DEC (i) ;
      IF PutCh (char (s, i)) # char (s, i)
      THEN
         Halt('assert failed', __FILE__, __FUNCTION__, __LINE__)
      END
   END
END PutStr ;


(*
   PutString - pushes a string onto the push back stack.
*)

PROCEDURE PutString (a: ARRAY OF CHAR) ;
VAR
   l: CARDINAL ;
BEGIN
   l := StrLen (a) ;
   WHILE l > 0 DO
      DEC (l) ;
      IF PutCh (a[l]) # a[l]
      THEN
         Halt ('assert failed', __FILE__, __FUNCTION__, __LINE__)
      END
   END
END PutString ;


(*
   PutCh - pushes a character onto the push back stack, it also
           returns the character which has been pushed.
*)

PROCEDURE PutCh (ch: CHAR) : CHAR ;
BEGIN
   IF StackPtr<MaxPushBackStack
   THEN
      CharStack[StackPtr] := ch ;
      INC(StackPtr)
   ELSE
      Halt('max push back stack exceeded, increase MaxPushBackStack',
           __FILE__, __FUNCTION__, __LINE__)
   END ;
   RETURN( ch )
END PutCh ;


(*
   Open - opens a file for reading.
*)

PROCEDURE Open (a: ARRAY OF CHAR) : File ;
BEGIN
   Init ;
   StrCopy(a, FileName) ;
   RETURN( OpenToRead(a) )
END Open ;


(*
   Close - closes the opened file.
*)

PROCEDURE Close (f: File) ;
BEGIN
   FIO.Close(f)
END Close ;


(*
   ErrChar - writes a char, ch, to stderr.
*)

PROCEDURE ErrChar (ch: CHAR) ;
BEGIN
   WriteChar(StdErr, ch)
END ErrChar ;


(*
   Error - emits an error message with the appropriate file, line combination.
*)

PROCEDURE Error (a: ARRAY OF CHAR) ;
BEGIN
   PushOutput(ErrChar) ;
   WriteString(FileName) ; Write(':') ; WriteCard(LineNo, 0) ; Write(':') ; WriteString(a) ; WriteLn ;
   PopOutput ;
   FIO.Close(StdErr) ;
   exit(1)
END Error ;


(*
   WarnError - emits an error message with the appropriate file, line combination.
               It does not terminate but when the program finishes an exit status of
               1 will be issued.
*)

PROCEDURE WarnError (a: ARRAY OF CHAR) ;
BEGIN
   PushOutput(ErrChar) ;
   WriteString(FileName) ; Write(':') ; WriteCard(LineNo, 0) ; Write(':') ; WriteString(a) ; WriteLn ;
   PopOutput ;
   ExitStatus := 1
END WarnError ;


(*
   WarnString - emits an error message with the appropriate file, line combination.
                It does not terminate but when the program finishes an exit status of
                1 will be issued.
*)

PROCEDURE WarnString (s: String) ;
VAR
   p : POINTER TO CHAR ;
BEGIN
   p := string(s) ;
   WriteString(FileName) ; Write(':') ; WriteCard(LineNo, 0) ; Write(':') ;
   REPEAT
      IF p#NIL
      THEN
         IF p^=lf
         THEN
            WriteLn ;
            WriteString(FileName) ; Write(':') ; WriteCard(LineNo, 0) ; Write(':')
         ELSE
            Write(p^)
         END ;
         INC(p)
      END ;
   UNTIL (p=NIL) OR (p^=nul) ;
   ExitStatus := 1
END WarnString ;


(*
   GetExitStatus - returns the exit status which will be 1 if any warnings were issued.
*)

PROCEDURE GetExitStatus () : CARDINAL ;
BEGIN
   RETURN( ExitStatus )
END GetExitStatus ;


(*
   SetDebug - sets the debug flag on or off.
*)

PROCEDURE SetDebug (d: BOOLEAN) ;
BEGIN
   Debugging := d
END SetDebug ;


(*
   GetColumnPosition - returns the column position of the current character.
*)

PROCEDURE GetColumnPosition () : CARDINAL ;
BEGIN
   IF StackPtr>Column
   THEN
      RETURN( 0 )
   ELSE
      RETURN( Column-StackPtr )
   END
END GetColumnPosition ;


(*
   GetCurrentLine - returns the current line number.
*)

PROCEDURE GetCurrentLine () : CARDINAL ;
BEGIN
   RETURN( LineNo )
END GetCurrentLine ;


(*
   Init - initialize global variables.
*)

PROCEDURE Init ;
BEGIN
   ExitStatus := 0 ;
   StackPtr   := 0 ;
   LineNo     := 1 ;
   Column     := 0
END Init ;


BEGIN
   SetDebug(FALSE) ;
   Init
END PushBackInput.
