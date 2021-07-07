(* EXCEPTIONS.mod implement the ISO EXCEPTIONS specification.

Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE EXCEPTIONS ;

IMPORT RTExceptions ;
IMPORT M2EXCEPTION ;
IMPORT M2RTS ;
IMPORT ASCII ;

FROM SYSTEM IMPORT ADR ;
FROM Storage IMPORT ALLOCATE ;


TYPE
   ExceptionSource = POINTER TO RECORD
                                   eh: RTExceptions.EHBlock ;
                                END ;
   (* values of this type are used within library modules to
      identify the source of raised exceptions *)


PROCEDURE AllocateSource (VAR newSource: ExceptionSource) ;
  (* Allocates a unique value of type ExceptionSource *)
BEGIN
   NEW(newSource) ;
   WITH newSource^ DO
      eh := RTExceptions.InitExceptionBlock()
   END
END AllocateSource ;


PROCEDURE RAISE (source: ExceptionSource;
                 number: ExceptionNumber;
                 message: ARRAY OF CHAR) ;
  (* Associates the given values of source, number and message with
     the current context and raises an exception.
  *)
BEGIN
   RTExceptions.SetExceptionSource(source) ;
   RTExceptions.SetExceptionBlock(source^.eh) ;
   RTExceptions.Raise(number, ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__), ADR(message)) ;
   (* we should never reach here as Raise should jump to the exception handler *)
   M2RTS.Halt(__FILE__, __LINE__, __FUNCTION__, 'should never return from RTException.Raise')
END RAISE ;


PROCEDURE CurrentNumber (source: ExceptionSource) : ExceptionNumber ;
  (* If the current coroutine is in the exceptional execution state
     because of the raising of an exception from source, returns the
     corresponding number, and otherwise raises an exception.
  *)
BEGIN
   IF RTExceptions.IsInExceptionState()
   THEN
      RETURN( RTExceptions.GetNumber(source^.eh) )
   ELSE
      RTExceptions.Raise(ORD(M2EXCEPTION.coException),
                         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
                         ADR('current coroutine is not in the exceptional execution state'))
   END
END CurrentNumber ;


PROCEDURE GetMessage (VAR text: ARRAY OF CHAR) ;
  (* If the current coroutine is in the exceptional execution state,
     returns the possibly truncated string associated with the
     current context.  Otherwise, in normal execution state,
     returns the empty string.
  *)
VAR
   p   : POINTER TO CHAR ;
   i, h: CARDINAL ;
BEGIN
   IF RTExceptions.IsInExceptionState()
   THEN
      h := HIGH(text) ;
      i := 0 ;
      p := RTExceptions.GetTextBuffer(RTExceptions.GetExceptionBlock()) ;
      WHILE (p#NIL) AND (p^#ASCII.nul) DO
         text[i] := p^ ;
         INC(i) ;
         INC(p)
      END ;
      IF i<=h
      THEN
         text[i] := ASCII.nul
      END
   ELSE
      text[0] := ASCII.nul
   END
END GetMessage ;


PROCEDURE IsCurrentSource (source: ExceptionSource) : BOOLEAN ;
  (* If the current coroutine is in the exceptional execution state
     because of the raising of an exception from source, returns TRUE,
     and otherwise returns FALSE.
  *)
BEGIN
   RETURN( RTExceptions.IsInExceptionState() AND (source=RTExceptions.GetExceptionSource()) )
END IsCurrentSource ;


PROCEDURE IsExceptionalExecution () : BOOLEAN ;
  (* If the current coroutine is in the exceptional execution state
     because of the raising of an exception, returns TRUE,
     and otherwise returns FALSE.
  *)
BEGIN
   RETURN( RTExceptions.IsInExceptionState() )
END IsExceptionalExecution ;


END EXCEPTIONS.
