(* RTExceptions.mod runtime exception handler routines.

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

IMPLEMENTATION MODULE RTExceptions ;

FROM ASCII IMPORT nul, nl ;
FROM StrLib IMPORT StrLen ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM SYSTEM IMPORT ADR, THROW ;
FROM libc IMPORT write, strlen ;
FROM M2RTS IMPORT HALT, Halt ;
FROM SysExceptions IMPORT InitExceptionHandlers ;

IMPORT M2EXCEPTION ;


CONST
   MaxBuffer = 4096 ;

TYPE
   Handler = POINTER TO RECORD
                           p    : ProcedureHandler ;
                           n    : CARDINAL ;
                           right,
                           left,
                           stack: Handler ;
                        END ;

   EHBlock = POINTER TO RECORD
                           buffer  : ARRAY [0..MaxBuffer] OF CHAR ;
                           number  : CARDINAL ;
                           handlers: Handler ;
                           right   : EHBlock ;
                        END ;

   PtrToChar = POINTER TO CHAR ;


VAR
   inException  : BOOLEAN ;
   freeHandler  : Handler ;
   freeEHB,
   currentEHB   : EHBlock ;
   currentSource: ADDRESS ;


(*
   SetExceptionSource - sets the current exception source to, source.
*)

PROCEDURE SetExceptionSource (source: ADDRESS) ;
BEGIN
   currentSource := source
END SetExceptionSource ;


(*
   GetExceptionSource - returns the current exception source.
*)

PROCEDURE GetExceptionSource () : ADDRESS ;
BEGIN
   RETURN currentSource
END GetExceptionSource ;


(*
   ErrorString - writes a string to stderr.
*)

PROCEDURE ErrorString (a: ARRAY OF CHAR) ;
VAR
   n: INTEGER ;
BEGIN
   n := write(2, ADR(a), StrLen(a))
END ErrorString ;


(*
   findHandler -
*)

PROCEDURE findHandler (e: EHBlock; number: CARDINAL) : Handler ;
VAR
   h: Handler ;
BEGIN
   h := e^.handlers^.right ;
   WHILE (h#e^.handlers) AND (number#h^.n) DO
      h := h^.right
   END ;
   IF h=e^.handlers
   THEN
      RETURN( NIL )
   ELSE
      RETURN( h )
   END
END findHandler ;


(*
   InvokeHandler - invokes the associated handler for the current
                   exception in the active EHB.
*)

PROCEDURE InvokeHandler  <* noreturn *> ;
VAR
   h: Handler ;
BEGIN
   h := findHandler (currentEHB, currentEHB^.number) ;
   IF h=NIL
   THEN
      THROW (GetNumber (GetExceptionBlock ()))
   ELSE
      h^.p ;
      HALT
   END
END InvokeHandler ;


(*
   DefaultErrorCatch - displays the current error message in
                       the current exception block and then
                       calls HALT.
*)

PROCEDURE DefaultErrorCatch ;
VAR
   e: EHBlock ;
   n: INTEGER ;
BEGIN
   e := GetExceptionBlock() ;
   n := write (2, GetTextBuffer (e), strlen (GetTextBuffer (e))) ;
   HALT
END DefaultErrorCatch ;


(*
   DoThrow - throw the exception number in the exception block.
*)

PROCEDURE DoThrow ;
BEGIN
   THROW (GetNumber (GetExceptionBlock ()))
END DoThrow ;


(*
   BaseExceptionsThrow - configures the Modula-2 exceptions to call
                         THROW which in turn can be caught by an
                         exception block.  If this is not called then
                         a Modula-2 exception will simply call an
                         error message routine and then HALT.
*)

PROCEDURE BaseExceptionsThrow ;
VAR
   i: M2EXCEPTION.M2Exceptions ;
BEGIN
   FOR i := MIN(M2EXCEPTION.M2Exceptions) TO MAX(M2EXCEPTION.M2Exceptions) DO
      PushHandler (GetExceptionBlock (), VAL (CARDINAL, i), DoThrow)
   END
END BaseExceptionsThrow ;


(*
   addChar - adds, ch, to the current exception handler text buffer
             at index, i.  The index in then incremented.
*)

PROCEDURE addChar (ch: CHAR; VAR i: CARDINAL) ;
BEGIN
   IF (i<=MaxBuffer) AND (currentEHB#NIL)
   THEN
      currentEHB^.buffer[i] := ch ;
      INC(i)
   END
END addChar ;


(*
   stripPath - returns the filename from the path.
*)

PROCEDURE stripPath (s: ADDRESS) : ADDRESS ;
VAR
   f, p: PtrToChar ;
BEGIN
   p := s ;
   f := s ;
   WHILE p^#nul DO
      IF p^='/'
      THEN
         INC(p) ;
         f := p
      ELSE
         INC(p)
      END
   END ;
   RETURN( f )
END stripPath ;


(*
   addFile - adds the filename determined by, s, however it strips
             any preceeding path.
*)

PROCEDURE addFile (s: ADDRESS; VAR i: CARDINAL) ;
VAR
   p: PtrToChar ;
BEGIN
   p := stripPath(s) ;
   WHILE (p#NIL) AND (p^#nul) DO
      addChar(p^, i) ;
      INC(p)
   END
END addFile ;


(*
   addStr - adds a C string from address, s, into the current
            handler text buffer.
*)

PROCEDURE addStr (s: ADDRESS; VAR i: CARDINAL) ;
VAR
   p: PtrToChar ;
BEGIN
   p := s ;
   WHILE (p#NIL) AND (p^#nul) DO
      addChar(p^, i) ;
      INC(p)
   END
END addStr ;


(*
   addNum - adds a number, n, to the current handler
            text buffer.
*)

PROCEDURE addNum (n: CARDINAL; VAR i: CARDINAL) ;
BEGIN
   IF n<10
   THEN
      addChar(CHR(n MOD 10 + ORD('0')), i)
   ELSE
      addNum(n DIV 10, i) ;
      addNum(n MOD 10, i)
   END
END addNum ;


(*
   Raise - invoke the exception handler associated with, number,
           in the active EHBlock.  It keeps a record of the number
           and message in the EHBlock for later use.
*)

PROCEDURE Raise (number: CARDINAL;
                 file: ADDRESS; line: CARDINAL;
                 column: CARDINAL; function: ADDRESS;
                 message: ADDRESS) ;
VAR
   i: CARDINAL ;
BEGIN
   currentEHB^.number := number ;
   i := 0 ;
   addFile (file, i) ;
   addChar (':', i) ;
   addNum (line, i) ;
   addChar (':', i) ;
   addNum (column, i) ;
   addChar (':', i) ;
   addChar (' ', i) ;
   addChar ('I', i) ;
   addChar ('n', i) ;
   addChar (' ', i) ;
   addStr (function, i) ;
   addChar (nl, i) ;
   addFile (file, i) ;
   addChar (':', i) ;
   addNum (line, i) ;
   addChar (':', i) ;
   addNum (column, i) ;
   addChar (':', i) ;
   addStr (message, i) ;
   addChar (nl, i) ;
   addChar (nul, i) ;
   InvokeHandler
END Raise ;


(*
   SetExceptionBlock - sets, source, as the active EHB.
*)

PROCEDURE SetExceptionBlock (source: EHBlock) ;
BEGIN
   currentEHB := source
END SetExceptionBlock ;


(*
   GetExceptionBlock - returns the active EHB.
*)

PROCEDURE GetExceptionBlock () : EHBlock ;
BEGIN
   RETURN( currentEHB )
END GetExceptionBlock ;


(*
   GetTextBuffer - returns the address of the EHB buffer.
*)

PROCEDURE GetTextBuffer (e: EHBlock) : ADDRESS ;
BEGIN
   RETURN( ADR(e^.buffer) )
END GetTextBuffer ;


(*
   GetTextBufferSize - return the size of the EHB text buffer.
*)

PROCEDURE GetTextBufferSize (e: EHBlock) : CARDINAL ;
BEGIN
   RETURN SIZE(e^.buffer)
END GetTextBufferSize ;


(*
   GetNumber - return the exception number associated with,
               source.
*)

PROCEDURE GetNumber (source: EHBlock) : CARDINAL ;
BEGIN
   RETURN( source^.number )
END GetNumber ;


(*
   New - returns a new EHBlock.
*)

PROCEDURE New () : EHBlock ;
VAR
   e: EHBlock ;
BEGIN
   IF freeEHB=NIL
   THEN
      NEW(e)
   ELSE
      e := freeEHB ;
      freeEHB := freeEHB^.right
   END ;
   RETURN( e )
END New ;


(*
   InitExceptionBlock - creates and returns a new exception block.
*)

PROCEDURE InitExceptionBlock () : EHBlock ;
VAR
   e: EHBlock ;
BEGIN
   e := New() ;
   WITH e^ DO
      number := MAX(CARDINAL) ;
      handlers := NewHandler() ;   (* add the dummy onto the head *)
      handlers^.right := handlers ;
      handlers^.left := handlers ;
      right := e
   END ;
   RETURN( e )
END InitExceptionBlock ;


(*
   KillExceptionBlock - destroys the EHB, e, and all its handlers.
*)

PROCEDURE KillExceptionBlock (e: EHBlock) : EHBlock ;
BEGIN
   e^.handlers := KillHandlers(e^.handlers) ;
   e^.right := freeEHB ;
   freeEHB := e ;
   RETURN( NIL )
END KillExceptionBlock ;


(*
   NewHandler - returns a new handler.
*)

PROCEDURE NewHandler () : Handler ;
VAR
   h: Handler ;
BEGIN
   IF freeHandler=NIL
   THEN
      NEW(h)
   ELSE
      h := freeHandler ;
      freeHandler := freeHandler^.right
   END ;
   RETURN( h )
END NewHandler ;


(*
   KillHandler - returns, NIL, and places, h, onto the free list.
*)

PROCEDURE KillHandler (h: Handler) : Handler ;
BEGIN
   h^.right := freeHandler ;
   freeHandler := h ;
   RETURN( NIL )
END KillHandler ;


(*
   KillHandlers - kills all handlers in the list.
*)

PROCEDURE KillHandlers (h: Handler) : Handler ;
BEGIN
   h^.left^.right := freeHandler ;
   freeHandler := h ;
   RETURN( NIL )
END KillHandlers ;


(*
   InitHandler -
*)

PROCEDURE InitHandler (h: Handler; l, r, s: Handler; number: CARDINAL; proc: ProcedureHandler) : Handler ;
BEGIN
   WITH h^ DO
      p := proc ;
      n := number ;
      right := r ;
      left := l ;
      stack := s
   END ;
   RETURN( h )
END InitHandler ;


(*
   SubHandler -
*)

PROCEDURE SubHandler (h: Handler) ;
BEGIN
   h^.right^.left := h^.left ;
   h^.left^.right := h^.right ;
END SubHandler ;


(*
   AddHandler - add, e, to the end of the list of handlers.
*)

PROCEDURE AddHandler (e: EHBlock; h: Handler) ;
BEGIN
   h^.right := e^.handlers ;
   h^.left := e^.handlers^.left ;
   e^.handlers^.left^.right := h ;
   e^.handlers^.left := h
END AddHandler ;


(*
   PushHandler - install a handler in EHB, e.
*)

PROCEDURE PushHandler (e: EHBlock; number: CARDINAL; p: ProcedureHandler) ;
VAR
   h, i: Handler ;
BEGIN
   h := findHandler(e, number) ;
   IF h=NIL
   THEN
      i := InitHandler(NewHandler(), NIL, NIL, NIL, number, p) ;
   ELSE
      (* remove, h, *)
      SubHandler(h) ;
      (* stack it onto a new handler *)
      i := InitHandler(NewHandler(), NIL, NIL, h, number, p) ;
   END ;
   (* add new handler *)
   AddHandler(e, i)
END PushHandler ;


(*
   PopHandler - removes the handler associated with, number, from
                EHB, e.
*)

PROCEDURE PopHandler (e: EHBlock; number: CARDINAL) ;
VAR
   h: Handler ;
BEGIN
   h := findHandler(e, number) ;
   IF h#NIL
   THEN
      (* remove, h, *)
      SubHandler(h) ;
      IF h^.stack#NIL
      THEN
         AddHandler(e, h^.stack)
      END ;
      h := KillHandler(h)
   END
END PopHandler ;


(*
   IsInExceptionState - returns TRUE if the program is currently
                        in the exception state.
*)

PROCEDURE IsInExceptionState () : BOOLEAN ;
BEGIN
   RETURN( inException )
END IsInExceptionState ;


(*
   SetExceptionState - returns the current exception state and
                       then sets the current exception state to,
                       to.
*)

PROCEDURE SetExceptionState (to: BOOLEAN) : BOOLEAN ;
VAR
   old: BOOLEAN ;
BEGIN
   old := inException ;
   inException := to ;
   RETURN( old )
END SetExceptionState ;


(*
   SwitchExceptionState - assigns, from, with the current exception
                          state and then assigns the current exception
                          to, to.
*)

PROCEDURE SwitchExceptionState (VAR from: BOOLEAN; to: BOOLEAN) ;
BEGIN
   from := inException ;
   inException := to
END SwitchExceptionState ;


(*
   GetBaseExceptionBlock - returns the initial language exception block
                           created.
*)

PROCEDURE GetBaseExceptionBlock () : EHBlock ;
BEGIN
   IF currentEHB=NIL
   THEN
      Halt('currentEHB has not been initialized yet',
           __FILE__, __FUNCTION__, __LINE__)
   ELSE
      RETURN( currentEHB )
   END
END GetBaseExceptionBlock ;


(*
   indexf - raise an index out of bounds exception.
*)

PROCEDURE indexf (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.indexException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("array index out of bounds"))
END indexf ;


(*
   range - raise an assignment out of range exception.
*)

PROCEDURE range (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.rangeException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("assignment out of range"))
END range ;


(*
   casef - raise a case selector out of range exception.
*)

PROCEDURE casef (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.caseSelectException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("case selector out of range"))
END casef ;


(*
   invalidloc - raise an invalid location exception.
*)

PROCEDURE invalidloc (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.invalidLocation),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("invalid address referenced"))
END invalidloc ;


(*
   function - raise a ... function ... exception.  --fixme-- what does this exception catch?
*)

PROCEDURE function (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.functionException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("... function ... "))  (* --fixme-- what has happened ? *)
END function ;


(*
   wholevalue - raise an illegal whole value exception.
*)

PROCEDURE wholevalue (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.wholeValueException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("illegal whole value exception"))
END wholevalue ;


(*
   wholediv - raise a division by zero exception.
*)

PROCEDURE wholediv (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.wholeDivException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("illegal whole value exception"))
END wholediv ;


(*
   realvalue - raise an illegal real value exception.
*)

PROCEDURE realvalue (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.realValueException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("illegal real value exception"))
END realvalue ;


(*
   realdiv - raise a division by zero in a real number exception.
*)

PROCEDURE realdiv (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.realDivException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("real number division by zero exception"))
END realdiv ;


(*
   complexvalue - raise an illegal complex value exception.
*)

PROCEDURE complexvalue (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.complexValueException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("illegal complex value exception"))
END complexvalue ;


(*
   complexdiv - raise a division by zero in a complex number exception.
*)

PROCEDURE complexdiv (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.complexDivException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("complex number division by zero exception"))
END complexdiv ;


(*
   protection - raise a protection exception.
*)

PROCEDURE protection (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.protException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("protection exception"))
END protection ;


(*
   systemf - raise a system exception.
*)

PROCEDURE systemf (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.sysException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("system exception"))
END systemf ;


(*
   coroutine - raise a coroutine exception.
*)

PROCEDURE coroutine (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.coException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("coroutine exception"))
END coroutine ;


(*
   exception - raise a exception exception.
*)

PROCEDURE exception (a: ADDRESS) ;
BEGIN
   Raise(ORD(M2EXCEPTION.exException),
         ADR(__FILE__), __LINE__, __COLUMN__, ADR(__FUNCTION__),
         ADR("exception exception"))
END exception ;


(*
   Init - initialises this module.
*)

PROCEDURE Init ;
BEGIN
   inException := FALSE ;
   freeHandler := NIL ;
   freeEHB := NIL ;
   currentEHB := InitExceptionBlock() ;
   currentSource := NIL ;
   BaseExceptionsThrow ;
   InitExceptionHandlers(indexf, range, casef, invalidloc,
                         function, wholevalue, wholediv,
                         realvalue, realdiv, complexvalue,
                         complexdiv, protection, systemf,
                         coroutine, exception)
END Init ;


(*
   TidyUp - deallocate memory used by this module.
*)

PROCEDURE TidyUp ;
VAR
   f: Handler ;
   e: EHBlock ;
BEGIN
   IF currentEHB#NIL
   THEN
      currentEHB := KillExceptionBlock(currentEHB)
   END ;
   WHILE freeHandler#NIL DO
      f := freeHandler ;
      freeHandler := freeHandler^.right ;
      DISPOSE(f)
   END ;
   WHILE freeEHB#NIL DO
      e := freeEHB ;
      freeEHB := freeEHB^.right ;
      DISPOSE(e)
   END
END TidyUp ;


BEGIN
   Init
FINALLY
   TidyUp
END RTExceptions.
