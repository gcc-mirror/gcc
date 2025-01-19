(* M2Error.mod error reporting interface.

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

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2Error ;

FROM NameKey IMPORT NulName, Name, KeyToCharStar ;
FROM DynamicStrings IMPORT String, InitString, InitStringCharStar, ConCat, ConCatChar, Mark, string, KillString, Dup ;
FROM FIO IMPORT StdOut, WriteNBytes, Close, FlushBuffer ;
FROM StrLib IMPORT StrLen, StrEqual ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3 ;
FROM M2LexBuf IMPORT FindFileNameFromToken, TokenToLineNo, TokenToColumnNo, GetTokenNo ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3 ;
FROM M2Options IMPORT Xcode ;
FROM M2RTS IMPORT ExitOnHalt ;
FROM SYSTEM IMPORT ADDRESS ;
FROM M2Emit IMPORT EmitError ;
FROM M2LexBuf IMPORT UnknownTokenNo ;
FROM M2StackAddress IMPORT StackOfAddress, InitStackAddress, PushAddress, PopAddress, NoOfItemsInStackAddress ;
FROM Indexing IMPORT Index, HighIndice, InitIndex, GetIndice, PutIndice ;
FROM M2Debug IMPORT Assert ;
FROM M2Pass IMPORT IsPass0, IsPass1 ;
FROM SymbolTable IMPORT NulSym ;

FROM M2ColorString IMPORT filenameColor, endColor, errorColor, warningColor, noteColor,
                          range1Color, range2Color, quoteOpen, quoteClose ;

IMPORT M2Emit ;


CONST
   Debugging  =  TRUE ;
   DebugTrace = FALSE ;
   DebugError = FALSE ;

TYPE
   Error = POINTER TO RECORD
                         parent,
                         child,
                         next     : Error ;
                         note,
                         fatal    : BOOLEAN ;
                         s        : String ;
                         (* index of token causing the error *)
                         token    : CARDINAL ;
                         color    : BOOLEAN ;
                         scope    : ErrorScope ;
                      END ;

   KindScope = (noscope, definition, implementation, program, module, procedure) ;

   ErrorScope = POINTER TO RECORD
                              scopeKind: KindScope ;
                              scopeName: Name ;
                              symbol   : CARDINAL ;   (* symbol table entry.  *)
                           END ;


VAR
   head        : Error ;
   InInternal  : BOOLEAN ;
   lastScope   : ErrorScope ;
   scopeIndex  : CARDINAL ;
   scopeArray  : Index ;
   currentScope: ErrorScope ;
   scopeStack  : StackOfAddress ;


(*
   SetColor - informs the error module that this error will have had colors
              assigned to it.  If an error is issued without colors assigned
              then the default colors will be assigned to the legacy error
              messages.
*)

PROCEDURE SetColor (e: Error) : Error ;
BEGIN
   e^.color := TRUE ;
   RETURN e
END SetColor ;


(*
   Cast - casts a := b
*)

PROCEDURE Cast (VAR a: ARRAY OF BYTE; b: ARRAY OF BYTE) ;
VAR
   i: CARDINAL ;
BEGIN
   IF HIGH(a)=HIGH(b)
   THEN
      FOR i := 0 TO HIGH(a) DO
         a[i] := b[i]
      END
   END
END Cast ;


(*
   TranslateNameToString - takes a format specification string, a, and
                           if they consist of of %a then this is translated
                           into a String and %a is replaced by %s.
*)

PROCEDURE TranslateNameToCharStar (VAR a: ARRAY OF CHAR;
                                   n: CARDINAL) : BOOLEAN ;
VAR
   argno,
   i, h : CARDINAL ;
BEGIN
   argno := 1 ;
   i := 0 ;
   h := StrLen(a) ;
   WHILE i<h DO
      IF (a[i]='%') AND (i+1<h)
      THEN
         IF (a[i+1]='a') AND (argno=n)
         THEN
            a[i+1] := 's' ;
            RETURN( TRUE )
         END ;
         INC(argno) ;
         IF argno>n
         THEN
            (* all done *)
            RETURN( FALSE )
         END
      END ;
      INC(i)
   END ;
   RETURN( FALSE )
END TranslateNameToCharStar ;


(*
   InternalError - displays an internal error message together with the compiler source
                   file and line number.
                   This function is not buffered and is used when the compiler is about
                   to give up.
*)

PROCEDURE InternalError (message: ARRAY OF CHAR) <* noreturn *> ;
BEGIN
   IF NOT InInternal
   THEN
      InInternal := TRUE ;
      FlushErrors
   END ;
   M2Emit.InternalError (message) ;
   HALT
END InternalError ;


(* ***************************************************************************
   The following routines are used for normal syntax and semantic error reporting
   *************************************************************************** *)


(*
   WriteFormat0 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat0 (a: ARRAY OF CHAR) ;
VAR
   e: Error ;
BEGIN
   e := NewError(GetTokenNo()) ;
   WITH e^ DO
      s := Sprintf0(Mark(InitString(a)))
   END
END WriteFormat0 ;


(*
   WarnFormat0 - displays the source module and line together
                 with the encapsulated format string.
                 Used for simple warning messages tied to the current token.
*)

PROCEDURE WarnFormat0 (a: ARRAY OF CHAR) ;
VAR
   e: Error ;
BEGIN
   e := NewWarning(GetTokenNo()) ;
   WITH e^ DO
      s := Sprintf0(Mark(InitString(a)))
   END
END WarnFormat0 ;


(*
   DoFormat1 -
*)

PROCEDURE DoFormat1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) : String ;
VAR
   s: String ;
   n: Name ;
BEGIN
   n := NulName ;
   IF TranslateNameToCharStar(a, 1)
   THEN
      Cast(n, w) ;
      s := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      s := Sprintf1(Mark(InitString(a)), s)
   ELSE
      s := Sprintf1(Mark(InitString(a)), w)
   END ;
   RETURN( s )
END DoFormat1 ;


(*
   WriteFormat1 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   e: Error ;
BEGIN
   e := NewError(GetTokenNo()) ;
   e^.s := DoFormat1(a, w)
END WriteFormat1 ;


(*
   WarnFormat1 - displays the source module and line together
                 with the encapsulated format string.
                 Used for simple warning messages tied to the current token.
*)

PROCEDURE WarnFormat1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   e: Error ;
BEGIN
   e := NewWarning(GetTokenNo()) ;
   e^.s := DoFormat1(a, w)
END WarnFormat1 ;


(*
   DoFormat2 -
*)

PROCEDURE DoFormat2 (a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) : String ;
VAR
   n     : Name ;
   s,
   s1, s2: String ;
   b     : BITSET ;
BEGIN
   b := {} ;
   n := NulName ;
   IF TranslateNameToCharStar(a, 1)
   THEN
      Cast(n, w1) ;
      s1 := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      INCL(b, 1)
   END ;
   IF TranslateNameToCharStar(a, 2)
   THEN
      Cast(n, w2) ;
      s2 := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      INCL(b, 2)
   END ;
   CASE b OF

   {}   :  s := Sprintf2(Mark(InitString(a)), w1, w2) |
   {1}  :  s := Sprintf2(Mark(InitString(a)), s1, w2) |
   {2}  :  s := Sprintf2(Mark(InitString(a)), w1, s2) |
   {1,2}:  s := Sprintf2(Mark(InitString(a)), s1, s2)

   ELSE
      HALT
   END ;
   RETURN( s )
END DoFormat2 ;


(*
   WriteFormat2 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat2 (a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) ;
VAR
   e: Error ;
BEGIN
   e := NewError(GetTokenNo()) ;
   e^.s := DoFormat2(a, w1, w2)
END WriteFormat2 ;


PROCEDURE DoFormat3 (a: ARRAY OF CHAR; w1, w2, w3: ARRAY OF BYTE) : String ;
VAR
   n            : Name ;
   s, s1, s2, s3: String ;
   b            : BITSET ;
BEGIN
   b := {} ;
   n := NulName ;
   IF TranslateNameToCharStar(a, 1)
   THEN
      Cast(n, w1) ;
      s1 := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      INCL(b, 1)
   END ;
   IF TranslateNameToCharStar(a, 2)
   THEN
      Cast(n, w2) ;
      s2 := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      INCL(b, 2)
   END ;
   IF TranslateNameToCharStar(a, 3)
   THEN
      Cast(n, w3) ;
      s3 := Mark(InitStringCharStar(KeyToCharStar(n))) ;
      INCL(b, 3)
   END ;
   CASE b OF

   {}     :  s := Sprintf3(Mark(InitString(a)), w1, w2, w3) |
   {1}    :  s := Sprintf3(Mark(InitString(a)), s1, w2, w3) |
   {2}    :  s := Sprintf3(Mark(InitString(a)), w1, s2, w3) |
   {1,2}  :  s := Sprintf3(Mark(InitString(a)), s1, s2, w3) |
   {3}    :  s := Sprintf3(Mark(InitString(a)), w1, w2, s3) |
   {1,3}  :  s := Sprintf3(Mark(InitString(a)), s1, w2, s3) |
   {2,3}  :  s := Sprintf3(Mark(InitString(a)), w1, s2, s3) |
   {1,2,3}:  s := Sprintf3(Mark(InitString(a)), s1, s2, s3)

   ELSE
      HALT
   END ;
   RETURN( s )
END DoFormat3 ;


(*
   WriteFormat3 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE WriteFormat3 (a: ARRAY OF CHAR; w1, w2, w3: ARRAY OF BYTE) ;
VAR
   e: Error ;
BEGIN
   e := NewError(GetTokenNo()) ;
   e^.s := DoFormat3(a, w1, w2, w3)
END WriteFormat3 ;


(*
   MoveError - repositions an error, e, to token, AtTokenNo, and returns, e.
*)

PROCEDURE MoveError (e: Error; AtTokenNo: CARDINAL) : Error ;
BEGIN
   IF e # NIL
   THEN
      e^.token := AtTokenNo
   END ;
   RETURN e
END MoveError ;


(*
   NewError - creates and returns a new error handle.
*)

PROCEDURE NewError (AtTokenNo: CARDINAL) : Error ;
VAR
   e, f: Error ;
BEGIN
   IF AtTokenNo = UnknownTokenNo
   THEN
      (* This could be used as a useful debugging hook as the front end
         has forgotten the token no.  *)
   END ;
   NEW(e) ;
   WITH e^ DO
      s      := NIL ;
      token  := AtTokenNo ;
      next   := NIL ;
      parent := NIL ;
      child  := NIL ;
      note   := FALSE ;
      fatal  := TRUE ;
      color  := FALSE ;
   END ;
   (* Assert (scopeKind # noscope) ;  *)
   e^.scope := currentScope ;
   IF (head=NIL) OR (head^.token>AtTokenNo)
   THEN
      e^.next := head ;
      head    := e
   ELSE
      f := head ;
      WHILE (f^.next#NIL) AND (f^.next^.token<AtTokenNo) DO
         f := f^.next
      END ;
      e^.next := f^.next ;
      f^.next := e
   END ;
   RETURN( e )
END NewError ;


(*
   NewWarning - creates and returns a new error handle suitable for a warning.
                A warning will not stop compilation.
*)

PROCEDURE NewWarning (AtTokenNo: CARDINAL) : Error ;
VAR
   e: Error ;
BEGIN
   e := NewError(AtTokenNo) ;
   e^.fatal := FALSE ;
   e^.note  := FALSE ;
   RETURN e
END NewWarning ;


(*
   NewNote - creates and returns a new error handle suitable for a note.
             A note will not stop compilation.
*)

PROCEDURE NewNote (AtTokenNo: CARDINAL) : Error ;
VAR
   e: Error ;
BEGIN
   e := NewError(AtTokenNo) ;
   e^.fatal := FALSE ;
   e^.note  := TRUE ;
   RETURN e
END NewNote ;


(*
   ChainError - creates and returns a new error handle, this new error
                is associated with, e, and is chained onto the end of, e.
                If, e, is NIL then the result to NewError is returned.
*)

PROCEDURE ChainError (AtTokenNo: CARDINAL; e: Error) : Error ;
VAR
   f: Error ;
BEGIN
   IF e=NIL
   THEN
      RETURN NewError (AtTokenNo)
   ELSE
      NEW (f) ;
      WITH f^ DO
         s         := NIL ;
         token     := AtTokenNo ;
         next      := e^.child ;
         parent    := e ;
         child     := NIL ;
         fatal     := e^.fatal ;
         scope     := e^.scope
      END ;
      e^.child := f
   END ;
   RETURN f
END ChainError ;


(*
   ErrorFormat routines provide a printf capability for the error handle.
*)

PROCEDURE ErrorFormat0 (e: Error; a: ARRAY OF CHAR) ;
BEGIN
   WITH e^ DO
      IF s=NIL
      THEN
         s := Sprintf0(Mark(InitString(a)))
      ELSE
         s := ConCat(s, Mark(Sprintf0(Mark(InitString(a)))))
      END
   END
END ErrorFormat0 ;


PROCEDURE ErrorFormat1 (e: Error; a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   s1: String ;
BEGIN
   s1 := DoFormat1(a, w) ;
   WITH e^ DO
      IF s=NIL
      THEN
         s := s1
      ELSE
         s := ConCat(s, Mark(s1))
      END
   END
END ErrorFormat1 ;


PROCEDURE ErrorFormat2 (e: Error; a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) ;
VAR
   s1: String ;
BEGIN
   s1 := DoFormat2(a, w1, w2) ;
   WITH e^ DO
      IF s=NIL
      THEN
         s := s1
      ELSE
         s := ConCat(s, Mark(s1))
      END
   END
END ErrorFormat2 ;


PROCEDURE ErrorFormat3 (e: Error; a: ARRAY OF CHAR;
                        w1, w2, w3: ARRAY OF BYTE) ;
VAR
   s1: String ;
BEGIN
   s1 := DoFormat3(a, w1, w2, w3) ;
   WITH e^ DO
      IF s=NIL
      THEN
         s := s1
      ELSE
         s := ConCat(s, Mark(s1))
      END
   END
END ErrorFormat3 ;


PROCEDURE ErrorString (e: Error; str: String) ;
BEGIN
   WITH e^ DO
      s := str
   END
END ErrorString ;


(*
   Init - initializes the error list.
*)

PROCEDURE Init ;
BEGIN
   head := NIL ;
   InInternal := FALSE ;
   scopeStack := InitStackAddress () ;
   scopeArray := InitIndex (1) ;
   currentScope := NIL ;
   scopeIndex := 0
END Init ;


(*
   CheckIncludes - generates a sequence of error messages which determine the relevant
                   included file and line number.
                   For example:

                   gcc a.c
                   In file included from b.h:1,
                                    from a.c:1:
                   c.h:1: parse error before `and'

                   where a.c is: #include "b.h"
                         b.h is: #include "c.h"
                         c.h is: and this and that

                   we attempt to follow the error messages that gcc issues.
*)

PROCEDURE CheckIncludes (token: CARDINAL; depth: CARDINAL) ;
VAR
   included: String ;
   lineno  : CARDINAL ;
BEGIN
   included := FindFileNameFromToken(token, depth+1) ;
   IF included#NIL
   THEN
      lineno := TokenToLineNo(token, depth+1) ;
      IF depth=0
      THEN
         printf2('In file included from %s:%d', included, lineno)
      ELSE
         printf2('                 from %s:%d', included, lineno)
      END ;
      IF FindFileNameFromToken(token, depth+2)=NIL
      THEN
         printf0(':\n')
      ELSE
         printf0(',\n')
      END ;
      CheckIncludes(token, depth+1)
   END
END CheckIncludes ;


(*
   FlushAll - flushes all errors in list, e.
*)

PROCEDURE FlushAll (e: Error; FatalStatus: BOOLEAN) : BOOLEAN ;
VAR
   f      : Error ;
   written: BOOLEAN ;
BEGIN
   written := FALSE ;
   IF e#NIL
   THEN
      REPEAT
         WITH e^ DO
            IF (FatalStatus=fatal) AND (s#NIL)
            THEN
               currentScope := scope ;
               CheckIncludes (token, 0) ;
               EmitError (fatal, note, token, AnnounceScope (e, s)) ;
               IF (child#NIL) AND FlushAll (child, FatalStatus)
               THEN
               END ;
               s := NIL ;
               written := TRUE
            END
         END ;
         f := e ;
         e := e^.next ;
         IF NOT Debugging
         THEN
            WITH f^ DO
               s := KillString (s)
            END ;
            DISPOSE (f)
         END ;
      UNTIL e=NIL
   END ;
   RETURN written
END FlushAll ;


(*
   FlushErrors - switches the output channel to the error channel
                 and then writes out all errors.
*)

PROCEDURE FlushErrors ;
BEGIN
   IF DebugTrace
   THEN
      printf0('\nFlushing all errors\n') ;
      printf0('===================\n')
   END ;
   IF FlushAll (head, TRUE)
   THEN
      ExitOnHalt(1) ;
      HALT
   END
END FlushErrors ;


(*
   FlushWarnings - switches the output channel to the error channel
                   and then writes out all warnings.
                   If an error is present the compilation is terminated,
                   if warnings only were emitted then compilation will
                   continue.
*)

PROCEDURE FlushWarnings ;
BEGIN
   IF FlushAll (head, FALSE)
   THEN
   END
END FlushWarnings ;


(*
   ErrorStringsAt2 - given error strings, s1, and, s2, it places these
                     strings at token positions, tok1 and tok2, respectively.
                     Both strings are consumed.
*)

PROCEDURE ErrorStringsAt2 (s1, s2: String; tok1, tok2: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   IF s1=s2
   THEN
      s2 := Dup(s1)
   END ;
   e := NewError(tok1) ;
   ErrorString(e, s1) ;
   ErrorString(ChainError(tok2, e), s2)
END ErrorStringsAt2 ;


(*
   ErrorStringAt2 - given an error string, s, it places this
                    string at token positions, tok1 and tok2, respectively.
                    The string is consumed.
*)

PROCEDURE ErrorStringAt2 (s: String; tok1, tok2: CARDINAL) ;
BEGIN
   ErrorStringsAt2(s, s, tok1, tok2)
END ErrorStringAt2 ;


(*
   ErrorStringAt - given an error string, s, it places this
                   string at token position, tok.
                   The string is consumed.
*)

PROCEDURE ErrorStringAt (s: String; tok: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   e := NewError(tok) ;
   ErrorString(e, s) ;
END ErrorStringAt ;


(*
   WarnStringsAt2 - given warning strings, s1, and, s2, it places these
                    strings at token positions, tok1 and tok2, respectively.
                    Both strings are consumed.
*)

PROCEDURE WarnStringsAt2 (s1, s2: String; tok1, tok2: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   IF s1=s2
   THEN
      s2 := Dup(s1)
   END ;
   e := NewWarning(tok1) ;
   ErrorString(e, s1) ;
   ErrorString(ChainError(tok2, e), s2)
END WarnStringsAt2 ;


(*
   WarnStringAt2 - given an warning string, s, it places this
                   string at token positions, tok1 and tok2, respectively.
                   The string is consumed.
*)

PROCEDURE WarnStringAt2 (s: String; tok1, tok2: CARDINAL) ;
BEGIN
   WarnStringsAt2(s, s, tok1, tok2)
END WarnStringAt2 ;


(*
   WarnStringAt - given an error string, s, it places this
                  string at token position, tok.
                  The string is consumed.
*)

PROCEDURE WarnStringAt (s: String; tok: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   e := NewWarning(tok) ;
   ErrorString(e, s) ;
END WarnStringAt ;


(*
   ErrorAbort0 - aborts compiling, it flushes all warnings and errors before aborting.
*)

PROCEDURE ErrorAbort0 (a: ARRAY OF CHAR) ;
BEGIN
   FlushWarnings ;
   IF NOT StrEqual(a, '')
   THEN
      WriteFormat0(a)
   END ;
   IF NOT FlushAll (head, TRUE)
   THEN
      WriteFormat0('unidentified error') ;
      IF FlushAll (head, TRUE)
      THEN
      END
   END ;
   ExitOnHalt(1) ;
   HALT
END ErrorAbort0 ;


(*
   IsErrorScopeNul - returns TRUE if es is NIL or it has a NulName.
*)

PROCEDURE IsErrorScopeNul (es: ErrorScope) : BOOLEAN ;
BEGIN
   RETURN (es = NIL) OR (es^.scopeName = NulName)
END IsErrorScopeNul ;


(*
   GetAnnounceScope - return message with the error scope attached to message.
                      filename and message are treated as read only by this
                      procedure function.
*)

PROCEDURE GetAnnounceScope (filename, message: String) : String ;
VAR
   pre,
   fmt,
   desc,
   quoted: String ;
BEGIN
   IF filename = NIL
   THEN
      pre := InitString ('')
   ELSE
      pre := Sprintf1 (Mark (InitString ("%s: ")), filename)
   END ;

   IF NOT IsErrorScopeNul (currentScope)
   THEN
      quoted := InitString ('') ;
      quoted := quoteOpen (quoted) ;
      quoted := ConCat (quoted, Mark (InitStringCharStar (KeyToCharStar (currentScope^.scopeName)))) ;
      quoted := quoteClose (quoted)
   END ;

   IF currentScope = NIL
   THEN
      desc := InitString ("no scope active")
   ELSE
      CASE currentScope^.scopeKind OF

      definition    :   desc := InitString ("In definition module") |
      implementation:   desc := InitString ("In implementation module") |
      program       :   desc := InitString ("In program module") |
      module        :   desc := InitString ("In inner module") |
      procedure     :   desc := InitString ("In procedure") |
      noscope       :   desc := InitString ("Unknown scope")

      END
   END ;
   fmt := ConCat (pre, Mark (desc)) ;
   IF IsErrorScopeNul (currentScope)
   THEN
      fmt := ConCat (fmt, Sprintf0 (Mark (InitString (": "))))
   ELSE
      fmt := ConCat (fmt, Sprintf1 (Mark (InitString (" %s: ")), quoted))
   END ;
   RETURN ConCat (fmt, message)
END GetAnnounceScope ;


(*
   IsSameScope - return TRUE if a and b refer to the same scope.
*)

PROCEDURE IsSameScope (a, b: ErrorScope) : BOOLEAN ;
BEGIN
   IF a = b
   THEN
      RETURN TRUE
   ELSIF (a = NIL) OR (b = NIL)
   THEN
      RETURN FALSE
   ELSE
      (* this does not compare the symbol field.  *)
      RETURN (a^.scopeKind = b^.scopeKind) AND (a^.scopeName = b^.scopeName)
   END
END IsSameScope ;


(*
   AnnounceScope - return the error string s with a scope description prepended
                   assuming that scope has changed.
*)

PROCEDURE AnnounceScope (e: Error; message: String) : String ;
BEGIN
   IF NOT IsSameScope (lastScope, e^.scope)
   THEN
      lastScope := e^.scope ;
      IF IsErrorScopeNul (lastScope)
      THEN
         RETURN ConCat (InitString ("no scope active: "), message)
      ELSE
         Assert ((e^.scope # NIL) AND (e^.scope^.scopeKind # noscope)) ;
         (* filename := FindFileNameFromToken (e^.token, 0) ; *)
         message := GetAnnounceScope (NIL, message)
      END
   END ;
   RETURN message
END AnnounceScope ;


(*
   newErrorScope - create an ErrorScope of kindScope and return the object.
                   It is also added the a dynamic array.
*)

PROCEDURE newErrorScope (kind: KindScope) : ErrorScope ;
VAR
   es: ErrorScope ;
   c : CARDINAL ;
BEGIN
   IF IsPass0 ()
   THEN
      NEW (es) ;
      es^.scopeKind := kind ;
      es^.scopeName := NulName ;
      es^.symbol := NulSym ;
      PutIndice (scopeArray, HighIndice (scopeArray) + 1, es) ;
      IF DebugError
      THEN
         c := HighIndice (scopeArray) ;
         printf2 ("pass 0:  %d  %d\n", c, kind)
      END
   ELSE
      INC (scopeIndex) ;
      es := GetIndice (scopeArray, scopeIndex) ;
      IF DebugError
      THEN
         IF IsPass1 ()
         THEN
            printf3 ("pass 1:  %d  %d  %d\n", scopeIndex, es^.scopeKind, kind)
         ELSE
            printf3 ("pass 2:  %d  %d  %d\n", scopeIndex, es^.scopeKind, kind)
         END
      END ;
      Assert (es^.scopeKind = kind)
   END ;
   RETURN es
END newErrorScope ;


(*
   DefaultProgramModule - sets up an unnamed program scope before the Ident is seen.
*)

PROCEDURE DefaultProgramModule ;
BEGIN
   PushAddress (scopeStack, currentScope) ;
   currentScope := newErrorScope (program)
END DefaultProgramModule ;


(*
   DefaultImplementationModule - sets up an unnamed implementation
                                 scope before the Ident is seen.
*)

PROCEDURE DefaultImplementationModule ;
BEGIN
   PushAddress (scopeStack, currentScope) ;
   currentScope := newErrorScope (implementation)
END DefaultImplementationModule ;


(*
   DefaultDefinitionModule - sets up an unnamed definition
                             scope before the Ident is seen.
*)

PROCEDURE DefaultDefinitionModule ;
BEGIN
   PushAddress (scopeStack, currentScope) ;
   currentScope := newErrorScope (definition)
END DefaultDefinitionModule ;


(*
   DefaultInnerModule - sets up an unnamed inner
                        scope before the Ident is seen.
*)

PROCEDURE DefaultInnerModule ;
BEGIN
   PushAddress (scopeStack, currentScope) ;
   currentScope := newErrorScope (module)
END DefaultInnerModule ;


(*
   DefaultProcedure - sets up an unnamed procedure
                      scope before the Ident is seen.
*)

PROCEDURE DefaultProcedure ;
BEGIN
   PushAddress (scopeStack, currentScope) ;
   currentScope := newErrorScope (procedure)
END DefaultProcedure ;


(*
   EnterImplementationScope - signifies to the error routines that the front end
                              has started to compile implementation module scopeName.
*)

PROCEDURE EnterImplementationScope (scopename: Name) ;
BEGIN
   Assert (currentScope # NIL) ;
   Assert (currentScope^.scopeKind = implementation) ;
   IF currentScope^.scopeName = NulName
   THEN
      IF DebugError
      THEN
         printf1 ("seen implementation: %a\n", scopename)
      END ;
      currentScope^.scopeName := scopename
   END
END EnterImplementationScope ;


(*
   EnterProgramScope - signifies to the error routines that the front end
                       has started to compile program module scopeName.
*)

PROCEDURE EnterProgramScope (scopename: Name) ;
BEGIN
   Assert (currentScope # NIL) ;
   Assert (currentScope^.scopeKind = program) ;
   IF currentScope^.scopeName = NulName
   THEN
      IF DebugError
      THEN
         printf1 ("seen program: %a\n", scopename)
      END ;
      currentScope^.scopeName := scopename
   END
END EnterProgramScope ;


(*
   EnterModuleScope - signifies to the error routines that the front end
                      has started to compile an inner module scopeName.
*)

PROCEDURE EnterModuleScope (scopename: Name) ;
BEGIN
   Assert (currentScope # NIL) ;
   Assert (currentScope^.scopeKind = module) ;
   IF currentScope^.scopeName = NulName
   THEN
      IF DebugError
      THEN
         printf1 ("seen module: %a\n", scopename)
      END ;
      currentScope^.scopeName := scopename
   END
END EnterModuleScope ;


(*
   EnterDefinitionScope - signifies to the error routines that the front end
                          has started to compile definition module scopeName.
*)

PROCEDURE EnterDefinitionScope (scopename: Name) ;
BEGIN
   Assert (currentScope # NIL) ;
   Assert (currentScope^.scopeKind = definition) ;
   IF currentScope^.scopeName = NulName
   THEN
      IF DebugError
      THEN
         printf1 ("seen definition: %a\n", scopename)
      END ;
      currentScope^.scopeName := scopename
   END
END EnterDefinitionScope ;


(*
   EnterProcedureScope - signifies to the error routines that the front end
                         has started to compile definition module scopeName.
*)

PROCEDURE EnterProcedureScope (scopename: Name) ;
BEGIN
   Assert (currentScope # NIL) ;
   Assert (currentScope^.scopeKind = procedure) ;
   IF currentScope^.scopeName = NulName
   THEN
      IF DebugError
      THEN
         printf1 ("seen procedure: %a\n", scopename)
      END ;
      currentScope^.scopeName := scopename
   END
END EnterProcedureScope ;


(*
   LeaveErrorScope - leave the current scope and pop into the previous one.
*)

PROCEDURE LeaveErrorScope ;
BEGIN
   currentScope := PopAddress (scopeStack)
END LeaveErrorScope ;


(*
   EnterErrorScope - pushes the currentScope and sets currentScope to scope.
*)

PROCEDURE EnterErrorScope (scope: ErrorScope) ;
BEGIN
   PushAddress (scopeStack, currentScope) ;
   currentScope := scope
END EnterErrorScope ;


(*
   GetCurrentErrorScope - returns currentScope.
*)

PROCEDURE GetCurrentErrorScope () : ErrorScope ;
BEGIN
   RETURN currentScope
END GetCurrentErrorScope ;


(*
   DepthScope - returns the depth of the scope stack.
*)

PROCEDURE DepthScope () : CARDINAL ;
BEGIN
   RETURN NoOfItemsInStackAddress (scopeStack)
END DepthScope ;


(*
   ResetErrorScope - should be called at the start of each pass to
                     reset the error scope index.
*)

PROCEDURE ResetErrorScope ;
BEGIN
   scopeIndex := 0
END ResetErrorScope ;


BEGIN
   Init
END M2Error.
