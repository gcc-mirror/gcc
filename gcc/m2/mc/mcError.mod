(* mcError.mod provides an interface between the string handling modules.

Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE mcError ;

FROM ASCII IMPORT nul, nl ;
FROM DynamicStrings IMPORT String, InitString, InitStringCharStar, ConCat, ConCatChar, Mark, string, KillString, Dup ;
FROM FIO IMPORT StdOut, WriteNBytes, Close, FlushBuffer ;
FROM StrLib IMPORT StrLen, StrEqual ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3 ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2RTS IMPORT ExitOnHalt ;
FROM SYSTEM IMPORT ADDRESS ;
IMPORT StdIO ;

FROM nameKey IMPORT Name, keyToCharStar ;
FROM mcLexBuf IMPORT findFileNameFromToken, tokenToLineNo, tokenToColumnNo, getTokenNo ;
FROM mcPrintf IMPORT printf0, printf1, printf2 ;


CONST
   Debugging  =  TRUE ;
   DebugTrace = FALSE ;
   Xcode      =  TRUE ;

TYPE
   error = POINTER TO RECORD
                         parent,
                         child,
                         next  : error ;
                         fatal : BOOLEAN ;
                         s     : String ;
                         token : CARDINAL ;  (* index of token causing the error *)
                      END ;

VAR
   head      : error ;
   inInternal: BOOLEAN ;


(*
   cast - casts a := b
*)

PROCEDURE cast (VAR a: ARRAY OF BYTE; b: ARRAY OF BYTE) ;
VAR
   i: CARDINAL ;
BEGIN
   IF HIGH(a)=HIGH(b)
   THEN
      FOR i := 0 TO HIGH(a) DO
         a[i] := b[i]
      END
   END
END cast ;


(*
   translateNameToString - takes a format specification string, a, and
                           if they consist of of %a then this is translated
                           into a String and %a is replaced by %s.
*)

PROCEDURE translateNameToCharStar (VAR a: ARRAY OF CHAR;
                                   n: CARDINAL) : BOOLEAN ;
VAR
   argno,
   i, h : CARDINAL ;
BEGIN
   argno := 1 ;
   i := 0 ;
   h := StrLen (a) ;
   WHILE i<h DO
      IF (a[i]='%') AND (i+1<h)
      THEN
         IF (a[i+1]='a') AND (argno=n)
         THEN
            a[i+1] := 's' ;
            RETURN TRUE
         END ;
         INC (argno) ;
         IF argno>n
         THEN
            (* all done *)
            RETURN FALSE
         END
      END ;
      INC (i)
   END ;
   RETURN FALSE
END translateNameToCharStar ;


(*
   outString - writes the contents of String to stdout.
               The string, s, is destroyed.
*)

PROCEDURE outString (file: String; line, col: CARDINAL; s: String) ;
VAR
   leader : String ;
   p, q   : POINTER TO CHAR ;
   space,
   newline: BOOLEAN ;
BEGIN
   INC (col) ;
   IF Xcode
   THEN
      leader := Sprintf2(Mark(InitString('%s:%d:')), file, line)
   ELSE
      leader := Sprintf3(Mark(InitString('%s:%d:%d:')), file, line, col)
   END ;
   p := string(s) ;
   newline := TRUE ;
   space := FALSE ;
   WHILE (p#NIL) AND (p^#nul) DO
      IF newline
      THEN
         q := string (leader) ;
         WHILE (q#NIL) AND (q^#nul) DO
            StdIO.Write (q^) ;
            INC (q)
         END
      END ;
      newline := (p^=nl) ;
      space := (p^=' ') ;
      IF newline AND Xcode
      THEN
         printf1 ('(pos: %d)', col)
      END ;
      StdIO.Write (p^) ;
      INC (p)
   END ;
   IF NOT newline
   THEN
      IF Xcode
      THEN
         IF NOT space
         THEN
            StdIO.Write (' ')
         END ;
         printf1 ('(pos: %d)', col)
      END ;
      StdIO.Write (nl)
   END ;
   FlushBuffer (StdOut) ;
   IF NOT Debugging
   THEN
      s      := KillString (s) ;
      leader := KillString (leader)
   END
END outString ;


(*
   internalError - displays an internal error message together with the compiler source
                   file and line number.
                   This function is not buffered and is used when the compiler is about
                   to give up.
*)

PROCEDURE internalError (a: ARRAY OF CHAR; file: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   ExitOnHalt (1) ;
   IF NOT inInternal
   THEN
      inInternal := TRUE ;
      flushErrors ;
      outString (findFileNameFromToken (getTokenNo (), 0),
                 tokenToLineNo (getTokenNo (), 0),
                 tokenToColumnNo (getTokenNo (), 0),
                 Mark(InitString ('*** fatal error ***')))
   END ;
   outString (Mark (InitString (file)), line, 0,
              ConCat (Mark (InitString('*** internal error *** ')), Mark (InitString (a)))) ;
   HALT
END internalError ;


(* ***************************************************************************
   The following routines are used for normal syntax and semantic error reporting
   *************************************************************************** *)


(*
   writeFormat0 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE writeFormat0 (a: ARRAY OF CHAR) ;
VAR
   e: error ;
BEGIN
   e := newError (getTokenNo ()) ;
   WITH e^ DO
      s := Sprintf0 (Mark (InitString(a)))
   END
END writeFormat0 ;


(*
   WarnFormat0 - displays the source module and line together
                 with the encapsulated format string.
                 Used for simple warning messages tied to the current token.
*)

PROCEDURE warnFormat0 (a: ARRAY OF CHAR) ;
VAR
   e: error ;
BEGIN
   e := newWarning (getTokenNo()) ;
   WITH e^ DO
      s := Sprintf0 (Mark (InitString (a)))
   END
END warnFormat0 ;


(*
   DoFormat1 -
*)

PROCEDURE doFormat1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) : String ;
VAR
   s: String ;
   n: Name ;
BEGIN
   IF translateNameToCharStar(a, 1)
   THEN
      cast(n, w) ;
      s := Mark (InitStringCharStar (keyToCharStar (n))) ;
      s := Sprintf1 (Mark (InitString (a)), s)
   ELSE
      s := Sprintf1 (Mark (InitString (a)), w)
   END ;
   RETURN s
END doFormat1 ;


(*
   writeFormat1 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE writeFormat1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   e: error ;
BEGIN
   e := newError (getTokenNo ()) ;
   e^.s := doFormat1 (a, w)
END writeFormat1 ;


(*
   warnFormat1 - displays the source module and line together
                 with the encapsulated format string.
                 Used for simple warning messages tied to the current token.
*)

PROCEDURE warnFormat1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   e: error ;
BEGIN
   e := newWarning (getTokenNo ()) ;
   e^.s := doFormat1 (a, w)
END warnFormat1 ;


(*
   doFormat2 -
*)

PROCEDURE doFormat2 (a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) : String ;
VAR
   n     : Name ;
   s,
   s1, s2: String ;
   b     : BITSET ;
BEGIN
   b := {} ;
   IF translateNameToCharStar (a, 1)
   THEN
      cast (n, w1) ;
      s1 := Mark (InitStringCharStar (keyToCharStar (n))) ;
      INCL (b, 1)
   END ;
   IF translateNameToCharStar(a, 2)
   THEN
      cast (n, w2) ;
      s2 := Mark (InitStringCharStar (keyToCharStar(n))) ;
      INCL (b, 2)
   END ;
   CASE b OF

   {}   :  s := Sprintf2 (Mark (InitString (a)), w1, w2) |
   {1}  :  s := Sprintf2 (Mark (InitString (a)), s1, w2) |
   {2}  :  s := Sprintf2 (Mark (InitString (a)), w1, s2) |
   {1,2}:  s := Sprintf2 (Mark (InitString (a)), s1, s2)

   ELSE
      HALT
   END ;
   RETURN s
END doFormat2 ;


(*
   writeFormat2 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE writeFormat2 (a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) ;
VAR
   e: error ;
BEGIN
   e := newError (getTokenNo()) ;
   e^.s := doFormat2 (a, w1, w2)
END writeFormat2 ;


PROCEDURE doFormat3 (a: ARRAY OF CHAR; w1, w2, w3: ARRAY OF BYTE) : String ;
VAR
   n            : Name ;
   s, s1, s2, s3: String ;
   b            : BITSET ;
BEGIN
   b := {} ;
   IF translateNameToCharStar (a, 1)
   THEN
      cast (n, w1) ;
      s1 := Mark (InitStringCharStar (keyToCharStar (n))) ;
      INCL(b, 1)
   END ;
   IF translateNameToCharStar (a, 2)
   THEN
      cast (n, w2) ;
      s2 := Mark (InitStringCharStar (keyToCharStar (n))) ;
      INCL (b, 2)
   END ;
   IF translateNameToCharStar (a, 3)
   THEN
      cast(n, w3) ;
      s3 := Mark (InitStringCharStar (keyToCharStar (n))) ;
      INCL (b, 3)
   END ;
   CASE b OF

   {}     :  s := Sprintf3 (Mark (InitString (a)), w1, w2, w3) |
   {1}    :  s := Sprintf3 (Mark (InitString (a)), s1, w2, w3) |
   {2}    :  s := Sprintf3 (Mark (InitString (a)), w1, s2, w3) |
   {1,2}  :  s := Sprintf3 (Mark (InitString (a)), s1, s2, w3) |
   {3}    :  s := Sprintf3 (Mark (InitString (a)), w1, w2, s3) |
   {1,3}  :  s := Sprintf3 (Mark (InitString (a)), s1, w2, s3) |
   {2,3}  :  s := Sprintf3 (Mark (InitString (a)), w1, s2, s3) |
   {1,2,3}:  s := Sprintf3 (Mark (InitString (a)), s1, s2, s3)

   ELSE
      HALT
   END ;
   RETURN s
END doFormat3 ;


(*
   writeFormat3 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*)

PROCEDURE writeFormat3 (a: ARRAY OF CHAR; w1, w2, w3: ARRAY OF BYTE) ;
VAR
   e: error ;
BEGIN
   e := newError (getTokenNo ()) ;
   e^.s := doFormat3 (a, w1, w2, w3)
END writeFormat3 ;


(*
   newError - creates and returns a new error handle.
*)

PROCEDURE newError (atTokenNo: CARDINAL) : error ;
VAR
   e, f: error ;
BEGIN
   NEW (e) ;
   WITH e^ DO
      s      := NIL ;
      token  := atTokenNo ;
      next   := NIL ;
      parent := NIL ;
      child  := NIL ;
      fatal  := TRUE
   END ;
   IF (head=NIL) OR (head^.token>atTokenNo)
   THEN
      e^.next := head ;
      head    := e
   ELSE
      f := head ;
      WHILE (f^.next#NIL) AND (f^.next^.token<atTokenNo) DO
         f := f^.next
      END ;
      e^.next := f^.next ;
      f^.next := e
   END ;
   RETURN e
END newError ;


(*
   newWarning - creates and returns a new error handle suitable for a warning.
                A warning will not stop compilation.
*)

PROCEDURE newWarning (atTokenNo: CARDINAL) : error ;
VAR
   e: error ;
BEGIN
   e := newError (atTokenNo) ;
   e^.fatal := FALSE ;
   RETURN e
END newWarning ;


(*
   chainError - creates and returns a new error handle, this new error
                is associated with, e, and is chained onto the end of, e.
                If, e, is NIL then the result to NewError is returned.
*)

PROCEDURE chainError (atTokenNo: CARDINAL; e: error) : error ;
VAR
   f: error ;
BEGIN
   IF e=NIL
   THEN
      RETURN newError (atTokenNo)
   ELSE
      NEW (f) ;
      WITH f^ DO
         s      := NIL ;
         token  := atTokenNo ;
         next   := e^.child ;
         parent := e ;
         child  := NIL ;
         fatal  := e^.fatal
      END ;
      e^.child := f
   END ;
   RETURN f
END chainError ;


(*
   errorFormat routines provide a printf capability for the error handle.
*)

PROCEDURE errorFormat0 (e: error; a: ARRAY OF CHAR) ;
BEGIN
   WITH e^ DO
      IF s=NIL
      THEN
         s := Sprintf0 (Mark (InitString (a)))
      ELSE
         s := ConCat(s, Mark(Sprintf0 (Mark (InitString (a)))))
      END
   END
END errorFormat0 ;


PROCEDURE errorFormat1 (e: error; a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   s1: String ;
BEGIN
   s1 := doFormat1 (a, w) ;
   WITH e^ DO
      IF s=NIL
      THEN
         s := s1
      ELSE
         s := ConCat (s, Mark (s1))
      END
   END
END errorFormat1 ;


PROCEDURE errorFormat2 (e: error; a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) ;
VAR
   s1: String ;
BEGIN
   s1 := doFormat2 (a, w1, w2) ;
   WITH e^ DO
      IF s=NIL
      THEN
         s := s1
      ELSE
         s := ConCat (s, Mark (s1))
      END
   END
END errorFormat2 ;


PROCEDURE errorFormat3 (e: error; a: ARRAY OF CHAR;
                        w1, w2, w3: ARRAY OF BYTE) ;
VAR
   s1: String ;
BEGIN
   s1 := doFormat3 (a, w1, w2, w3) ;
   WITH e^ DO
      IF s=NIL
      THEN
         s := s1
      ELSE
         s := ConCat (s, Mark (s1))
      END
   END
END errorFormat3 ;


PROCEDURE errorString (e: error; str: String) ;
BEGIN
   WITH e^ DO
      s := str
   END
END errorString ;


(*
   init - initializes the error list.
*)

PROCEDURE init ;
BEGIN
   head := NIL ;
   inInternal := FALSE
END init ;


(*
   checkIncludes - generates a sequence of error messages which determine the relevant
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

PROCEDURE checkIncludes (token: CARDINAL; depth: CARDINAL) ;
VAR
   included: String ;
   lineno  : CARDINAL ;
BEGIN
   included := findFileNameFromToken (token, depth+1) ;
   IF included#NIL
   THEN
      lineno := tokenToLineNo (token, depth+1) ;
      IF depth=0
      THEN
         printf2('In file included from %s:%d', included, lineno)
      ELSE
         printf2('                 from %s:%d', included, lineno)
      END ;
      IF findFileNameFromToken (token, depth+2)=NIL
      THEN
         printf0(':\n')
      ELSE
         printf0(',\n')
      END ;
      checkIncludes (token, depth+1)
   END
END checkIncludes ;


(*
   flushAll - flushes all errors in list, e.
*)

PROCEDURE flushAll (e: error; FatalStatus: BOOLEAN) : BOOLEAN ;
VAR
   f      : error ;
   written: BOOLEAN ;
BEGIN
   written := FALSE ;
   IF e#NIL
   THEN
      REPEAT
         WITH e^ DO
            IF (FatalStatus=fatal) AND (s#NIL)
            THEN
               checkIncludes (token, 0) ;
               IF fatal
               THEN
                  s := ConCat (InitString (' error: '), Mark (s))
               ELSE
                  s := ConCat (InitString(' warning: '), Mark (s))
               END ;
               outString (findFileNameFromToken (token, 0),
                          tokenToLineNo (token, 0), tokenToColumnNo (token, 0), s) ;
               IF (child#NIL) AND flushAll (child, FatalStatus)
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
               s := KillString(s)
            END ;
            DISPOSE (f)
         END ;
      UNTIL e=NIL
   END ;
   RETURN written
END flushAll ;


(*
   flushErrors - switches the output channel to the error channel
                 and then writes out all errors.
*)

PROCEDURE flushErrors ;
BEGIN
   IF DebugTrace
   THEN
      printf0 ('\nFlushing all errors\n') ;
      printf0 ('===================\n')
   END ;
   IF flushAll (head, TRUE)
   THEN
      ExitOnHalt (1) ;
      HALT
   END
END flushErrors ;


(*
   flushWarnings - switches the output channel to the error channel
                   and then writes out all warnings.
                   If an error is present the compilation is terminated,
                   if warnings only were emitted then compilation will
                   continue.
*)

PROCEDURE flushWarnings ;
BEGIN
   IF flushAll (head, FALSE)
   THEN
   END
END flushWarnings ;


(*
   errorStringsAt2 - given error strings, s1, and, s2, it places these
                     strings at token positions, tok1 and tok2, respectively.
                     Both strings are consumed.
*)

PROCEDURE errorStringsAt2 (s1, s2: String; tok1, tok2: CARDINAL) ;
VAR
   e: error ;
BEGIN
   IF s1=s2
   THEN
      s2 := Dup (s1)
   END ;
   e := newError (tok1) ;
   errorString (e, s1) ;
   errorString (chainError (tok2, e), s2)
END errorStringsAt2 ;


(*
   errorStringAt2 - given an error string, s, it places this
                    string at token positions, tok1 and tok2, respectively.
                    The string is consumed.
*)

PROCEDURE errorStringAt2 (s: String; tok1, tok2: CARDINAL) ;
BEGIN
   errorStringsAt2 (s, s, tok1, tok2)
END errorStringAt2 ;


(*
   errorStringAt - given an error string, s, it places this
                   string at token position, tok.
                   The string is consumed.
*)

PROCEDURE errorStringAt (s: String; tok: CARDINAL) ;
VAR
   e: error ;
BEGIN
   e := newError (tok) ;
   errorString (e, s)
END errorStringAt ;


(*
   warnStringsAt2 - given warning strings, s1, and, s2, it places these
                    strings at token positions, tok1 and tok2, respectively.
                    Both strings are consumed.
*)

PROCEDURE warnStringsAt2 (s1, s2: String; tok1, tok2: CARDINAL) ;
VAR
   e: error ;
BEGIN
   IF s1=s2
   THEN
      s2 := Dup (s1)
   END ;
   e := newWarning (tok1) ;
   errorString (e, s1) ;
   errorString (chainError (tok2, e), s2)
END warnStringsAt2 ;


(*
   warnStringAt2 - given an warning string, s, it places this
                   string at token positions, tok1 and tok2, respectively.
                   The string is consumed.
*)

PROCEDURE warnStringAt2 (s: String; tok1, tok2: CARDINAL) ;
BEGIN
   warnStringsAt2 (s, s, tok1, tok2)
END warnStringAt2 ;


(*
   warnStringAt - given an error string, s, it places this
                  string at token position, tok.
                  The string is consumed.
*)

PROCEDURE warnStringAt (s: String; tok: CARDINAL) ;
VAR
   e: error ;
BEGIN
   e := newWarning (tok) ;
   errorString (e, s)
END warnStringAt ;


(*
   errorAbort0 - aborts compiling, it flushes all warnings and errors before aborting.
*)

PROCEDURE errorAbort0 (a: ARRAY OF CHAR) ;
BEGIN
   flushWarnings ;
   IF NOT StrEqual (a, '')
   THEN
      writeFormat0(a)
   END ;
   IF NOT flushAll(head, TRUE)
   THEN
      writeFormat0 ('unidentified error') ;
      IF flushAll (head, TRUE)
      THEN
      END
   END ;
   ExitOnHalt (1) ;
   HALT
END errorAbort0 ;


BEGIN
   init
END mcError.
