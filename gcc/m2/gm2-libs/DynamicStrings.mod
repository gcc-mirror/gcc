(* DynamicStrings.mod provides a dynamic string type and procedures.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE DynamicStrings ;

FROM libc IMPORT strlen, strncpy, write, exit ;
FROM StrLib IMPORT StrLen ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Assertion IMPORT Assert ;
FROM SYSTEM IMPORT ADR ;
FROM ASCII IMPORT nul, tab, lf ;
FROM M2RTS IMPORT Halt ;

CONST
   MaxBuf   = 127 ;
   PoisonOn = FALSE ;    (* to enable debugging of this module, turn on PoisonOn and DebugOn.  *)
   DebugOn  = FALSE ;
   CheckOn  = FALSE ;    (* to enable debugging of users of this module turn on                *)
   TraceOn  = FALSE ;    (* CheckOn and TraceOn.  Enabling both of these is very expensive.    *)

TYPE
   Contents = RECORD
                 buf : ARRAY [0..MaxBuf-1] OF CHAR ;
                 len : CARDINAL ;
                 next: String ;
              END ;

   Descriptor = POINTER TO descriptor ;

   String = POINTER TO stringRecord ;

   DebugInfo = RECORD
                  next: String ;   (* a mechanism for tracking used/lost strings *)
                  file: ADDRESS ;
                  line: CARDINAL ;
                  proc: ADDRESS ;
               END ;

   stringRecord = RECORD
                     contents: Contents ;
                     head    : Descriptor ;
                     debug   : DebugInfo ;
                  END ;

   desState = (inuse, marked, onlist, poisoned) ;

   descriptor = RECORD
                   charStarUsed : BOOLEAN ;     (* can we garbage collect this? *)
                   charStar     : ADDRESS ;
                   charStarSize : CARDINAL ;
                   charStarValid: BOOLEAN ;
                   state        : desState ;
                   garbage      : String ;      (* temporary strings to be destroyed
                                                   once this string is killed *)
                END ;

   frame    = POINTER TO frameRec ;
   frameRec =            RECORD
                            alloc, dealloc: String ;
                            next          : frame ;
                         END ;

VAR
   Initialized: BOOLEAN ;
   frameHead  : frame ;
   captured   : String ;  (* debugging aid.  *)


(* writeStringDesc write out debugging information about string, s.  *)

PROCEDURE writeStringDesc (s: String) ;
BEGIN
   writeCstring (s^.debug.file) ; writeString (':') ;
   writeCard (s^.debug.line) ; writeString (':') ;
   writeCstring (s^.debug.proc) ; writeString (' ') ;
   writeAddress (s) ;
   writeString (' ') ;
   CASE s^.head^.state OF

   inuse   :  writeString ("still in use (") ; writeCard (s^.contents.len) ; writeString (") characters") |
   marked  :  writeString ("marked") |
   onlist  :  writeString ("on a (lost) garbage list") |
   poisoned:  writeString ("poisoned")

   ELSE
      writeString ("unknown state")
   END
END writeStringDesc ;


(*
   writeNspace -
*)

PROCEDURE writeNspace (n: CARDINAL) ;
BEGIN
   WHILE n > 0 DO
      writeString (' ') ;
      DEC (n)
   END
END writeNspace ;


(*
   DumpStringInfo -
*)

PROCEDURE DumpStringInfo (s: String; i: CARDINAL) ;
VAR
   t: String ;
BEGIN
   IF s # NIL
   THEN
      writeNspace (i) ; writeStringDesc (s) ; writeLn ;
      IF s^.head^.garbage # NIL
      THEN
         writeNspace (i) ; writeString ('garbage list:') ; writeLn ;
         REPEAT
            s := s^.head^.garbage ;
            DumpStringInfo (s, i+1) ; writeLn
         UNTIL s = NIL
      END
   END
END DumpStringInfo ;


PROCEDURE stop ;
END stop ;


(*
   PopAllocationExemption - test to see that all strings are deallocated, except
                            string, e, since the last push.
                            Then it pops to the previous allocation/deallocation
                            lists.

                            If halt is true then the application terminates
                            with an exit code of 1.
*)

PROCEDURE PopAllocationExemption (halt: BOOLEAN; e: String) : String ;
VAR
   s: String ;
   f: frame ;
   b: BOOLEAN ;
BEGIN
   Init ;
   IF CheckOn
   THEN
      IF frameHead = NIL
      THEN
         stop ;
         Halt (__FILE__, __LINE__, __FUNCTION__,
               "mismatched number of PopAllocation's compared to PushAllocation's")
         (* writeString ("mismatched number of PopAllocation's compared to PushAllocation's") *)
      ELSE
         IF frameHead^.alloc # NIL
         THEN
            b := FALSE ;
            s := frameHead^.alloc ;
            WHILE s # NIL DO
               IF NOT ((e = s) OR IsOnGarbage (e, s) OR IsOnGarbage (s, e))
               THEN
                  IF NOT b
                  THEN
                     writeString ("the following strings have been lost") ; writeLn ;
                     b := TRUE
                  END ;
                  DumpStringInfo (s, 0)
               END ;
               s := s^.debug.next
            END ;
            IF b AND halt
            THEN
               exit (1)
            END
         END ;
         frameHead := frameHead^.next
      END
   END ;
   RETURN e
END PopAllocationExemption ;


(*
   PopAllocation - test to see that all strings are deallocated since
                   the last push.  Then it pops to the previous
                   allocation/deallocation lists.

                   If halt is true then the application terminates
                   with an exit code of 1.
*)

PROCEDURE PopAllocation (halt: BOOLEAN) ;
BEGIN
   IF CheckOn
   THEN
      IF PopAllocationExemption (halt, NIL) = NIL
      THEN
      END
   END
END PopAllocation ;


(*
   PushAllocation - pushes the current allocation/deallocation lists.
*)

PROCEDURE PushAllocation ;
VAR
   f: frame ;
BEGIN
   IF CheckOn
   THEN
      Init ;
      NEW (f) ;
      WITH f^ DO
         next := frameHead ;
         alloc := NIL ;
         dealloc := NIL
      END ;
      frameHead := f
   END
END PushAllocation ;


(*
   doDSdbEnter -
*)

PROCEDURE doDSdbEnter ;
BEGIN
   IF CheckOn
   THEN
      PushAllocation
   END
END doDSdbEnter ;


(*
   doDSdbExit -
*)

PROCEDURE doDSdbExit (s: String) ;
BEGIN
   IF CheckOn
   THEN
      s := PopAllocationExemption (TRUE, s)
   END
END doDSdbExit ;


(*
   DSdbEnter -
*)

PROCEDURE DSdbEnter ;
BEGIN
END DSdbEnter ;


(*
   DSdbExit -
*)

PROCEDURE DSdbExit (s: String) ;
BEGIN
END DSdbExit ;


(*
 *   #undef GM2_DEBUG_DYNAMICSTINGS
 *   #if defined(GM2_DEBUG_DYNAMICSTINGS)
 *   #  define DSdbEnter doDSdbEnter
 *   #  define DSdbExit  doDSdbExit
 *   #  define CheckOn   TRUE
 *   #  define TraceOn   TRUE
 *   #endif
 *)


PROCEDURE Capture (s: String) : CARDINAL ;
BEGIN
   captured := s ;
   RETURN 1
END Capture ;


(*
   Min -
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a < b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END Min ;


(*
   Max -
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a > b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END Max ;


(*
   writeString - writes a string to stdout.
*)

PROCEDURE writeString (a: ARRAY OF CHAR) ;
VAR
   i: INTEGER ;
BEGIN
   i := write (1, ADR (a), StrLen (a))
END writeString ;


(*
   writeCstring - writes a C string to stdout.
*)

PROCEDURE writeCstring (a: ADDRESS) ;
VAR
   i: INTEGER ;
BEGIN
   IF a = NIL
   THEN
      writeString ('(null)')
   ELSE
      i := write (1, a, strlen (a))
   END
END writeCstring ;


(*
   writeCard -
*)

PROCEDURE writeCard (c: CARDINAL) ;
VAR
   ch: CHAR ;
   i : INTEGER ;
BEGIN
   IF c > 9
   THEN
      writeCard (c DIV 10) ;
      writeCard (c MOD 10)
   ELSE
      ch := CHR (ORD ('0') + c) ;
      i := write (1, ADR (ch), 1)
   END
END writeCard ;


(*
   writeLongcard -
*)

PROCEDURE writeLongcard (l: LONGCARD) ;
VAR
   ch: CHAR ;
   i : INTEGER ;
BEGIN
   IF l > 16
   THEN
      writeLongcard (l DIV 16) ;
      writeLongcard (l MOD 16)
   ELSIF l < 10
   THEN
      ch := CHR (ORD ('0') + VAL (CARDINAL, l)) ;
      i := write(1, ADR(ch), 1)
   ELSIF l<16
   THEN
      ch := CHR (ORD ('a') + VAL(CARDINAL, l) - 10) ;
      i := write (1, ADR (ch), 1)
   END
END writeLongcard ;


(*
   writeAddress -
*)

PROCEDURE writeAddress (a: ADDRESS) ;
BEGIN
   writeLongcard (VAL (LONGCARD, a))
END writeAddress ;


(*
   writeLn - writes a newline.
*)

PROCEDURE writeLn ;
VAR
   ch: CHAR ;
   i : INTEGER ;
BEGIN
   ch := lf ;
   i := write (1, ADR (ch), 1)
END writeLn ;


(*
   AssignDebug - assigns, file, and, line, information to string, s.
*)

PROCEDURE AssignDebug (s: String; file: ARRAY OF CHAR; line: CARDINAL; proc: ARRAY OF CHAR) : String ;
VAR
   f, p: ADDRESS ;
BEGIN
   f := ADR (file) ;
   p := ADR (proc) ;
   WITH s^ DO
      ALLOCATE (debug.file, StrLen (file) + 1) ;
      IF strncpy(debug.file, f, StrLen(file)+1)=NIL
      THEN
      END ;
      debug.line := line ;
      ALLOCATE (debug.proc, StrLen (proc) + 1) ;
      IF strncpy (debug.proc, p, StrLen (proc) + 1) = NIL
      THEN
      END
   END ;
   RETURN( s )
END AssignDebug ;


(*
   CopyOut - copies string, s, to a.
*)

PROCEDURE CopyOut (VAR a: ARRAY OF CHAR; s: String) ;
VAR
   i, l: CARDINAL ;
BEGIN
   l := Min (HIGH (a) + 1, Length (s)) ;
   i := 0 ;
   WHILE i < l DO
      a[i] := char (s, i) ;
      INC (i)
   END ;
   IF i <= HIGH (a)
   THEN
      a[i] := nul
   END
END CopyOut ;


(*
   IsOn - returns TRUE if, s, is on one of the debug lists.
*)

PROCEDURE IsOn (list, s: String) : BOOLEAN ;
BEGIN
   WHILE (list # s) AND (list # NIL) DO
      list := list^.debug.next
   END ;
   RETURN list = s
END IsOn ;


(*
   AddTo - adds string, s, to, list.
*)

PROCEDURE AddTo (VAR list: String; s: String) ;
BEGIN
   IF list = NIL
   THEN
      list := s ;
      s^.debug.next := NIL
   ELSE
      s^.debug.next := list ;
      list := s
   END
END AddTo ;


(*
   SubFrom - removes string, s, from, list.
*)

PROCEDURE SubFrom (VAR list: String; s: String) ;
VAR
   p: String ;
BEGIN
   IF list = s
   THEN
      list := s^.debug.next ;
   ELSE
      p := list ;
      WHILE (p^.debug.next # NIL) AND (p^.debug.next # s) DO
         p := p^.debug.next
      END ;
      IF p^.debug.next = s
      THEN
         p^.debug.next := s^.debug.next
      ELSE
         (* not found, quit *)
         RETURN
      END
   END ;
   s^.debug.next := NIL
END SubFrom ;


(*
   AddAllocated - adds string, s, to the head of the allocated list.
*)

PROCEDURE AddAllocated (s: String) ;
BEGIN
   Init ;
   AddTo (frameHead^.alloc, s)
END AddAllocated ;


(*
   AddDeallocated - adds string, s, to the head of the deallocated list.
*)

PROCEDURE AddDeallocated (s: String) ;
BEGIN
   Init ;
   AddTo (frameHead^.dealloc, s)
END AddDeallocated ;


(*
   IsOnAllocated - returns TRUE if the string, s, has ever been allocated.
*)

PROCEDURE IsOnAllocated (s: String) : BOOLEAN ;
VAR
   f: frame ;
BEGIN
   Init ;
   f := frameHead ;
   REPEAT
      IF IsOn (f^.alloc, s)
      THEN
         RETURN TRUE
      ELSE
         f := f^.next
      END
   UNTIL f = NIL ;
   RETURN FALSE
END IsOnAllocated ;


(*
   IsOnDeallocated - returns TRUE if the string, s, has ever been deallocated.
*)

PROCEDURE IsOnDeallocated (s: String) : BOOLEAN ;
VAR
   f: frame ;
BEGIN
   Init ;
   f := frameHead ;
   REPEAT
      IF IsOn (f^.dealloc, s)
      THEN
         RETURN TRUE
      ELSE
         f := f^.next
      END
   UNTIL f = NIL ;
   RETURN FALSE
END IsOnDeallocated ;


(*
   SubAllocated - removes string, s, from the list of allocated strings.
*)

PROCEDURE SubAllocated (s: String) ;
VAR
   f: frame ;
BEGIN
   Init ;
   f := frameHead ;
   REPEAT
      IF IsOn (f^.alloc, s)
      THEN
         SubFrom (f^.alloc, s) ;
         RETURN
      ELSE
         f := f^.next
      END
   UNTIL f = NIL
END SubAllocated ;


(*
   SubDeallocated - removes string, s, from the list of deallocated strings.
*)

PROCEDURE SubDeallocated (s: String) ;
VAR
   f: frame ;
BEGIN
   Init ;
   f := frameHead ;
   REPEAT
      IF IsOn (f^.dealloc, s)
      THEN
         SubFrom (f^.dealloc, s) ;
         RETURN
      ELSE
         f := f^.next
      END
   UNTIL f = NIL
END SubDeallocated ;


(*
   SubDebugInfo - removes string, s, from the list of allocated strings.
*)

PROCEDURE SubDebugInfo (s: String) ;
BEGIN
   IF IsOnDeallocated (s)
   THEN
      Assert (NOT DebugOn) ;
      (* string has already been deallocated *)
      RETURN
   END ;
   IF IsOnAllocated (s)
   THEN
      SubAllocated (s) ;
      AddDeallocated (s)
   ELSE
      Assert (NOT DebugOn)
      (* string has not been allocated *)
   END
END SubDebugInfo ;


(*
   AddDebugInfo - adds string, s, to the list of allocated strings.
*)

PROCEDURE AddDebugInfo (s: String) ;
BEGIN
   WITH s^ DO
      debug.next := NIL ;
      debug.file := NIL ;
      debug.line := 0 ;
      debug.proc := NIL ;
   END ;
   IF CheckOn
   THEN
      AddAllocated (s)
   END
END AddDebugInfo ;


(*
   ConcatContents - add the contents of string, a, where, h, is the
                    total length of, a. The offset is in, o.
*)

PROCEDURE ConcatContents (VAR c: Contents; a: ARRAY OF CHAR; h, o: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := c.len ;
   WHILE (o < h) AND (i < MaxBuf) DO
      c.buf[i] := a[o] ;
      INC (o) ;
      INC (i)
   END ;
   IF o < h
   THEN
      c.len := MaxBuf ;
      NEW (c.next) ;
      WITH c.next^ DO
         head := NIL ;
         contents.len := 0 ;
         contents.next := NIL ;
         ConcatContents (contents, a, h, o)
      END ;
      AddDebugInfo (c.next) ;
      c.next := AssignDebug (c.next, __FILE__, __LINE__, __FUNCTION__)
   ELSE
      c.len := i
   END
END ConcatContents ;


(*
   InitString - creates and returns a String type object.
                Initial contents are, a.
*)

PROCEDURE InitString (a: ARRAY OF CHAR) : String ;
VAR
   s: String ;
BEGIN
   NEW(s) ;
   WITH s^ DO
      WITH contents DO
         len := 0 ;
         next := NIL
      END ;
      ConcatContents (contents, a, StrLen (a), 0) ;
      NEW (head) ;
      WITH head^ DO
         charStarUsed  := FALSE ;
         charStar      := NIL ;
         charStarSize  := 0;
         charStarValid := FALSE ;
         garbage       := NIL ;
         state         := inuse ;
      END
   END ;
   AddDebugInfo (s) ;
   IF TraceOn
   THEN
      s := AssignDebug (s, __FILE__, __LINE__, __FUNCTION__)
   END ;
   RETURN s
END InitString ;


(*
   DeallocateCharStar - deallocates any charStar.
*)

PROCEDURE DeallocateCharStar (s: String) ;
BEGIN
   IF (s # NIL) AND (s^.head # NIL)
   THEN
      WITH s^.head^ DO
         IF charStarUsed AND (charStar # NIL)
         THEN
            DEALLOCATE (charStar, charStarSize)
         END ;
         charStarUsed  := FALSE ;
         charStar      := NIL ;
         charStarSize  := 0 ;
         charStarValid := FALSE
      END
   END
END DeallocateCharStar ;


(*
   CheckPoisoned - checks for a poisoned string, s.
*)

PROCEDURE CheckPoisoned (s: String) : String ;
BEGIN
   IF PoisonOn AND (s # NIL) AND (s^.head # NIL) AND (s^.head^.state = poisoned)
   THEN
      HALT
   END ;
   RETURN s
END CheckPoisoned ;


(*
   KillString - frees String, s, and its contents.
                NIL is returned.
*)

PROCEDURE KillString (s: String) : String ;
VAR
   t: String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   IF s # NIL
   THEN
      IF CheckOn
      THEN
         IF IsOnAllocated (s)
         THEN
            SubAllocated (s)
         ELSIF IsOnDeallocated (s)
         THEN
            SubDeallocated (s)
         END
      END ;
      WITH s^ DO
         IF head # NIL
         THEN
            WITH head^ DO
               state := poisoned ;
               garbage := KillString (garbage) ;
               IF NOT PoisonOn
               THEN
                  DeallocateCharStar (s)
               END
            END ;
            IF NOT PoisonOn
            THEN
               DISPOSE (head) ;
               head := NIL
            END
         END ;
         t := KillString (s^.contents.next) ;
         IF NOT PoisonOn
         THEN
            DISPOSE (s)
         END
      END
   END ;
   RETURN NIL
END KillString ;


(*
   Fin - finishes with a string, it calls KillString with, s.
         The purpose of the procedure is to provide a short cut
         to calling KillString and then testing the return result.
*)

PROCEDURE Fin (s: String) ;
BEGIN
   IF KillString (s) # NIL
   THEN
      HALT
   END
END Fin ;


(*
   MarkInvalid - marks the char * version of String, s, as invalid.
*)

PROCEDURE MarkInvalid (s: String) ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   IF s^.head # NIL
   THEN
      s^.head^.charStarValid := FALSE
   END
END MarkInvalid ;


(*
   ConcatContentsAddress - concatenate the string, a, where, h, is the
                           total length of, a.
*)

PROCEDURE ConcatContentsAddress (VAR c: Contents; a: ADDRESS; h: CARDINAL) ;
VAR
   p   : POINTER TO CHAR ;
   i, j: CARDINAL ;
BEGIN
   j := 0 ;
   i := c.len ;
   p := a ;
   WHILE (j < h) AND (i < MaxBuf) DO
      c.buf[i] := p^ ;
      INC (i) ;
      INC (j) ;
      INC (p)
   END ;
   IF j < h
   THEN
      c.len := MaxBuf ;
      NEW (c.next) ;
      WITH c.next^ DO
         head          := NIL ;
         contents.len  := 0 ;
         contents.next := NIL ;
         ConcatContentsAddress (contents, p, h - j)
      END ;
      AddDebugInfo (c.next) ;
      IF TraceOn
      THEN
         c.next := AssignDebug (c.next, __FILE__, __LINE__, __FUNCTION__)
      END
   ELSE
      c.len := i ;
      c.next := NIL
   END
END ConcatContentsAddress ;


(*
   InitStringCharStar - initializes and returns a String to contain the C string.
*)

PROCEDURE InitStringCharStar (a: ADDRESS) : String ;
VAR
   s: String ;
BEGIN
   NEW (s) ;
   WITH s^ DO
      WITH contents DO
         len := 0 ;
         next := NIL
      END ;
      IF a#NIL
      THEN
         ConcatContentsAddress (contents, a, strlen (a))
      END ;
      NEW (head) ;
      WITH head^ DO
         charStarUsed  := FALSE ;
         charStar      := NIL ;
         charStarSize  := 0 ;
         charStarValid := FALSE ;
         garbage       := NIL ;
         state         := inuse
      END
   END ;
   AddDebugInfo (s) ;
   IF TraceOn
   THEN
      s := AssignDebug (s, __FILE__, __LINE__, __FUNCTION__)
   END ;
   RETURN s
END InitStringCharStar ;


(*
   InitStringChar - initializes and returns a String to contain the single character, ch.
*)

PROCEDURE InitStringChar (ch: CHAR) : String ;
VAR
   a: ARRAY [0..1] OF CHAR ;
   s: String ;
BEGIN
   a[0] := ch ;
   a[1] := nul ;
   s := InitString (a) ;
   IF TraceOn
   THEN
      s := AssignDebug (s, __FILE__, __LINE__, __FUNCTION__)
   END ;
   RETURN s
END InitStringChar ;


(*
   Mark - marks String, s, ready for garbage collection.
*)

PROCEDURE Mark (s: String) : String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   IF (s # NIL) AND (s^.head^.state = inuse)
   THEN
      s^.head^.state := marked
   END ;
   RETURN s
END Mark ;


(*
   AddToGarbage - adds String, b, onto the garbage list of, a.  Providing
                  the state of b is marked.  The state is then altered to
                  onlist.  String, a, is returned.
*)

PROCEDURE AddToGarbage (a, b: String) : String ;
VAR
   c: String ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned (a) ;
      b := CheckPoisoned (b)
   END ;
(*
   IF (a#NIL) AND (a#b) AND (a^.head^.state=marked)
   THEN
      writeString('warning trying to add to a marked string') ; writeLn
   END ;
*)
   IF (a # b) AND (a # NIL) AND (b # NIL) AND (b^.head^.state = marked) AND (a^.head^.state = inuse)
   THEN
      c := a ;
      WHILE c^.head^.garbage # NIL DO
         c := c^.head^.garbage
      END ;
      c^.head^.garbage := b ;
      b^.head^.state := onlist ;
      IF CheckOn
      THEN
         SubDebugInfo (b)
      END
   END ;
   RETURN a
END AddToGarbage ;


(*
   IsOnGarbage - returns TRUE if, s, is on string, e, garbage list.
*)

PROCEDURE IsOnGarbage (e, s: String) : BOOLEAN ;
BEGIN
   IF (e # NIL) AND (s # NIL)
   THEN
      WHILE e^.head^.garbage # NIL DO
         IF e^.head^.garbage = s
         THEN
            RETURN TRUE
         ELSE
            e := e^.head^.garbage
         END
      END
   END ;
   RETURN FALSE
END IsOnGarbage ;


(*
   Length - returns the length of the String, s.
*)

PROCEDURE Length (s: String) : CARDINAL ;
BEGIN
   IF s = NIL
   THEN
      RETURN 0
   ELSE
      RETURN s^.contents.len + Length (s^.contents.next)
   END
END Length ;


(*
   ConCat - returns String, a, after the contents of, b, have been appended.
*)

PROCEDURE ConCat (a, b: String) : String ;
VAR
   t: String ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned (a) ;
      b := CheckPoisoned (b)
   END ;
   IF a = b
   THEN
      RETURN ConCat (a, Mark (Dup (b)))
   ELSIF a # NIL
   THEN
      a := AddToGarbage (a, b) ;
      MarkInvalid (a) ;
      t := a ;
      WHILE b # NIL DO
         WHILE (t^.contents.len = MaxBuf) AND (t^.contents.next # NIL) DO
            t := t^.contents.next
         END ;
         ConcatContents (t^.contents, b^.contents.buf, b^.contents.len, 0) ;
         b := b^.contents.next
      END
   END ;
   IF (a = NIL) AND (b # NIL)
   THEN
      HALT
   END ;
   RETURN a
END ConCat ;


(*
   ConCatChar - returns String, a, after character, ch, has been appended.
*)

PROCEDURE ConCatChar (a: String; ch: CHAR) : String ;
VAR
   b: ARRAY [0..1] OF CHAR ;
   t: String ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned (a)
   END ;
   b[0] := ch ;
   b[1] := nul ;
   t := a ;
   MarkInvalid (a) ;
   WHILE (t^.contents.len = MaxBuf) AND (t^.contents.next # NIL) DO
      t := t^.contents.next
   END ;
   ConcatContents (t^.contents, b, 1, 0) ;
   RETURN a
END ConCatChar ;


(*
   Assign - assigns the contents of, b, into, a.
            String, a, is returned.
*)

PROCEDURE Assign (a, b: String) : String ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned (a) ;
      b := CheckPoisoned (b)
   END ;
   IF (a # NIL) AND (b # NIL)
   THEN
      WITH a^ DO
         contents.next := KillString (contents.next) ;
         contents.len  := 0
      END
   END ;
   RETURN ConCat (a, b)
END Assign ;


(*
   Dup - duplicate a String, s, returning the copy of s.
*)

PROCEDURE Dup (s: String) : String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   s := Assign (InitString (''), s) ;
   IF TraceOn
   THEN
      s := AssignDebug (s, __FILE__, __LINE__, __FUNCTION__)
   END ;
   RETURN s
END Dup ;


(*
   Add - returns a new String which contains the contents of a and b.
*)

PROCEDURE Add (a, b: String) : String ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned (a) ;
      b := CheckPoisoned (b)
   END ;
   a := ConCat (ConCat (InitString (''), a), b) ;
   IF TraceOn
   THEN
      a := AssignDebug (a, __FILE__, __LINE__, __FUNCTION__)
   END ;
   RETURN a
END Add ;


(*
   Equal - returns TRUE if String, a, and, b, are equal.
*)

PROCEDURE Equal (a, b: String) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned (a) ;
      b := CheckPoisoned (b)
   END ;
   IF Length (a) = Length (b)
   THEN
      WHILE (a # NIL) AND (b # NIL) DO
         i := 0 ;
         Assert (a^.contents.len = b^.contents.len) ;
         WHILE i<a^.contents.len DO
            IF a^.contents.buf[i] # a^.contents.buf[i]
            THEN
               HALT
            END ;
            IF b^.contents.buf[i] # b^.contents.buf[i]
            THEN
               HALT
            END ;
            IF a^.contents.buf[i] # b^.contents.buf[i]
            THEN
               RETURN FALSE
            END ;
            INC (i)
         END ;
         a := a^.contents.next ;
         b := b^.contents.next
      END ;
      RETURN TRUE
   ELSE
      RETURN FALSE
   END
END Equal ;


(*
   EqualCharStar - returns TRUE if contents of String, s, is the same as the
                   string, a.
*)

PROCEDURE EqualCharStar (s: String; a: ADDRESS) : BOOLEAN ;
VAR
   t: String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   t := InitStringCharStar (a) ;
   IF TraceOn
   THEN
      t := AssignDebug (t, __FILE__, __LINE__, __FUNCTION__)
   END ;
   t := AddToGarbage (t, s) ;
   IF Equal (t, s)
   THEN
      t := KillString (t) ;
      RETURN TRUE
   ELSE
      t := KillString (t) ;
      RETURN FALSE
   END
END EqualCharStar ;


(*
   EqualArray - returns TRUE if contents of String, s, is the same as the
                string, a.
*)

PROCEDURE EqualArray (s: String; a: ARRAY OF CHAR) : BOOLEAN ;
VAR
   t: String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   t := InitString (a) ;
   IF TraceOn
   THEN
      t := AssignDebug (t, __FILE__, __LINE__, __FUNCTION__)
   END ;
   t := AddToGarbage (t, s) ;
   IF Equal (t, s)
   THEN
      t := KillString (t) ;
      RETURN TRUE
   ELSE
      t := KillString (t) ;
      RETURN FALSE
   END
END EqualArray ;


(*
   Mult - returns a new string which is n concatenations of String, s.
*)

PROCEDURE Mult (s: String; n: CARDINAL) : String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   IF n<=0
   THEN
      s := AddToGarbage (InitString (''), s)
   ELSE
      s := ConCat (Mult (s, n-1), s)
   END ;
   IF TraceOn
   THEN
      s := AssignDebug (s, __FILE__, __LINE__, __FUNCTION__)
   END ;
   RETURN s
END Mult ;


(*
   Slice - returns a new string which contains the elements
           low..high-1

           strings start at element 0
           Slice(s, 0, 2)  will return elements 0, 1 but not 2
           Slice(s, 1, 3)  will return elements 1, 2 but not 3
           Slice(s, 2, 0)  will return elements 2..max
           Slice(s, 3, -1) will return elements 3..max-1
           Slice(s, 4, -2) will return elements 4..max-2
*)

PROCEDURE Slice (s: String; low, high: INTEGER) : String ;
VAR
   d, t         : String ;
   start, end, o: INTEGER ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   IF low < 0
   THEN
      low := VAL (INTEGER, Length (s)) + low
   END ;
   IF high <= 0
   THEN
      high := VAL (INTEGER, Length (s)) + high
   ELSE
      (* make sure high is <= Length (s) *)
      high := Min (Length (s), high)
   END ;
   d := InitString ('') ;
   d := AddToGarbage (d, s) ;
   o := 0 ;
   t := d ;
   WHILE s # NIL DO
      IF low < o + VAL (INTEGER, s^.contents.len)
      THEN
         IF o > high
         THEN
            s := NIL
         ELSE
            (* found sliceable unit *)
            IF low < o
            THEN
               start := 0
            ELSE
               start := low - o
            END ;
            end := Max (Min (MaxBuf, high - o), 0) ;
            WHILE t^.contents.len = MaxBuf DO
               IF t^.contents.next = NIL
               THEN
                  NEW (t^.contents.next) ;
                  WITH t^.contents.next^ DO
                     head         := NIL ;
                     contents.len := 0
                  END ;
                  AddDebugInfo (t^.contents.next) ;
                  IF TraceOn
                  THEN
                     t^.contents.next := AssignDebug (t^.contents.next, __FILE__, __LINE__, __FUNCTION__)
                  END
               END ;
               t := t^.contents.next
            END ;
            ConcatContentsAddress (t^.contents,
                                   ADR (s^.contents.buf[start]), end - start) ;
            INC (o, s^.contents.len) ;
            s := s^.contents.next
         END
      ELSE
         INC (o, s^.contents.len) ;
         s := s^.contents.next
      END ;
   END ;
   IF TraceOn
   THEN
      d := AssignDebug (d, __FILE__, __LINE__, __FUNCTION__)
   END ;
   RETURN d
END Slice ;


(*
   Index - returns the indice of the first occurance of, ch, in
           String, s. -1 is returned if, ch, does not exist.
           The search starts at position, o.
*)

PROCEDURE Index (s: String; ch: CHAR; o: CARDINAL) : INTEGER ;
VAR
   i, k: CARDINAL ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   k := 0 ;
   WHILE s # NIL DO
      WITH s^ DO
         IF k + contents.len < o
         THEN
            INC (k, contents.len)
         ELSE
            i := o - k ;
            WHILE i < contents.len DO
               IF contents.buf[i] = ch
               THEN
                  RETURN k + i
               END ;
               INC (i)
            END ;
            INC (k, i) ;
            o := k
         END
      END ;
      s := s^.contents.next
   END ;
   RETURN -1
END Index ;


(*
   RIndex - returns the indice of the last occurance of, ch,
            in String, s. The search starts at position, o.
            -1 is returned if, ch, is not found.
*)

PROCEDURE RIndex (s: String; ch: CHAR; o: CARDINAL) : INTEGER ;
VAR
   i, k: CARDINAL ;
   j   : INTEGER ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   j := -1 ;
   k :=  0 ;
   WHILE s # NIL DO
      WITH s^ DO
         IF k + contents.len < o
         THEN
            INC (k, contents.len)
         ELSE
            IF o < k
            THEN
               i := 0
            ELSE
               i := o - k
            END ;
            WHILE i < contents.len DO
               IF contents.buf[i] = ch
               THEN
                  j := k
               END ;
               INC (k) ;
               INC (i)
            END
         END
      END ;
      s := s^.contents.next
   END ;
   RETURN j
END RIndex ;


(*
   RemoveComment - assuming that, comment, is a comment delimiter
                   which indicates anything to its right is a comment
                   then strip off the comment and also any white space
                   on the remaining right hand side.
                   It leaves any white space on the left hand side alone.
*)

PROCEDURE RemoveComment (s: String; comment: CHAR) : String ;
VAR
   i: INTEGER ;
BEGIN
   i := Index (s, comment, 0) ;
   IF i = 0
   THEN
      s := InitString ('')
   ELSIF i > 0
   THEN
      s := RemoveWhitePostfix (Slice (Mark (s), 0, i))
   END ;
   IF TraceOn
   THEN
      s := AssignDebug (s, __FILE__, __LINE__, __FUNCTION__)
   END ;
   RETURN s
END RemoveComment ;


(*
   char - returns the character, ch, at position, i, in String, s.
*)

PROCEDURE char (s: String; i: INTEGER) : CHAR ;
VAR
   c: CARDINAL ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   IF i<0
   THEN
      c := VAL (CARDINAL, VAL (INTEGER, Length (s)) + i)
   ELSE
      c := i
   END ;
   WHILE (s # NIL) AND (c >= s^.contents.len) DO
      DEC (c, s^.contents.len) ;
      s := s^.contents.next
   END ;
   IF (s = NIL) OR (c >= s^.contents.len)
   THEN
      RETURN nul
   ELSE
      RETURN s^.contents.buf[c]
   END
END char ;


(*
   string - returns the C style char * of String, s.
*)

PROCEDURE string (s: String) : ADDRESS ;
VAR
   a   : String ;
   l, i: CARDINAL ;
   p   : POINTER TO CHAR ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned (s)
   END ;
   IF s = NIL
   THEN
      RETURN NIL
   ELSE
      IF NOT s^.head^.charStarValid
      THEN
         l := Length (s) ;
         WITH s^.head^ DO
            IF NOT (charStarUsed AND (charStarSize > l))
            THEN
               DeallocateCharStar (s) ;
               ALLOCATE (charStar, l+1) ;
               charStarSize := l+1 ;
               charStarUsed := TRUE
            END ;
            p := charStar ;
         END ;
         a := s ;
         WHILE a#NIL DO
            i := 0 ;
            WHILE i < a^.contents.len DO
               p^ := a^.contents.buf[i] ;
               INC (i) ;
               INC (p)
            END ;
            a := a^.contents.next
         END ;
         p^ := nul ;
         s^.head^.charStarValid := TRUE
      END ;
      RETURN s^.head^.charStar
   END
END string ;


(*
   IsWhite - returns TRUE if, ch, is a space or a tab.
*)

PROCEDURE IsWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch = ' ') OR (ch = tab)
END IsWhite ;


(*
   RemoveWhitePrefix - removes any leading white space from String, s.
                       A new string is returned.
*)

PROCEDURE RemoveWhitePrefix (s: String) : String ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE IsWhite (char (s, i)) DO
      INC (i)
   END ;
   s := Slice (s, INTEGER (i), 0) ;
   IF TraceOn
   THEN
      s := AssignDebug (s, __FILE__, __LINE__, __FUNCTION__)
   END ;
   RETURN s
END RemoveWhitePrefix ;


(*
   RemoveWhitePostfix - removes any leading white space from String, s.
                        A new string is returned.
*)

PROCEDURE RemoveWhitePostfix (s: String) : String ;
VAR
   i: INTEGER ;
BEGIN
   i := VAL(INTEGER, Length (s)) - 1 ;
   WHILE (i >= 0) AND IsWhite (char (s, i)) DO
      DEC (i)
   END ;
   s := Slice (s, 0, i+1) ;
   IF TraceOn
   THEN
      s := AssignDebug (s, __FILE__, __LINE__, __FUNCTION__)
   END ;
   RETURN s
END RemoveWhitePostfix ;


(*
   ToUpper - returns string, s, after it has had its lower case characters
             replaced by upper case characters.
             The string, s, is not duplicated.
*)

PROCEDURE ToUpper (s: String) : String ;
VAR
   ch: CHAR ;
   i : CARDINAL ;
   t : String ;
BEGIN
   IF s # NIL
   THEN
      MarkInvalid (s) ;
      t := s ;
      WHILE t # NIL DO
         WITH t^ DO
            i := 0 ;
            WHILE i < contents.len DO
               ch := contents.buf[i] ;
               IF (ch >= 'a') AND (ch <= 'z')
               THEN
                  contents.buf[i] := CHR (ORD (ch) - ORD ('a') + ORD ('A'))
               END ;
               INC (i)
            END
         END ;
         t := t^.contents.next
      END
   END ;
   RETURN s
END ToUpper ;


(*
   ToLower - returns string, s, after it has had its upper case characters
             replaced by lower case characters.
             The string, s, is not duplicated.
*)

PROCEDURE ToLower (s: String) : String ;
VAR
   ch: CHAR ;
   i : CARDINAL ;
   t : String ;
BEGIN
   IF s # NIL
   THEN
      MarkInvalid (s) ;
      t := s ;
      WHILE t # NIL DO
         WITH t^ DO
            i := 0 ;
            WHILE i < contents.len DO
               ch := contents.buf[i] ;
               IF (ch >= 'A') AND (ch <= 'Z')
               THEN
                  contents.buf[i] := CHR (ORD (ch) - ORD ('A') + ORD ('a'))
               END ;
               INC (i)
            END
         END ;
         t := t^.contents.next
      END
   END ;
   RETURN s
END ToLower ;


(*
   InitStringDB - the debug version of InitString.
*)

PROCEDURE InitStringDB (a: ARRAY OF CHAR; file: ARRAY OF CHAR; line: CARDINAL) : String ;
BEGIN
   RETURN AssignDebug (InitString (a), file, line, 'InitString')
END InitStringDB ;


(*
   InitStringCharStarDB - the debug version of InitStringCharStar.
*)

PROCEDURE InitStringCharStarDB (a: ADDRESS; file: ARRAY OF CHAR; line: CARDINAL) : String ;
BEGIN
   RETURN AssignDebug (InitStringCharStar (a), file, line, 'InitStringCharStar')
END InitStringCharStarDB ;


(*
   InitStringCharDB - the debug version of InitStringChar.
*)

PROCEDURE InitStringCharDB (ch: CHAR; file: ARRAY OF CHAR; line: CARDINAL) : String ;
BEGIN
   RETURN AssignDebug (InitStringChar (ch), file, line, 'InitStringChar')
END InitStringCharDB ;


(*
   MultDB - the debug version of MultDB.
*)

PROCEDURE MultDB (s: String; n: CARDINAL; file: ARRAY OF CHAR; line: CARDINAL) : String ;
BEGIN
   RETURN AssignDebug (Mult (s, n), file, line, 'Mult')
END MultDB ;


(*
   DupDB - the debug version of Dup.
*)

PROCEDURE DupDB (s: String; file: ARRAY OF CHAR; line: CARDINAL) : String ;
BEGIN
   RETURN AssignDebug (Dup (s), file, line, 'Dup')
END DupDB ;


(*
   SliceDB - debug version of Slice.
*)

PROCEDURE SliceDB (s: String; low, high: INTEGER;
                   file: ARRAY OF CHAR; line: CARDINAL) : String ;
BEGIN
   DSdbEnter ;
   s := AssignDebug (Slice (s, low, high), file, line, 'Slice') ;
   DSdbExit (s) ;
   RETURN s
END SliceDB ;


(*
   DumpState -
*)

PROCEDURE DumpState (s: String) ;
BEGIN
   CASE s^.head^.state OF

   inuse   :  writeString ("still in use (") ; writeCard (s^.contents.len) ; writeString (") characters") |
   marked  :  writeString ("marked") |
   onlist  :  writeString ("on a garbage list") |
   poisoned:  writeString ("poisoned")

   ELSE
      writeString ("unknown state")
   END
END DumpState ;


(*
   DumpStringSynopsis -
*)

PROCEDURE DumpStringSynopsis (s: String) ;
BEGIN
   writeCstring (s^.debug.file) ; writeString (':') ;
   writeCard (s^.debug.line) ; writeString (':') ;
   writeCstring (s^.debug.proc) ;
   writeString (' string ') ;
   writeAddress (s) ;
   writeString (' ') ;
   DumpState (s) ;
   IF IsOnAllocated (s)
   THEN
      writeString (' globally allocated')
   ELSIF IsOnDeallocated (s)
   THEN
      writeString (' globally deallocated')
   ELSE
      writeString (' globally unknown')
   END ;
   writeLn
END DumpStringSynopsis ;


(*
   DumpString - displays the contents of string, s.
*)

PROCEDURE DumpString (s: String) ;
VAR
   t: String ;
BEGIN
   IF s # NIL
   THEN
      DumpStringSynopsis (s) ;
      IF (s^.head # NIL) AND (s^.head^.garbage # NIL)
      THEN
         writeString ('display chained strings on the garbage list') ; writeLn ;
         t := s^.head^.garbage ;
         WHILE t # NIL DO
            DumpStringSynopsis (t) ;
            t := t^.head^.garbage
         END
      END
   END
END DumpString ;


(*
   Init - initialize the module.
*)

PROCEDURE Init ;
BEGIN
   IF NOT Initialized
   THEN
      Initialized := TRUE ;
      frameHead := NIL ;
      PushAllocation ;
   END
END Init ;


BEGIN
   Initialized := FALSE ;
   Init
END DynamicStrings.
