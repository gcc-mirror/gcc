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

IMPLEMENTATION MODULE mcPretty ;

FROM DynamicStrings IMPORT String, InitString, KillString, Length, char ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;


TYPE
   pretty = POINTER TO RECORD
                          write      : writeProc ;
			  writeln    : writeLnProc ;
                          needsSpace,
                          needsIndent: BOOLEAN ;
			  seekPos,
			  curLine,
                          curPos,
		          indent     : CARDINAL ;
			  stacked    : pretty ;
                       END ;


(*
   initPretty - initialise a pretty print data structure.
*)

PROCEDURE initPretty (w: writeProc; l: writeLnProc) : pretty ;
VAR
   p: pretty ;
BEGIN
   NEW (p) ;
   WITH p^ DO
      write := w ;
      writeln := l ;
      needsSpace := FALSE ;
      needsIndent := FALSE ;
      curPos := 0 ;
      curLine := 0 ;
      seekPos := 0 ;
      indent := 0 ;
      stacked := NIL
   END ;
   RETURN p
END initPretty ;


(*
   dupPretty - duplicate a pretty print data structure.
*)

PROCEDURE dupPretty (p: pretty) : pretty ;
VAR
   q: pretty ;
BEGIN
   NEW (q) ;
   q^ := p^ ;
   RETURN q
END dupPretty ;


(*
   killPretty - destroy a pretty print data structure.
                Post condition:  p is assigned to NIL.
*)

PROCEDURE killPretty (VAR p: pretty) ;
BEGIN
   p := NIL ;
   RETURN ;
   DISPOSE (p) ;
   p := NIL
END killPretty ;


(*
   pushPretty - duplicate, p.  Push, p, and return the duplicate.
*)

PROCEDURE pushPretty (p: pretty) : pretty ;
VAR
   q: pretty ;
BEGIN
   q := dupPretty (p) ;
   q^.stacked := p ;
   RETURN q
END pushPretty ;


(*
   popPretty - pops the pretty object from the stack.
*)

PROCEDURE popPretty (p: pretty) : pretty ;
VAR
   q: pretty ;
BEGIN
   q := p^.stacked ;
   q^.needsIndent := p^.needsIndent ;
   q^.needsSpace := p^.needsSpace ;
   q^.curPos := p^.curPos ;
   q^.seekPos := p^.seekPos ;
   q^.curLine := p^.curLine ;
   killPretty (p) ;
   RETURN q
END popPretty ;


(*
   getindent - returns the current indent value.
*)

PROCEDURE getindent (p: pretty) : CARDINAL ;
BEGIN
   RETURN p^.indent
END getindent ;


(*
   setindent - sets the current indent to, n.
*)

PROCEDURE setindent (p: pretty; n: CARDINAL) ;
BEGIN
   p^.indent := n
END setindent ;


(*
   getcurpos - returns the current cursor position.
*)

PROCEDURE getcurpos (s: pretty) : CARDINAL ;
BEGIN
   IF s^.needsSpace
   THEN
      RETURN s^.curPos+1
   ELSE
      RETURN s^.curPos
   END
END getcurpos ;


(*
   getcurline - returns the current line number.
*)

PROCEDURE getcurline (s: pretty) : CARDINAL ;
BEGIN
   RETURN s^.curLine
END getcurline ;


(*
   getseekpos - returns the seek position.
*)

PROCEDURE getseekpos (s: pretty) : CARDINAL ;
BEGIN
   RETURN s^.seekPos
END getseekpos ;


(*
   setneedSpace - sets needSpace flag to TRUE.
*)

PROCEDURE setNeedSpace (s: pretty) ;
BEGIN
   s^.needsSpace := TRUE
END setNeedSpace ;


(*
   noSpace - unset needsSpace.
*)

PROCEDURE noSpace (s: pretty) ;
BEGIN
   s^.needsSpace := FALSE
END noSpace ;


(*
   flushSpace -
*)

PROCEDURE flushSpace (p: pretty) ;
BEGIN
   IF p^.needsSpace
   THEN
      p^.write (' ') ;
      p^.needsSpace := FALSE ;
      INC (p^.curPos) ;
      INC (p^.seekPos)
   END
END flushSpace ;


(*
   flushIndent -
*)

PROCEDURE flushIndent (p: pretty) ;
VAR
   i: CARDINAL ;
BEGIN
   flushSpace (p) ;
   IF p^.needsIndent
   THEN
      WHILE p^.curPos<p^.indent DO
         p^.write (' ') ;
         INC (p^.curPos) ;
         INC (p^.seekPos)
      END ;
      p^.needsIndent := FALSE
   END
END flushIndent ;


(*
   print - print a string using, p.
*)

PROCEDURE print (p: pretty; a: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := InitString (a) ;
   prints (p, s) ;
   s := KillString (s)
END print ;


(*
   prints - print a string using, p.
*)

PROCEDURE prints (p: pretty; s: String) ;
VAR
   l, i: CARDINAL ;
BEGIN
   l := Length (s) ;
   i := 0 ;
   flushSpace (p) ;
   WHILE i<l DO
      IF (i+2<=l) AND (char (s, i)='\') AND (char (s, i+1)='n')
      THEN
         p^.needsIndent := TRUE ;
         p^.needsSpace := FALSE ;
         p^.curPos := 0 ;
         p^.writeln ;
         INC (p^.seekPos) ;
         INC (p^.curLine) ;
         INC (i)
      ELSE
         flushIndent (p) ;
         p^.write (char (s, i)) ;
         INC (p^.curPos) ;
         INC (p^.seekPos)
      END ;
      INC (i)
   END
END prints ;


(*
   raw - print out string, s, without any translation of
         escape sequences.
*)

PROCEDURE raw (p: pretty; s: String) ;
VAR
   l, i: CARDINAL ;
BEGIN
   l := Length (s) ;
   i := 0 ;
   flushSpace (p) ;
   flushIndent (p) ;
   WHILE i < l DO
      p^.write (char (s, i)) ;
      INC (p^.curPos) ;
      INC (p^.seekPos) ;
      INC (i)
   END
END raw ;


END mcPretty.
