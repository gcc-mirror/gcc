(* TextIO.mod implement the ISO TextIO specification.

Copyright (C) 2008-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE TextIO ;


IMPORT IOChan, IOConsts, CharClass, ASCII ;
FROM SYSTEM IMPORT ADR ;
FROM FIO IMPORT FlushOutErr ;

  (* The following procedures do not read past line marks *)

PROCEDURE CanRead (cid: IOChan.ChanId) : BOOLEAN ;
BEGIN
   RETURN( (IOChan.ReadResult(cid)=IOConsts.notKnown) OR
           (IOChan.ReadResult(cid)=IOConsts.allRight) )
END CanRead ;

PROCEDURE WasGoodChar (cid: IOChan.ChanId) : BOOLEAN ;
BEGIN
   RETURN( (IOChan.ReadResult(cid)#IOConsts.endOfLine) AND
           (IOChan.ReadResult(cid)#IOConsts.endOfInput) )
END WasGoodChar ;


(*
   SetResult - assigns the result in cid.
               If s is empty then leave as endOfInput
                  or endOfLine
               If s is not empty then assign allRight
               If range and i exceeds, h, then assign outOfRange
*)

PROCEDURE SetResult (cid: IOChan.ChanId; i: CARDINAL;
                     VAR s: ARRAY OF CHAR; range: BOOLEAN) ;
BEGIN
   IF i<=HIGH(s)
   THEN
      s[i] := ASCII.nul ;
      IF i>0
      THEN
         IOChan.SetReadResult(cid, IOConsts.allRight)
      END
   ELSIF range
   THEN
      IOChan.SetReadResult(cid, IOConsts.outOfRange)
   END
END SetResult ;


PROCEDURE ReadChar (cid: IOChan.ChanId; VAR ch: CHAR);
  (* If possible, removes a character from the input stream
     cid and assigns the corresponding value to ch.  The
     read result is set to the value allRight, endOfLine, or
     endOfInput.
  *)
VAR
   res: IOConsts.ReadResults ;
BEGIN
   FlushOutErr ;
   IF CanRead(cid)
   THEN
      IOChan.Look(cid, ch, res) ;
      IF res=IOConsts.allRight
      THEN
         IOChan.Skip(cid)
      END
   END
END ReadChar ;

PROCEDURE ReadRestLine (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
  (* Removes any remaining characters from the input stream
     cid before the next line mark,  copying to s as many as
     can be accommodated as a string value.  The read result is
     set to the value allRight, outOfRange, endOfLine, or
     endOfInput.
  *)
VAR
   i, h    : CARDINAL ;
   finished: BOOLEAN ;
BEGIN
   h := HIGH(s) ;
   i := 0 ;
   finished := FALSE ;
   WHILE (i<=h) AND CanRead(cid) AND (NOT finished) DO
      ReadChar(cid, s[i]) ;
      IF WasGoodChar(cid)
      THEN
         INC(i)
      ELSE
         finished := TRUE
      END
   END ;
   WHILE CanRead(cid) DO
      IOChan.Skip(cid)
   END ;
   SetResult(cid, i, s, TRUE)
END ReadRestLine ;

PROCEDURE ReadString (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
  (* Removes only those characters from the input stream cid
     before the next line mark that can be accommodated in s
     as a string value, and copies them to s.  The read result
     is set to the value allRight, endOfLine, or endOfInput.
  *)
VAR
   i, h    : CARDINAL ;
   finished: BOOLEAN ;
BEGIN
   h := HIGH(s) ;
   i := 0 ;
   finished := FALSE ;
   WHILE (i<=h) AND CanRead(cid) AND (NOT finished) DO
      ReadChar(cid, s[i]) ;
      IF WasGoodChar(cid)
      THEN
         INC(i)
      ELSE
         finished := TRUE
      END
   END ;
   SetResult(cid, i, s, FALSE)
END ReadString ;


(*
   SkipSpaces - skips any spaces.
*)

PROCEDURE SkipSpaces (cid: IOChan.ChanId) ;
VAR
   ch : CHAR ;
   res: IOConsts.ReadResults ;
BEGIN
   WHILE CanRead(cid) DO
      IOChan.Look(cid, ch, res) ;
      IF res=IOConsts.allRight
      THEN
         IF CharClass.IsWhiteSpace(ch)
         THEN
            IOChan.Skip(cid)
         ELSE
            RETURN
         END
      ELSE
         RETURN
      END
   END
END SkipSpaces ;


PROCEDURE ReadToken (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
  (* Skips leading spaces, and then removes characters from
     the input stream cid before the next space or line mark,
     copying to s as many as can be accommodated as a string
     value.  The read result is set to the value allRight,
     outOfRange, endOfLine, or endOfInput.
  *)
VAR
   i, h: CARDINAL ;
BEGIN
   SkipSpaces(cid) ;
   h := HIGH(s) ;
   i := 0 ;
   WHILE (i<=h) AND CanRead(cid) DO
      ReadChar(cid, s[i]) ;
      IF (s[i]=ASCII.nul) OR CharClass.IsWhiteSpace(s[i])
      THEN
         SetResult(cid, i, s, TRUE) ;
         RETURN
      END ;
      INC(i)
   END ;
   SetResult(cid, i, s, TRUE)
END ReadToken ;

  (* The following procedure reads past the next line mark *)

PROCEDURE SkipLine (cid: IOChan.ChanId);
  (* Removes successive items from the input stream cid up
     to and including the next line mark, or until the end
     of input is reached.  The read result is set to the
     value allRight, or endOfInput.
  *)
VAR
   ch : CHAR ;
   res: IOConsts.ReadResults ;
BEGIN
   IOChan.Look(cid, ch, res) ;
   WHILE res=IOConsts.allRight DO
      IOChan.SkipLook(cid, ch, res)
   END ;
   IF res=IOConsts.endOfLine
   THEN
      IOChan.Skip(cid)
   END
END SkipLine ;

  (* Output procedures *)

PROCEDURE WriteChar (cid: IOChan.ChanId; ch: CHAR);
  (* Writes the value of ch to the output stream cid. *)
BEGIN
   IOChan.TextWrite(cid, ADR(ch), SIZE(ch))
END WriteChar ;

PROCEDURE WriteLn (cid: IOChan.ChanId);
  (* Writes a line mark to the output stream cid. *)
BEGIN
   IOChan.WriteLn(cid)
END WriteLn ;

PROCEDURE WriteString (cid: IOChan.ChanId; s: ARRAY OF CHAR);
  (* Writes the string value in s to the output stream cid. *)
BEGIN
   IOChan.TextWrite(cid, ADR(s), LENGTH(s))
END WriteString ;


END TextIO.
