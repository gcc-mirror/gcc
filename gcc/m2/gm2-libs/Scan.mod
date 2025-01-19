(* Scan.mod Provides a primitive symbol fetching from input.

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

IMPLEMENTATION MODULE Scan ;


IMPORT StdIO ;

FROM ASCII IMPORT nul, lf, cr, bs, del, bel ;
FROM StdIO IMPORT Write ;
FROM StrLib IMPORT StrEqual, StrLen, StrCopy ;
FROM NumberIO IMPORT WriteCard, CardToStr ;
FROM FIO IMPORT OpenToRead, IsNoError, Close, File, ReadChar ;
FROM StrIO IMPORT WriteLn, WriteString ;
FROM libc IMPORT exit ;


CONST
   MaxLength = 255 ;  (* Max Length of Source Line *)

VAR
   FileName,
   CurrentString       : ARRAY [0..MaxLength] OF CHAR ;
   CurrentLineNo       : CARDINAL ;
   CurrentCursorPos    : CARDINAL ;
   EOF                 : BOOLEAN ;
   LengthOfCurSym      : CARDINAL ;
   f                   : File ;
   Opened              : BOOLEAN ;
   HaltOnError         : BOOLEAN ;
   AllowComments       : BOOLEAN ;
   CommentLeader,
   CommentTrailer      : ARRAY [0..MaxLength] OF CHAR ;
   TerminateOnEndOfLine: BOOLEAN ;
   InString            : BOOLEAN ;


PROCEDURE OpenSource (a: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   StrCopy(a, FileName) ;
   f := OpenToRead(a) ;
   IF IsNoError(f)
   THEN
      StrCopy( '', CurrentString ) ;
      LengthOfCurSym := 0 ;
      CurrentCursorPos := 0 ;
      EOF := FALSE ;
      CurrentLineNo := 1 ;
      Opened := TRUE
   ELSE
      Opened := FALSE
   END ;
   RETURN( Opened )
END OpenSource ;


PROCEDURE CloseSource ;
BEGIN
   IF Opened
   THEN
      Close( f ) ;
      Opened := FALSE
   END
END CloseSource ;


(*
   IsStartOfComment - returns TRUE if we are looking at the start of a comment.
*)

PROCEDURE IsStartOfComment () : BOOLEAN ;
VAR
   i, h: CARDINAL ;
BEGIN
   IF AllowComments
   THEN
      i := 0 ;
      h := StrLen(CommentLeader) ;
      WHILE (i<h) AND (CommentLeader[i]=CurrentString[CurrentCursorPos+i]) DO
         INC(i)
      END ;
      RETURN( i=h )
   ELSE
      RETURN( FALSE )
   END
END IsStartOfComment ;


(*
   IsEndOfComment - returns TRUE if we can see the end of comment string.
                    If TRUE is returned then we also have consumed the string.
*)

PROCEDURE IsEndOfComment () : BOOLEAN ;
VAR
   i, h: CARDINAL ;
BEGIN
   IF AllowComments
   THEN
      IF TerminateOnEndOfLine AND (SymbolChar()=nul)
      THEN
         NextChar ;
         RETURN( TRUE )
      ELSE
         i := 0 ;
         h := StrLen(CommentTrailer) ;
         WHILE (i<h) AND (CommentTrailer[i]=CurrentString[CurrentCursorPos+i]) DO
            INC(i)
         END ;
         IF (i=h) AND (h#0)
         THEN
            (* seen tailer therefore eat it *)
            INC(CurrentCursorPos, i) ;
            RETURN( TRUE )
         ELSE
            RETURN( FALSE )
         END
      END
   ELSE
      RETURN( FALSE )
   END
END IsEndOfComment ;


(*
   IsQuote - returns TRUE if the current character is a quote.
*)

PROCEDURE IsQuote () : BOOLEAN ;
BEGIN
   RETURN( SymbolChar()='"' )
END IsQuote ;


(*
   GetNextSymbol - returns the next symbol from the source file.
                   It ignores comments and treats strings differently
                   from normal symbols. Strings will return " string ".
*)

PROCEDURE GetNextSymbol (VAR a: ARRAY OF CHAR) ;
VAR
   index,
   High    : CARDINAL ;
BEGIN
   index := 0 ;
   High  := HIGH( a ) ;
   ChuckUpToSymbol ;

   IF InString
   THEN
      IF (NOT EOF) AND (NOT IsStartOfComment()) AND (index<High) AND IsQuote()
      THEN
         (* found final quote *)
         a[index] := SymbolChar() ;
         NextChar ;
         INC(index) ;
         InString := FALSE ;
      ELSE
         (* copy literal into, a *)
         WHILE (index<High) AND (NOT EOF) AND (SymbolChar()#nul) AND (NOT IsQuote()) DO
            a[index] := SymbolChar() ;
            NextChar ;
            INC(index)
         END ;
         IF NOT IsQuote()
         THEN
            WriteError('unterminated string, strings must terminate before the end of a line')
         END ;
      END
   ELSE
      IF (NOT EOF) AND (NOT IsStartOfComment())
      THEN
         IF (index<High) AND IsQuote()
         THEN
            (* found string start *)
            a[index] := SymbolChar() ;
            NextChar ; (* skip quote *)
            INC(index) ;
            InString := TRUE ;
         ELSE
            (* normal symbol, not a comment and not a string *)
            WHILE (index<High) AND (NOT NonSymbolChar()) AND (NOT IsStartOfComment()) DO
               a[index] := SymbolChar() ;
               NextChar ;
               INC(index)
            END
         END
      END
   END ;
   IF index<High
   THEN
      a[index] := nul
   END ;
   LengthOfCurSym := index
END GetNextSymbol ;


(*
   ChuckUpToSymbol - throws away white space and comments.
*)

PROCEDURE ChuckUpToSymbol ;
BEGIN
   REPEAT
      IF (NOT EOF) AND IsStartOfComment()
      THEN
         NextChar ;
         WHILE (NOT EOF) AND (NOT IsEndOfComment()) DO
            NextChar
         END
      END ;
      WHILE (NOT EOF) AND NonSymbolChar() DO
         NextChar
      END
   UNTIL EOF OR (NOT IsStartOfComment())
END ChuckUpToSymbol ;


(*
   SymbolChar - returns a character from the CurrentString, if the end
                of CurrentString is found then SymbolChar returns nul.
*)

PROCEDURE SymbolChar () : CHAR ;
BEGIN
   IF EOF
   THEN
      RETURN( nul )
   ELSE
      IF CurrentCursorPos<StrLen(CurrentString)
      THEN
         RETURN( CurrentString[CurrentCursorPos] )
      ELSE
         RETURN( nul )
      END
   END
END SymbolChar ;


(* NextChar advances the CurrentCursorPos along a line of the source, *)
(* resetting the CurrentCursorPos every time a newline is read.       *)

PROCEDURE NextChar ;
BEGIN
   IF NOT EOF
   THEN
      IF CurrentCursorPos<StrLen(CurrentString)
      THEN
         INC(CurrentCursorPos)
      ELSE
         ReadString(CurrentString) ;
         (* WriteString( CurrentString ) ; WriteLn ; *)
         INC(CurrentLineNo) ;
         CurrentCursorPos := 0 ;
         LengthOfCurSym := 0
      END
   END
END NextChar ;


PROCEDURE NonSymbolChar () : BOOLEAN ;
BEGIN
   RETURN( CurrentString[CurrentCursorPos]<=' ' )
END NonSymbolChar ;


PROCEDURE WriteError (a: ARRAY OF CHAR) ;
VAR
   i, j  : CARDINAL ;
   LineNo: ARRAY [0..20] OF CHAR ;
BEGIN
   WriteString(FileName) ;
   Write(':') ;
   CardToStr(CurrentLineNo, 0, LineNo) ;
   WriteString(LineNo) ;
   Write(':') ;
   WriteString( CurrentString ) ; WriteLn ;
   WriteString(FileName) ;
   Write(':') ;
   WriteString(LineNo) ;
   Write(':') ;
   i := 0 ;
   j := CurrentCursorPos-LengthOfCurSym ;
   WHILE i<j DO
      Write(' ') ;
      INC( i )
   END ;
   FOR i := 1 TO LengthOfCurSym DO
      Write('^')
   END ;
   WriteLn ;
   WriteString(FileName) ;
   Write(':') ;
   WriteString(LineNo) ;
   Write(':') ;
   WriteString( a ) ; WriteLn ;
   IF HaltOnError
   THEN
      exit(1)
   END
END WriteError ;


PROCEDURE ReadString (VAR a: ARRAY OF CHAR) ;
VAR
   n    ,
   high : CARDINAL ;
   ch   : CHAR ;
BEGIN
   high := HIGH( a ) ;
   n := 0 ;
   REPEAT
      Read( ch ) ;
      IF (ch=del) OR (ch=bs)
      THEN
         IF n=0
         THEN
            Write( bel )
         ELSE
            Write( bs ) ;
            Write(' ') ;
            Write( bs ) ;
            DEC( n )
         END
      ELSIF n <= high
      THEN
         IF (ch = cr) OR (cr = lf)
         THEN
            a[n] := nul
         ELSE
(*            Write( ch ) ;
 *)           a[n] := ch
         END ;
         INC( n )
      ELSE
         ch := cr  (* exit gracefully *)
      END
   UNTIL ch = cr
END ReadString ;


PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   IF Opened
   THEN
      ch := ReadChar(f) ;
      EOF := NOT IsNoError(f)
   ELSE
      StdIO.Read( ch )
   END ;
   IF ch=lf THEN ch := cr END
END Read ;


(*
   TerminateOnError - exits with status 1 if we call WriteError.
*)

PROCEDURE TerminateOnError ;
BEGIN
   HaltOnError := TRUE
END TerminateOnError ;


(*
   DefineComments - defines the start of comments within the source
                    file.

                    The characters in Start define the comment start
                    and characters in End define the end.
                    The BOOLEAN eoln determine whether the comment
                    is terminated by end of line. If eoln is TRUE
                    then End is ignored.
*)

PROCEDURE DefineComments (Start, End: ARRAY OF CHAR; eoln: BOOLEAN) ;
BEGIN
   TerminateOnEndOfLine := eoln ;
   StrCopy(Start, CommentLeader) ;
   StrCopy(End, CommentTrailer) ;
   AllowComments := StrLen(CommentLeader)>0
END DefineComments ;


BEGIN
   InString             := FALSE ;
   AllowComments        := FALSE ;
   TerminateOnEndOfLine := FALSE ;
   StrCopy(''           , CurrentString) ;
   LengthOfCurSym       := 0 ;
   CurrentCursorPos     := 0 ;
   EOF                  := FALSE ;
   CurrentLineNo        := 1 ;
   Opened               := FALSE ;
   HaltOnError          := FALSE
END Scan.
