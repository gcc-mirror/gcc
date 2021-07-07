(* M2Lex.mod provides a non tokenised lexical analyser.

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

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2Lex ;


FROM FIO IMPORT File, OpenToRead, ReadChar, Close, IsNoError ;
FROM StrIO IMPORT ReadString, WriteString, WriteLn ;
FROM StdIO IMPORT Write ;
FROM NumberIO IMPORT WriteCard ;
FROM ASCII IMPORT nul, lf, cr, EOL ;
FROM StrLib IMPORT StrCopy, StrEqual, StrLen ;


CONST
   LineBuf = 1 ;
   Wrap    = LineBuf+1 ;
   eof     = 032C ;
   MaxStack= 10 ;

VAR
   f: File ;
   Opened        : BOOLEAN ;
   CurrentChar   : CHAR ;
   NextChar      : CHAR ;
   FileName      : ARRAY [0..MaxLine] OF CHAR ;
   Lines         : ARRAY [0..LineBuf] OF ARRAY [0..255] OF CHAR ;
                (* Need two lines since the delimiter of the CurrentSymbol *)
                (* maybe on the next line.                                 *)
   HighNext      : CARDINAL ;  (* Length of the NextChar line.             *)
   CurLine       : CARDINAL ;  (* Line number of the Current Char Line.    *)
   NextLine      : CARDINAL ;  (* Line number of the Next Char Line.       *)
   IndexCur      : CARDINAL ;  (* Index to the Lines array for Current Ln  *)
   IndexNext     : CARDINAL ;  (* Index to the Lines array for NextChar Ln *)
   CurSym        : CARDINAL ;  (* Character start of the CurrentSymbol     *)
   CurSymLine    : CARDINAL ;  (* Line number of the CurrentSymbol         *)
   CurCharIndex  : CARDINAL ;  (* Character number of CurChar.             *)
   NextCharIndex : CARDINAL ;  (* Character number of NextChar.            *)
   Eof           : BOOLEAN ;   (* End of source file.                      *)
   InQuotes      : BOOLEAN ;   (* If we are in quotes.                     *)
   QuoteChar     : CHAR ;      (* Quote character expected.                *)
   Stack         : ARRAY [0..MaxStack] OF ARRAY [0..255] OF CHAR ;
   StackPtr      : CARDINAL ;


(*
   IsSym - returns the result of the comparison between CurrentSymbol
           and Name.
*)

PROCEDURE IsSym (Name: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   RETURN( StrEqual(CurrentSymbol, Name) )
END IsSym ;


(*
   SymIs - if Name is equal to the CurrentSymbol the next Symbol is read
           and true is returned, otherwise false is returned.
*)

PROCEDURE SymIs (Name: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   IF StrEqual(CurrentSymbol, Name)
   THEN
      GetSymbol ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END SymIs ;


(*
   WriteError - displays the source line and points to the symbol in error.
                The message, a, is displayed.
*)

PROCEDURE WriteError (a: ARRAY OF CHAR) ;
VAR
   i: CARDINAL ;
BEGIN
   WriteString(FileName) ; Write(':') ; WriteCard(CurSymLine, 0) ; Write(':') ; WriteString(a) ;
   WriteLn ;
   WriteString( Lines[IndexCur] ) ; WriteLn ;
   i := CurSym ;
   WHILE i>0 DO
      Write(' ') ;
      DEC(i)
   END ;
   i := StrLen(CurrentSymbol) ;
   WHILE i>0 DO
      Write('^') ;
      DEC(i)
   END ;
   WriteLn ;
   WriteString(a) ; WriteLn ;
END WriteError ;


(*
   OpenSource - Attempts to open the source file, a.
                The success of the operation is returned.
*)

PROCEDURE OpenSource (a: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   f := OpenToRead(a) ;
   IF IsNoError(f)
   THEN
      StrCopy(a, FileName) ;
      Opened := TRUE ;
      Init ;
      RETURN( TRUE )
   ELSE
      Opened := FALSE ;
      Eof := TRUE ;
      RETURN( FALSE )
   END
END OpenSource ;


(*
   CloseSource - Closes the current open file.
*)

PROCEDURE CloseSource ;
BEGIN
   IF Opened=TRUE
   THEN
      Opened := FALSE ;
      Close( f )
   END
END CloseSource ;


(*
   GetSymbol - gets the next Symbol into CurrentSymbol.
*)

PROCEDURE GetSymbol ;
BEGIN
   StrCopy( CurrentSymbol, LastSymbol ) ;
   IF StackPtr>0
   THEN
      DEC(StackPtr) ;
      StrCopy( Stack[StackPtr], CurrentSymbol )
   ELSE
      ReadSymbol( CurrentSymbol )
   END
END GetSymbol ;


(*
   PutSymbol - pushes a symbol, Name, back onto the input.
               GetSymbol will set CurrentSymbol to, Name.
*)

PROCEDURE PutSymbol (Name: ARRAY OF CHAR) ;
BEGIN
   IF StackPtr=MaxStack
   THEN
      WriteError('Maximum push back symbol exceeded - Increase CONST MaxStack')
   ELSE
      StrCopy(Name, Stack[StackPtr]) ;
      INC(StackPtr)
   END
END PutSymbol ;


PROCEDURE ReadSymbol (VAR a: ARRAY OF CHAR) ;
VAR
   high,
   i    : CARDINAL ;
   ok   : BOOLEAN ;
BEGIN
   high := HIGH(a) ;
   IF NOT Eof
   THEN
      IF InQuotes
      THEN
         i := 0 ;
         IF CurrentChar=QuoteChar
         THEN
            InQuotes := FALSE ;
            a[i] := QuoteChar ;
            INC(i) ;
            AdvanceChar
         ELSE
            (* Fill in string or character *)
            i := 0 ;
            REPEAT
               a[i] := CurrentChar ;
               INC(i) ;
               AdvanceChar
            UNTIL (CurrentChar=QuoteChar) OR Eof OR (i>high) ;
         END
      ELSE
         (* Get rid of all excess spaces *)

         REPEAT
            IF CurrentChar=' '
            THEN
               WHILE (CurrentChar=' ') AND (NOT Eof) DO
                  AdvanceChar
               END ;
               ok := FALSE
            ELSIF (CurrentChar='(') AND (NextChar='*')
            THEN
      	       ConsumeComments ;
               ok := FALSE
            ELSE
              ok := TRUE
            END
         UNTIL ok ;
         i := 0 ;
         CurSym := CurCharIndex ;
         CurSymLine := CurLine ;
         IF (CurrentChar='"') OR (CurrentChar="'")
         THEN
            InQuotes := TRUE ;
            QuoteChar := CurrentChar ;
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i)
         ELSIF DoubleDelimiter()
         THEN
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i) ;
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i)
         ELSIF Delimiter()
         THEN
            a[i] := CurrentChar ;
            AdvanceChar ;
            INC(i)
         ELSE
            REPEAT
               a[i] := CurrentChar ;
               AdvanceChar ;
               INC(i)
            UNTIL Delimiter() OR (i>high) OR (CurrentChar=' ') OR Eof
         END
      END
   ELSE
      (* eof *)
      i := 0 ;
      a[i] := eof ;
      INC(i)
   END ;
   IF i<=HIGH(a)
   THEN
      a[i] := nul
   END
END ReadSymbol ;


(*
   ConsumeComments - consumes Modula-2 comments.
*)

PROCEDURE ConsumeComments ;
VAR
   Level: CARDINAL ;
BEGIN
   Level := 0 ;
   REPEAT
      IF (CurrentChar='(') AND (NextChar='*')
      THEN
         INC(Level)
      ELSIF (CurrentChar='*') AND (NextChar=')')
      THEN
         DEC(Level)
      END ;
      AdvanceChar ;
   UNTIL (Level=0) OR Eof ;
   AdvanceChar
END ConsumeComments;


(* Delimiter returns true if and only if CurrentChar is a delimiter *)

PROCEDURE Delimiter() : BOOLEAN ;
BEGIN
   IF (CurrentChar='-') OR
      (CurrentChar='+') OR (CurrentChar='*') OR (CurrentChar='\') OR
      (CurrentChar='|') OR (CurrentChar='(') OR (CurrentChar=')') OR
      (CurrentChar='"') OR (CurrentChar="'") OR (CurrentChar='{')
   THEN
      RETURN( TRUE )
   ELSIF
      (CurrentChar='}') OR (CurrentChar='[') OR (CurrentChar=']') OR
      (CurrentChar='#') OR (CurrentChar='=') OR (CurrentChar='<')
   THEN
      RETURN( TRUE )
   ELSIF
      (CurrentChar='>') OR (CurrentChar='.') OR (CurrentChar=';') OR
      (CurrentChar=':') OR (CurrentChar='^') OR (CurrentChar=',')
   THEN
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END Delimiter ;


PROCEDURE DoubleDelimiter () : BOOLEAN ;
BEGIN
   RETURN (
           ((CurrentChar='>') AND (NextChar='=')) OR
           ((CurrentChar='<') AND (NextChar='=')) OR
           ((CurrentChar='<') AND (NextChar='>')) OR
           ((CurrentChar=':') AND (NextChar='=')) OR
           ((CurrentChar='.') AND (NextChar='.'))
          )
END DoubleDelimiter ;


PROCEDURE AdvanceChar ;
BEGIN
   IF NOT Eof
   THEN
      CurrentChar := NextChar ;
      CurCharIndex := NextCharIndex ;
      IndexCur := IndexNext ;
      CurLine := NextLine ;
      IF CurrentChar=eof
      THEN
         Eof := TRUE
      ELSIF NextCharIndex=HighNext
      THEN
         IndexNext := (IndexCur+1) MOD Wrap ;
         HighNext := 0 ;
         REPEAT
            NextChar := ReadChar(f) ;
            IF NOT IsNoError(f)
            THEN
               NextChar := eof ;
               Lines[IndexNext][HighNext] := NextChar ;
               INC( HighNext )
            END ;
            WHILE (NextChar#eof) AND (NextChar#lf) AND (NextChar#cr) AND (HighNext<MaxLine) DO
               Lines[IndexNext][HighNext] := NextChar ;
               INC( HighNext ) ;
               NextChar := ReadChar(f) ;
               IF NOT IsNoError(f)
               THEN
                  NextChar := eof
               END
            END ;
            IF (NextChar=eof) OR (NextChar=lf) OR (NextChar=cr)
            THEN
               IF InQuotes
               THEN
                  Lines[IndexNext][HighNext] := ' ' ;  (* Space for delimiter *)
                  Lines[IndexNext][HighNext+1] := nul ;
                  WriteError('missing end of quote on this source line') ; HALT
               END ;
               INC( NextLine )
            END
         UNTIL HighNext>0 ;
         IF HighNext>=MaxLine THEN WriteError('Line too long') ; HALT END ;
         Lines[IndexNext][HighNext] := ' ' ;  (* Space for delimiter *)
         Lines[IndexNext][HighNext+1] := nul ;
         NextCharIndex := 0 ;
         NextChar := Lines[IndexNext][NextCharIndex]
      ELSE
         INC(NextCharIndex) ;
         NextChar := Lines[IndexNext][NextCharIndex]
      END
   END
END AdvanceChar ;


PROCEDURE Init ;
BEGIN
   StackPtr := 0 ;
   InQuotes := FALSE ;
   Eof := FALSE ;
   IndexCur := 1 ;
   IndexNext := 0 ;
   CurCharIndex := 0 ;
   Lines[IndexCur][0] := nul ;
   HighNext := 0 ;
   NextCharIndex := 0 ;
   CurLine := 1 ;
   NextLine := 1 ;
   CurrentChar := ' ' ;
   NextChar := ' ' ;
   StrCopy("", CurrentSymbol) ;
   StrCopy("", LastSymbol) ;
   IndexCur := IndexNext
END Init ;


BEGIN
   Init
END M2Lex.
