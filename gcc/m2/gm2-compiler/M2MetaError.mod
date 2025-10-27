(* M2MetaError.mod provides a set of high level error routines.

Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2MetaError ;


FROM M2Base IMPORT ZType, RType, IsPseudoBaseFunction, IsPseudoBaseProcedure ;
FROM NameKey IMPORT Name, KeyToCharStar, NulName ;
FROM StrLib IMPORT StrLen ;
FROM M2LexBuf IMPORT GetTokenNo, UnknownTokenNo ;
FROM M2Error IMPORT Error, NewError, NewWarning, NewNote, ErrorString, InternalError, ChainError, SetColor, FlushErrors, FlushWarnings ;
FROM FIO IMPORT StdOut, WriteLine ;
FROM SFIO IMPORT WriteS ;
FROM StringConvert IMPORT ctos ;
FROM M2Printf IMPORT printf1, printf0 ;
FROM M2Options IMPORT LowerCaseKeywords ;
FROM StrCase IMPORT Lower ;
FROM libc IMPORT printf ;
FROM SYSTEM IMPORT ADDRESS ;
FROM M2Error IMPORT MoveError ;
FROM M2Debug IMPORT Assert ;
FROM Storage IMPORT ALLOCATE ;

FROM Indexing IMPORT Index, InitIndex, KillIndex, GetIndice, PutIndice,
                     DeleteIndice, HighIndice ;

FROM DynamicStrings IMPORT String, InitString, InitStringCharStar,
                           ConCat, ConCatChar, Mark, string, KillString,
                           Dup, char, Length, Mult, EqualArray, Equal ;

FROM SymbolTable IMPORT NulSym,
                        IsDefImp, IsModule, IsInnerModule,
                        IsUnknown, IsType, IsProcedure, IsParameter,
                        IsParameterUnbounded, IsParameterVar, IsVarParam,
                        IsUnboundedParamAny, IsPointer, IsRecord, IsVarient,
                        IsFieldVarient, IsEnumeration, IsFieldEnumeration,
                        IsUnbounded, IsArray, IsRecordField, IsProcType,
                        IsVar, IsConst, IsConstString, IsConstLit, IsConstSet,
                        IsConstructor, IsDummy, IsTemporary, IsVarAParam,
                        IsSubscript, IsSubrange, IsSet, IsHiddenType,
                        IsError, GetSymName, GetScope, IsExported,
                        GetType, SkipType, GetDeclaredDef, GetDeclaredMod,
                        GetDeclaredFor, GetDeclaredModule,
                        GetDeclaredDefinition, GetScope,
                        GetFirstUsed, IsNameAnonymous, GetErrorScope,
                        GetVarDeclTok, GetVarDeclTypeTok, GetVarDeclFullTok ;

IMPORT M2ColorString ;
IMPORT M2Error ;


CONST
   MaxStack   = 10 ;
   Debugging  = FALSE ;
   ColorDebug = FALSE ;

TYPE
   GetTokProcedure = PROCEDURE (CARDINAL) : CARDINAL ;

   errorType = (none, error, warning, note, chained, aborta) ;
   colorType = (unsetColor, noColor, quoteColor, filenameColor, errorColor,
                warningColor, noteColor, keywordColor, locusColor,
                insertColor, deleteColor, typeColor, range1Color, range2Color) ;

   errorBlock = RECORD
                   useError  : BOOLEAN ;
                   e         : Error ;
                   type      : errorType ;
                   out, in   : String ;
                   highplus1 : CARDINAL ;
                   len,
                   ini       : INTEGER ;
                   glyph,
                   chain,
                   root,
                   quotes,
                   positive  : BOOLEAN ;
                   currentCol,
                   beginCol,                (* the color at the start of the string.          *)
                   endCol    : colorType ;  (* the color at the end of the text before.       *)
                   colorStack: ARRAY [0..MaxStack] OF colorType ;
                   stackPtr  : CARDINAL ;
                END ;


    dictionaryEntry = POINTER TO RECORD
                                    key,
                                    value: String ;
                                    next : dictionaryEntry ;
                                 END ;


VAR
   lastRoot  : Error ;
   lastColor : colorType ;
   seenAbort : BOOLEAN ;
   dictionary : Index ;
   outputStack: Index ;
   freeEntry  : dictionaryEntry ;


(*
   pushOutput -
*)

PROCEDURE pushOutput (VAR eb: errorBlock) ;
BEGIN
   PutIndice (outputStack, HighIndice (outputStack)+1, eb.out) ;
   eb.out := InitString ('') ;
   eb.glyph := FALSE
END pushOutput ;


(*
   readWord - reads and returns a word delimited by '}' it uses '%' as
              the escape character.
*)

PROCEDURE readWord (VAR eb: errorBlock) : String ;
VAR
   word: String ;
BEGIN
   word := InitString ('') ;
   WHILE (eb.ini < eb.len) AND (char (eb.in, eb.ini) # "}") DO
      IF char (eb.in, eb.ini) = "%"
      THEN
         INC (eb.ini)
      END ;
      word := ConCatChar (word, char (eb.in, eb.ini)) ;
      INC (eb.ini)
   END ;
   RETURN word
END readWord ;


(*
   addEntry -
*)

PROCEDURE addEntry (key, value: String) ;
VAR
   e: dictionaryEntry ;
   s: String ;
   i: CARDINAL ;
BEGIN
   s := lookupString (key) ;
   IF s = NIL
   THEN
      e := newEntry () ;
      e^.key := key ;
      e^.value := value ;
      PutIndice (dictionary, HighIndice (dictionary)+1, e)
   ELSE
      i := 1 ;
      WHILE i <= HighIndice (dictionary) DO
         e := GetIndice (dictionary, i) ;
         IF Equal (e^.key, key)
         THEN
            e^.value := KillString (e^.value) ;
            e^.value := value ;
            RETURN
         END ;
         INC (i)
      END
   END
END addEntry ;


(*
   popOutput -
*)

PROCEDURE popOutput (VAR eb: errorBlock) ;
VAR
   key,
   previous: String ;
BEGIN
   IF HighIndice (outputStack) >= 1
   THEN
      previous := GetIndice (outputStack, HighIndice (outputStack)) ;
      DeleteIndice (outputStack, HighIndice (outputStack)) ;
      key := readWord (eb) ;
      addEntry (key, eb.out) ;
      eb.out := previous
   END
END popOutput ;


(*
   newEntry -
*)

PROCEDURE newEntry () : dictionaryEntry ;
VAR
   e: dictionaryEntry ;
BEGIN
   IF freeEntry = NIL
   THEN
      NEW (e)
   ELSE
      e := freeEntry ;
      freeEntry := freeEntry^.next
   END ;
   WITH e^ DO
      key := NIL ;
      value := NIL ;
      next := NIL
   END ;
   RETURN e
END newEntry ;


(*
   killEntry - dispose e and delete any strings.
*)

PROCEDURE killEntry (e: dictionaryEntry) ;
BEGIN
   e^.next := freeEntry ;
   freeEntry := e ;
   IF e^.key # NIL
   THEN
      e^.key := KillString (e^.key)
   END ;
   IF e^.value # NIL
   THEN
      e^.value := KillString (e^.value)
   END
END killEntry ;


(*
   resetDictionary - remove all entries in the dictionary.
*)

PROCEDURE resetDictionary ;
VAR
   i: CARDINAL ;
   e: dictionaryEntry ;
BEGIN
   i := 1 ;
   WHILE i <= HighIndice (dictionary) DO
      e := GetIndice (dictionary, i) ;
      killEntry (e) ;
      INC (i)
   END ;
   dictionary := KillIndex (dictionary) ;
   dictionary := InitIndex (1)
END resetDictionary ;


(*
   lookupString - lookup and return a duplicate of the string value for key s.
                  NIL is returned if the key s is unknown.
*)

PROCEDURE lookupString (s: String) : String ;
VAR
   i: CARDINAL ;
   e: dictionaryEntry ;
BEGIN
   i := 1 ;
   WHILE i <= HighIndice (dictionary) DO
      e := GetIndice (dictionary, i) ;
      IF Equal (e^.key, s)
      THEN
         RETURN Dup (e^.value)
      END ;
      INC (i)
   END ;
   RETURN NIL
END lookupString ;


(*
   lookupDefine - looks up the word in the input string (ending with '}').
                  It uses this word as a key into the dictionary and returns
                  the entry.
*)

PROCEDURE lookupDefine (VAR eb: errorBlock) : String ;
VAR
   s: String ;
BEGIN
   s := InitString ('') ;
   WHILE (eb.ini < eb.len) AND (char (eb.in, eb.ini) # "}") DO
      IF char (eb.in, eb.ini) = "%"
      THEN
         INC (eb.ini)
      END ;
      s := ConCatChar (s, char (eb.in, eb.ini)) ;
      INC (eb.ini)
   END ;
   s := lookupString (s) ;
   IF s = NIL
   THEN
      s := InitString ('')
   END ;
   RETURN s
END lookupDefine ;


(*
   processDefine - place contents of dictionary entry name onto the output string.
*)

PROCEDURE processDefine (VAR eb: errorBlock) ;
BEGIN
   eb.out := ConCat (eb.out, lookupDefine (eb))
END processDefine ;


(*
   lookupColor - looks up the color enum from the string.
*)

PROCEDURE lookupColor (s: String) : colorType ;
BEGIN
   IF EqualArray (s, "filename")
   THEN
      RETURN filenameColor
   ELSIF EqualArray (s, "quote")
   THEN
      RETURN quoteColor
   ELSIF EqualArray (s, "error")
   THEN
      RETURN errorColor
   ELSIF EqualArray (s, "warning")
   THEN
      RETURN warningColor ;
   ELSIF EqualArray (s, "note")
   THEN
      RETURN warningColor ;
   ELSIF EqualArray (s, "locus")
   THEN
      RETURN locusColor
   ELSIF EqualArray (s, "insert")
   THEN
      RETURN insertColor
   ELSIF EqualArray (s, "delete")
   THEN
      RETURN deleteColor
   ELSIF EqualArray (s, "type")
   THEN
      RETURN typeColor
   ELSIF EqualArray (s, "range1")
   THEN
      RETURN range1Color
   ELSIF EqualArray (s, "range2")
   THEN
      RETURN range2Color
   END ;
   RETURN noColor
END lookupColor ;


(*
   readColor -
*)

PROCEDURE readColor (VAR eb: errorBlock) : colorType ;
VAR
   s: String ;
   c: colorType ;
BEGIN
   s := InitString ('') ;
   WHILE (eb.ini < eb.len) AND (char (eb.in, eb.ini) # "}") DO
      IF char (eb.in, eb.ini) = "%"
      THEN
         INC (eb.ini)
      END ;
      s := ConCatChar (s, char (eb.in, eb.ini)) ;
      INC (eb.ini)
   END ;
   c := lookupColor (s) ;
   s := KillString (s) ;
   RETURN c
END readColor ;


(*
   keyword - copy characters until the '}' in the input string and convert them to
             the keyword color/font.
*)

PROCEDURE keyword (VAR eb: errorBlock) ;
BEGIN
   IF CAP (char (eb.in, eb.ini)) = 'K'
   THEN
      INC (eb.ini) ;
      pushColor (eb) ;
      changeColor (eb, keywordColor) ;
      WHILE (eb.ini < eb.len) AND (char (eb.in, eb.ini) # "}") DO
         IF Debugging
         THEN
            dump (eb)
         END ;
         IF char (eb.in, eb.ini) = "%"
         THEN
            INC (eb.ini)
         END ;
         copyKeywordChar (eb) ;
         INC (eb.ini)
      END ;
      popColor (eb)
   ELSE
      InternalError ('expecting index to be on the K for keyword')
   END
END keyword ;


(*
   filename - copy characters until the '}' in the input string and convert them to
              the filename color/font.
*)

PROCEDURE filename (VAR eb: errorBlock) ;
BEGIN
   IF CAP (char (eb.in, eb.ini)) = 'F'
   THEN
      INC (eb.ini) ;
      pushColor (eb) ;
      changeColor (eb, filenameColor) ;
      WHILE (eb.ini < eb.len) AND (char (eb.in, eb.ini) # "}") DO
         IF Debugging
         THEN
            dump (eb)
         END ;
         IF char (eb.in, eb.ini) = "%"
         THEN
            INC (eb.ini)
         END ;
         copyChar (eb) ;
         INC (eb.ini)
      END ;
      popColor (eb)
   ELSE
      InternalError ('expecting index to be on the F for filename')
   END
END filename ;


(*
   pushColor -
*)

PROCEDURE pushColor (VAR eb: errorBlock) ;
BEGIN
   WITH eb DO
      IF stackPtr > MaxStack
      THEN
         HALT
      ELSE
         colorStack[stackPtr] := currentCol ;
         INC (stackPtr)
      END
   END
END pushColor ;


(*
   popColor -
*)

PROCEDURE popColor (VAR eb: errorBlock) ;
BEGIN
   WITH eb DO
      IF stackPtr > 0
      THEN
         DEC (stackPtr)
      ELSE
         HALT
      END ;
      currentCol := colorStack[stackPtr] ;
      IF currentCol = unsetColor
      THEN
         currentCol := noColor
      END
   END
END popColor ;


(*
   initErrorBlock - initialise an error block with the, input, string.
*)

PROCEDURE initErrorBlock (VAR eb: errorBlock; input: String; sym: ARRAY OF CARDINAL) ;
BEGIN
   WITH eb DO
      useError   := TRUE ;
      e          := NIL ;
      type       := error ;  (* default to the error color.  *)
      out        := InitString ('') ;
      in         := input ;
      highplus1  := HIGH (sym) + 1 ;
      len        := Length (input) ;
      ini        := 0 ;
      glyph      := FALSE ;  (* nothing to output yet.  *)
      quotes     := TRUE ;
      positive   := TRUE ;
      root       := FALSE ;
      chain      := FALSE ;
      currentCol := findColorType (input) ;
      beginCol   := unsetColor ;
      endCol     := unsetColor ;
      stackPtr   := 0
   END
END initErrorBlock ;


(*
   push - performs a push from the oldblock to the newblock.
          It copies all fields except the output string.
*)

PROCEDURE push (VAR newblock: errorBlock; oldblock: errorBlock) ;
BEGIN
   pushColor (oldblock) ;  (* save the current color.  *)
   newblock := oldblock ;  (* copy all the fields.  *)
   newblock.out := NIL ;  (* must do this before a clear as we have copied the address.  *)
   clear (newblock) ;
   newblock.quotes := TRUE
END push ;


(*
   pop - copies contents of oldblock into newblock.  It only copies the error
         handle if the toblock.e is NIL.
*)

PROCEDURE pop (VAR toblock, fromblock: errorBlock) ;
VAR
   c: colorType ;
BEGIN
   IF empty (fromblock)
   THEN
      toblock.stackPtr := fromblock.stackPtr ;
      toblock.colorStack := fromblock.colorStack ;
      popColor (toblock)   (* and restore the color from the push start.  *)
   ELSE
      IF fromblock.quotes
      THEN
         (* string needs to be quoted.  *)
         IF toblock.currentCol = unsetColor
         THEN
            (* caller has not yet assigned a color, so use the callee color at the end.  *)
            OutOpenQuote (toblock) ;
            OutGlyphS (toblock, fromblock.out) ;
            OutCloseQuote (toblock) ;
            changeColor (toblock, fromblock.currentCol)
         ELSE
            shutdownColor (fromblock) ;
            (* caller has assigned a color, so use it after the new string.  *)
            c := toblock.currentCol ;
            OutOpenQuote (toblock) ;
            OutGlyphS (toblock, fromblock.out) ;
            OutCloseQuote (toblock) ;
            toblock.currentCol := c
         END
      ELSE
         IF toblock.currentCol = unsetColor
         THEN
            OutGlyphS (toblock, fromblock.out) ;
            toblock.endCol := fromblock.endCol ;
            changeColor (toblock, fromblock.endCol)
         ELSE
            pushColor (toblock) ;
            OutGlyphS (toblock, fromblock.out) ;
            toblock.endCol := fromblock.endCol ;
            popColor (toblock)
         END
      END
   END ;
   IF toblock.e = NIL
   THEN
      toblock.e := fromblock.e
   END ;
   toblock.chain := fromblock.chain ;
   toblock.root := fromblock.root ;
   toblock.ini := fromblock.ini ;
   toblock.type := fromblock.type   (* might have been changed by the callee.  *)
END pop ;


(*
   OutOpenQuote -
*)

PROCEDURE OutOpenQuote (VAR eb: errorBlock) ;
BEGIN
   eb.currentCol := noColor ;
   flushColor (eb) ;
   eb.out := ConCat (eb.out, openQuote (InitString ('')))
END OutOpenQuote ;


(*
   OutCloseQuote -
*)

PROCEDURE OutCloseQuote (VAR eb: errorBlock) ;
BEGIN
   eb.out := ConCat (eb.out, closeQuote (InitString (''))) ;
   eb.currentCol := noColor ;
   eb.endCol := noColor
END OutCloseQuote ;


(*
   findColorType - return the color of the string.  This is determined by the first
                   occurrance of an error, warning or note marker.  An error message
                   is assumed to either be: a keyword category, error category, note
                   category, warning category or to be chained from a previous error.
*)

PROCEDURE findColorType (s: String) : colorType ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE i < Length (s) DO
      IF char (s, i) = "{"
      THEN
         INC (i) ;
         IF char (s, i) = "%"
         THEN
            INC (i) ;
            WHILE (i < Length (s)) AND (char (s, i) # "}") DO
               IF char (s, i) = "%"
               THEN
                  INC (i)
               END ;
               CASE char (s, i) OF

               "K":  RETURN errorColor |   (* keyword errors start with the fatal error color.  *)
               "E":  RETURN errorColor |
               "A":  RETURN errorColor |
               "O":  RETURN noteColor |
               "W":  RETURN warningColor |
               "C":  RETURN lastColor

               ELSE
               END ;
               INC (i)
            END
         END
      END ;
      INC (i)
   END ;
   RETURN errorColor  (* default to the error color.  *)
END findColorType ;


(*
   killErrorBlock - deallocates the dynamic strings associated with the error block.
*)

PROCEDURE killErrorBlock (VAR eb: errorBlock) ;
BEGIN
   WITH eb DO
      out := KillString (out) ;
      in := KillString (in)
   END
END killErrorBlock ;


(*
   ebnf := { percent
             | lbra
             | any                  % copy ch %
           }
         =:

   percent := '%' ( "<" |           % open quote
                    ">" |           % close quote
                    anych )         % copy anych %
            =:

   lbra := '{' [ '!' ] percenttoken '}' =:

   percenttoken := '%' (
                         '1'        % doOperand(1) %
                             op
                       | '2'        % doOperand(2) %
                             op
                       | '3'        % doOperand(3) %
                             op
                       | '4'        % doOperand(4) %
                             op
                       )
                 =:

   op := {'a'|'q'|'t'|'d'|'n'|'s'|'B'|'D'|'F'|'G'|'H'|'M'|'U'|'E'|'V'|'W'|'A'} then =:

   then := [ ':' ebnf ] =:
*)

(*
    {%1V}     set the error message location to the name of the symbol declared.
              For example foo: bar
                          ^^^  some error message.
    {%1H}     set the error message location to the whole declaration of the symbol.
              For example foo: bar
                          ^^^^^^^^ some error message.
    {%1B}     set the error message location to the type declaration of the symbol.
              For example foo: bar
                               ^^^ some error message.
*)


(*
   InternalFormat - produces an informative internal error.
*)

PROCEDURE InternalFormat (eb: errorBlock; m: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   printf1 ("M2MetaError.mod:%d:internalformat error detected\n", line) ;
   dump (eb) ;
   InternalError (m)
END InternalFormat ;


(*
   x - checks to see that a=b.
*)

PROCEDURE x (a, b: String) : String ;
BEGIN
   IF a # b
   THEN
      InternalError ('different string returned')
   END ;
   RETURN a
END x ;


(*
   IsWhite - returns TRUE if, ch, is a space.
*)

PROCEDURE IsWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN ch = ' '
END IsWhite ;


(*
   skip - skips over this level input until the next '}'.
*)

PROCEDURE skip (VAR sb: errorBlock) ;
VAR
   level: INTEGER ;
BEGIN
   level := 0 ;
   WHILE sb.ini < sb.len DO
      IF (level = 0) AND (char (sb.in, sb.ini) = "}")
      THEN
         RETURN
      END ;
      IF char (sb.in, sb.ini) = "}"
      THEN
         DEC (level)
      ELSIF char (sb.in, sb.ini) = "{"
      THEN
         INC (level)
      END ;
      INC (sb.ini)
   END
END skip ;


(*
   ifNonNulThen := [ ':' ebnf ] =:
*)

PROCEDURE ifNonNulThen (VAR eb: errorBlock;
                        sym: ARRAY OF CARDINAL) ;
BEGIN
   IF char (eb.in, eb.ini) = ':'
   THEN
      INC (eb.ini) ;
      IF eb.positive
      THEN
         IF empty (eb) AND (Length (eb.out) # 0)
         THEN
            printf0 ("inconsistency found\n") ;
            dump (eb)
         END ;
         IF empty (eb)
         THEN
            IF Debugging
            THEN
               printf0 ("empty expression, skip\n")
            END ;
            clear (eb) ;
            (* skip over this level of input text.  *)
            skip (eb)
         ELSE
            IF Debugging
            THEN
               dump (eb) ;
               printf0 ("non empty expression, clear and continue\n") ;
            END ;
            clear (eb) ;
            IF Debugging
            THEN
               dump (eb) ;
               printf0 ("cleared, continue\n") ;
               dump (eb)
            END ;
            (* carry on processing input text.  *)
            ebnf (eb, sym) ;
            IF Debugging
            THEN
               printf0 ("evaluated\n") ;
               dump (eb)
            END
         END
      ELSE
         IF empty (eb)
         THEN
            clear (eb) ;
            (* carry on processing input text.  *)
            ebnf (eb, sym)
         ELSE
            clear (eb) ;
            (* skip over this level of input text.  *)
            skip (eb)
         END
      END ;
      IF (eb.ini < eb.len) AND (char (eb.in, eb.ini) # '}')
      THEN
         InternalFormat (eb, 'expecting to see }', __LINE__)
      END
   END
END ifNonNulThen ;


(*
   doNumber -
*)

PROCEDURE doNumber (VAR eb: errorBlock;
                    sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF empty (eb)
   THEN
      eb.quotes := FALSE ;
      OutGlyphS (eb, ctos (sym[bol], 0, ' '))
   END
END doNumber ;


(*
   doCount -
*)

PROCEDURE doCount (VAR eb: errorBlock;
                   sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF empty (eb)
   THEN
      eb.quotes := FALSE ;
      OutGlyphS (eb, ctos(sym[bol], 0, ' ')) ;
      CASE sym[bol] MOD 100 OF

      11..13:  OutGlyphS (eb, Mark (InitString ('th')))

      ELSE
         CASE sym[bol] MOD 10 OF

         1:  OutGlyphS (eb, Mark (InitString ('st'))) |
         2:  OutGlyphS (eb, Mark (InitString ('nd'))) |
         3:  OutGlyphS (eb, Mark (InitString ('rd')))

         ELSE
            OutGlyphS (eb, Mark (InitString ('th')))
         END
      END
   END
END doCount ;


PROCEDURE doAscii (VAR eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF (sym[bol] = NulSym) OR (NOT empty (eb)) OR IsTemporary (sym[bol]) OR IsNameAnonymous (sym[bol])
   THEN
      RETURN
   ELSE
      OutGlyphS (eb, InitStringCharStar (KeyToCharStar (GetSymName (sym[bol]))))
   END
END doAscii ;


(*
   unquotedKeyword -
*)

PROCEDURE unquotedKeyword (VAR eb: errorBlock) ;
BEGIN
   eb.quotes := FALSE ;
   keyword (eb)
END unquotedKeyword ;


(*
   OutArray -
*)

PROCEDURE OutArray (VAR eb: errorBlock; a: ARRAY OF CHAR) ;
BEGIN
   OutGlyphS (eb, Mark (InitString (a)))
END OutArray ;


(*
   OutGlyphS - outputs a string of glyphs.
*)

PROCEDURE OutGlyphS (VAR eb: errorBlock; s: String) ;
BEGIN
   IF Length (s) > 0
   THEN
      flushColor (eb) ;
      checkMe ;
      eb.glyph := TRUE ;
      eb.out := ConCat (eb.out, s)
   END
END OutGlyphS ;


(*
   OutColorS - outputs a string of color requests.
*)

(*
PROCEDURE OutColorS (VAR eb: errorBlock; s: String) ;
BEGIN
   flushColor (eb) ;
   eb.out := ConCat (eb.out, s)
END OutColorS ;
*)


(*
   empty - returns TRUE if the output string is empty.
           It ignores color changes.
*)

PROCEDURE empty (VAR eb: errorBlock) : BOOLEAN ;
BEGIN
   RETURN NOT eb.glyph
END empty ;


(*
   clear - remove the output string.
*)

PROCEDURE clear (VAR eb: errorBlock) ;
BEGIN
   eb.out := KillString (eb.out) ;
   eb.out := InitString ('') ;
   eb.glyph := FALSE ;
   eb.beginCol := unsetColor ;
   eb.quotes := FALSE
END clear ;


PROCEDURE doName (VAR eb: errorBlock;
                  sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF (NOT empty (eb)) OR (sym[bol] = NulSym) OR IsTemporary (sym[bol]) OR IsNameAnonymous (sym[bol])
   THEN
      RETURN
   ELSE
      IF sym[bol] = ZType
      THEN
         eb.quotes := FALSE ;
         OutArray (eb, 'the ZType')
      ELSIF sym[bol] = RType
      THEN
         eb.quotes := FALSE ;
         OutArray (eb, 'the RType')
      ELSE
         doAscii (eb, sym, bol)
      END
   END
END doName ;


PROCEDURE doQualified (VAR eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
VAR
   mod: ARRAY [0..1] OF CARDINAL ;
BEGIN
   IF (NOT empty (eb)) OR (sym[bol] = NulSym) OR IsTemporary (sym[bol]) OR IsNameAnonymous (sym[bol])
   THEN
      RETURN
   ELSE
      mod[0] := GetScope (sym[bol]) ;
      IF IsDefImp (mod[0]) AND IsExported (mod[0], sym[bol])
      THEN
         doAscii (eb, mod, 0) ;
         OutArray (eb, '.') ;
         OutGlyphS (eb, Mark (InitStringCharStar (KeyToCharStar (GetSymName (sym[bol])))))
      ELSE
         doAscii (eb, sym, bol)
      END
   END
END doQualified ;


(*
   doType - returns a string containing the type name of
            sym.
*)

PROCEDURE doType (VAR eb: errorBlock;
                  sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF (NOT empty (eb)) OR (sym[bol] = NulSym)
   THEN
      RETURN
   ELSE
      sym[bol] := GetType (sym[bol]) ;
      doAscii (eb, sym, bol)
   END
END doType ;


(*
   doSkipType - will skip all pseudonym types.  It also
                returns the type symbol found and name.
*)

PROCEDURE doSkipType (eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF (NOT empty (eb)) OR (sym[bol] = NulSym)
   THEN
      RETURN
   ELSE
      sym[bol] := SkipType(sym[bol]) ;
      WHILE IsType(sym[bol]) AND ((GetSymName (sym[bol]) = NulName) OR
                                  IsNameAnonymous (sym[bol])) DO
         sym[bol] := GetType (sym[bol])
      END ;
      doAscii (eb, sym, bol)
   END
END doSkipType ;


(*
   doGetType - attempts to get the type of sym[bol].
*)

PROCEDURE doGetType (VAR eb: errorBlock;
                     VAR sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF (bol > HIGH (sym)) OR (NOT empty (eb)) OR (sym[bol] = NulSym)
   THEN
      RETURN
   ELSE
      sym[bol] := GetType (sym[bol])
   END
END doGetType ;


(*
   doGetSkipType - will skip all pseudonym types.  It also
                   returns the type symbol found and name.
*)

PROCEDURE doGetSkipType (VAR eb: errorBlock; VAR sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
VAR
   prev: CARDINAL ;
BEGIN
   IF (bol > HIGH (sym)) OR (NOT empty (eb)) OR (sym[bol] = NulSym)
   THEN
      RETURN
   ELSE
      REPEAT
         prev := sym[bol] ;
         sym[bol] := SkipType (sym[bol]) ;
         IF IsType(sym[bol]) AND ((GetSymName (sym[bol]) = NulName) OR
                                  IsNameAnonymous (sym[bol])) AND
            (GetType(sym[bol]) # NulSym)
         THEN
            sym[bol] := GetType (sym[bol])
         END
      UNTIL sym[bol] = prev
   END
END doGetSkipType ;


(*
   doChain -
*)

PROCEDURE doChain (VAR eb: errorBlock; tok: CARDINAL) ;
BEGIN
   IF lastRoot=NIL
   THEN
      InternalError ('should not be chaining an error onto an empty error note')
   ELSE
      eb.e := ChainError (tok, lastRoot)
   END
END doChain ;


(*
   doError - creates and returns an error note.
*)

PROCEDURE doError (VAR eb: errorBlock; tok: CARDINAL) ;
BEGIN
   IF eb.useError
   THEN
      chooseError (eb, tok)
   END
END doError ;


(*
   defaultError - adds the default error location to, tok, if one has not already been
                  assigned.
*)

PROCEDURE defaultError (VAR eb: errorBlock; tok: CARDINAL) ;
BEGIN
   IF eb.e = NIL
   THEN
      doError (eb, tok)
   END
END defaultError ;


(*
   chooseError - choose the error kind dependant upon type.
                 Either an error, warning or note will be generated.
*)

PROCEDURE chooseError (VAR eb: errorBlock; tok: CARDINAL) ;
BEGIN
   IF eb.chain
   THEN
      doChain (eb, tok)
   ELSE
      CASE eb.type OF

      chained:  doChain (eb, tok) |
      none,
      aborta,
      error  :  IF eb.e=NIL
                THEN
                   eb.e := NewError (tok)
                ELSE
                   eb.e := MoveError (eb.e, tok)
                END |
      warning:  IF eb.e=NIL
                THEN
                   eb.e := NewWarning (tok)
                ELSE
                   eb.e := MoveError (eb.e, tok)
                END |
      note   :  IF eb.e=NIL
                THEN
                   eb.e := NewNote (tok)
                ELSE
                   eb.e := MoveError (eb.e, tok)
                END

      ELSE
         InternalError ('unexpected enumeration value')
      END
   END ;
   IF eb.root
   THEN
      lastRoot := eb.e ;
      lastColor := findColorType (eb.in)
   END ;
   eb.e := SetColor (eb.e)
END chooseError ;


(*
   doErrorScopeModule -
*)

PROCEDURE doErrorScopeModule (VAR eb: errorBlock; sym: CARDINAL) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetScope (sym) ;
   IF IsModule (scope)
   THEN
      IF IsInnerModule (scope)
      THEN
         doError (eb, GetDeclaredMod (sym))
      ELSE
         doError (eb, GetDeclaredMod (sym))
      END
   ELSE
      Assert (IsDefImp (scope)) ;
      (* if this fails then we need to skip to the outer scope.
         REPEAT
            OuterModule := GetScope(OuterModule)
         UNTIL GetScope(OuterModule)=NulSym.  *)
      IF GetDeclaredModule (sym) = UnknownTokenNo
      THEN
         doError (eb, GetDeclaredDef (sym))
      ELSE
         doError (eb, GetDeclaredMod (sym))
      END
   END
END doErrorScopeModule ;


(*
   doErrorScopeForward -
*)

PROCEDURE doErrorScopeForward (VAR eb: errorBlock; sym: CARDINAL) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetScope (sym) ;
   IF IsModule (scope)
   THEN
      IF IsInnerModule (scope)
      THEN
         doError (eb, GetDeclaredFor (sym))
      ELSE
         doError (eb, GetDeclaredFor (sym))
      END
   ELSE
      Assert (IsDefImp (scope)) ;
      (* if this fails then we need to skip to the outer scope.
         REPEAT
            OuterModule := GetScope(OuterModule)
         UNTIL GetScope(OuterModule)=NulSym.  *)
      IF GetDeclaredModule (sym) = UnknownTokenNo
      THEN
         doError (eb, GetDeclaredDef (sym))
      ELSE
         doError (eb, GetDeclaredFor (sym))
      END
   END
END doErrorScopeForward ;


(*
   doErrorScopeMod - potentially create an error referring to the definition
                     module, fall back to the implementation or program module if
                     there is no declaration in the definition module.
*)

PROCEDURE doErrorScopeMod (VAR eb: errorBlock; sym: CARDINAL) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetScope (sym) ;
   IF scope = NulSym
   THEN
      M2Error.EnterErrorScope (NIL) ;
      doError (eb, GetDeclaredMod (sym))
   ELSE
      M2Error.EnterErrorScope (GetErrorScope (scope)) ;
      IF IsProcedure (scope)
      THEN
         doError (eb, GetDeclaredMod (sym))
      ELSE
         doErrorScopeModule (eb, sym)
      END
   END ;
   M2Error.LeaveErrorScope
END doErrorScopeMod ;


(*
   doErrorScopeFor - potentially create an error referring to the
                     forward declaration, definition module, fall back
                     to the implementation or program module if
                     there is no declaration in the definition module.
*)

PROCEDURE doErrorScopeFor (VAR eb: errorBlock; sym: CARDINAL) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetScope (sym) ;
   IF scope = NulSym
   THEN
      M2Error.EnterErrorScope (NIL) ;
      doError (eb, GetDeclaredFor (sym))
   ELSE
      M2Error.EnterErrorScope (GetErrorScope (scope)) ;
      IF IsProcedure (scope)
      THEN
         doError (eb, GetDeclaredFor (sym))
      ELSE
         doErrorScopeForward (eb, sym)
      END
   END ;
   M2Error.LeaveErrorScope
END doErrorScopeFor ;


(*
   doDeclaredMod - creates an error note where sym[bol] was declared.
*)

PROCEDURE declaredMod (VAR eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF bol <= HIGH (sym)
   THEN
      doErrorScopeMod (eb, sym[bol])
   END
END declaredMod ;


(*
   doErrorScopeDefinition - use the declaration in the definitio module if one is available.
*)

PROCEDURE doErrorScopeDefinition (VAR eb: errorBlock; sym: CARDINAL) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetScope (sym) ;
   IF IsModule (scope)
   THEN
      (* No definition module for a program module.  *)
      doError (eb, GetDeclaredMod (sym))
   ELSE
      Assert (IsDefImp (scope)) ;
      IF GetDeclaredDefinition (sym) = UnknownTokenNo
      THEN
         (* Fall back to the implementation module if no declaration exists
            in the definition module.  *)
         doError (eb, GetDeclaredMod (sym))
      ELSE
         doError (eb, GetDeclaredDef (sym))
      END
   END
END doErrorScopeDefinition ;


(*
   doErrorScopeDef - potentially create an error referring to the definition
                     module, fall back to the implementation or program module if
                     there is no declaration in the definition module.
*)

PROCEDURE doErrorScopeDef (VAR eb: errorBlock; sym: CARDINAL) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetScope (sym) ;
   IF scope = NulSym
   THEN
      M2Error.EnterErrorScope (NIL) ;
      doError (eb, GetDeclaredFor (sym))
   ELSE
      M2Error.EnterErrorScope (GetErrorScope (scope)) ;
      IF IsProcedure (scope)
      THEN
         doError (eb, GetDeclaredDef (sym))
      ELSE
         doErrorScopeDefinition (eb, sym)
      END
   END ;
   M2Error.LeaveErrorScope
END doErrorScopeDef ;


(*
   doDeclaredDef - creates an error note where sym[bol] was declared.
*)

PROCEDURE declaredDef (VAR eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF bol <= HIGH (sym)
   THEN
      doErrorScopeDef (eb, sym[bol])
   END
END declaredDef ;


(*
   doDeclaredFor - creates an error note where sym[bol] was declared.
*)

PROCEDURE declaredFor (VAR eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF bol <= HIGH (sym)
   THEN
      doErrorScopeFor (eb, sym[bol])
   END
END declaredFor ;


(*
   doErrorScopeProc - determine the location for the error or warning from
                      the default declaration.  For example parameters can be
                      declared in definition, forward or in modules (proper procedure).
                      Use GetVarParamTok to obtain a variable or parameter location.
*)

PROCEDURE doErrorScopeProc (VAR eb: errorBlock; sym: CARDINAL;
                            GetVarParamTok: GetTokProcedure) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetScope (sym) ;
   IF scope = NulSym
   THEN
      M2Error.EnterErrorScope (NIL) ;
      doError (eb, GetDeclaredDef (sym))
   ELSE
      M2Error.EnterErrorScope (GetErrorScope (scope)) ;
      IF IsVar (sym) OR IsParameter (sym)
      THEN
         doError (eb, GetVarParamTok (sym))
      ELSIF IsProcedure (scope)
      THEN
         doError (eb, GetDeclaredDef (sym))
      ELSIF IsModule (scope)
      THEN
         doError (eb, GetDeclaredMod (sym))
      ELSE
         Assert (IsDefImp (scope)) ;
         IF GetDeclaredDefinition (sym) = UnknownTokenNo
         THEN
            doError (eb, GetDeclaredMod (sym))
         ELSE
            doError (eb, GetDeclaredDef (sym))
         END
      END
   END ;
   M2Error.LeaveErrorScope
END doErrorScopeProc ;


(*
   doDeclaredVar - creates an error note where sym[bol] was declared.
*)

PROCEDURE declaredVar (VAR eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF bol <= HIGH (sym)
   THEN
      doErrorScopeProc (eb, sym[bol], GetVarDeclTok)
   END
END declaredVar ;


(*
   doDeclaredType - creates an error note where sym[bol] was declared.
*)

PROCEDURE declaredType (VAR eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF bol <= HIGH (sym)
   THEN
      doErrorScopeProc (eb, sym[bol], GetVarDeclTypeTok)
   END
END declaredType ;


(*
   doDeclaredFull - creates an error note where sym[bol] was declared.
*)

PROCEDURE declaredFull (VAR eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF bol <= HIGH (sym)
   THEN
      doErrorScopeProc (eb, sym[bol], GetVarDeclFullTok)
   END
END declaredFull ;


(*
   used - creates an error note where sym[bol] was first used.
*)

PROCEDURE used (VAR eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF bol <= HIGH (sym)
   THEN
      doError (eb, GetFirstUsed (sym[bol]))
   END
END used ;


(*
   ConCatWord - joins sentances, a, b, together.
*)

(*
PROCEDURE ConCatWord (a, b: String) : String ;
BEGIN
   IF (Length (a) = 1) AND (char(a, 0) = 'a')
   THEN
      a := x (a, ConCatChar (a, 'n'))
   ELSIF (Length (a) > 1) AND (char (a, -1) = 'a') AND IsWhite (char(a, -2))
   THEN
      a := x (a, ConCatChar (a, 'n'))
   END ;
   IF (Length (a) > 0) AND (NOT IsWhite (char (a, -1)))
   THEN
      a := x (a, ConCatChar (a, ' '))
   END ;
   RETURN x (a, ConCat(a, b))
END ConCatWord ;
*)


(*
   symDesc -
*)

PROCEDURE symDesc (sym: CARDINAL) : String ;
BEGIN
   IF IsConstLit (sym)
   THEN
      RETURN InitString ('constant literal')
   ELSIF IsConstSet (sym)
   THEN
      RETURN InitString ('constant set')
   ELSIF IsConstructor (sym)
   THEN
      RETURN InitString ('constructor')
   ELSIF IsConst(sym)
   THEN
      RETURN InitString('constant')
   ELSIF IsArray(sym)
   THEN
      RETURN InitString('array')
   ELSIF IsVar(sym)
   THEN
      IF IsTemporary (sym)
      THEN
         RETURN InitString('expression')
      ELSE
         RETURN InitString('variable')
      END
   ELSIF IsEnumeration(sym)
   THEN
      RETURN InitString('enumeration type')
   ELSIF IsFieldEnumeration(sym)
   THEN
      RETURN InitString('enumeration field')
   ELSIF IsUnbounded(sym)
   THEN
      RETURN InitString('unbounded parameter')
   ELSIF IsProcType(sym)
   THEN
      RETURN InitString('procedure type')
   ELSIF IsPseudoBaseFunction (sym)
   THEN
      RETURN InitString('standard function procedure')
   ELSIF IsPseudoBaseProcedure (sym)
   THEN
      RETURN InitString('standard procedure')
   ELSIF IsProcedure(sym)
   THEN
      RETURN InitString('procedure')
   ELSIF IsPointer(sym)
   THEN
      RETURN InitString('pointer')
   ELSIF IsParameter(sym)
   THEN
      IF IsParameterVar(sym)
      THEN
         RETURN InitString('var parameter')
      ELSE
         RETURN InitString('parameter')
      END
   ELSIF IsType(sym)
   THEN
      IF IsHiddenType (sym)
      THEN
         RETURN InitString('opaque type')
      ELSE
         RETURN InitString('type')
      END
   ELSIF IsRecord(sym)
   THEN
      RETURN InitString('record')
   ELSIF IsRecordField(sym)
   THEN
      RETURN InitString('record field')
   ELSIF IsVarient(sym)
   THEN
      RETURN InitString('varient record')
   ELSIF IsModule(sym)
   THEN
      RETURN InitString('module')
   ELSIF IsDefImp(sym)
   THEN
      RETURN InitString('definition or implementation module')
   ELSIF IsSet(sym)
   THEN
      RETURN InitString('set')
   ELSIF IsUnknown(sym)
   THEN
      RETURN InitString('an unknown')
   ELSIF IsSubrange(sym)
   THEN
      RETURN InitString('subrange')
   ELSE
      RETURN InitString ('')
   END
END symDesc ;


(*
   doDesc -
*)

PROCEDURE doDesc (VAR eb: errorBlock;
                  sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF empty (eb)
   THEN
      OutGlyphS (eb, symDesc (sym[bol])) ;
      IF NOT empty (eb)
      THEN
         eb.quotes := FALSE
      END
   END
END doDesc ;


(*
   copySym - copies, n+1, symbols, from, ->, to.
*)

(*
PROCEDURE copySym (from: ARRAY OF CARDINAL; VAR to: ARRAY OF CARDINAL; n: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   IF n>HIGH(to)
   THEN
      InternalError ('not enough room in the destination array')
   ELSE
      i := 0 ;
      WHILE i<=n DO
         to[i] := from[i] ;
         INC(i)
      END
   END
END copySym ;
*)


(*
   op := {'!'|'a'|'c'|'d'|'k'|'n'|'p'|'q'|'s'|'t'|'u'|
          'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'K'|'M'|'N'|
          'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'} then =:
*)

PROCEDURE op (VAR eb: errorBlock;
              sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   WHILE (eb.ini < eb.len) AND (char (eb.in, eb.ini) # '}') DO
      IF Debugging
      THEN
         printf0 ("while loop in op\n") ;
         dump (eb)
      END ;
      CASE char (eb.in, eb.ini) OF

      '!':  eb.positive := NOT eb.positive |
      'a':  doName (eb, sym, bol) |
      'c':  eb.currentCol := readColor (eb) ;
            DEC (eb.ini) |
      'd':  doDesc (eb, sym, bol) |
      'k':  unquotedKeyword (eb) ;
            DEC (eb.ini) |
      'n':  doNumber (eb, sym, bol) |
      'p':  popColor (eb) |
      'q':  doQualified (eb, sym, bol) |
      's':  doSkipType (eb, sym, bol) |
      't':  doType (eb, sym, bol) |
      'u':  eb.quotes := FALSE |
      'A':  eb.type := aborta ;
            seenAbort := TRUE |
      'B':  declaredType (eb, sym, bol) |
      'C':  eb.chain := TRUE |
      'D':  declaredDef (eb, sym, bol) |
      'E':  eb.type := error |
      'F':  filename (eb) ;
            DEC (eb.ini) |
      'G':  declaredFor (eb, sym, bol) |
      'H':  declaredFull (eb, sym, bol) |
      'K':  keyword (eb) ;
            DEC (eb.ini) |
      'M':  declaredMod (eb, sym, bol) |
      'N':  doCount (eb, sym, bol) |
      'O':  eb.type := note |
      'P':  pushColor (eb) |
      'Q':  resetDictionary |
      'R':  eb.root := TRUE |
      'S':  doGetSkipType (eb, sym, bol) |
      'T':  doGetType (eb, sym, bol) |
      'U':  used (eb, sym, bol) |
      'V':  declaredVar (eb, sym, bol) |
      'W':  eb.type := warning |
      'X':  pushOutput (eb) |
      'Y':  processDefine (eb) |
      'Z':  popOutput (eb) |
      ':':  ifNonNulThen (eb, sym) ;
            DEC (eb.ini) |
      '1':  InternalError ('incorrect format spec, expecting %1 rather than % spec 1') |
      '2':  InternalError ('incorrect format spec, expecting %2 rather than % spec 2') |
      '3':  InternalError ('incorrect format spec, expecting %3 rather than % spec 3') |
      '4':  InternalError ('incorrect format spec, expecting %4 rather than % spec 4')

      ELSE
         InternalFormat (eb, 'expecting one of [akqtdnpsuCDEFGKNOPQRSTUWXYZ:<>%]', __LINE__)
      END ;
      INC (eb.ini)
   END ;
   IF Debugging
   THEN
      printf0 ("finishing op\n") ;
      dump (eb)
   END
END op ;


(*
   percenttoken := '%' (
                         '1'        % doOperand(1) %
                             op
                       | '2'        % doOperand(2) %
                             op
                       | '3'        % doOperand(3) %
                             op
                       | '4'        % doOperand(4) %
                             op
                       )
                       } =:
*)

PROCEDURE percenttoken (VAR eb: errorBlock; sym: ARRAY OF CARDINAL) ;
BEGIN
   IF char (eb.in, eb.ini) = '%'
   THEN
      INC (eb.ini) ;
      CASE char (eb.in, eb.ini) OF

      '1':  INC (eb.ini) ;
            op (eb, sym, 0) |
      '2':  INC (eb.ini) ;
            op (eb, sym, 1) |
      '3':  INC (eb.ini) ;
            op (eb, sym, 2) |
      '4':  INC (eb.ini) ;
            op (eb, sym, 3)

      ELSE
         op (eb, sym, 0)
      END ;
      IF (eb.ini < eb.len) AND (char (eb.in, eb.ini) # '}')
      THEN
         InternalFormat (eb, 'expecting to see }', __LINE__)
      END
   END
END percenttoken ;


(*
   changeColor - changes to color, c.
*)

PROCEDURE changeColor (VAR eb: errorBlock; c: colorType) ;
BEGIN
   eb.currentCol := c
END changeColor ;


(*
   shutdownColor - shutdown existing color if it exists.
*)

PROCEDURE shutdownColor (VAR eb: errorBlock) ;
BEGIN
   IF (eb.endCol # unsetColor) AND (eb.endCol # noColor)
   THEN
      eb.out := colorEnd (eb.out) ;
      eb.endCol := noColor
   END
END shutdownColor ;


(*
   flushColor - flushes any outstanding color change.
*)

PROCEDURE flushColor (VAR eb: errorBlock) ;
BEGIN
   IF eb.endCol # eb.currentCol
   THEN
      shutdownColor (eb) ;
      IF eb.endCol # eb.currentCol
      THEN
         emitColor (eb, eb.currentCol) ;
         eb.endCol := eb.currentCol
      END ;
      IF eb.beginCol = unsetColor
      THEN
         eb.beginCol := eb.currentCol
      END
   END
END flushColor ;


(*
   emitColorGCC -
*)

PROCEDURE emitColorGCC (VAR eb: errorBlock; c: colorType) ;
BEGIN
   CASE c OF

   unsetColor   :  |
   noColor      :  eb.out := M2ColorString.endColor (eb.out) |
   quoteColor   :  eb.out := M2ColorString.quoteColor (eb.out) |
   filenameColor:  eb.out := M2ColorString.filenameColor (eb.out) |
   errorColor   :  eb.out := M2ColorString.errorColor (eb.out) |
   warningColor :  eb.out := M2ColorString.warningColor (eb.out) |
   noteColor    :  eb.out := M2ColorString.noteColor (eb.out) |
   keywordColor :  eb.out := M2ColorString.locusColor (eb.out) |
   locusColor   :  eb.out := M2ColorString.locusColor (eb.out) |
   insertColor  :  eb.out := M2ColorString.insertColor (eb.out) |
   deleteColor  :  eb.out := M2ColorString.deleteColor (eb.out) |
   typeColor    :  eb.out := M2ColorString.typeColor (eb.out) |
   range1Color  :  eb.out := M2ColorString.range1Color (eb.out) |
   range2Color  :  eb.out := M2ColorString.range2Color (eb.out)

   END
END emitColorGCC ;


(*
   emitColorTag -
*)

PROCEDURE emitColorTag (VAR eb: errorBlock; c: colorType) ;
VAR
   s: String ;
BEGIN
   CASE c OF

   unsetColor   :  s := InitString ('<unset>') |
   noColor      :  s := InitString ('<nocol>') ; stop |
   quoteColor   :  s := InitString ('<quote>') |
   filenameColor:  s := InitString ('<filename>') |
   errorColor   :  s := InitString ('<error>') |
   warningColor :  s := InitString ('<warn>') |
   noteColor    :  s := InitString ('<note>') |
   keywordColor :  s := InitString ('<key>') |
   locusColor   :  s := InitString ('<locus>') |
   insertColor  :  s := InitString ('<insert>') |
   deleteColor  :  s := InitString ('<delete>') |
   typeColor    :  s := InitString ('<type>') |
   range1Color  :  s := InitString ('<range1>') |
   range2Color  :  s := InitString ('<range2>')

   END ;
   eb.out := ConCat (eb.out, Mark (s))
END emitColorTag ;


(*
   emitColor - adds the appropriate color string to the output string.
*)

PROCEDURE emitColor (VAR eb: errorBlock; c: colorType) ;
BEGIN
   IF ColorDebug
   THEN
      emitColorTag (eb, c)
   ELSE
      emitColorGCC (eb, c)
   END
END emitColor ;


(*
   openQuote -
*)

PROCEDURE openQuote (s: String) : String ;
BEGIN
   IF ColorDebug
   THEN
      RETURN ConCat (s, Mark (InitString ('<openquote>')))
   ELSE
      RETURN M2ColorString.quoteOpen (s)
   END
END openQuote ;


(*
   closeQuote -
*)

PROCEDURE closeQuote (s: String) : String ;
BEGIN
   IF ColorDebug
   THEN
      RETURN ConCat (s, Mark (InitString ('<closequote>')))
   ELSE
      RETURN M2ColorString.quoteClose (s)
   END
END closeQuote ;


(*
   colorEnd -
*)

PROCEDURE colorEnd (s: String) : String ;
BEGIN
   stop ;
   IF ColorDebug
   THEN
      RETURN ConCat (s, Mark (InitString ('<nocol>')))
   ELSE
      RETURN M2ColorString.endColor (s)
   END
END colorEnd ;


(*
   copyChar - copies a character from in string to out string.
*)

PROCEDURE copyChar (VAR eb: errorBlock) ;
BEGIN
   IF eb.ini < eb.len
   THEN
      flushColor (eb) ;
      checkMe ;
      eb.glyph := TRUE ;
      eb.out := x (eb.out, ConCatChar (eb.out, char (eb.in, eb.ini)))
   END
END copyChar ;


(*
   copyKeywordChar - copies a character from in string to out string
                     it will convert the character to lower case if the
                     -fm2-lower-case option was specified.
*)

PROCEDURE copyKeywordChar (VAR eb: errorBlock) ;
VAR
   ch: CHAR ;
BEGIN
   IF eb.ini < eb.len
   THEN
      flushColor (eb) ;
      ch := char (eb.in, eb.ini) ;
      IF LowerCaseKeywords
      THEN
         ch := Lower (ch)
      END ;
      eb.glyph := TRUE ;
      eb.out := x (eb.out, ConCatChar (eb.out, ch))
   END
END copyKeywordChar ;


(*
   percent := '%' anych           % copy anych %
            =:
*)

PROCEDURE percent (VAR eb: errorBlock; sym: ARRAY OF CARDINAL) ;
BEGIN
   IF char (eb.in, eb.ini)='%'
   THEN
      INC (eb.ini) ;
      IF eb.ini < eb.len
      THEN
         IF char (eb.in, eb.ini) = '<'
         THEN
            (* %< is a quotation symbol.  *)
            pushColor (eb) ;
            eb.currentCol := noColor ;
            flushColor (eb) ;
            changeColor (eb, quoteColor) ;
            eb.endCol := quoteColor ;  (* the openQuote will change the color.  *)
            (* OutGlyphS performs a flush and we are emitting the open quote glyph.  *)
            OutGlyphS (eb, openQuote (InitString ('')))
         ELSIF char (eb.in, eb.ini) = '>'
         THEN
            OutGlyphS (eb, closeQuote (InitString (''))) ;
            eb.endCol := noColor ;  (* closeQuote also turns off color.  *)
            popColor (eb)
         ELSE
            copyChar (eb)
         END
      END
   END
END percent ;


(*
   lbra := '{' [ '!' ] percenttoken '}' =:
*)

PROCEDURE lbra (VAR eb: errorBlock; sym: ARRAY OF CARDINAL) ;
BEGIN
   IF char (eb.in, eb.ini) = '{'
   THEN
      eb.positive := TRUE ;
      INC (eb.ini) ;
      IF char (eb.in, eb.ini) = '!'
      THEN
         eb.positive := FALSE ;
         INC (eb.ini)
      END ;
      IF char (eb.in, eb.ini) # '%'
      THEN
         InternalFormat (eb, 'expecting to see %', __LINE__)
      END ;
      percenttoken (eb, sym) ;
      IF (eb.ini < eb.len) AND (char (eb.in, eb.ini) # '}')
      THEN
         InternalFormat (eb, 'expecting to see }', __LINE__)
      END
   END
END lbra ;


PROCEDURE stop ; BEGIN END stop ;

PROCEDURE checkMe ; BEGIN END checkMe ;


(*
   dumpErrorType -
*)

PROCEDURE dumpErrorType (e: errorType) ;
BEGIN
   CASE e OF

   none   :  printf0 ("none") |
   error  :  printf0 ("error") |
   warning:  printf0 ("warning") |
   note   :  printf0 ("note") |
   chained:  printf0 ("chained") |
   aborta :  printf0 ("abort")

   END
END dumpErrorType ;


(*
   dumpColorType -
*)

PROCEDURE dumpColorType (c: colorType) ;
BEGIN
   CASE c OF

   unsetColor   :  printf0 ("unsetColor") |
   noColor      :  printf0 ("noColor") |
   quoteColor   :  printf0 ("quoteColor") |
   filenameColor:  printf0 ("filenameColor") |
   errorColor   :  printf0 ("errorColor") |
   warningColor :  printf0 ("warningColor") |
   noteColor    :  printf0 ("noteColor") |
   keywordColor :  printf0 ("keywordColor") |
   locusColor   :  printf0 ("locusColor") |
   insertColor  :  printf0 ("insertColor") |
   deleteColor  :  printf0 ("deleteColor") |
   typeColor    :  printf0 ("typeColor") |
   range1Color  :  printf0 ("range1Color") |
   range2Color  :  printf0 ("range2Color")

   END
END dumpColorType ;


(*
   dump -

*)

PROCEDURE dump (eb: errorBlock) ;
VAR
   ch: CHAR ;
   l : CARDINAL ;
   i : INTEGER ;
BEGIN
   l := Length (eb.out) ;
   printf0 ("\n\nerrorBlock\n") ;
   printf0 ("\ntype      = ") ; dumpErrorType (eb.type) ;
   printf1 ("\nout       = |%s|", eb.out) ;
   printf1 ("\nin        = |%s|", eb.in) ;
   printf1 ("\nLength (out) = %d", l) ;
   printf1 ("\nlen       = %d", eb.len) ;
   printf1 ("\nhighplus1 = %d", eb.highplus1) ;
   printf1 ("\nglyph     = %d", eb.glyph) ;
   printf1 ("\nquotes    = %d", eb.quotes) ;
   printf1 ("\npositive  = %d", eb.positive) ;
   printf0 ("\nbeginCol  = ") ; dumpColorType (eb.beginCol) ;
   printf0 ("\nendCol    = ") ; dumpColorType (eb.endCol) ;
   printf0 ("\ncurrentCol = ") ; dumpColorType (eb.currentCol) ;
   printf1 ("\nini        = %d", eb.ini) ;
   IF eb.ini < eb.len
   THEN
      ch := char (eb.in, eb.ini) ;
      printf1 ("\ncurrent char = %c", ch) ;
      printf1 ("\n%s\n", eb.in) ;
      i := 0 ;
      WHILE i<eb.ini DO
         printf0 (" ") ;
         INC (i)
      END ;
      printf0 ("^\n")
   END ;
   printf0 ("\n")
END dump ;


(*
   ebnf := { percent
             | lbra
             | any                    % copy ch %
           }
         =:
*)

PROCEDURE ebnf (VAR eb: errorBlock; sym: ARRAY OF CARDINAL) ;
VAR
   nb: errorBlock ;
BEGIN
   IF Debugging
   THEN
      printf0 ("top of ebnf\n") ;
      dump (eb)
   END ;
   WHILE eb.ini < eb.len DO
      IF Debugging
      THEN
         printf0 ("while loop ebnf\n") ;
         dump (eb)
      END ;
      CASE char (eb.in, eb.ini) OF

      '!':  eb.positive := NOT eb.positive |
      '%':  percent (eb, sym) |
      '{':  push (nb, eb) ;
            lbra (nb, sym) ;
            pop (eb, nb) ;
            IF (eb.ini < eb.len) AND (char (eb.in, eb.ini) # '}')
            THEN
               InternalFormat (eb, 'expecting to see }', __LINE__)
            END |
      '}':  RETURN

      ELSE
         IF ((IsWhite (char (eb.in, eb.ini)) AND (Length (eb.out) > 0) AND
              (NOT IsWhite (char (eb.out, -1)))) OR
            (NOT IsWhite (char (eb.in, eb.ini)))) AND (eb.highplus1 > 0)
         THEN
            eb.quotes := FALSE ;  (* copying a normal character, don't quote the result.  *)
            copyChar (eb)
         END
      END ;
      INC (eb.ini)
   END ;
   eb.currentCol := noColor ;
   flushColor (eb) ;
   IF Debugging
   THEN
      printf0 ("finishing ebnf\n") ;
      dump (eb)
   END
END ebnf ;


PROCEDURE MetaErrorStringT0 (tok: CARDINAL; m: String) ;
VAR
   eb : errorBlock ;
   sym: ARRAY [0..0] OF CARDINAL ;
BEGIN
   sym[0] := NulSym ;
   initErrorBlock (eb, m, sym) ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   defaultError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb) ;
   checkAbort
END MetaErrorStringT0 ;


PROCEDURE MetaErrorT0 (tok: CARDINAL; m: ARRAY OF CHAR) ;
BEGIN
   MetaErrorStringT0 (tok, InitString(m))
END MetaErrorT0 ;


PROCEDURE MetaErrorStringT1 (tok: CARDINAL; m: String; s: CARDINAL) ;
VAR
   eb : errorBlock ;
   sym: ARRAY [0..0] OF CARDINAL ;
BEGIN
   sym[0] := s ;
   initErrorBlock (eb, m, sym) ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   defaultError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb) ;
   checkAbort
END MetaErrorStringT1 ;


PROCEDURE MetaErrorT1 (tok: CARDINAL; m: ARRAY OF CHAR; s: CARDINAL) ;
BEGIN
   MetaErrorStringT1 (tok, InitString (m), s)
END MetaErrorT1 ;


PROCEDURE MetaErrorStringT2 (tok: CARDINAL; m: String; s1, s2: CARDINAL) ;
VAR
   eb : errorBlock ;
   sym: ARRAY [0..1] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   initErrorBlock (eb, m, sym) ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   defaultError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb) ;
   checkAbort
END MetaErrorStringT2 ;


PROCEDURE MetaErrorT2 (tok: CARDINAL; m: ARRAY OF CHAR; s1, s2: CARDINAL) ;
BEGIN
   MetaErrorStringT2 (tok, InitString (m), s1, s2)
END MetaErrorT2 ;


PROCEDURE MetaErrorStringT3 (tok: CARDINAL; m: String; s1, s2, s3: CARDINAL) ;
VAR
   eb : errorBlock ;
   sym: ARRAY [0..2] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   initErrorBlock (eb, m, sym) ;
   eb.highplus1 := HIGH (sym) + 1 ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   defaultError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb) ;
   checkAbort
END MetaErrorStringT3 ;


PROCEDURE MetaErrorT3 (tok: CARDINAL; m: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
BEGIN
   MetaErrorStringT3 (tok, InitString (m), s1, s2, s3) ;
END MetaErrorT3 ;


PROCEDURE MetaErrorStringT4 (tok: CARDINAL; m: String; s1, s2, s3, s4: CARDINAL) ;
VAR
   eb : errorBlock ;
   sym: ARRAY [0..3] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   sym[3] := s4 ;
   initErrorBlock (eb, m, sym) ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   defaultError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb) ;
   checkAbort
END MetaErrorStringT4 ;


PROCEDURE MetaErrorT4 (tok: CARDINAL; m: ARRAY OF CHAR; s1, s2, s3, s4: CARDINAL) ;
BEGIN
   MetaErrorStringT4 (tok, InitString (m), s1, s2, s3, s4) ;
END MetaErrorT4 ;


PROCEDURE MetaError0 (m: ARRAY OF CHAR) ;
BEGIN
   MetaErrorT0 (GetTokenNo (), m)
END MetaError0 ;


PROCEDURE MetaError1 (m: ARRAY OF CHAR; s: CARDINAL) ;
BEGIN
   MetaErrorT1 (GetTokenNo (), m, s)
END MetaError1 ;


PROCEDURE MetaError2 (m: ARRAY OF CHAR; s1, s2: CARDINAL) ;
BEGIN
   MetaErrorT2 (GetTokenNo (), m, s1, s2)
END MetaError2 ;


PROCEDURE MetaError3 (m: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
BEGIN
   MetaErrorT3 (GetTokenNo (), m, s1, s2, s3)
END MetaError3 ;


PROCEDURE MetaError4 (m: ARRAY OF CHAR; s1, s2, s3, s4: CARDINAL) ;
BEGIN
   MetaErrorT4 (GetTokenNo (), m, s1, s2, s3, s4)
END MetaError4 ;


(*
   wrapErrors -
*)

PROCEDURE wrapErrors (tok: CARDINAL;
                      m1, m2: ARRAY OF CHAR;
                      sym: ARRAY OF CARDINAL) ;
VAR
   eb: errorBlock ;
BEGIN
   initErrorBlock (eb, InitString (m1), sym) ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   defaultError (eb, tok) ;
   lastRoot := eb.e ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb) ;
   initErrorBlock (eb, InitString (m2), sym) ;
   eb.type := chained ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   defaultError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb)
END wrapErrors ;


PROCEDURE MetaErrorsT1 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s: CARDINAL) ;
VAR
   sym: ARRAY [0..0] OF CARDINAL ;
BEGIN
   sym[0] := s ;
   wrapErrors (tok, m1, m2, sym)
END MetaErrorsT1 ;


PROCEDURE MetaErrorsT2 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2: CARDINAL) ;
VAR
   sym: ARRAY [0..1] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   wrapErrors (tok, m1, m2, sym)
END MetaErrorsT2 ;


PROCEDURE MetaErrorsT3 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
VAR
   sym : ARRAY [0..2] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   wrapErrors (tok, m1, m2, sym)
END MetaErrorsT3 ;


PROCEDURE MetaErrorsT4 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2, s3, s4: CARDINAL) ;
VAR
   sym : ARRAY [0..3] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   sym[3] := s4 ;
   wrapErrors (tok, m1, m2, sym)
END MetaErrorsT4 ;


PROCEDURE MetaErrors1 (m1, m2: ARRAY OF CHAR; s: CARDINAL) ;
BEGIN
   MetaErrorsT1 (GetTokenNo (), m1, m2, s)
END MetaErrors1 ;


PROCEDURE MetaErrors2 (m1, m2: ARRAY OF CHAR; s1, s2: CARDINAL) ;
BEGIN
   MetaErrorsT2 (GetTokenNo (), m1, m2, s1, s2)
END MetaErrors2 ;


PROCEDURE MetaErrors3 (m1, m2: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
BEGIN
   MetaErrorsT3 (GetTokenNo (), m1, m2, s1, s2, s3)
END MetaErrors3 ;


PROCEDURE MetaErrors4 (m1, m2: ARRAY OF CHAR; s1, s2, s3, s4: CARDINAL) ;
BEGIN
   MetaErrorsT4 (GetTokenNo (), m1, m2, s1, s2, s3, s4)
END MetaErrors4 ;


PROCEDURE MetaErrorString0 (m: String) ;
BEGIN
   MetaErrorStringT0 (GetTokenNo (), m)
END MetaErrorString0 ;


PROCEDURE MetaErrorString1 (m: String; s: CARDINAL) ;
BEGIN
   MetaErrorStringT1 (GetTokenNo (), m, s)
END MetaErrorString1 ;


PROCEDURE MetaErrorString2 (m: String; s1, s2: CARDINAL) ;
BEGIN
   MetaErrorStringT2 (GetTokenNo (), m, s1, s2)
END MetaErrorString2 ;


PROCEDURE MetaErrorString3 (m: String; s1, s2, s3: CARDINAL) ;
BEGIN
   MetaErrorStringT3 (GetTokenNo (), m, s1, s2, s3)
END MetaErrorString3 ;


PROCEDURE MetaErrorString4 (m: String; s1, s2, s3, s4: CARDINAL) ;
BEGIN
   MetaErrorStringT4 (GetTokenNo (), m, s1, s2, s3, s4)
END MetaErrorString4 ;


(*
   checkAbort - checks to see if the boolean flag seenAbort has been set,
                if so it flushes all existing errors and terminates.
*)

PROCEDURE checkAbort ;
BEGIN
   IF seenAbort
   THEN
      FlushWarnings ;
      FlushErrors
   END
END checkAbort ;


(*
   translate -
*)

PROCEDURE translate (m, s: String; VAR i: INTEGER; name: Name) : String ;
VAR
   l : INTEGER ;
   ch: CHAR ;
BEGIN
   l := Length (m) ;
   WHILE (i >= 0) AND (i < l) DO
      ch := char (m, i) ;
      IF (ch = '%') AND (i < l)
      THEN
         INC (i) ;
         ch := char (m, i) ;
         INC (i) ;
         IF ch = 'a'
         THEN
            s := ConCat (s, Mark (InitString ('%<'))) ;
            s := ConCat (s, Mark (InitStringCharStar (KeyToCharStar (name)))) ;
            s := ConCat (s, Mark (InitString ('%>'))) ;
            RETURN s
         END ;
         s := ConCatChar (s, '%')
      END ;
      s := ConCatChar (s, ch) ;
      INC (i)
   END ;
   RETURN s
END translate ;


(*
   MetaErrorNT0 - generate an error message at tok using format.
*)

PROCEDURE MetaErrorNT0 (tok: CARDINAL; format: ARRAY OF CHAR) ;
BEGIN
   MetaErrorStringT0 (tok, InitString (format))
END MetaErrorNT0 ;


(*
   MetaErrorNT1 - generate an error message at tok using format and name.
                  The format should contain %a for name substitution.
*)

PROCEDURE MetaErrorNT1 (tok: CARDINAL; format: ARRAY OF CHAR; name: Name) ;
VAR
   i  : INTEGER ;
   s,
   fmt: String ;
BEGIN
   i := 0 ;
   fmt := InitString (format) ;
   s := InitString ('') ;
   s := translate (fmt, s, i, name) ;
   MetaErrorStringT0 (tok, s) ;
   fmt := KillString (fmt) ;
END MetaErrorNT1 ;


(*
   MetaErrorN1 -
*)

PROCEDURE MetaErrorN1 (m: ARRAY OF CHAR; n: Name) ;
BEGIN
   MetaErrorNT1 (GetTokenNo (), m, n)
END MetaErrorN1 ;


(*
   MetaErrorNT1 - generate an error message at tok using format, name1
                  and name2.  The format should contain two occurances of %a
                  for name substitution.
*)

PROCEDURE MetaErrorNT2 (tok: CARDINAL; format: ARRAY OF CHAR; name1, name2: Name) ;
VAR
   i  : INTEGER ;
   s,
   fmt: String ;
BEGIN
   i := 0 ;
   fmt := InitString (format) ;
   s := InitString ('') ;
   s := translate (fmt, s, i, name1) ;
   s := translate (fmt, s, i, name2) ;
   MetaErrorStringT0 (tok, s) ;
   fmt := KillString (fmt) ;
END MetaErrorNT2 ;


(*
   MetaErrorN2 -
*)

PROCEDURE MetaErrorN2 (m: ARRAY OF CHAR; n1, n2: Name) ;
BEGIN
   MetaErrorNT2 (GetTokenNo (), m, n1, n2)
END MetaErrorN2 ;


(*
   wrapString - return a string which has been formatted with the specifier codes.
                Color is disabled.  The result string is returned.
*)

PROCEDURE wrapString (m: String;
                      sym: ARRAY OF CARDINAL) : String ;
VAR
   eb : errorBlock ;
   s  : String ;
   old: BOOLEAN ;
BEGIN
   old := M2ColorString.SetEnableColor (FALSE) ;
   initErrorBlock (eb, Dup (m), sym) ;
   eb.useError := FALSE ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   s := Dup (eb.out) ;
   killErrorBlock (eb) ;
   old := M2ColorString.SetEnableColor (old) ;
   RETURN s
END wrapString ;


PROCEDURE MetaString0 (m: String) : String ;
VAR
   sym: ARRAY [0..0] OF CARDINAL ;
BEGIN
   sym[0] := NulSym ;
   RETURN wrapString (m, sym)
END MetaString0 ;


PROCEDURE MetaString1 (m: String; s: CARDINAL) : String ;
VAR
   sym: ARRAY [0..0] OF CARDINAL ;
BEGIN
   sym[0] := s ;
   RETURN wrapString (m, sym)
END MetaString1 ;


PROCEDURE MetaString2 (m: String; s1, s2: CARDINAL) : String ;
VAR
   sym: ARRAY [0..1] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   RETURN wrapString (m, sym)
END MetaString2 ;


PROCEDURE MetaString3 (m: String; s1, s2, s3: CARDINAL) : String ;
VAR
   sym: ARRAY [0..2] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   RETURN wrapString (m, sym)
END MetaString3 ;


PROCEDURE MetaString4 (m: String; s1, s2, s3, s4: CARDINAL) : String ;
VAR
   sym: ARRAY [0..3] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   sym[3] := s4 ;
   RETURN wrapString (m, sym)
END MetaString4 ;


(*
   MetaErrorDecl - if sym is a variable or parameter then generate a
                   declaration error or warning message.  If error is
                   FALSE then a warning is issued.
*)

PROCEDURE MetaErrorDecl (sym: CARDINAL; error: BOOLEAN) ;
BEGIN
   IF (sym # NulSym) AND IsVar (sym)
   THEN
      IF error
      THEN
         IF IsVarAParam (sym)
         THEN
            MetaErrorT1 (GetVarDeclFullTok (sym), 'parameter declaration for {%1ad}', sym)
         ELSE
            MetaErrorT1 (GetVarDeclFullTok (sym), 'variable declaration for {%1ad}', sym)
         END
      ELSE
         IF IsVarAParam (sym)
         THEN
            MetaErrorT1 (GetVarDeclFullTok (sym), 'parameter declaration for {%1Wad}', sym)
         ELSE
            MetaErrorT1 (GetVarDeclFullTok (sym), 'variable declaration for {%1Wad}', sym)
         END
      END
   END
END MetaErrorDecl ;


BEGIN
   lastRoot := NIL ;
   lastColor := noColor ;
   seenAbort := FALSE ;
   outputStack := InitIndex (1) ;
   dictionary := InitIndex (1) ;
   freeEntry := NIL
END M2MetaError.
