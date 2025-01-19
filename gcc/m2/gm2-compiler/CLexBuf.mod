(* CLexBuf.mod provides a lexical buffer for clex.

Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE CLexBuf ;

IMPORT cflex ;

FROM SYSTEM IMPORT ADDRESS ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM DynamicStrings IMPORT string, InitString, InitStringCharStar, Equal, Mark, KillString ;
FROM FormatStrings IMPORT Sprintf1 ;
FROM NameKey IMPORT Name, NulName, makekey, KeyToCharStar ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3 ;
FROM Assertion IMPORT Assert ;
FROM SymbolKey IMPORT NulKey, SymbolTree, InitTree, DelSymKey, PutSymKey, GetSymKey ;
FROM Indexing IMPORT Index, InitIndex, IsIndiceInIndex, GetIndice, PutIndice ;

CONST
   MaxBucketSize = 100 ;
   Debugging     = FALSE ;

TYPE
   SourceList = POINTER TO sourcelist ;
   sourcelist =            RECORD
                              left,
                              right: SourceList ;
                              name : String ;
                              line : CARDINAL ;
                           END ;

   TokenDesc = RECORD
                  token: toktype ;
                  str  : Name ;
                  int  : INTEGER ;
                  line : CARDINAL ;
                  file : SourceList ;
               END ;

   TokenBucket = POINTER TO tokenbucket ;
   tokenbucket =            RECORD
                               buf : ARRAY [0..MaxBucketSize] OF TokenDesc ;
                               len : CARDINAL ;
                               next: TokenBucket ;
                            END ;

   ListDesc = RECORD
                 head,
                 tail            : TokenBucket ;
                 LastBucketOffset: CARDINAL ;
              END ;

   MacroArgs = POINTER TO macroargs ;
   macroargs =            RECORD
                             next: MacroArgs ;
                             str : Name ;
                          END ;

   Macro = POINTER TO macro ;
   macro =            RECORD
                         str   : Name ;
                         tokno : CARDINAL ;
                         noArgs: CARDINAL ;
                         args  : MacroArgs ;
                      END ;

VAR
   CurrentSource    : SourceList ;
   UseBufferedTokens,
   CurrentUsed      : BOOLEAN ;
   ListOfTokens     : ListDesc ;
   CurrentTokNo     : CARDINAL ;
   MacroDefinitions : SymbolTree ;
   MacroIndex       : Index ;
   DefineNo         : CARDINAL ;
   EnabledMacros    : BOOLEAN ;


(* M A C R O *)

(*
   EnableMacroSubstitutions -
*)

PROCEDURE EnableMacroSubstitutions (b: BOOLEAN) ;
BEGIN
   EnabledMacros := b
END EnableMacroSubstitutions ;


(*
   IsMacroDefined - returns TRUE if macro, n, was defined.
*)

PROCEDURE IsMacroDefined (n: Name) : BOOLEAN ;
VAR
   i: CARDINAL ;
   m: Macro ;
BEGIN
   i := GetSymKey(MacroDefinitions, n) ;
   IF i=0
   THEN
      RETURN( FALSE )
   ELSE
      m := GetIndice(MacroIndex, i) ;
      IF m=NIL
      THEN
         RETURN( FALSE )
      ELSE
         RETURN( TRUE )
      END
   END
END IsMacroDefined ;


(*
   NoArgs - returns the number of arguments for macro, n.
            -1 if the macro does not exist
*)

PROCEDURE NoArgs (n: Name) : INTEGER ;
VAR
   m: Macro ;
   i: CARDINAL ;
BEGIN
   IF IsMacroDefined(n)
   THEN
      i := GetSymKey(MacroDefinitions, n) ;
      m := GetIndice(MacroIndex, i) ;
      RETURN( m^.noArgs )
   ELSE
      RETURN( -1 )
   END
END NoArgs ;


(*
   DefineMacro - defines macro, n, as defined to start at token, t.
*)

PROCEDURE DefineMacro (n: Name; t: CARDINAL) ;
VAR
   m: Macro ;
   i: CARDINAL ;
BEGIN
   NEW(m) ;
   WITH m^ DO
      str := n ;
      tokno := t ;
      noArgs := 0 ;
      args := NIL
   END ;
   UnDefineMacro(n) ;
   i := GetSymKey(MacroDefinitions, n) ;
   IF i=NulKey
   THEN
      PutSymKey(MacroDefinitions, n, DefineNo) ;
      i := DefineNo ;
      INC(DefineNo)
   END ;
   PutIndice(MacroIndex, i, m)
END DefineMacro ;


(*
   UnDefineMacro -
*)

PROCEDURE UnDefineMacro (n: Name) ;
VAR
   m: Macro ;
   i: CARDINAL ;
BEGIN
   IF IsMacroDefined(n)
   THEN
      i := GetSymKey(MacroDefinitions, n) ;
      m := GetIndice(MacroIndex, i) ;
      PutIndice(MacroIndex, i, NIL) ;
      DISPOSE(m)
   END
END UnDefineMacro ;


(*
   PushMacroDefinition - pushes the macro definition, n, onto the token stream.
                         It returns TRUE if the macro was found and pushed.
*)

PROCEDURE PushMacroDefinition (n: Name) : BOOLEAN ;
VAR
   m: Macro ;
   t: CARDINAL ;
   b: TokenBucket ;
   i: CARDINAL ;
BEGIN
   IF EnabledMacros AND IsMacroDefined(n)
   THEN
      i := GetSymKey(MacroDefinitions, n) ;
      m := GetIndice(MacroIndex, i) ;
      WITH m^ DO
         IF tokno>0
         THEN
            t := tokno ;
            LOOP
               b := FindTokenBucket(t) ;
               WITH b^.buf[t] DO
                  IF token=endhashtok
                  THEN
                     RETURN( TRUE )
                  ELSE
                     IF IsMacroDefined(str) AND (str#n)
                     THEN
                        IF PushMacroDefinition(str)
                        THEN
                        END
                     ELSE
                        AddTokToList(token, str, int, line, file)
                     END
                  END
               END ;
               INC(t)
            END
         END
      END ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END PushMacroDefinition ;


(*   e n d    o f    M A C R O    r o u t i n e s   *)

PROCEDURE stop ; BEGIN END stop ;

(*
   Init - initializes the token list and source list.
*)

PROCEDURE Init ;
BEGIN
   currenttoken := eoftok ;
   CurrentTokNo := 0 ;
   CurrentSource := NIL ;
   ListOfTokens.head := NIL ;
   ListOfTokens.tail := NIL ;
   UseBufferedTokens := FALSE ;
   InitTree(MacroDefinitions) ;
   EnabledMacros := TRUE ;
   DefineNo := 1 ;
   MacroIndex := InitIndex(1)
END Init ;


(*
   AddTo - adds a new element to the end of SourceList, CurrentSource.
*)

PROCEDURE AddTo (l: SourceList) ;
BEGIN
   l^.right := CurrentSource ;
   l^.left  := CurrentSource^.left ;
   CurrentSource^.left^.right := l ;
   CurrentSource^.left := l ;
   l^.left^.line := cflex.GetLineNo()
END AddTo ;


(*
   SubFrom - subtracts, l, from the source list.
*)

PROCEDURE SubFrom (l: SourceList) ;
BEGIN
   l^.left^.right := l^.right ;
   l^.right^.left := l^.left
END SubFrom ;


(*
   NewElement - returns a new SourceList
*)

PROCEDURE NewElement (s: ADDRESS) : SourceList ;
VAR
   l: SourceList ;
BEGIN
   NEW(l) ;
   IF l=NIL
   THEN
      HALT
   ELSE
      WITH l^ DO
         name  := InitStringCharStar(s) ;
         left  := NIL ;
         right := NIL
      END
   END ;
   RETURN( l )
END NewElement ;


(*
   NewList - initializes an empty list with the classic dummy header element.
*)

PROCEDURE NewList () : SourceList ;
VAR
   l: SourceList ;
BEGIN
   NEW(l) ;
   WITH l^ DO
      left  := l ;
      right := l ;
      name  := NIL
   END ;
   RETURN( l )
END NewList ;


(*
   CheckIfNeedToDuplicate - checks to see whether the CurrentSource has
                            been used, if it has then duplicate the list.
*)

PROCEDURE CheckIfNeedToDuplicate ;
VAR
   l, h: SourceList ;
BEGIN
   IF CurrentUsed
   THEN
      l := CurrentSource^.right ;
      h := CurrentSource ;
      CurrentSource := NewList() ;
      WHILE l#h DO
         AddTo(NewElement(l^.name)) ;
         l := l^.right
      END
   END
END CheckIfNeedToDuplicate ;


(*
   PushFile - indicates that, filename, has just been included.
*)

PROCEDURE PushFile (filename: ADDRESS) ;
VAR
   l: SourceList ;
BEGIN
   CheckIfNeedToDuplicate ;
   AddTo(NewElement(filename)) ;
   IF Debugging
   THEN
      IF CurrentSource^.right#CurrentSource
      THEN
         l := CurrentSource ;
         REPEAT
            printf2('name = %s, line = %d\n', l^.name, l^.line) ;
            l := l^.right
         UNTIL l=CurrentSource
      END
   END
END PushFile ;


(*
   PopFile - indicates that we are returning to, filename, having finished
             an include.
*)

PROCEDURE PopFile (filename: ADDRESS) ;
VAR
   l: SourceList ;
BEGIN
   CheckIfNeedToDuplicate ;
   IF (CurrentSource#NIL) AND (CurrentSource^.left#CurrentSource)
   THEN
      l := CurrentSource^.left ;  (* last element *)
      SubFrom(l) ;
      DISPOSE(l) ;
      IF (CurrentSource^.left#CurrentSource) AND
         (NOT Equal(CurrentSource^.name, Mark(InitStringCharStar(filename))))
      THEN
         (* mismatch in source file names after preprocessing files *)
      END
   ELSE
      (* source file list is empty, cannot pop an include.. *)
   END
END PopFile ;


(*
   KillList - kills the SourceList providing that it has not been used.
*)

PROCEDURE KillList ;
VAR
   l, k: SourceList ;
BEGIN
   IF (NOT CurrentUsed) AND (CurrentSource#NIL)
   THEN
      l := CurrentSource ;
      REPEAT
         k := l ;
         l := l^.right ;
         DISPOSE(k)
      UNTIL l=CurrentSource
   END
END KillList ;


(*
   ReInitialize - re-initialize the all the data structures.
*)

PROCEDURE ReInitialize ;
VAR
   s, t: TokenBucket ;
BEGIN
   IF ListOfTokens.head#NIL
   THEN
      t := ListOfTokens.head ;
      REPEAT
         s := t ;
         t := t^.next ;
         DISPOSE(s) ;
      UNTIL t=NIL ;
      CurrentUsed := FALSE ;
      KillList
   END ;
   Init
END ReInitialize ;


(*
   SetFile - sets the current filename to, filename.
*)

PROCEDURE SetFile (filename: ADDRESS) ;
BEGIN
   KillList ;
   CurrentUsed   := FALSE ;
   CurrentSource := NewList() ;
   AddTo(NewElement(filename))
END SetFile ;


(*
   OpenSource - Attempts to open the source file, s.
                The success of the operation is returned.
*)

PROCEDURE OpenSource (s: String) : BOOLEAN ;
BEGIN
   IF UseBufferedTokens
   THEN
      GetToken ;
      RETURN( TRUE )
   ELSE
      IF cflex.OpenSource(string(s))
      THEN
         SetFile(string(s)) ;
         SyncOpenWithBuffer ;
         GetToken ;
         RETURN( TRUE )
      ELSE
         RETURN( FALSE )
      END
   END
END OpenSource ;


(*
   CloseSource - closes the current open file.
*)

PROCEDURE CloseSource ;
BEGIN
   IF UseBufferedTokens
   THEN
      WHILE currenttoken#eoftok DO
         GetToken
      END
   ELSE
      (* a subsequent call to cflex.OpenSource will really close the file *)
   END
END CloseSource ;


(*
   ResetForNewPass - reset the buffer pointers to the beginning ready for
                     a new pass
*)

PROCEDURE ResetForNewPass ;
BEGIN
   CurrentTokNo := 0 ;
   UseBufferedTokens := TRUE
END ResetForNewPass ;


(*
   DisplayToken -
*)

PROCEDURE DisplayToken ;
VAR
   n: Name ;
BEGIN
   cflex.CError(string(InitString('current token'))) ;
   IF currenttoken=identtok
   THEN
      n := makekey(currentstring) ;
      printf1('currenttoken = %a\n', n)
   ELSE
      CASE currenttoken OF

      eoftok               : printf0('eoftok\n') |
      startok              : printf0('*\n') |
      arrowtok             : printf0('->\n') |
      structtok            : printf0('struct\n') |
      lsbratok             : printf0('[\n') |
      rsbratok             : printf0(']\n') |
      lcbratok             : printf0('{\n') |
      rcbratok             : printf0('}\n') |
      lparatok             : printf0('(\n') |
      rparatok             : printf0(')\n') |
      semicolontok         : printf0(';\n') |
      longtok              : printf0('long\n') |
      inttok               : printf0('int\n') |
      chartok              : printf0('char\n') |
      enumtok              : printf0('enum\n') |
      typedeftok           : printf0('typedef\n') |
      floattok             : printf0('float\n') |
      doubletok            : printf0('double\n') |
      unsignedtok          : printf0('unsigned\n') |
      consttok             : printf0('const\n') |
      periodperiodperiodtok: printf0('...\n') |
      integertok           : printf0('integer number\n') |
      hexintegertok        : printf0('hexadecimal number\n') |
      octintegertok        : printf0('octal number\n') |
      identtok             : printf0('identifier\n') |
      realtok              : printf0('real number\n') |
      conststringtok       : printf0('constant string\n') |
      constchartok         : printf0('constant char\n') |
      codetok              : printf0('some C code\n') |
      starthashtok         : printf0('start#\n') |
      endhashtok           : printf0('end#\n') |
      definetok            : printf0('define\n') |
      definedtok           : stop ; printf0('defined\n') |
      undeftok             : printf0('undef\n') |
      iftok                : printf0('if\n') |
      elsetok              : printf0('else\n') |
      endiftok             : printf0('endif\n') |
      ifdeftok             : printf0('ifdef\n') |
      ifndeftok            : printf0('ifndef\n') |
      nottok               : printf0('not\n') |
      includetok           : printf0('include\n') |
      commatok             : printf0('comma\n') |
      periodtok            : printf0('period\n') |
      gretok               : printf0('gre\n') |
      lesstok              : printf0('less\n') |
      ortok                : printf0('or\n') |
      andtok               : printf0('and\n') |
      bartok               : printf0('bar\n') |
      ambersandtok         : printf0('ambersand\n') |
      shiftlefttok         : printf0('shiftleft\n') |
      shiftrighttok        : printf0('shiftright\n') |
      divtok               : printf0('div\n') |
      modtok               : printf0('mod\n') |
      sizeoftok            : printf0('sizeof\n') |
      hattok               : printf0('hat\n') |
      equaltok             : printf0('equal\n') |
      notequaltok          : printf0('notequal\n') |
      greequaltok          : printf0('greequal\n') |
      lessequaltok         : printf0('lessequal\n') |
      plustok              : printf0('plus\n') |
      minustok             : printf0('minus\n') |
      tildetok             : printf0('tilde\n') |
      externtok            : printf0('extern\n') |
      statictok            : printf0('static\n') |
      autotok              : printf0('auto\n') |
      registertok          : printf0('register\n') |
      voidtok              : printf0('void\n') |
      shorttok             : printf0('short\n') |
      signedtok            : printf0('signed\n') |
      uniontok             : printf0('union\n') |
      colontok             : printf0('colon\n') |
      becomestok           : printf0('becomes\n') |
      volatiletok          : printf0('volatile\n') |
      typetok              : printf0('type\n')

      ELSE
         cflex.CError(string(InitString('unrecognised token')))
      END
   END
END DisplayToken ;


(*
   GetToken - gets the next token into currenttoken.
*)

PROCEDURE GetToken ;
VAR
   t: CARDINAL ;
   b: TokenBucket ;
   l: CARDINAL ;
BEGIN
   IF UseBufferedTokens
   THEN
      t := CurrentTokNo ;
      b := FindTokenBucket(t) ;
      WITH b^.buf[t] DO
         currenttoken   := token ;
         currentstring  := KeyToCharStar(str) ;
         currentinteger := int ;
         IF Debugging
         THEN
            l := line
         END
      END ;
      IF Debugging
      THEN
         printf3('line %d (# %d  %d) ', l, t, CurrentTokNo) ;
         DisplayToken
      END ;
      INC(CurrentTokNo)
   ELSE
      IF ListOfTokens.tail=NIL
      THEN
         cflex.AdvanceToken ;
         IF ListOfTokens.tail=NIL
         THEN
            HALT
         END
      END ;

      IF ListOfTokens.LastBucketOffset>CurrentTokNo
      THEN
         t := CurrentTokNo ;
         b := FindTokenBucket(t) ;
         WITH b^.buf[t] DO
            currenttoken   := token ;
            currentstring  := KeyToCharStar(str) ;
            currentinteger := int ;
            IF Debugging
            THEN
               l := line
            END
         END ;
         INC(CurrentTokNo)
      ELSE
         WITH ListOfTokens.tail^ DO
            IF CurrentTokNo-ListOfTokens.LastBucketOffset<len
            THEN
               WITH buf[CurrentTokNo-ListOfTokens.LastBucketOffset] DO
                  currenttoken   := token ;
                  currentstring  := KeyToCharStar(str) ;
                  currentinteger := int
               END ;
               IF Debugging
               THEN
                  (* printf1('# %d ', CurrentTokNo) ; *)
                  DisplayToken
               END ;
               INC(CurrentTokNo)
            ELSE
               cflex.AdvanceToken ;
               GetToken ;
               (* printf0('\n'); cflex.CError(string(InitString('current token'))) ; *)
            END
         END
      END
   END
END GetToken ;


(*
   FlushTokens - removes the last token.
*)

PROCEDURE FlushTokens ;
BEGIN
   INC(CurrentTokNo)
END FlushTokens ;


(*
   SyncOpenWithBuffer - synchronise the buffer with the start of a file.
                        Skips all the tokens to do with the previous file.
*)

PROCEDURE SyncOpenWithBuffer ;
BEGIN
   IF ListOfTokens.tail#NIL
   THEN
      WITH ListOfTokens.tail^ DO
         CurrentTokNo := ListOfTokens.LastBucketOffset+len
      END
   END
END SyncOpenWithBuffer ;


(*
   InsertToken - inserts a symbol, token, infront of the current token
                 ready for the next pass.
*)

PROCEDURE InsertToken (token: toktype) ;
BEGIN
   IF ListOfTokens.tail#NIL
   THEN
      WITH ListOfTokens.tail^ DO
         IF len>0
         THEN
            buf[len-1].token := token
         END
      END ;
      AddTokToList(currenttoken, NulName, 0, GetLineNo(), CurrentSource) ;
      GetToken
   END
END InsertToken ;


(*
   InsertTokenAndRewind - inserts a symbol, token, infront of the current token
                          and then moves the token stream back onto the inserted token.
*)

PROCEDURE InsertTokenAndRewind (token: toktype) ;
BEGIN
   IF ListOfTokens.tail#NIL
   THEN
      WITH ListOfTokens.tail^ DO
         IF len>0
         THEN
            buf[len-1].token := token
         END
      END ;
      AddTokToList(currenttoken, NulName, 0, GetLineNo(), CurrentSource) ;
      currenttoken := token
   END
END InsertTokenAndRewind ;


(*
   GetLineNo - returns the current line number where the symbol occurs in
               the source file.
*)

PROCEDURE GetLineNo () : CARDINAL ;
BEGIN
   IF CurrentTokNo=0
   THEN
      RETURN( 0 )
   ELSE
      RETURN( TokenToLineNo(GetTokenNo(), 0) )
   END
END GetLineNo ;


(*
   GetTokenNo - returns the current token number.
*)

PROCEDURE GetTokenNo () : CARDINAL ;
BEGIN
   IF CurrentTokNo=0
   THEN
      RETURN( 0 )
   ELSE
      RETURN( CurrentTokNo-1 )
   END
END GetTokenNo ;


(*
   FindTokenBucket - returns the TokenBucket corresponding to the TokenNo.
*)

PROCEDURE FindTokenBucket (VAR TokenNo: CARDINAL) : TokenBucket ;
VAR
   b: TokenBucket ;
BEGIN
   b := ListOfTokens.head ;
   WHILE b#NIL DO
      WITH b^ DO
         IF TokenNo<len
         THEN
            RETURN( b )
         ELSE
            DEC(TokenNo, len)
         END
      END ;
      b := b^.next
   END ;
   RETURN( NIL )
END FindTokenBucket ;


(*
   TokenToLineNo - returns the line number of the current file for the
                   TokenNo. The depth refers to the include depth.
                   A depth of 0 is the current file, depth of 1 is the file
                   which included the current file. Zero is returned if the
                   depth exceeds the file nesting level.
*)

PROCEDURE TokenToLineNo (TokenNo: CARDINAL; depth: CARDINAL) : CARDINAL ;
VAR
   b: TokenBucket ;
   l: SourceList ;
BEGIN
   b := FindTokenBucket(TokenNo) ;
   IF b=NIL
   THEN
      RETURN( 0 )
   ELSE
      IF depth=0
      THEN
         RETURN( b^.buf[TokenNo].line )
      ELSE
         l := b^.buf[TokenNo].file^.left ;
         WHILE depth>0 DO
            l := l^.left ;
            IF l=b^.buf[TokenNo].file^.left
            THEN
               RETURN( 0 )
            END ;
            DEC(depth)
         END ;
         RETURN( l^.line )
      END
   END
END TokenToLineNo ;


(*
   FindFileNameFromToken - returns the complete FileName for the appropriate
                           source file yields the token number, TokenNo.
                           The, Depth, indicates the include level: 0..n
                           Level 0 is the current. NIL is returned if n+1
                           is requested.
*)

PROCEDURE FindFileNameFromToken (TokenNo: CARDINAL; depth: CARDINAL) : String ;
VAR
   b: TokenBucket ;
   l: SourceList ;
BEGIN
   b := FindTokenBucket(TokenNo) ;
   IF b=NIL
   THEN
      RETURN( NIL )
   ELSE
      l := b^.buf[TokenNo].file^.left ;
      WHILE depth>0 DO
         l := l^.left ;
         IF l=b^.buf[TokenNo].file^.left
         THEN
            RETURN( NIL )
         END ;
         DEC(depth)
      END ;
      RETURN( l^.name )
   END
END FindFileNameFromToken ;


(*
   GetFileName - returns a String defining the current file.
*)

PROCEDURE GetFileName () : String ;
BEGIN
   RETURN( FindFileNameFromToken(GetTokenNo(), 0) )
END GetFileName ;


(*
   AddTokToList - adds a token to a dynamic list.
*)

PROCEDURE AddTokToList (t: toktype; n: Name;
                        i: INTEGER; l: CARDINAL; f: SourceList) ;
BEGIN
   IF ListOfTokens.head=NIL
   THEN
      NEW(ListOfTokens.head) ;
      IF ListOfTokens.head=NIL
      THEN
         (* list error *)
      END ;
      ListOfTokens.tail := ListOfTokens.head ;
      ListOfTokens.tail^.len := 0
   ELSIF ListOfTokens.tail^.len=MaxBucketSize
   THEN
      Assert(ListOfTokens.tail^.next=NIL) ;
      NEW(ListOfTokens.tail^.next) ;
      IF ListOfTokens.tail^.next=NIL
      THEN
         (* list error *)
      ELSE
         ListOfTokens.tail := ListOfTokens.tail^.next ;
         ListOfTokens.tail^.len := 0
      END ;
      INC(ListOfTokens.LastBucketOffset, MaxBucketSize)
   END ;
   WITH ListOfTokens.tail^ DO
      next := NIL ;
      WITH buf[len] DO
         token := t ;
         str   := n ;
         int   := i ;
         line  := l ;
         file  := f
      END ;
      INC(len)
   END
END AddTokToList ;


(*
   IsLastTokenEof - returns TRUE if the last token was an eoftok
*)

PROCEDURE IsLastTokenEof () : BOOLEAN ;
VAR
   b: TokenBucket ;
BEGIN
   IF ListOfTokens.tail#NIL
   THEN
      IF ListOfTokens.tail^.len=0
      THEN
         b := ListOfTokens.head ;
         IF b=ListOfTokens.tail
         THEN
            RETURN( FALSE )
         END ;
         WHILE b^.next#ListOfTokens.tail DO
            b := b^.next
         END ;
      ELSE
         b := ListOfTokens.tail
      END ;
      WITH b^ DO
         RETURN( buf[len-1].token=eoftok )
      END
   END ;
   RETURN( FALSE )
END IsLastTokenEof ;


(* ***********************************************************************
 *
 * These functions allow c.flex to deliver tokens into the buffer
 *
 ************************************************************************* *)

(*
   AddTok - adds a token to the buffer.
*)

PROCEDURE AddTok (t: toktype) ;
BEGIN
   IF NOT ((t=eoftok) AND IsLastTokenEof())
   THEN
      AddTokToList(t, NulName, 0, cflex.GetLineNo(), CurrentSource) ;
      CurrentUsed := TRUE
   END
END AddTok ;


(*
   AddTokCharStar - adds a token to the buffer and an additional string, s.
                    A copy of string, s, is made.
*)

PROCEDURE AddTokCharStar (t: toktype; s: ADDRESS) ;
BEGIN
   IF (t=identtok) AND PushMacroDefinition(makekey(s))
   THEN
      (* do nothing *)
   ELSE
      AddTokToList(t, makekey(s), 0, cflex.GetLineNo(), CurrentSource) ;
      CurrentUsed := TRUE
   END
END AddTokCharStar ;


(*
   AddTokInteger - adds a token and an integer to the buffer.
*)

PROCEDURE AddTokInteger (t: toktype; i: INTEGER) ;
VAR
   s: String ;
   lineno: CARDINAL ;
BEGIN
   lineno := cflex.GetLineNo() ;
   s := Sprintf1(Mark(InitString('%d')), lineno) ;
   AddTokToList(t, makekey(string(s)), i, lineno, CurrentSource) ;
   s := KillString(s) ;
   CurrentUsed := TRUE
END AddTokInteger ;


BEGIN
   Init
END CLexBuf.
