(* M2LexBuf.mod provides a buffer for m2.lex.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2LexBuf ;

IMPORT m2flex ;
IMPORT FIO ;

FROM libc IMPORT strlen ;
FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM DynamicStrings IMPORT string, InitString, InitStringCharStar, Equal, Mark, KillString ;
FROM FormatStrings IMPORT Sprintf1 ;
FROM NameKey IMPORT NulName, Name, makekey, MakeKey, KeyToCharStar ;
FROM M2Reserved IMPORT toktype, tokToTok ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3 ;
FROM M2Debug IMPORT Assert ;
FROM NameKey IMPORT makekey ;
FROM NumberIO IMPORT CardToStr ;
FROM m2linemap IMPORT location_t, GetLocationBinary ;
FROM M2Emit IMPORT UnknownLocation, BuiltinsLocation ;
FROM M2Error IMPORT WarnStringAt ;
FROM M2MetaError IMPORT MetaErrorT0 ;
FROM M2Options IMPORT GetDebugTraceToken ;
FROM M2LangDump IMPORT GetDumpFile ;

FROM Indexing IMPORT Index, InitIndex, GetIndice, PutIndice,
                     KillIndex, ForeachIndiceInIndexDo,
                     LowIndice, HighIndice, IsEmpty, InBounds,
                     InitIndexTuned ;


CONST
   Tracing            = FALSE ;
   Debugging          = FALSE ;
   DebugRecover       = FALSE ;
   BadTokenNo         = 32579 ;
   InitialSourceToken = 2 ;   (* 0 is unknown, 1 is builtin.  *)

TYPE
   SourceList = POINTER TO RECORD
                              left,
                              right: SourceList ;
                              name : String ;
                              line : CARDINAL ;
                              col  : CARDINAL ;
                           END ;

   TokenDesc = POINTER TO RECORD
                  token : toktype ;
                  str   : Name ;          (* ident name or string literal.  *)
                  int   : INTEGER ;
                  line  : CARDINAL ;
                  col   : CARDINAL ;
                  file  : SourceList ;
                  loc   : location_t ;
                  insert: Index ;         (* Contains any inserted tokens.  *)
               END ;

VAR
   CurrentSource    : SourceList ;
   UseBufferedTokens,
   CurrentUsed      : BOOLEAN ;
   ListOfTokens     : Index ;
   CurrentTokNo     : CARDINAL ;
   InsertionIndex   : CARDINAL ;
   SeenEof          : BOOLEAN ;  (* Have we seen eof since the last call
                                    to OpenSource.  *)


PROCEDURE stop ;
END stop ;


(*
   InitTokenDesc - returns a TokenDesc filled in with the parameters and
                   the insert field set to NIL.
*)

PROCEDURE InitTokenDesc (token: toktype; str: Name; int: INTEGER;
                         line, col: CARDINAL;
                         file: SourceList; loc: location_t) : TokenDesc ;
VAR
   tokdesc: TokenDesc ;
BEGIN
   NEW (tokdesc) ;
   tokdesc^.token := token ;
   tokdesc^.str := str ;
   tokdesc^.int := int ;
   tokdesc^.line := line ;
   tokdesc^.col := col ;
   tokdesc^.file := file ;
   tokdesc^.loc := loc ;
   tokdesc^.insert := NIL ;
   RETURN tokdesc
END InitTokenDesc ;


(*
   DeleteTokenDesc - delete tokdesc and any sub indices.
*)

PROCEDURE DeleteTokenDesc (tokdesc: TokenDesc) ;
BEGIN
   IF tokdesc^.insert # NIL
   THEN
      ForeachIndiceInIndexDo (tokdesc^.insert, DeleteTokenDesc)
   END ;
   DISPOSE (tokdesc)
END DeleteTokenDesc ;


(*
   Append - appends tokdesc to the end of the list defined by index.
*)

PROCEDURE Append (index: Index; tokdesc: TokenDesc) ;
BEGIN
   IF IsEmpty (index)
   THEN
      PutIndice (index, LowIndice (index), tokdesc)
   ELSE
      PutIndice (index, HighIndice (index) +1, tokdesc)
   END
END Append ;


(*
   InitTokenList - creates an empty token list, which starts the first source token
                   at position 2.  This allows position 0 to be used for the unknown
                   location and position 1 for the builtin token.
*)

PROCEDURE InitTokenList ;
BEGIN
   (* 65K elements in the array and when it becomes full it will grow to 1M, 16M etc elements.  *)
   ListOfTokens := InitIndexTuned (0, 1024*1024 DIV 16, 16) ;
   Append (ListOfTokens, InitTokenDesc (eoftok, NulName, 0, 0, 0, NIL, UnknownLocation ())) ;
   Append (ListOfTokens, InitTokenDesc (eoftok, NulName, 0, 0, 0, NIL, BuiltinsLocation ()))
END InitTokenList ;


(*
   Init - initializes the token list and source list.
*)

PROCEDURE Init ;
BEGIN
   SeenEof := FALSE ;
   InsertionIndex := 0 ;
   currenttoken := eoftok ;
   CurrentTokNo := InitialSourceToken ;
   CurrentSource := NIL ;
   UseBufferedTokens := FALSE ;
   InitTokenList
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
   WITH l^.left^ DO
      line := m2flex.GetLineNo() ;
      col  := m2flex.GetColumnNo()
   END
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
   NEW (l) ;
   IF l = NIL
   THEN
      HALT
   ELSE
      WITH l^ DO
         name  := InitStringCharStar(s) ;
         left  := NIL ;
         right := NIL
      END
   END ;
   RETURN l
END NewElement ;


(*
   NewList - initializes an empty list with the classic dummy header element.
*)

PROCEDURE NewList () : SourceList ;
VAR
   l: SourceList ;
BEGIN
   NEW (l) ;
   WITH l^ DO
      left  := l ;
      right := l ;
      name  := NIL
   END ;
   RETURN l
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
         AddTo (NewElement (l^.name)) ;
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
   AddTo (NewElement (filename)) ;
   IF Debugging
   THEN
      IF CurrentSource^.right#CurrentSource
      THEN
         l := CurrentSource ;
         REPEAT
            printf3('name = %s, line = %d, col = %d\n', l^.name, l^.line, l^.col) ;
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
      SubFrom (l) ;
      DISPOSE (l) ;
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
BEGIN
   ForeachIndiceInIndexDo (ListOfTokens, DeleteTokenDesc) ;
   CurrentUsed := FALSE ;
   KillList ;
   Init
END ReInitialize ;


(*
   SetFile - sets the current filename to, filename.
*)

PROCEDURE SetFile (filename: ADDRESS) ;
BEGIN
   KillList ;
   CurrentUsed   := FALSE ;
   CurrentSource := NewList () ;
   AddTo (NewElement (filename))
END SetFile ;


(*
   OpenSource - Attempts to open the source file, s.
                The success of the operation is returned.
*)

PROCEDURE OpenSource (s: String) : BOOLEAN ;
BEGIN
   tprintf1 ("OpenSource (%s)\n", s) ;
   SeenEof := FALSE ;
   IF UseBufferedTokens
   THEN
      GetToken ;
      RETURN TRUE
   ELSE
      IF m2flex.OpenSource (string (s))
      THEN
         SetFile (string (s)) ;
         GetToken ;
         IF IsLastTokenEof ()
         THEN
            MetaErrorT0 (GetTokenNo (), "source file is empty")
         END ;
         RETURN TRUE
      ELSE
         RETURN FALSE
      END
   END
END OpenSource ;


(*
   CloseSource - closes the current open file.
*)

PROCEDURE CloseSource ;
BEGIN
   tprintf0 ("CloseSource\n") ;
   IF UseBufferedTokens
   THEN
      WHILE currenttoken#eoftok DO
         GetToken
      END
   ELSE
      (* a subsequent call to m2flex.OpenSource will really close the file *)
   END
END CloseSource ;


(*
   ResetForNewPass - reset the buffer pointers to the beginning ready for
                     a new pass
*)

PROCEDURE ResetForNewPass ;
BEGIN
   InsertionIndex := 0 ;
   CurrentTokNo := InitialSourceToken ;
   UseBufferedTokens := TRUE
END ResetForNewPass ;


(*
   DisplayToken - display the token name using printf0 no newline is emitted.
*)

PROCEDURE DisplayToken (tok: toktype) ;
BEGIN
   CASE tok OF

   eoftok: printf0('eoftok') |
   plustok: printf0('plustok') |
   minustok: printf0('minustok') |
   timestok: printf0('timestok') |
   dividetok: printf0('dividetok') |
   becomestok: printf0('becomestok') |
   ambersandtok: printf0('ambersandtok') |
   periodtok: printf0('periodtok') |
   commatok: printf0('commatok') |
   semicolontok: printf0('semicolontok') |
   lparatok: printf0('lparatok') |
   rparatok: printf0('rparatok') |
   lsbratok: printf0('lsbratok') |
   rsbratok: printf0('rsbratok') |
   lcbratok: printf0('lcbratok') |
   rcbratok: printf0('rcbratok') |
   uparrowtok: printf0('uparrowtok') |
   singlequotetok: printf0('singlequotetok') |
   equaltok: printf0('equaltok') |
   hashtok: printf0('hashtok') |
   lesstok: printf0('lesstok') |
   greatertok: printf0('greatertok') |
   lessgreatertok: printf0('lessgreatertok') |
   lessequaltok: printf0('lessequaltok') |
   greaterequaltok: printf0('greaterequaltok') |
   periodperiodtok: printf0('periodperiodtok') |
   colontok: printf0('colontok') |
   doublequotestok: printf0('doublequotestok') |
   bartok: printf0('bartok') |
   andtok: printf0('andtok') |
   arraytok: printf0('arraytok') |
   begintok: printf0('begintok') |
   bytok: printf0('bytok') |
   casetok: printf0('casetok') |
   consttok: printf0('consttok') |
   definitiontok: printf0('definitiontok') |
   divtok: printf0('divtok') |
   dotok: printf0('dotok') |
   elsetok: printf0('elsetok') |
   elsiftok: printf0('elsiftok') |
   endtok: printf0('endtok') |
   exittok: printf0('exittok') |
   exporttok: printf0('exporttok') |
   fortok: printf0('fortok') |
   fromtok: printf0('fromtok') |
   iftok: printf0('iftok') |
   implementationtok: printf0('implementationtok') |
   importtok: printf0('importtok') |
   intok: printf0('intok') |
   looptok: printf0('looptok') |
   modtok: printf0('modtok') |
   moduletok: printf0('moduletok') |
   nottok: printf0('nottok') |
   oftok: printf0('oftok') |
   ortok: printf0('ortok') |
   pointertok: printf0('pointertok') |
   proceduretok: printf0('proceduretok') |
   qualifiedtok: printf0('qualifiedtok') |
   unqualifiedtok: printf0('unqualifiedtok') |
   recordtok: printf0('recordtok') |
   repeattok: printf0('repeattok') |
   returntok: printf0('returntok') |
   settok: printf0('settok') |
   thentok: printf0('thentok') |
   totok: printf0('totok') |
   typetok: printf0('typetok') |
   untiltok: printf0('untiltok') |
   vartok: printf0('vartok') |
   whiletok: printf0('whiletok') |
   withtok: printf0('withtok') |
   asmtok: printf0('asmtok') |
   volatiletok: printf0('volatiletok') |
   periodperiodperiodtok: printf0('periodperiodperiodtok') |
   datetok: printf0('datetok') |
   linetok: printf0('linetok') |
   filetok: printf0('filetok') |
   integertok: printf0('integertok') |
   identtok: printf0('identtok') |
   realtok: printf0('realtok') |
   stringtok: printf0('stringtok')

   ELSE
   END
END DisplayToken ;


VAR
   indent: CARDINAL ;


(*
   DumpToken -
*)

PROCEDURE DumpToken (tokdesc: TokenDesc) ;
VAR
   n: CARDINAL ;
BEGIN
   n := indent ;
   WHILE n > 0 DO
      printf0 (" ") ;
      DEC (n)
   END ;
   WITH tokdesc^ DO
      DisplayToken (token) ;
      IF str # NulName
      THEN
         printf1 (" %a", str)
      END ;
      IF insert # NIL
      THEN
         printf0 ("inserted error recovery tokens\n") ;
         INC (indent, 2) ;
         ForeachIndiceInIndexDo (insert, DumpToken) ;
         DEC (indent, 2)
      END
   END
END DumpToken ;


(*
   DumpTokens - displays all tokens.
*)

PROCEDURE DumpTokens ;
VAR
   high,
   ind : CARDINAL ;
BEGIN
   IF IsEmpty (ListOfTokens)
   THEN
      printf0 ("The token buffer is empty\n")
   ELSE
      ind := LowIndice (ListOfTokens) ;
      high := HighIndice (ListOfTokens) ;
      WHILE ind <= high DO
         printf1 ("%5d ", ind) ;
         DumpToken (GetIndice (ListOfTokens, ind)) ;
         INC (ind)
      END
   END
END DumpTokens ;


(*
   CopyOutCurrent - copies the token in buffer[index][insertion] into
                    then current token global variables.
*)

PROCEDURE CopyOutCurrent (buffer: Index; index, insertion: CARDINAL) ;
VAR
   tokdesc: TokenDesc ;
BEGIN
   tokdesc := GetIndice (buffer, index) ;
   IF insertion # 0
   THEN
      tokdesc := GetIndice (tokdesc^.insert, insertion)
   END ;
   WITH tokdesc^ DO
      currenttoken   := token ;
      currentstring  := KeyToCharStar (str) ;
      currentcolumn  := col ;
      currentinteger := int
   END
END CopyOutCurrent ;


(*
   UpdateToken - update the global current token variables from buffer[index]
                 using inserted tokens if directed by InsertionIndex.
*)

PROCEDURE UpdateToken (buffer: Index; index: CARDINAL) ;
VAR
   tokdesc: TokenDesc ;
BEGIN
   tokdesc := GetIndice (buffer, index) ;
   IF InsertionIndex > 0
   THEN
      (* We have an inserted token to use.  *)
      Assert (tokdesc^.insert # NIL) ;
      CopyOutCurrent (buffer, index, InsertionIndex) ;
      (* Move InsertionIndex to the next position.  *)
      INC (InsertionIndex) ;
      IF InsertionIndex > HighIndice (tokdesc^.insert)
      THEN
         (* We are done consuming the inserted tokens, so move
            onto the next original source token.  *)
         InsertionIndex := 0 ;
         INC (CurrentTokNo)
      END
   ELSIF (tokdesc^.insert # NIL) AND (InsertionIndex = 0)
   THEN
      (* This source token has extra tokens appended after it by the error recovery.
         Set the index ready for the next UpdateToken which will read the extra
         tokens.  *)
      InsertionIndex := 1 ;
      (* However this call must read the original token.  *)
      CopyOutCurrent (buffer, index, 0)
   ELSE
      CopyOutCurrent (buffer, index, 0) ;
      (* Move onto the next original source token.  *)
      INC (CurrentTokNo)
   END
END UpdateToken ;


(*
   GetTokenFiltered - providing that we have not already seen an eof for this source
                      file call m2flex.GetToken and GetToken if requested.
*)

PROCEDURE GetTokenFiltered (callGetToken: BOOLEAN) ;
BEGIN
   IF SeenEof
   THEN
      currenttoken := eoftok
   ELSE
      (* Call the lexical phase to place a new token into the last bucket.  *)
      m2flex.GetToken () ;
      IF callGetToken
      THEN
         GetToken
      END
   END
END GetTokenFiltered ;


(*
   GetToken - gets the next token into currenttoken.
*)

PROCEDURE GetToken ;
VAR
   buf: ARRAY [0..20] OF CHAR ;
BEGIN
   IF UseBufferedTokens
   THEN
      UpdateToken (ListOfTokens, CurrentTokNo) ;
      IF GetDebugTraceToken ()
      THEN
         CardToStr (CurrentTokNo, 0, buf) ;
         FIO.WriteString (GetDumpFile (), 'token: ') ;
         FIO.WriteString (GetDumpFile (), buf) ;
         FIO.WriteLine (GetDumpFile ())
      END
   ELSE
      IF NOT InBounds (ListOfTokens, CurrentTokNo)
      THEN
         GetTokenFiltered (FALSE)
      END ;
      UpdateToken (ListOfTokens, CurrentTokNo) ;
      IF GetDebugTraceToken ()
      THEN
         CardToStr (CurrentTokNo, 0, buf) ;
         m2flex.M2Error (ADR (buf))
      END
   END
END GetToken ;


(*
   AppendInsertToken -
*)

PROCEDURE AppendInsertToken (index: Index; tokdesc: TokenDesc) ;
BEGIN
   IF IsEmpty (index)
   THEN
      PutIndice (index, LowIndice (index), tokdesc)
   ELSE
      PutIndice (index, HighIndice (index) +1, tokdesc)
   END
END AppendInsertToken ;


(*
   DupTok - duplicate tokdesc and replaces the token field with token.
*)

PROCEDURE DupTok (tokdesc: TokenDesc; token: toktype) : TokenDesc ;
VAR
   dup: TokenDesc ;
BEGIN
   NEW (dup) ;
   Assert (dup # NIL) ;
   dup^ := tokdesc^ ;
   dup^.token := token ;
   RETURN dup
END DupTok ;


(*
   InsertToken - inserts a symbol token infront of the current token
                 ready for the next pass.
*)

PROCEDURE InsertToken (token: toktype) ;
VAR
   prev   : CARDINAL ;
   tokdesc: TokenDesc ;
BEGIN
   Assert (ListOfTokens # NIL) ;
   Assert (NOT IsEmpty (ListOfTokens)) ;
   prev := GetTokenNo () -1 ;
   tokdesc := GetIndice (ListOfTokens, prev) ;
   IF tokdesc^.insert = NIL
   THEN
      tokdesc^.insert := InitIndex (1)
   END ;
   AppendInsertToken (tokdesc^.insert, DupTok (tokdesc, token))
END InsertToken ;


(*
   InsertTokenAndRewind - inserts a symbol token infront of the current token
                          and then moves the token stream back onto the inserted
                          token.
*)

PROCEDURE InsertTokenAndRewind (token: toktype) ;
VAR
   position : CARDINAL ;
   tokdesc  : TokenDesc ;
BEGIN
   IF GetTokenNo () > 0
   THEN
      InsertToken (token) ;
      position := CurrentTokNo -2 ;
      tokdesc := GetIndice (ListOfTokens, position) ;
      IF tokdesc^.insert = NIL
      THEN
         tokdesc^.insert := InitIndex (1)
      END ;
      AppendInsertToken (tokdesc^.insert, DupTok (tokdesc, token)) ;
      InsertionIndex := HighIndice (tokdesc^.insert) ;
      DEC (CurrentTokNo, 2) ;
      GetToken
   END
END InsertTokenAndRewind ;


(*
   GetPreviousTokenLineNo - returns the line number of the previous token.
*)

PROCEDURE GetPreviousTokenLineNo () : CARDINAL ;
BEGIN
   RETURN GetLineNo ()
END GetPreviousTokenLineNo ;


(*
   GetLineNo - returns the current line number where the symbol occurs in
               the source file.
*)

PROCEDURE GetLineNo () : CARDINAL ;
BEGIN
   IF CurrentTokNo = 0
   THEN
      RETURN 0
   ELSE
      RETURN TokenToLineNo (GetTokenNo (), 0)
   END
END GetLineNo ;


(*
   GetColumnNo - returns the current column where the symbol occurs in
                 the source file.
*)

PROCEDURE GetColumnNo () : CARDINAL ;
BEGIN
   IF CurrentTokNo = 0
   THEN
      RETURN 0
   ELSE
      RETURN TokenToColumnNo (GetTokenNo (), 0)
   END
END GetColumnNo ;


(*
   GetTokenNo - returns the current token number.
*)

PROCEDURE GetTokenNo () : CARDINAL ;
BEGIN
   IF CurrentTokNo = 0
   THEN
      RETURN 0
   ELSE
      RETURN CurrentTokNo-1
   END
END GetTokenNo ;


(*
   GetTokenName - returns the token name given the tokenno.
*)

PROCEDURE GetTokenName (tokenno: CARDINAL) : Name ;
VAR
   tokdesc: TokenDesc ;
   name   : Name ;
BEGIN
   IF InBounds (ListOfTokens, tokenno)
   THEN
      tokdesc := GetIndice (ListOfTokens, tokenno) ;
      name := tokToTok (tokdesc^.token) ;
      IF name = NulName
      THEN
         RETURN tokdesc^.str
      ELSE
         RETURN name
      END
   END ;
   RETURN NulName
END GetTokenName ;


(*
   TokenToLineNo - returns the line number of the current file for the
                   tokenno. The depth refers to the include depth.
                   A depth of 0 is the current file, depth of 1 is the file
                   which included the current file. Zero is returned if the
                   depth exceeds the file nesting level.
*)

PROCEDURE TokenToLineNo (tokenno: CARDINAL; depth: CARDINAL) : CARDINAL ;
VAR
   tokdesc: TokenDesc ;
   level  : SourceList ;
 BEGIN
   IF (tokenno # UnknownTokenNo) AND (tokenno # BuiltinTokenNo)
   THEN
      IF InBounds (ListOfTokens, tokenno)
      THEN
         tokdesc := GetIndice (ListOfTokens, tokenno) ;
         IF depth = 0
         THEN
            RETURN tokdesc^.line
         ELSE
            level := tokdesc^.file^.left ;
            WHILE depth > 0 DO
               level := level^.left ;
               IF level = tokdesc^.file^.left
               THEN
                  RETURN 0
               END ;
               DEC (depth)
            END ;
            RETURN level^.line
         END
      END
   END ;
   RETURN 0
END TokenToLineNo ;


(*
   TokenToColumnNo - returns the column number of the current file for the
                     tokenno. The depth refers to the include depth.
                     A depth of 0 is the current file, depth of 1 is the file
                     which included the current file. Zero is returned if the
                     depth exceeds the file nesting level.
*)

PROCEDURE TokenToColumnNo (tokenno: CARDINAL; depth: CARDINAL) : CARDINAL ;
VAR
   tokdesc: TokenDesc ;
   level  : SourceList ;
BEGIN
   IF (tokenno # UnknownTokenNo) AND (tokenno # BuiltinTokenNo)
   THEN
      IF InBounds (ListOfTokens, tokenno)
      THEN
         tokdesc := GetIndice (ListOfTokens, tokenno) ;
         IF depth = 0
         THEN
            RETURN tokdesc^.col
         ELSE
            level := tokdesc^.file^.left ;
            WHILE depth > 0 DO
               level := level^.left ;
               IF level = tokdesc^.file^.left
               THEN
                  RETURN 0
               END ;
               DEC (depth)
            END ;
            RETURN level^.col
         END
      END
   END ;
   RETURN 0
END TokenToColumnNo ;


(*
   TokenToLocation - returns the location_t corresponding to tokenno.
*)

PROCEDURE TokenToLocation (tokenno: CARDINAL) : location_t ;
VAR
   tokdesc: TokenDesc ;
BEGIN
   IF tokenno = UnknownTokenNo
   THEN
      RETURN UnknownLocation ()
   ELSIF tokenno = BuiltinTokenNo
   THEN
      RETURN BuiltinsLocation ()
   ELSIF InBounds (ListOfTokens, tokenno)
   THEN
      tokdesc := GetIndice (ListOfTokens, tokenno) ;
      RETURN tokdesc^.loc
   END ;
   RETURN UnknownLocation ()
END TokenToLocation ;


(*
   FindFileNameFromToken - returns the complete FileName for the appropriate
                           source file yields the token number, TokenNo.
                           The, Depth, indicates the include level: 0..n
                           Level 0 is the current. NIL is returned if n+1
                           is requested.
*)

PROCEDURE FindFileNameFromToken (tokenno: CARDINAL; depth: CARDINAL) : String ;
VAR
   tokdesc: TokenDesc ;
   level  : SourceList ;
BEGIN
   IF (tokenno # UnknownTokenNo) AND (tokenno # BuiltinTokenNo)
   THEN
      IF InBounds (ListOfTokens, tokenno)
      THEN
         tokdesc := GetIndice (ListOfTokens, tokenno) ;
         level := tokdesc^.file^.left ;
         WHILE depth > 0 DO
            level := level^.left ;
            IF level = tokdesc^.file^.left
            THEN
               RETURN NIL
            END ;
            DEC (depth)
         END ;
         RETURN level^.name
      END
   END ;
   RETURN NIL
END FindFileNameFromToken ;


(*
   GetFileName - returns a String defining the current file.
*)

PROCEDURE GetFileName () : String ;
BEGIN
   RETURN FindFileNameFromToken (GetTokenNo (), 0)
END GetFileName ;


(*
   AddTokToList - adds a token to a dynamic list.
*)

PROCEDURE AddTokToList (token: toktype; str: Name;
                        int: INTEGER; line: CARDINAL; col: CARDINAL;
                        file: SourceList; location: location_t) ;
BEGIN
   Append (ListOfTokens, InitTokenDesc (token, str, int, line,
                                        col, file, location))
END AddTokToList ;


(*
   IsLastTokenEof - returns TRUE if the last token was an eoftok
*)

PROCEDURE IsLastTokenEof () : BOOLEAN ;
VAR
   tokdesc: TokenDesc ;
BEGIN
   IF IsEmpty (ListOfTokens)
   THEN
      RETURN FALSE
   ELSE
      tokdesc := GetIndice (ListOfTokens, HighIndice (ListOfTokens)) ;
      RETURN tokdesc^.token = eoftok
   END
END IsLastTokenEof ;


(*
   PrintTokenNo - displays token and the location of the token.
*)

PROCEDURE PrintTokenNo (tokenno: CARDINAL) ;
VAR
   s: String ;
BEGIN
   printf1 ("tokenno = %d, ", tokenno) ;
   s := InitStringCharStar (KeyToCharStar (GetTokenName (tokenno))) ;
   printf1 ("%s\n", s) ;
   s := KillString (s)
END PrintTokenNo ;


(*
   isSrcToken - returns TRUE if tokenno is associated with
                program source code.
*)

PROCEDURE isSrcToken (tokenno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (tokenno # UnknownTokenNo) AND (tokenno # BuiltinTokenNo)
END isSrcToken ;


(*
   MakeVirtualTok - providing caret, left, right are associated with a source file
                    and exist on the same src line then
                    create and return a new tokenno which is created from
                    tokenno left and right.  Otherwise return caret.
*)

PROCEDURE MakeVirtualTok (caret, left, right: CARDINAL) : CARDINAL ;
VAR
   descLeft, descRight: TokenDesc ;
   lc, ll, lr         : location_t ;
BEGIN
   IF isSrcToken (caret) AND isSrcToken (left) AND isSrcToken (right)
   THEN
      lc := TokenToLocation (caret) ;
      ll := TokenToLocation (left) ;
      lr := TokenToLocation (right) ;
      IF InBounds (ListOfTokens, left) AND InBounds (ListOfTokens, right)
      THEN
         descLeft := GetIndice (ListOfTokens, left) ;
         descRight := GetIndice (ListOfTokens, right) ;
         IF (descLeft^.line = descRight^.line) AND
            (descLeft^.file = descRight^.file)
         THEN
            (* On the same line, create a new token and location.  *)
            AddTokToList (virtualrangetok, NulName, 0,
                          descLeft^.line, descLeft^.col, descLeft^.file,
                          GetLocationBinary (lc, ll, lr)) ;
            caret := HighIndice (ListOfTokens)
         END
      END
   END ;
   IF caret = BadTokenNo
   THEN
      stop
   END ;
   RETURN caret
END MakeVirtualTok ;


(*
   MakeVirtual2Tok - creates and return a new tokenno which is created from
                     two tokens left and right.
*)

PROCEDURE MakeVirtual2Tok (left, right: CARDINAL) : CARDINAL ;
BEGIN
   RETURN MakeVirtualTok (left, left, right) ;
END MakeVirtual2Tok ;


(*
   tprintf0 -
*)

PROCEDURE tprintf0 (format: ARRAY OF CHAR) ;
BEGIN
   IF Tracing
   THEN
      printf0 (format)
   END
END tprintf0 ;


(*
   tprintf1 -
*)

PROCEDURE tprintf1 (format: ARRAY OF CHAR; str: String) ;
BEGIN
   IF Tracing
   THEN
      printf1 (format, str)
   END
END tprintf1 ;


(* ***********************************************************************
 *
 * These functions allow m2.flex to deliver tokens into the buffer
 *
 ************************************************************************* *)

(*
   AddTok - adds a token to the buffer.
*)

PROCEDURE AddTok (t: toktype) ;
VAR
   s: String ;
BEGIN
   IF Tracing
   THEN
      printf0 (" m2.flex -> AddTok ") ;
      DisplayToken (t) ;
      printf0 ("\n") ;
   END ;
   IF (t=eoftok) AND SeenEof
   THEN
      IF Debugging
      THEN
         printf0 ("extra eoftok ignored as buffer already contains eoftok\n")
      END
   ELSE
      IF Debugging
      THEN
         printf0 ("adding token: ") ; DisplayToken (t) ;
         printf0 ("\n")
      END ;
      AddTokToList(t, NulName, 0,
                   m2flex.GetLineNo(), m2flex.GetColumnNo(), CurrentSource,
                   m2flex.GetLocation()) ;
      CurrentUsed := TRUE ;
      IF Debugging
      THEN
         (* display each token as a warning.  *)
         s := InitStringCharStar (KeyToCharStar (GetTokenName (GetTokenNo ()))) ;
         WarnStringAt (s, GetTokenNo ())
      END ;
      IF t = eoftok
      THEN
         SeenEof := TRUE
      END
   END
END AddTok ;


(*
   AddTokCharStar - adds a token to the buffer and an additional string, s.
                    A copy of string, s, is made.
*)

PROCEDURE AddTokCharStar (t: toktype; s: ADDRESS) ;
VAR
   str: String ;
BEGIN
   Assert (t # eoftok) ;
   IF Tracing
   THEN
      printf0 (" m2.flex -> AddTokCharStar ") ;
      DisplayToken (t) ;
      str := InitStringCharStar (s) ;
      printf1 (" %s\n", str) ;
      str := KillString (str)
   END ;
   IF Debugging
   THEN
      printf0 ("AddTokCharStar: ") ; DisplayToken (t) ; printf0 ("\n")
   END ;
   AddTokToList(t, makekey(s), 0, m2flex.GetLineNo(),
                m2flex.GetColumnNo(), CurrentSource, m2flex.GetLocation()) ;
   CurrentUsed := TRUE
END AddTokCharStar ;


(*
   AddTokInteger - adds a token and an integer to the buffer.
*)

PROCEDURE AddTokInteger (t: toktype; i: INTEGER) ;
VAR
   s: String ;
   c,
   l: CARDINAL ;
BEGIN
   Assert (t # eoftok) ;
   IF Tracing
   THEN
      printf0 (" m2.flex -> AddTokInteger ") ;
      DisplayToken (t) ;
      printf1 (" %d\n", i) ;
   END ;
   l := m2flex.GetLineNo() ;
   c := m2flex.GetColumnNo() ;
   s := Sprintf1(Mark(InitString('%d')), i) ;
   AddTokToList(t, makekey(string(s)), i, l, c, CurrentSource, m2flex.GetLocation()) ;
   s := KillString(s) ;
   CurrentUsed := TRUE
END AddTokInteger ;


BEGIN
   Init
END M2LexBuf.
