(* M2LexBuf.mod provides a buffer for m2.lex.

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

IMPLEMENTATION MODULE M2LexBuf ;

IMPORT m2flex ;

FROM libc IMPORT strlen ;
FROM SYSTEM IMPORT ADDRESS ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM DynamicStrings IMPORT string, InitString, InitStringCharStar, Equal, Mark, KillString ;
FROM FormatStrings IMPORT Sprintf1 ;
FROM NameKey IMPORT NulName, Name, makekey, MakeKey, KeyToCharStar ;
FROM M2Reserved IMPORT toktype, tokToTok ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3 ;
FROM M2Debug IMPORT Assert ;
FROM NameKey IMPORT makekey ;
FROM m2linemap IMPORT location_t, GetLocationBinary ;
FROM M2Emit IMPORT UnknownLocation, BuiltinsLocation ;
FROM M2Error IMPORT WarnStringAt ;

CONST
   MaxBucketSize      = 100 ;
   Debugging          = FALSE ;
   InitialSourceToken = 2 ;   (* 0 is unknown, 1 is builtin.  *)

TYPE
   SourceList = POINTER TO sourcelist ;
   sourcelist =            RECORD
                              left,
                              right: SourceList ;
                              name : String ;
                              line : CARDINAL ;
                              col  : CARDINAL ;
                           END ;

   TokenDesc = RECORD
                  token: toktype ;
                  str  : Name ;
                  int  : INTEGER ;
                  line : CARDINAL ;
                  col  : CARDINAL ;
                  file : SourceList ;
                  loc  : location_t ;
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

VAR
   CurrentSource    : SourceList ;
   UseBufferedTokens,
   CurrentUsed      : BOOLEAN ;
   ListOfTokens     : ListDesc ;
   CurrentTokNo     : CARDINAL ;


(*
   InitTokenList - creates an empty token list, which starts the first source token
                   at position 2.  This allows position 0 to be for unknown location
                   and position 1 for builtin token.
*)

PROCEDURE InitTokenList ;
BEGIN
   NEW(ListOfTokens.head) ;
   ListOfTokens.tail := ListOfTokens.head ;
   ListOfTokens.tail^.len := InitialSourceToken
END InitTokenList ;


(*
   Init - initializes the token list and source list.
*)

PROCEDURE Init ;
BEGIN
   currenttoken := eoftok ;
   CurrentTokNo := InitialSourceToken ;
   CurrentSource := NIL ;
   ListOfTokens.head := NIL ;
   ListOfTokens.tail := NIL ;
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
      RETURN TRUE
   ELSE
      IF m2flex.OpenSource (string (s))
      THEN
         SetFile (string (s)) ;
         SyncOpenWithBuffer ;
         GetToken ;
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
   CurrentTokNo := InitialSourceToken ;
   UseBufferedTokens := TRUE
END ResetForNewPass ;


(*
   DisplayToken -
*)

PROCEDURE DisplayToken (tok: toktype) ;
BEGIN
   CASE tok OF

   eoftok: printf0('eoftok\n') |
   plustok: printf0('plustok\n') |
   minustok: printf0('minustok\n') |
   timestok: printf0('timestok\n') |
   dividetok: printf0('dividetok\n') |
   becomestok: printf0('becomestok\n') |
   ambersandtok: printf0('ambersandtok\n') |
   periodtok: printf0('periodtok\n') |
   commatok: printf0('commatok\n') |
   semicolontok: printf0('semicolontok\n') |
   lparatok: printf0('lparatok\n') |
   rparatok: printf0('rparatok\n') |
   lsbratok: printf0('lsbratok\n') |
   rsbratok: printf0('rsbratok\n') |
   lcbratok: printf0('lcbratok\n') |
   rcbratok: printf0('rcbratok\n') |
   uparrowtok: printf0('uparrowtok\n') |
   singlequotetok: printf0('singlequotetok\n') |
   equaltok: printf0('equaltok\n') |
   hashtok: printf0('hashtok\n') |
   lesstok: printf0('lesstok\n') |
   greatertok: printf0('greatertok\n') |
   lessgreatertok: printf0('lessgreatertok\n') |
   lessequaltok: printf0('lessequaltok\n') |
   greaterequaltok: printf0('greaterequaltok\n') |
   periodperiodtok: printf0('periodperiodtok\n') |
   colontok: printf0('colontok\n') |
   doublequotestok: printf0('doublequotestok\n') |
   bartok: printf0('bartok\n') |
   andtok: printf0('andtok\n') |
   arraytok: printf0('arraytok\n') |
   begintok: printf0('begintok\n') |
   bytok: printf0('bytok\n') |
   casetok: printf0('casetok\n') |
   consttok: printf0('consttok\n') |
   definitiontok: printf0('definitiontok\n') |
   divtok: printf0('divtok\n') |
   dotok: printf0('dotok\n') |
   elsetok: printf0('elsetok\n') |
   elsiftok: printf0('elsiftok\n') |
   endtok: printf0('endtok\n') |
   exittok: printf0('exittok\n') |
   exporttok: printf0('exporttok\n') |
   fortok: printf0('fortok\n') |
   fromtok: printf0('fromtok\n') |
   iftok: printf0('iftok\n') |
   implementationtok: printf0('implementationtok\n') |
   importtok: printf0('importtok\n') |
   intok: printf0('intok\n') |
   looptok: printf0('looptok\n') |
   modtok: printf0('modtok\n') |
   moduletok: printf0('moduletok\n') |
   nottok: printf0('nottok\n') |
   oftok: printf0('oftok\n') |
   ortok: printf0('ortok\n') |
   pointertok: printf0('pointertok\n') |
   proceduretok: printf0('proceduretok\n') |
   qualifiedtok: printf0('qualifiedtok\n') |
   unqualifiedtok: printf0('unqualifiedtok\n') |
   recordtok: printf0('recordtok\n') |
   repeattok: printf0('repeattok\n') |
   returntok: printf0('returntok\n') |
   settok: printf0('settok\n') |
   thentok: printf0('thentok\n') |
   totok: printf0('totok\n') |
   typetok: printf0('typetok\n') |
   untiltok: printf0('untiltok\n') |
   vartok: printf0('vartok\n') |
   whiletok: printf0('whiletok\n') |
   withtok: printf0('withtok\n') |
   asmtok: printf0('asmtok\n') |
   volatiletok: printf0('volatiletok\n') |
   periodperiodperiodtok: printf0('periodperiodperiodtok\n') |
   datetok: printf0('datetok\n') |
   linetok: printf0('linetok\n') |
   filetok: printf0('filetok\n') |
   integertok: printf0('integertok\n') |
   identtok: printf0('identtok\n') |
   realtok: printf0('realtok\n') |
   stringtok: printf0('stringtok\n')

   ELSE
   END
END DisplayToken ;


(*
   UpdateFromBucket - updates the global variables:  currenttoken,
                      currentstring, currentcolumn and currentinteger
                      from TokenBucket, b, and, offset.
*)

PROCEDURE UpdateFromBucket (b: TokenBucket; offset: CARDINAL) ;
BEGIN
   WITH b^.buf[offset] DO
      currenttoken   := token ;
      currentstring  := KeyToCharStar(str) ;
      currentcolumn  := col ;
      currentinteger := int ;
      IF Debugging
      THEN
         printf3('line %d (# %d  %d) ', line, offset, CurrentTokNo) ;
      END
   END
END UpdateFromBucket ;


(*
   GetToken - gets the next token into currenttoken.
*)

PROCEDURE GetToken ;
VAR
   t: CARDINAL ;
   b: TokenBucket ;
BEGIN
   IF UseBufferedTokens
   THEN
      t := CurrentTokNo ;
      b := FindTokenBucket(t) ;
      UpdateFromBucket(b, t)
   ELSE
      IF ListOfTokens.tail=NIL
      THEN
         m2flex.GetToken () ;
         IF ListOfTokens.tail=NIL
         THEN
            HALT
         END
      END ;
      IF CurrentTokNo>=ListOfTokens.LastBucketOffset
      THEN
         (* CurrentTokNo is in the last bucket or needs to be read *)
         IF CurrentTokNo-ListOfTokens.LastBucketOffset<ListOfTokens.tail^.len
         THEN
            UpdateFromBucket(ListOfTokens.tail,
                             CurrentTokNo-ListOfTokens.LastBucketOffset)
         ELSE
            (* call the lexical phase to place a new token into the last bucket *)
            m2flex.GetToken () ;
            GetToken ;  (* and call ourselves again to collect the token from bucket *)
            RETURN
         END
      ELSE
         t := CurrentTokNo ;
         b := FindTokenBucket (t) ;
         UpdateFromBucket (b, t)
      END
   END ;
   IF Debugging
   THEN
      DisplayToken (currenttoken)
   END ;
   INC (CurrentTokNo)
END GetToken ;


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
      AddTokToList(currenttoken, NulName, 0,
                   GetLineNo(), GetColumnNo(), CurrentSource, m2flex.GetLocation()) ;
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
      AddTokToList(currenttoken, NulName, 0,
                   GetLineNo(), GetColumnNo(), CurrentSource, m2flex.GetLocation()) ;
      currenttoken := token
   END
END InsertTokenAndRewind ;


(*
   GetPreviousTokenLineNo - returns the line number of the previous token.
*)

PROCEDURE GetPreviousTokenLineNo () : CARDINAL ;
BEGIN
   (*
   IF GetTokenNo()>0
   THEN
      RETURN( TokenToLineNo(GetTokenNo()-1, 0) )
   ELSE
      RETURN( 0 )
   END
      *)
   RETURN( GetLineNo() )
END GetPreviousTokenLineNo ;


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
   GetColumnNo - returns the current column where the symbol occurs in
                 the source file.
*)

PROCEDURE GetColumnNo () : CARDINAL ;
BEGIN
   IF CurrentTokNo=0
   THEN
      RETURN( 0 )
   ELSE
      RETURN( TokenToColumnNo(GetTokenNo(), 0) )
   END
END GetColumnNo ;


(*
   GetTokenNo - returns the current token number.
*)

PROCEDURE GetTokenNo () : CARDINAL ;
BEGIN
   IF CurrentTokNo=0
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
   b: TokenBucket ;
   n: Name ;
BEGIN
   b := FindTokenBucket(tokenno) ;
   IF b=NIL
   THEN
      RETURN NulName
   ELSE
      WITH b^.buf[tokenno] DO
         n := tokToTok (token) ;
	 IF n=NulName
         THEN
            RETURN str
         ELSE
            RETURN n
         END
      END
   END
END GetTokenName ;


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
            RETURN b
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
   IF (TokenNo = UnknownTokenNo) OR (TokenNo = BuiltinTokenNo)
   THEN
      RETURN 0
   ELSE
      b := FindTokenBucket(TokenNo) ;
      IF b=NIL
      THEN
         RETURN( 0 )
      ELSE
         IF depth=0
         THEN
            RETURN( b^.buf[TokenNo].line)
         ELSE
            l := b^.buf[TokenNo].file^.left ;
            WHILE depth>0 DO
               l := l^.left ;
               IF l=b^.buf[TokenNo].file^.left
               THEN
                  RETURN 0
               END ;
               DEC(depth)
            END ;
            RETURN l^.line
         END
      END
   END
END TokenToLineNo ;


(*
   TokenToColumnNo - returns the column number of the current file for the
                     TokenNo. The depth refers to the include depth.
                     A depth of 0 is the current file, depth of 1 is the file
                     which included the current file. Zero is returned if the
                     depth exceeds the file nesting level.
*)

PROCEDURE TokenToColumnNo (TokenNo: CARDINAL; depth: CARDINAL) : CARDINAL ;
VAR
   b: TokenBucket ;
   l: SourceList ;
BEGIN
   IF (TokenNo = UnknownTokenNo) OR (TokenNo = BuiltinTokenNo)
   THEN
      RETURN 0
   ELSE
      b := FindTokenBucket(TokenNo) ;
      IF b=NIL
      THEN
         RETURN 0
      ELSE
         IF depth=0
         THEN
            RETURN( b^.buf[TokenNo].col )
         ELSE
            l := b^.buf[TokenNo].file^.left ;
            WHILE depth>0 DO
               l := l^.left ;
               IF l=b^.buf[TokenNo].file^.left
               THEN
                  RETURN 0
               END ;
               DEC(depth)
            END ;
            RETURN l^.col
         END
      END
   END
END TokenToColumnNo ;


(*
   TokenToLocation - returns the location_t corresponding to, TokenNo.
*)

PROCEDURE TokenToLocation (TokenNo: CARDINAL) : location_t ;
VAR
   b: TokenBucket ;
BEGIN
   IF TokenNo = UnknownTokenNo
   THEN
      RETURN UnknownLocation ()
   ELSIF TokenNo = BuiltinTokenNo
   THEN
      RETURN BuiltinsLocation ()
   ELSE
      b := FindTokenBucket(TokenNo) ;
      IF b=NIL
      THEN
         RETURN UnknownLocation ()
      ELSE
         RETURN( b^.buf[TokenNo].loc )
      END
   END
END TokenToLocation ;


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
      RETURN NIL
   ELSE
      IF TokenNo = UnknownTokenNo
      THEN
         RETURN NIL
      ELSIF TokenNo = BuiltinTokenNo
      THEN
         RETURN NIL
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
   END
END FindFileNameFromToken ;


(*
   GetFileName - returns a String defining the current file.
*)

PROCEDURE GetFileName () : String ;
BEGIN
   RETURN FindFileNameFromToken (GetTokenNo (), 0)
END GetFileName ;


PROCEDURE stop ; BEGIN END stop ;


(*
   AddTokToList - adds a token to a dynamic list.
*)

PROCEDURE AddTokToList (t: toktype; n: Name;
                        i: INTEGER; l: CARDINAL; c: CARDINAL;
                        f: SourceList; location: location_t) ;
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
         col   := c ;
         file  := f ;
         loc   := location
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
            RETURN FALSE
         END ;
         WHILE b^.next#ListOfTokens.tail DO
            b := b^.next
         END ;
      ELSE
         b := ListOfTokens.tail
      END ;
      WITH b^ DO
         Assert (len>0) ;     (* len should always be >0 *)
         RETURN buf[len-1].token=eoftok
      END
   END ;
   RETURN FALSE
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
   isSrcToken -
*)

PROCEDURE isSrcToken (tokenno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (tokenno # UnknownTokenNo) AND (tokenno # BuiltinTokenNo)
END isSrcToken ;


(*
   MakeVirtualTok - providing caret, left, right are associated with a source file
                    and exist on the same src line then
                    create and return a new tokenno which is created from
                    tokenno range1 and range2.  Otherwise return caret.
*)

PROCEDURE MakeVirtualTok (caret, left, right: CARDINAL) : CARDINAL ;
VAR
   bufLeft, bufRight: TokenBucket ;
   lc, ll, lr       : location_t ;
BEGIN
   IF FALSE
   THEN
      RETURN caret
   END ;
   IF isSrcToken (caret) AND isSrcToken (left) AND isSrcToken (right)
   THEN
      lc := TokenToLocation (caret) ;
      ll := TokenToLocation (left) ;
      lr := TokenToLocation (right) ;
      bufLeft := FindTokenBucket (left) ;   (* left maybe changed now.  *)
      bufRight := FindTokenBucket (right) ;  (* right maybe changed now.  *)

      IF (bufLeft^.buf[left].line = bufRight^.buf[right].line) AND
         (bufLeft^.buf[left].file = bufRight^.buf[right].file)
      THEN
         (* on the same line, create a new token and location.  *)
         AddTokToList (virtualrangetok, NulName, 0,
                       bufLeft^.buf[left].line, bufLeft^.buf[left].col, bufLeft^.buf[left].file,
                       GetLocationBinary (lc, ll, lr)) ;
         IF ListOfTokens.LastBucketOffset + ListOfTokens.tail^.len - 1 = 2862
         THEN
            stop
         END ;
         RETURN ListOfTokens.LastBucketOffset + ListOfTokens.tail^.len - 1
      END
   END ;
   RETURN caret
END MakeVirtualTok ;


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
   IF NOT ((t=eoftok) AND IsLastTokenEof())
   THEN
      AddTokToList(t, NulName, 0,
                   m2flex.GetLineNo(), m2flex.GetColumnNo(), CurrentSource,
                   m2flex.GetLocation()) ;
      CurrentUsed := TRUE ;
      IF Debugging
      THEN
         (* display each token as a warning.  *)
         s := InitStringCharStar (KeyToCharStar (GetTokenName (GetTokenNo ()))) ;
         WarnStringAt (s, GetTokenNo ())
      END
   END
END AddTok ;


(*
   AddTokCharStar - adds a token to the buffer and an additional string, s.
                    A copy of string, s, is made.
*)

PROCEDURE AddTokCharStar (t: toktype; s: ADDRESS) ;
BEGIN
   IF strlen(s)>80
   THEN
      stop
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
