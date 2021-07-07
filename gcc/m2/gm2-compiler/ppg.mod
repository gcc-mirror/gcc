(* ppg.mod master source file of the ebnf parser generator.

Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

MODULE ppg ;

FROM PushBackInput IMPORT WarnError, WarnString, GetColumnPosition, GetCurrentLine ;
FROM bnflex IMPORT IsSym, SymIs, TokenType, GetCurrentToken, GetCurrentTokenType, GetChar, PutChar,
                   SkipWhite, SkipUntilEoln, AdvanceToken, IsReserved, OpenSource, CloseSource,
                   PushBackToken, SetDebugging ;
FROM StrLib IMPORT StrCopy, StrEqual, StrLen, StrConCat ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM NameKey IMPORT Name, MakeKey, WriteKey, LengthKey, GetKey, KeyToCharStar, NulName ;
FROM NumberIO IMPORT CardToStr, WriteCard ;
FROM SymbolKey IMPORT InitTree, SymbolTree, PutSymKey, GetSymKey, ForeachNodeDo, ContainsSymKey, NulKey ;
FROM Lists IMPORT InitList, IsItemInList, IncludeItemIntoList, RemoveItemFromList, KillList, List ;
FROM DynamicStrings IMPORT String, InitString, KillString, ConCat, Mark, ConCatChar,
                           InitStringCharStar, char, Length ;
FROM ASCII IMPORT nul, lf, tab ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT Write ;
FROM Debug IMPORT Halt ;
FROM Args IMPORT GetArg, Narg ;
FROM SYSTEM IMPORT WORD ;
FROM libc IMPORT exit ;


CONST
   MaxCodeHunkLength = 8192 ;
   MaxFileName       = 8192 ;
   MaxString         = 8192 ;
   DefaultRecovery   = TRUE ;   (* default is to generate a parser which will recover from errors.  *)
   MaxElementsInSet  =   32 ;

   (* formatting constants *)
   BaseRightLimit    = 75 ;
   BaseRightMargin   = 60 ;
   BaseNewLine       =  3 ;

TYPE
   ElementType = (idel, tokel, litel) ;

   m2condition = (m2none, m2if, m2elsif, m2while) ;

   TraverseResult = (unknown, true, false) ;

   IdentDesc      = POINTER TO RECORD
                                  definition: ProductionDesc ;   (* where this idents production is defined *)
                                  name      : Name ;
                                  line      : CARDINAL ;
                               END ;

   SetDesc        = POINTER TO RECORD
                                  next          : SetDesc ;
                                  CASE type: ElementType OF

                                  idel  : ident : IdentDesc |
                                  tokel,
                                  litel : string: Name

                                  END
                               END ;

(* note that epsilon refers to whether we can satisfy this component part
   of a sentance without consuming a token. Reachend indicates we can get
   to the end of the sentance without consuming a token.

   For expression, statement, productions, terms: the epsilon value should
   equal the reachend value but for factors the two may differ.
*)

   FollowDesc     = POINTER TO RECORD
                                  calcfollow  : BOOLEAN ;          (* have we solved the follow set yet? *)
                                  follow      : SetDesc ;          (* the follow set *)
                                  reachend    : TraverseResult ;   (* can we see the end of the sentance (due to multiple epsilons) *)
                                  epsilon     : TraverseResult ;   (* potentially no token may be consumed within this component of the sentance *)
                                  line        : CARDINAL ;
                    END ;

   TermDesc       = POINTER TO termdesc ;

   ExpressionDesc = POINTER TO RECORD
                                  term      : TermDesc ;
                                  followinfo: FollowDesc ;
                                  line      : CARDINAL ;
                               END ;

   StatementDesc  = POINTER TO RECORD
                                  ident      : IdentDesc ;
                                  expr       : ExpressionDesc ;
                                  followinfo : FollowDesc ;
                                  line       : CARDINAL ;
                               END ;

   CodeHunk       = POINTER TO RECORD
                                  codetext  : ARRAY [0..MaxCodeHunkLength] OF CHAR ;
                                  next      : CodeHunk ;
                               END ;

   CodeDesc       = POINTER TO RECORD
                                  code      : CodeHunk ;
                                  indent    : CARDINAL ;         (* column of the first % *)
                                  line      : CARDINAL ;
                               END ;

   FactorType     = (id, lit, sub, opt, mult, m2) ;

   FactorDesc     = POINTER TO RECORD
                                  followinfo: FollowDesc ;
                                  next      : FactorDesc ;   (* chain of successive factors *)
                                  line      : CARDINAL ;
                                  pushed    : FactorDesc ;   (* chain of pushed code factors *)
                                  CASE type: FactorType OF

                                  id  : ident : IdentDesc |
                                  lit : string: Name |
                                  sub,
                                  opt,
                                  mult: expr  : ExpressionDesc |
                                  m2  : code  : CodeDesc ;

                                  END
                               END ;

   termdesc       =            RECORD
                                  factor    : FactorDesc ;
                                  next      : TermDesc ;  (* chain of alternative terms *)
                                  followinfo: FollowDesc ;
                                  line      : CARDINAL ;
                               END ;

   ProductionDesc = POINTER TO RECORD
                                  next        : ProductionDesc ;   (* the chain of productions *)
                                  statement   : StatementDesc ;
                                  first       : SetDesc ;          (* the first set *)
                                  firstsolved : BOOLEAN ;
                                  followinfo  : FollowDesc ;
                                  line        : CARDINAL ;
                                  description : Name ;
                               END ;

   DoProcedure    = PROCEDURE (ProductionDesc) ;


VAR
   LastLineNo         : CARDINAL ;
   Finished,
   SuppressFileLineTag,
   KeywordFormatting,
   PrettyPrint,
   EmitCode,
   Texinfo,
   FreeDocLicense,
   Debugging,
   WasNoError         : BOOLEAN ;
   LinePrologue,
   LineEpilogue,
   LineDeclaration    : CARDINAL ;
   CodePrologue,
   CodeEpilogue,
   CodeDeclaration    : CodeHunk ;
   CurrentProduction,
   TailProduction,
   HeadProduction     : ProductionDesc ;
   CurrentExpression  : ExpressionDesc ;
   CurrentTerm        : TermDesc ;
   CurrentFactor      : FactorDesc ;
   CurrentIdent       : IdentDesc ;
   CurrentStatement   : StatementDesc ;
   CurrentSetDesc     : SetDesc ;
   ReverseValues,
   Values,                           (* tree of tokens and their ORD value        *)
   ReverseAliases,
   Aliases            : SymbolTree ;
   ModuleName         : Name ;
   LastLiteral        : Name ;
   LastIdent          : Name ;
   SymIsProc,                        (* the name of the SymIs function tests and consumes token *)
   TokenTypeProc,                    (* the name of the function which yields the current token type *)
   ErrorProcArray,
   ErrorProcString    : Name ;       (* the name of the error procedures *)
   FileName           : ARRAY [0..MaxFileName] OF CHAR ;
   OnLineStart,
   BeginningOfLine    : BOOLEAN ;
   Indent             : CARDINAL ;
   EmittedVar         : BOOLEAN ;    (* have we written VAR yet?                   *)
   ErrorRecovery      : BOOLEAN ;    (* do we want to recover from parsing errors? *)
   LargestValue       : CARDINAL ;   (* the number of tokens we are using.         *)
   InitialElement     : BOOLEAN ;    (* used to determine whether we are writing   *)
                                     (* the first element of a case statement.     *)

(* % declaration *)

(*
   AddEntry - adds an entry into, t, containing [def:value].
*)

PROCEDURE AddEntry (VAR t: SymbolTree; def, value: Name) ;
BEGIN
   IF ContainsSymKey(t, def)
   THEN
      WarnError1("already seen a definition for token '%s'", def)
   ELSE
      PutSymKey(t, def, value)
   END
END AddEntry ;


(*
   Format1 - converts string, src, into, dest, together with encapsulated
             entity, n. It only formats the first %s or %d with n.
*)

PROCEDURE Format1 (src: ARRAY OF CHAR; n: WORD; VAR dest: ARRAY OF CHAR) ;
VAR
   HighSrc,
   HighDest,
   i, j    : CARDINAL ;
   str     : ARRAY [0..MaxString] OF CHAR ;
BEGIN
   HighSrc := StrLen(src) ;
   HighDest := HIGH(dest) ;
   i := 0 ;
   j := 0 ;
   WHILE (i<HighSrc) AND (src[i]#nul) AND (j<HighDest) AND (src[i]#'%') DO
      dest[j] := src[i] ;
      INC(i) ;
      INC(j)
   END ;
   IF (i+1<HighSrc) AND (src[i]='%') AND (j<HighDest)
   THEN
      IF src[i+1]='s'
      THEN
         dest[j] := nul ;
         GetKey(n, str) ;
         StrConCat(dest, str, dest) ;
         j := StrLen(dest) ;
         INC(i, 2)
      ELSIF src[i+1]='d'
      THEN
         dest[j] := nul ;
         CardToStr(n, 0, str) ;
         StrConCat(dest, str, dest) ;
         j := StrLen(dest) ;
         INC(i, 2)
      ELSE
         dest[j] := src[i] ;
         INC(i) ;
         INC(j)
      END
   END ;
   (* and finish off copying src into dest *)
   WHILE (i<HighSrc) AND (src[i]#nul) AND (j<HighDest) DO
      dest[j] := src[i] ;
      INC(i) ;
      INC(j)
   END ;
   IF j<HighDest
   THEN
      dest[j] := nul
   END ;
END Format1 ;


(*
   WarnError1 -
*)

PROCEDURE WarnError1 (a: ARRAY OF CHAR; n: WORD) ;
VAR
   line: ARRAY [0..MaxString] OF CHAR ;
BEGIN
   Format1(a, n, line) ;
   WarnError(line)
END WarnError1 ;


(*
   PrettyFollow -
*)

PROCEDURE PrettyFollow (start, end: ARRAY OF CHAR; f: FollowDesc) ;
BEGIN
   IF Debugging
   THEN
      WriteString(start) ;
      IF f#NIL
      THEN
         WITH f^ DO
            IF calcfollow
            THEN
               WriteString('followset defined as:') ;
               EmitSet(follow, 0, 0)
            END ;
            CASE reachend OF

            true :   WriteString(' [E]') |
            false:   WriteString(' [C]') |
            unknown: WriteString(' [U]')

            ELSE
            END ;
            CASE epsilon OF

            true   : WriteString(' [e]') |
            false  : |
            unknown: WriteString(' [u]')

            ELSE
            END
         END
      END ;
      WriteString(end)
   END
END PrettyFollow ;


(*
   NewFollow - creates a new follow descriptor and returns the data structure.
*)

PROCEDURE NewFollow () : FollowDesc ;
VAR
   f: FollowDesc ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      follow       := NIL ;
      reachend     := unknown ;
      epsilon      := unknown ;
   END ;
   RETURN( f )
END NewFollow ;


(*
   AssignEpsilon - assigns the epsilon value and sets the epsilon to value,
                   providing condition is TRUE.
*)

PROCEDURE AssignEpsilon (condition: BOOLEAN; f: FollowDesc; value: TraverseResult) ;
BEGIN
   WITH f^ DO
      IF condition AND (value#unknown) AND (epsilon=unknown)
      THEN
         epsilon := value ;
         Finished := FALSE
      END
   END
END AssignEpsilon ;


(*
   GetEpsilon - returns the value of epsilon
*)

PROCEDURE GetEpsilon (f: FollowDesc) : TraverseResult ;
BEGIN
   IF f=NIL
   THEN
      Halt('why is the follow info NIL?', __LINE__, __FILE__)
   ELSE
      RETURN( f^.epsilon )
   END
END GetEpsilon ;


(*
   AssignReachEnd - assigns the reachend value providing that, condition, is TRUE.
*)

PROCEDURE AssignReachEnd (condition: BOOLEAN; f: FollowDesc; value: TraverseResult) ;
BEGIN
   IF condition
   THEN
      WITH f^ DO
         IF (reachend=unknown) AND (value#unknown)
         THEN
            reachend := value ;
            Finished := FALSE
         END
      END
   END
END AssignReachEnd ;


(*
   GetReachEnd - returns the value of reachend
*)

PROCEDURE GetReachEnd (f: FollowDesc) : TraverseResult ;
BEGIN
   IF f=NIL
   THEN
      Halt('why is the follow info NIL?', __LINE__, __FILE__)
   ELSE
      RETURN( f^.reachend )
   END
END GetReachEnd ;


(*
   AssignFollow - assigns the follow set and sets the calcfollow to TRUE.
*)

PROCEDURE AssignFollow (f: FollowDesc; s: SetDesc) ;
BEGIN
   WITH f^ DO
      IF calcfollow
      THEN
         Halt('why are we reassigning this follow set?', __LINE__, __FILE__)
      END ;
      follow     := s ;
      calcfollow := TRUE
   END
END AssignFollow ;


(*
   GetFollow - returns the follow set.
*)

PROCEDURE GetFollow (f: FollowDesc) : SetDesc ;
BEGIN
   IF f=NIL
   THEN
      Halt('why is the follow info NIL?', __LINE__, __FILE__)
   ELSE
      WITH f^ DO
         IF calcfollow
         THEN
            RETURN( follow )
         ELSE
            Halt('not calculated the follow set yet..', __LINE__, __FILE__)
         END
      END
   END
END GetFollow ;


(*
   NewProduction - creates a new production and returns the data structure.
*)

PROCEDURE NewProduction () : ProductionDesc ;
VAR
   p: ProductionDesc ;
BEGIN
   NEW(p) ;
   IF TailProduction#NIL
   THEN
      TailProduction^.next := p
   END ;
   TailProduction := p ;
   IF HeadProduction=NIL
   THEN
      HeadProduction := p
   END ;
   WITH p^ DO
      next         := NIL ;
      statement    := NIL ;
      first        := NIL ;
      firstsolved  := FALSE ;
      followinfo   := NewFollow() ;
      line         := GetCurrentLine() ;
      description  := NulName
   END ;
   RETURN( p )
END NewProduction ;


(*
   NewFactor -
*)

PROCEDURE NewFactor () : FactorDesc ;
VAR
   f: FactorDesc ;
BEGIN
   NEW(f) ;
   WITH f^ DO
      next       := NIL ;
      followinfo := NewFollow() ;
      line       := GetCurrentLine()
   END ;
   RETURN( f )
END NewFactor ;


(*
   NewTerm - returns a new term.
*)

PROCEDURE NewTerm () : TermDesc ;
VAR
   t: TermDesc ;
BEGIN
   NEW(t) ;
   WITH t^ DO
      factor     := NIL ;
      followinfo := NewFollow() ;
      next       := NIL ;
      line       := GetCurrentLine()
   END ;
   RETURN( t )
END NewTerm ;


(*
   NewExpression - returns a new expression.
*)

PROCEDURE NewExpression () : ExpressionDesc ;
VAR
   e: ExpressionDesc ;
BEGIN
   NEW(e) ;
   WITH e^ DO
      term       := NIL ;
      followinfo := NewFollow() ;
      line       := GetCurrentLine()
   END ;
   RETURN( e )
END NewExpression ;


(*
   NewStatement - returns a new statement.
*)

PROCEDURE NewStatement () : StatementDesc ;
VAR
   s: StatementDesc ;
BEGIN
   NEW(s) ;
   WITH s^ DO
      ident      := NIL ;
      expr       := NIL ;
      followinfo := NewFollow() ;
      line       := GetCurrentLine()
   END ;
   RETURN( s )
END NewStatement ;


(*
   NewSetDesc - creates a new set description and returns the data structure.
*)

PROCEDURE NewSetDesc () : SetDesc ;
VAR
   s: SetDesc ;
BEGIN
   NEW(s) ;
   WITH s^ DO
      next := NIL
   END ;
   RETURN( s )
END NewSetDesc ;


(*
   NewCodeDesc - creates a new code descriptor and initializes all fields to zero.
*)

PROCEDURE NewCodeDesc () : CodeDesc ;
VAR
   c: CodeDesc ;
BEGIN
   NEW(c) ;
   WITH c^ DO
      code   := NIL ;
      indent := 0 ;
      line   := GetCurrentLine()
   END ;
   RETURN( c )
END NewCodeDesc ;


(*
   CodeFragmentPrologue - consumes code text up to a "%" after a newline.
*)

PROCEDURE CodeFragmentPrologue ;
BEGIN
   LinePrologue := GetCurrentLine() ;
   GetCodeFragment(CodePrologue)
END CodeFragmentPrologue ;


(*
   CodeFragmentEpilogue - consumes code text up to a "%" after a newline.
*)

PROCEDURE CodeFragmentEpilogue ;
BEGIN
   LineEpilogue := GetCurrentLine() ;
   GetCodeFragment(CodeEpilogue)
END CodeFragmentEpilogue ;


(*
   CodeFragmentDeclaration - consumes code text up to a "%" after a newline.
*)

PROCEDURE CodeFragmentDeclaration ;
BEGIN
   LineDeclaration := GetCurrentLine() ;
   GetCodeFragment(CodeDeclaration)
END CodeFragmentDeclaration ;


(*
   GetCodeFragment - collects the code fragment up until ^ %
*)

PROCEDURE GetCodeFragment (VAR h: CodeHunk) ;
VAR
   i : CARDINAL ;
   ch: CHAR ;
BEGIN
   h := NIL ;
   i := 0 ;
   WHILE (PutChar(GetChar())#'%') AND (PutChar(GetChar())#nul) DO
      REPEAT
         WHILE (PutChar(GetChar())#nul) AND (PutChar(GetChar())#lf) DO
            h := Add(h, GetChar(), i)
         END ;
         IF PutChar(GetChar())=lf
         THEN
            (* consume line feed *)
            h := Add(h, GetChar(), i) ;
            ch := PutChar(lf)
         ELSIF PutChar(GetChar())=nul
         THEN
            ch := PutChar(nul) ;
            ch := PutChar(lf)
         ELSE
            ch := PutChar(PutChar(GetChar()))
         END
      UNTIL GetChar()=lf
   END ;
   IF PutChar(GetChar())='%'
   THEN
      h := Add(h, nul, i) ;
      ch := PutChar(' ') ;  (* to give the following token % a delimiter infront of it *)
      AdvanceToken
   ELSE
      WarnError('expecting % to terminate code fragment, found end of file')
   END
END GetCodeFragment ;


(*
   WriteCodeHunkList - writes the CodeHunk list in the correct order.
*)

PROCEDURE WriteCodeHunkList (l: CodeHunk) ;
BEGIN
   IF l#NIL
   THEN
      OnLineStart := FALSE ;
      (* recursion *)
      WITH l^ DO
         WriteCodeHunkList(next) ;
         WriteString(codetext)
      END
   END
END WriteCodeHunkList ;


(*
   WriteIndent - writes, n, spaces.
*)

PROCEDURE WriteIndent (n: CARDINAL) ;
BEGIN
   WHILE n>0 DO
      Write(' ') ;
      DEC(n)
   END ;
   OnLineStart := FALSE
END WriteIndent ;


(*
   CheckWrite -
*)

PROCEDURE CheckWrite (ch: CHAR; VAR curpos: CARDINAL; left: CARDINAL; VAR seentext: BOOLEAN) ;
BEGIN
   IF ch=lf
   THEN
      NewLine(left) ;
      curpos := 0 ;
      seentext := FALSE
   ELSE
      Write(ch) ;
      INC(curpos)
   END
END CheckWrite ;


(*
   WriteStringIndent - writes a string but it will try and remove upto indent spaces
                       if they exist.
*)

PROCEDURE WriteStringIndent (a: ARRAY OF CHAR; indent: CARDINAL;
                             VAR curpos: CARDINAL;
                             left: CARDINAL; VAR seentext: BOOLEAN) ;
VAR
   l, i: CARDINAL ;
BEGIN
   i := 0 ;
   l := StrLen(a) ;
   WHILE i<l DO
      IF seentext
      THEN
         CheckWrite(a[i], curpos, left, seentext)
      ELSE
         IF a[i]=' '
         THEN
            (* ignore space for now *)
            INC(curpos)
         ELSE
            IF curpos>=indent
            THEN
               WriteIndent(curpos-indent)
            END ;
            seentext := TRUE ;
            CheckWrite(a[i], curpos, left, seentext)
         END
      END ;
      INC(i)
   END
END WriteStringIndent ;


(*
   WriteCodeHunkListIndent - writes the CodeHunk list in the correct order
                             but it removes up to indent spaces if they exist.
*)

PROCEDURE WriteCodeHunkListIndent (l: CodeHunk; indent: CARDINAL;
                                   VAR curpos: CARDINAL;
                                   left: CARDINAL; VAR seentext: BOOLEAN) ;
BEGIN
   IF l#NIL
   THEN
      (* recursion *)
      WITH l^ DO
         WriteCodeHunkListIndent(next, indent, curpos, left, seentext) ;
         WriteStringIndent(codetext, indent, curpos, left, seentext)
      END
   END
END WriteCodeHunkListIndent ;


(*
   Add - adds a character to a code hunk and creates another code hunk if necessary.
*)

PROCEDURE Add (VAR p: CodeHunk; ch: CHAR; VAR i: CARDINAL) : CodeHunk ;
VAR
   q: CodeHunk ;
BEGIN
   IF (p=NIL) OR (i>MaxCodeHunkLength)
   THEN
      NEW(q) ;
      q^.next := p ;
      q^.codetext[0] := ch ;
      i := 1 ;
      RETURN( q )
   ELSE
      p^.codetext[i] := ch ;
      INC(i) ;
      RETURN( p )
   END
END Add ;


(*
   ConsHunk - combine two possible code hunks.
*)

PROCEDURE ConsHunk (VAR p: CodeHunk; q: CodeHunk) ;
VAR
   r: CodeHunk ;
BEGIN
   IF p#NIL
   THEN
      r := q ;
      WHILE r^.next#NIL DO
         r := r^.next
      END ;
      r^.next := p ;
   END ;
   p := q
END ConsHunk ;


(*
   GetName - returns the next symbol which is checked for a legal name.
*)

PROCEDURE GetName () : Name ;
VAR
   name: Name ;
BEGIN
   IF IsReserved(GetCurrentToken())
   THEN
      WarnError('expecting a name and found a reserved word') ;
      AdvanceToken ;  (* move on to another token *)
      RETURN( NulName )
   ELSE
      name := GetCurrentToken() ;
      AdvanceToken ;
      RETURN( name )
   END
END GetName ;


(* % rules *)

(*
    Note that all the code from here down to the end of the module as
    delimited by the comment will all be hidden when the buildpg
    script is invoked. Also be careful not to duplicate or remove these
    critical comments below..
    Check buildpg for sed details.
*)

(* StartNonErrorChecking *)

(* actually these two are not strictly rules but hand built primitives *)


(*
   Ident - non error checking varient of Ident
*)

PROCEDURE Ident () : BOOLEAN ;
BEGIN
   IF GetCurrentTokenType()=identtok
   THEN
      NEW(CurrentIdent) ;
      WITH CurrentIdent^ DO
         definition := NIL ;
         name       := GetName() ;
         line       := GetCurrentLine()
      END ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END Ident ;


(*
   Modula2Code - non error checking varient of Modula2Code
*)

PROCEDURE Modula2Code () : BOOLEAN ;
VAR
   p       : CodeHunk ;
   i       : CARDINAL ;
   quote   : BOOLEAN ;
   line,
   position: CARDINAL ;
BEGIN
   line := GetCurrentLine() ;
   PushBackToken(GetCurrentToken()) ;
   position := GetColumnPosition() ;
   p := NIL ;
   SkipWhite ;
   WHILE (PutChar(GetChar())#'%') AND (PutChar(GetChar())#nul) DO
      IF PutChar(GetChar())='"'
      THEN
         REPEAT
            p := Add(p, GetChar(), i)
         UNTIL (PutChar(GetChar())='"') OR (PutChar(GetChar())=nul) ;
         p := Add(p, '"', i) ;
         IF (PutChar(GetChar())='"') AND (GetChar()='"')
         THEN
         END
      ELSIF PutChar(GetChar())="'"
      THEN
         REPEAT
            p := Add(p, GetChar(), i)
         UNTIL (PutChar(GetChar())="'") OR (PutChar(GetChar())=nul) ;
         p := Add(p, "'", i) ;
         IF (PutChar(GetChar())="'") AND (GetChar()="'")
         THEN
         END
      ELSIF (PutChar(GetChar())='\') AND (GetChar()='\')
      THEN
         p := Add(p, GetChar(), i)
      ELSIF PutChar(GetChar())#'%'
      THEN
         p := Add(p, GetChar(), i)
      END
   END ;
   p := Add(p, nul, i) ;
   WITH CurrentFactor^ DO
      type := m2 ;
      code := NewCodeDesc() ;
      WITH code^ DO
         code   := p ;
         indent := position
      END
   END ;
   IF PutChar(' ')=' '
   THEN
   END ;
   AdvanceToken ;  (* read the next token ready for the parser *)
   IF NOT WasNoError
   THEN
      WarnError1('error probably occurred before the start of inline code on line %d', line)
   END ;
   RETURN( TRUE )
END Modula2Code ;


(*
   StartModName    := % ModuleName := GetName() ; (* ignore begintok *) CodeFragmentPrologue % =:
*)

PROCEDURE StartModName () : BOOLEAN ;
BEGIN
   ModuleName := GetName() ;
   CodeFragmentPrologue ;
   RETURN( TRUE )
END StartModName ;

(*
   EndModName    :=
*)

PROCEDURE EndModName () : BOOLEAN ;
BEGIN
   IF ModuleName#GetName()
   THEN
      WarnError('expecting same module name at end as beginning')
   END ;
   (* ignore endtok as it consumes the token afterwards *)
   CodeFragmentEpilogue ;
   RETURN( TRUE )
END EndModName ;

(*
   DoDeclaration := % CodeFragmentDeclaration % =:
*)

PROCEDURE DoDeclaration () : BOOLEAN ;
BEGIN
   IF ModuleName#GetName()
   THEN
      WarnError('expecting same module name in declaration as in the beginning')
   END ;
   (* ignore begintok as it consumes the token afterwards *)
   CodeFragmentDeclaration ;
   RETURN( TRUE )
END DoDeclaration ;

(* EndNonErrorChecking  now for the real ebnf rules *)

TYPE
   SetOfStop = SET OF TokenType ;

(* **************************************************************************
    E r r o r    R e c o v e r y    I d e n t    &    M o d u l a 2 C o d e
   **************************************************************************

(* StartErrorChecking *)


(*
   SyntaxError - after a syntax error we skip all tokens up until we reach
                 a stop symbol.
*)

PROCEDURE SyntaxError (stop: SetOfStop) ;
BEGIN
   DescribeError(stop) ;
   IF Debugging
   THEN
      WriteLn ;
      WriteString('skipping token *** ')
   END ;
   WHILE NOT (GetCurrentTokenType() IN stop) DO
      AdvanceToken
   END ;
   IF Debugging
   THEN
      WriteString(' ***') ; WriteLn
   END ;
   WasNoError := FALSE
END SyntaxError ;


(*
   SyntaxCheck -
*)

PROCEDURE SyntaxCheck (stop: SetOfStop) ;
BEGIN
   IF NOT (GetCurrentTokenType() IN stop)
   THEN
      SyntaxError(stop)
   END
END SyntaxCheck ;


(*
   Expect -
*)

PROCEDURE Expect (t: TokenType; stop: SetOfStop) ;
BEGIN
   IF GetCurrentTokenType()=t
   THEN
      AdvanceToken
   ELSE
      SyntaxError(stop)
   END ;
   SyntaxCheck(stop)
END Expect ;


(*
   Ident - error checking varient of Ident
*)

PROCEDURE Ident (stop: SetOfStop) ;
BEGIN
   IF GetCurrentTokenType()=identtok
   THEN
      NEW(CurrentIdent) ;
      WITH CurrentIdent^ DO
         definition := NIL ;
         name       := GetName() ;
         line       := GetCurrentLine()
      END ;
   END
END Ident ;


(*
   Modula2Code - error checking varient of Modula2Code
*)

PROCEDURE Modula2Code (stop: SetOfStop) ;
VAR
   p       : CodeHunk ;
   i       : CARDINAL ;
   quote   : BOOLEAN ;
   line,
   position: CARDINAL ;
BEGIN
   line := GetCurrentLine() ;
   PushBackToken(GetCurrentToken()) ;
   position := GetColumnPosition() ;
   p := NIL ;
   SkipWhite ;
   WHILE (PutChar(GetChar())#'%') AND (PutChar(GetChar())#nul) DO
      IF PutChar(GetChar())='"'
      THEN
         REPEAT
            p := Add(p, GetChar(), i)
         UNTIL (PutChar(GetChar())='"') OR (PutChar(GetChar())=nul) ;
         p := Add(p, '"', i) ;
         IF (PutChar(GetChar())='"') AND (GetChar()='"')
         THEN
         END
      ELSIF PutChar(GetChar())="'"
      THEN
         REPEAT
            p := Add(p, GetChar(), i)
         UNTIL (PutChar(GetChar())="'") OR (PutChar(GetChar())=nul) ;
         p := Add(p, "'", i) ;
         IF (PutChar(GetChar())="'") AND (GetChar()="'")
         THEN
         END
      ELSIF (PutChar(GetChar())='\') AND (GetChar()='\')
      THEN
         p := Add(p, GetChar(), i)
      ELSIF PutChar(GetChar())#'%'
      THEN
         p := Add(p, GetChar(), i)
      END
   END ;
   p := Add(p, nul, i) ;
   WITH CurrentFactor^ DO
      type := m2 ;
      code := NewCodeDesc() ;
      WITH code^ DO
         code   := p ;
         indent := position
      END
   END ;
   IF PutChar(' ')=' '
   THEN
   END ;
   AdvanceToken ;  (* read the next token ready for the parser *)
   IF NOT WasNoError
   THEN
      WarnError1('error probably occurred before the start of inline code on line %d', line)
   END
END Modula2Code ;


(*
   StartModName    := % ModuleName := GetName() ; (* ignore begintok *) CodeFragmentPrologue % =:
*)

PROCEDURE StartModName (stop: SetOfStop) ;
BEGIN
   ModuleName := GetName() ;
   CodeFragmentPrologue
END StartModName ;


(*
   EndModName    :=
*)

PROCEDURE EndModName (stop: SetOfStop) ;
BEGIN
   IF ModuleName#GetName()
   THEN
      WarnError('expecting same module name at end as beginning')
   END ;
   (* ignore endtok as it consumes the token afterwards *)
   CodeFragmentEpilogue
END EndModName ;


(*
   DoDeclaration := % CodeFragmentDeclaration % =:
*)

PROCEDURE DoDeclaration (stop: SetOfStop) ;
BEGIN
   IF ModuleName#GetName()
   THEN
      WarnError('expecting same module name in declaration as in the beginning')
   END ;
   (* ignore begintok as it consumes the token afterwards *)
   CodeFragmentDeclaration
END DoDeclaration ;


(* EndErrorChecking  now for the real ebnf rules *)

*****************************************************************
  l e a v e   a b o v e   c o d e   a l o n e   (f o r    S E D)
***************************************************************** *)

(* this code below will be recreated by ppg *)

PROCEDURE DescribeError (stop: SetOfStop) ;
BEGIN
   WarnError('syntax error')
END DescribeError ;

PROCEDURE Main () : BOOLEAN ;
BEGIN
   IF Header()
   THEN
      IF Decls()
      THEN
         IF Footer()
         THEN
            IF Rules()
            THEN
               RETURN( TRUE )
            END
         END
      END
   END ;
   RETURN( FALSE )
END Main ;

PROCEDURE Header () : BOOLEAN ;
BEGIN
   IF SymIs(codetok)
   THEN
      IF SymIs(moduletok)
      THEN
         ModuleName := GetName() ;
         (* ignore the begintok as we are looking one symbol ahead and we dont want to move over MODULE *)
         CodeFragmentPrologue ;
         RETURN( TRUE )
      ELSE
         WarnError('expecting module')
      END
   END ;
   RETURN( FALSE )
END Header ;

PROCEDURE Footer () : BOOLEAN ;
BEGIN
   IF SymIs(codetok)
   THEN
      IF SymIs(moduletok)
      THEN
         IF ModuleName#GetName()
         THEN
            WarnError('expecting same module name at end as beginning')
         END ;
         (* ignore endtok as it consumes the token afterwards *)
         CodeFragmentEpilogue ;
         RETURN( TRUE )
      ELSE
         WarnError('expecting module')
      END
   END ;
   RETURN( FALSE )
END Footer ;

PROCEDURE Decls () : BOOLEAN ;
BEGIN
   IF SymIs(codetok)
   THEN
      IF SymIs(declarationtok)
      THEN
         RETURN( DoDeclaration() )
      ELSE
         WarnError('expecting declaration')
      END
   END ;
   RETURN( FALSE )
END Decls ;


(*
    Rules      := " % " " rules " { Defs } ExtBNF =:
*)

PROCEDURE Rules () : BOOLEAN ;
BEGIN
   IF SymIs(codetok)
   THEN
      IF SymIs(rulestok)
      THEN
         WHILE Defs() DO
         END ;
         IF ExtBNF()
         THEN
            RETURN( TRUE )
         ELSE
            WarnError('expecting some BNF rules to be present')
         END
      END
   END ;
   RETURN( FALSE )
END Rules ;


(*
   Defs       := " special " Special | " token " Token | " error " ErrorProcedures |
                 "tokenfunc" TokenProcedure =:
*)

PROCEDURE Defs () : BOOLEAN ;
BEGIN
   IF SymIs(specialtok)
   THEN
      IF Special()
      THEN
         RETURN( TRUE )
      END
   ELSIF SymIs(tokentok)
   THEN
      IF Token()
      THEN
         RETURN( TRUE )
      END
   ELSIF SymIs(errortok)
   THEN
      IF ErrorProcedures()
      THEN
         RETURN( TRUE )
      END
   ELSIF SymIs(tfunctok)
   THEN
      IF TokenProcedure()
      THEN
         RETURN( TRUE )
      END
   ELSIF SymIs(symfunctok)
   THEN
      IF SymProcedure()
      THEN
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END Defs ;


(*
   Special := Name First Follow [ "epsilon" ] =:
*)

PROCEDURE Special () : BOOLEAN ;
VAR
   p: ProductionDesc ;
BEGIN
   IF Ident()
   THEN
      p                           := NewProduction() ;
      p^.statement                := NewStatement() ;
      p^.statement^.followinfo^.calcfollow := TRUE ;
      p^.statement^.followinfo^.epsilon    := false ;
      p^.statement^.followinfo^.reachend   := false ;
      p^.statement^.ident         := CurrentIdent ;
      p^.statement^.expr          := NIL ;
      p^.firstsolved              := TRUE ;
      p^.followinfo^.calcfollow   := TRUE ;
      p^.followinfo^.epsilon      := false ;
      p^.followinfo^.reachend     := false ;
      IF First()
      THEN
         IF Follow()
         THEN
            IF SymIs(epsilontok)
            THEN
               p^.statement^.followinfo^.epsilon  := true ;  (* these are not used - but they are displayed when debugging *)
               p^.statement^.followinfo^.reachend := true ;
               p^.followinfo^.epsilon  := true ;
               p^.followinfo^.reachend := true
            END ;
            IF Literal()
            THEN
               p^.description := LastLiteral
            END ;
            RETURN( TRUE )
         ELSE
            WarnError('Follow - expected') ;
            RETURN( FALSE )
         END ;
      ELSE
         WarnError('First - expected') ;
         RETURN( FALSE )
      END
   ELSE
      RETURN( FALSE )
   END
END Special ;


(*
   First := 'first' '{' { LitOrTokenOrIdent % WITH LastSetDesc^ DO
                                  next := HeadProduction^.first ;
                               END ;
                               TailProduction^.first := LastSetDesc ;
                              %
                           } '}'
*)

PROCEDURE First () : BOOLEAN ;
BEGIN
   IF SymIs(firsttok)
   THEN
      IF SymIs(lcparatok)
      THEN
         WHILE LitOrTokenOrIdent() DO
            WITH CurrentSetDesc^ DO
               next := TailProduction^.first ;
            END ;
            TailProduction^.first := CurrentSetDesc
         END ;  (* while *)
         IF SymIs(rcparatok)
         THEN
            RETURN( TRUE )
         ELSE
            WarnError("'}' - expected") ;
            RETURN( FALSE )
         END ;
      ELSE
         WarnError("'{' - expected") ;
         RETURN( FALSE )
      END ;
   ELSE
      RETURN( FALSE )
   END ;
END First ;


(*
   Follow := 'follow' '{' { LitOrTokenOrIdent  } '}'
*)

PROCEDURE Follow () : BOOLEAN ;
BEGIN
   IF SymIs(followtok)
   THEN
      IF SymIs(lcparatok)
      THEN
         WHILE LitOrTokenOrIdent() DO
            WITH CurrentSetDesc^ DO
               next := TailProduction^.followinfo^.follow ;
            END ;
            TailProduction^.followinfo^.follow := CurrentSetDesc
         END ;  (* while *)
         IF SymIs(rcparatok)
         THEN
            RETURN( TRUE )
         ELSE
            WarnError("'}' - expected") ;
            RETURN( FALSE )
         END ;
      ELSE
         WarnError("'{' - expected") ;
         RETURN( FALSE )
      END ;
   ELSE
      RETURN( FALSE )
   END ;
END Follow ;


(*
   LitOrTokenOrIdent := Literal % CurrentSetDesc := NewSetDesc() ;
                               WITH CurrentSetDesc^ DO
                                  type   := litel ;
                                  string := LastLiteral ;
                               END ;
                              %
                         | '<' % CurrentSetDesc := NewSetDesc() ;
                           WITH CurrentSetDesc^ DO
                              type   := tokel ;
                              string := GetCurrentToken() ;
                           END ;
                           AdvanceToken() ;
                          %
                        '>'  | Ident % CurrentSetDesc := NewSetDesc() ;
                               WITH CurrentSetDesc^ DO
                                  type   := idel ;
                                  ident  := CurrentIdent ;
                               END ;
                              %

*)

PROCEDURE LitOrTokenOrIdent () : BOOLEAN ;
BEGIN
   IF Literal()
   THEN
      CurrentSetDesc := NewSetDesc() ;
      WITH CurrentSetDesc^ DO
         type   := litel ;
         string := LastLiteral
      END ;
      RETURN( TRUE )
   ELSIF SymIs(lesstok)
   THEN
      CurrentSetDesc := NewSetDesc() ;
      WITH CurrentSetDesc^ DO
         type   := tokel ;
         string := GetCurrentToken() ;
      END ;
      IF GetSymKey(Aliases, GetCurrentToken())=NulKey
      THEN
(*
         PutSymKey(Values, GetCurrentToken(), LargestValue) ;
         PutSymKey(Aliases, GetCurrentToken(), GetCurrentToken()) ;
         PutSymKey(ReverseAliases, GetCurrentToken(), GetCurrentToken()) ;
         INC(LargestValue) ;
*)
      END ;
      AdvanceToken() ;
      IF SymIs(gretok)
      THEN
         RETURN( TRUE )
      ELSE
         WarnError("'>' - expected") ;
         RETURN( FALSE )
      END ;
   ELSIF Ident()
   THEN
      CurrentSetDesc := NewSetDesc() ;
      WITH CurrentSetDesc^ DO
         type   := idel ;
         ident  := CurrentIdent ;
      END ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END ;  (* elsif *)
END LitOrTokenOrIdent ;


(*
   Literal -
*)

PROCEDURE Literal () : BOOLEAN ;
BEGIN
   IF SymIs(squotetok)
   THEN
      LastLiteral := GetCurrentToken() ;
      AdvanceToken ;
      IF SymIs(squotetok)
      THEN
         RETURN( TRUE )
      END
   ELSIF SymIs(dquotetok)
   THEN
      LastLiteral := GetCurrentToken() ;
      AdvanceToken ;
      IF SymIs(dquotetok)
      THEN
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END Literal ;


(*
   Token      := Literal % VAR l: CARDINAL ;
                        l := GetCurrentToken() ; %
              Name % PutSymKey(Aliases, l, GetCurrentToken()) ; % =:
*)

PROCEDURE Token () : BOOLEAN ;
BEGIN
   IF Literal()
   THEN
      AddEntry(Aliases, LastLiteral, GetCurrentToken()) ;
      AddEntry(ReverseAliases, GetCurrentToken(), LastLiteral) ;
      AddEntry(Values, GetCurrentToken(), LargestValue) ;
      AddEntry(ReverseValues, Name(LargestValue), GetCurrentToken()) ;
      INC(LargestValue) ;
      AdvanceToken ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END Token ;


(*
   ErrorProcedures  := Literal % ErrorProcArray := LastLiteral %
                       Literal % ErrorProcString := LastLiteral % =:
*)

PROCEDURE ErrorProcedures () : BOOLEAN ;
BEGIN
   IF Literal()
   THEN
      ErrorProcArray := LastLiteral ;
      IF Literal()
      THEN
         ErrorProcString := LastLiteral ;
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END ErrorProcedures ;


(*
   TokenProcedure := Literal % TokenTypeProc := LastLiteral % =:
*)

PROCEDURE TokenProcedure () : BOOLEAN ;
BEGIN
   IF Literal()
   THEN
      TokenTypeProc := LastLiteral ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END TokenProcedure ;


(*
   SymProcedure := Literal % SymIsProc := LastLiteral % =:
*)

PROCEDURE SymProcedure () : BOOLEAN ;
BEGIN
   IF Literal()
   THEN
      SymIsProc := LastLiteral ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END SymProcedure ;


(*
   ExtBNF     := " BNF " { Production } " FNB " =:
*)

PROCEDURE ExtBNF () : BOOLEAN ;
BEGIN
   IF SymIs(BNFtok)
   THEN
      WHILE Production() DO
      END ;
      IF SymIs(FNBtok)
      THEN
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END ExtBNF ;


(*
    Production := Statement =:
*)

PROCEDURE Production () : BOOLEAN ;
BEGIN
   IF Statement()
   THEN
      RETURN( TRUE )
   END ;
   RETURN( FALSE )
END Production ;


(*
   Statement  := % VAR i: IdentDesc ; %
                 Ident
                 % i := CurrentIdent ; %
                 " := "
                 % VAR e: ExpressionDesc ;
                   e := NewExpression() ; %
                 Expression
                 % WITH CurrentStatement^ DO
                      ident := i ;
                      expr  := e ;
                      first := NIL ;
                   END ;
                 %
                 " =: " =:
*)

PROCEDURE Statement () : BOOLEAN ;
VAR
   i: IdentDesc ;
   s: StatementDesc ;
   e: ExpressionDesc ;
   p: ProductionDesc ;
BEGIN
   IF Ident()
   THEN
      p := FindDefinition(CurrentIdent^.name) ;
      IF p=NIL
      THEN
         p := NewProduction()
      ELSE
         IF NOT ((p^.statement=NIL) OR (p^.statement^.expr=NIL))
         THEN
            WarnError1('already declared rule %s', CurrentIdent^.name)
         END
      END ;
      i := CurrentIdent ;
      IF SymIs(lbecomestok)
      THEN
         e := NewExpression() ;
         CurrentExpression := e ;
         s := NewStatement() ;
         WITH s^ DO
            ident := i ;
            expr  := e ;
         END ;
         IF Expression()
         THEN
            p^.statement := s ;
            IF SymIs(rbecomestok)
            THEN
               RETURN( TRUE )
            END
         END
      END
   END ;
   RETURN( FALSE )
END Statement ;


(*
    Expression := % CurrentTerm := NIL %
                  Term { " | " % CurrentTerm := NewTerm() % Term } =:
*)

PROCEDURE Expression () : BOOLEAN ;
VAR
   t1, t2: TermDesc ;
   e     : ExpressionDesc ;
BEGIN
   e := CurrentExpression ;
   t1 := NewTerm() ;
   CurrentTerm := t1 ;
   IF Term()
   THEN
      e^.term := t1 ;
      WHILE SymIs(bartok) DO
         t2 := NewTerm() ;
         CurrentTerm := t2 ;
         IF Term()
         THEN
            t1^.next := t2 ;
            t1 := t2
         ELSE
            WarnError('term expected')
         END
      END ;
      RETURN( TRUE )
   ELSE
      (* DISPOSE(t1) ; *)
      RETURN( FALSE )
   END
END Expression ;


(*
   Term       := Factor { Factor } =:
*)

PROCEDURE Term () : BOOLEAN ;
VAR
   t1: TermDesc ;
   f1, f2: FactorDesc ;
BEGIN
   CurrentFactor := NewFactor() ;
   f1 := CurrentFactor ;
   t1 := CurrentTerm ;
   IF Factor()
   THEN
      t1^.factor := f1 ;
      f2 := NewFactor() ;
      CurrentFactor := f2 ;
      WHILE Factor() DO
         f1^.next := f2 ;
         f1 := f2 ;
         f2 := NewFactor() ;
         CurrentFactor := f2 ;
      END ;
      (* DISPOSE(f2) ; *)
      RETURN( TRUE )
   ELSE
      (* DISPOSE(f1) ; *)
      RETURN( FALSE )
   END
END Term ;


(*
   Factor     := " % " Modula2Code " % " % AssignCode ; % |
                  ( Ident | Literal | " { " Expression " } " |
                    " [ " Expression " ] " | " ( " Expression " ) " )  =:
*)

PROCEDURE Factor () : BOOLEAN ;
BEGIN
   IF SymIs(codetok)
   THEN
      IF Modula2Code()
      THEN
         IF SymIs(codetok)
         THEN
            RETURN( TRUE )
         END
      END
   ELSE
      IF Ident()
      THEN
         WITH CurrentFactor^ DO
            type  := id ;
            ident := CurrentIdent
         END ;
         RETURN( TRUE )
      ELSIF Literal()
      THEN
         WITH CurrentFactor^ DO
            type   := lit ;
            string := LastLiteral ;
            IF GetSymKey(Aliases, LastLiteral)=NulKey
            THEN
               WarnError1('no token defined for literal %s', LastLiteral)
            END
         END ;
         RETURN( TRUE )
      ELSIF SymIs(lcparatok)
      THEN
         WITH CurrentFactor^ DO
            type := mult ;
            expr := NewExpression() ;
            CurrentExpression := expr ;
            IF Expression()
            THEN
               IF SymIs(rcparatok)
               THEN
                  RETURN( TRUE )
               ELSE
                  WarnError('} expected')
               END
            END
         END
      ELSIF SymIs(lsparatok)
      THEN
         WITH CurrentFactor^ DO
            type := opt ;
            expr := NewExpression() ;
            CurrentExpression := expr ;
            IF Expression()
            THEN
               IF SymIs(rsparatok)
               THEN
                  RETURN( TRUE )
               ELSE
                  WarnError('] expected')
               END
            END
         END
      ELSIF SymIs(lparatok)
      THEN
         WITH CurrentFactor^ DO
            type := sub ;
            expr := NewExpression() ;
            CurrentExpression := expr ;
            IF Expression()
            THEN
               IF SymIs(rparatok)
               THEN
                  RETURN( TRUE )
               ELSE
                  WarnError(') expected')
               END
            END
         END
      END
   END ;
   RETURN( FALSE )
END Factor ;

(* % module pg end *)


(*
   GetDefinitionName - returns the name of the rule inside, p.
*)

PROCEDURE GetDefinitionName (p: ProductionDesc) : Name ;
BEGIN
   IF p#NIL
   THEN
      WITH p^ DO
         IF (statement#NIL) AND (statement^.ident#NIL)
         THEN
            RETURN( statement^.ident^.name )
         END
      END
   END ;
   RETURN( NulName )
END GetDefinitionName ;


(*
   FindDefinition - searches and returns the rule which defines, n.
*)

PROCEDURE FindDefinition (n: Name) : ProductionDesc ;
VAR
   p, f: ProductionDesc ;
BEGIN
   p := HeadProduction ;
   f := NIL ;
   WHILE p#NIL DO
      IF GetDefinitionName(p)=n
      THEN
         IF f=NIL
         THEN
            f := p
         ELSE
            WriteString('multiple definition for rule: ') ; WriteKey(n) ; WriteLn
         END
      END ;
      p := p^.next
   END ;
   RETURN( f )
END FindDefinition ;


(*
   BackPatchIdent - found an ident, i, we must look for the corresponding rule and
                    set the definition accordingly.
*)

PROCEDURE BackPatchIdent (i: IdentDesc) ;
BEGIN
   IF i#NIL
   THEN
      WITH i^ DO
         definition := FindDefinition(name) ;
         IF definition=NIL
         THEN
            WarnError1('unable to find production %s', name) ;
            WasNoError := FALSE
         END
      END
   END
END BackPatchIdent ;


(*
   BackPatchFactor - runs through the factor looking for an ident
*)

PROCEDURE BackPatchFactor (f: FactorDesc) ;
BEGIN
   WHILE f#NIL DO
      WITH f^ DO
         CASE type OF

         id  :  BackPatchIdent(ident) |
         sub ,
         opt ,
         mult:  BackPatchExpression(expr)

         ELSE
         END
      END ;
      f := f^.next
   END
END BackPatchFactor ;


(*
   BackPatchTerm - runs through all terms to find idents.
*)

PROCEDURE BackPatchTerm (t: TermDesc) ;
BEGIN
   WHILE t#NIL DO
      BackPatchFactor(t^.factor) ;
      t := t^.next
   END
END BackPatchTerm ;


(*
   BackPatchExpression - runs through the term to find any idents.
*)

PROCEDURE BackPatchExpression (e: ExpressionDesc) ;
BEGIN
   IF e#NIL
   THEN
      BackPatchTerm(e^.term)
   END
END BackPatchExpression ;


(*
   BackPatchSet -
*)

PROCEDURE BackPatchSet (s: SetDesc) ;
BEGIN
   WHILE s#NIL DO
      WITH s^ DO
         CASE type OF

         idel:  BackPatchIdent(ident)

         ELSE
         END
      END ;
      s := s^.next
   END
END BackPatchSet ;


(*
   BackPatchIdentToDefinitions - search through all the rules and add a link from any ident
                                 to the definition.
*)

PROCEDURE BackPatchIdentToDefinitions (d: ProductionDesc) ;
BEGIN
   IF (d#NIL) AND (d^.statement#NIL)
   THEN
      BackPatchExpression(d^.statement^.expr)
   END
END BackPatchIdentToDefinitions ;


(*
   CalculateFirstAndFollow -
*)

PROCEDURE CalculateFirstAndFollow (p: ProductionDesc) ;
BEGIN
   IF Debugging
   THEN
      WriteLn ;
      WriteKey(p^.statement^.ident^.name) ; WriteLn ;
      WriteString('  calculating first')
   END ;
   CalcFirstProduction(p, p, p^.first) ;
   BackPatchSet(p^.first) ;
   IF Debugging
   THEN
      WriteString('  calculating follow set')
   END ;
   IF p^.followinfo^.follow=NIL
   THEN
      CalcFollowProduction(p)
   END ;
   BackPatchSet(p^.followinfo^.follow)
END CalculateFirstAndFollow ;


(*
   ForeachRuleDo -
*)

PROCEDURE ForeachRuleDo (p: DoProcedure) ;
BEGIN
   CurrentProduction := HeadProduction ;
   WHILE CurrentProduction#NIL DO
      p(CurrentProduction) ;
      CurrentProduction := CurrentProduction^.next
   END
END ForeachRuleDo ;


(*
   WhileNotCompleteDo -
*)

PROCEDURE WhileNotCompleteDo (p: DoProcedure) ;
BEGIN
   REPEAT
      Finished := TRUE ;
      ForeachRuleDo(p) ;
   UNTIL Finished
END WhileNotCompleteDo ;


(*
   NewLine - generate a newline and indent.
*)

PROCEDURE NewLine (Left: CARDINAL) ;
BEGIN
   WriteLn ;
   BeginningOfLine := TRUE ;
   Indent := 0 ;
   WHILE Indent<Left DO
      Write(' ') ;
      INC(Indent)
   END
END NewLine ;


(*
   CheckNewLine -
*)

PROCEDURE CheckNewLine (Left: CARDINAL) ;
BEGIN
   IF Indent=Left
   THEN
      Left := BaseNewLine
   END ;
   IF Indent>BaseRightMargin
   THEN
      NewLine(Left)
   END
END CheckNewLine ;


(*
   IndentString - writes out a string with a preceeding indent.
*)

PROCEDURE IndentString (a: ARRAY OF CHAR) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE i<Indent DO
      Write(' ') ;
      INC(i)
   END ;
   WriteString(a) ;
   LastLineNo := 0
END IndentString ;


(*
   KeyWord - writes out a keywork with optional formatting directives.
*)

PROCEDURE KeyWord (n: Name) ;
BEGIN
   IF KeywordFormatting
   THEN
      WriteString('{%K') ;
      IF (n = MakeKey('}')) OR (n = MakeKey('{')) OR (n = MakeKey('%'))
      THEN
         Write('%')   (* escape }, { or % *)
      END ;
      WriteKey(n) ;
      Write('}')
   ELSE
      WriteKey(n)
   END
END KeyWord ;


(*
   PrettyPara -
*)

PROCEDURE PrettyPara (c1, c2: ARRAY OF CHAR; e: ExpressionDesc; Left: CARDINAL) ;
BEGIN
   WriteString(c1) ;
   INC(Indent, StrLen(c1)) ;
   Left := Indent ;
   PrettyCommentExpression(e, Left) ;
   WriteString(c2) ;
   INC(Indent, StrLen(c2))
END PrettyPara ;


(*
   WriteKeyTexinfo -
*)

PROCEDURE WriteKeyTexinfo (s: Name) ;
VAR
   ds  : String ;
   ch  : CHAR ;
   i, l: CARDINAL ;
BEGIN
   IF Texinfo
   THEN
      ds := InitStringCharStar(KeyToCharStar(s)) ;
      l := Length(ds) ;
      i := 0 ;
      WHILE i<l DO
         ch := char(ds, i) ;
         IF (ch='{') OR (ch='}')
         THEN
            Write('@')
         END ;
         Write(ch) ;
         INC(i)
      END
   ELSE
      WriteKey(s)
   END
END WriteKeyTexinfo ;


(*
   PrettyCommentFactor -
*)

PROCEDURE PrettyCommentFactor (f: FactorDesc; Left: CARDINAL) ;
VAR
   curpos  : CARDINAL ;
   seentext: BOOLEAN ;
BEGIN
   WHILE f#NIL DO
      CheckNewLine(Left) ;
      WITH f^ DO
         CASE type OF

         id  :  WriteKey(ident^.name) ; WriteString(' ') ;
                INC(Indent, LengthKey(ident^.name)+1) |
         lit :  IF MakeKey("'")=string
                THEN
                   Write('"') ; WriteKeyTexinfo(string) ; WriteString('" ')
                ELSE
                   Write("'") ; WriteKeyTexinfo(string) ; WriteString("' ")
                END ;
                INC(Indent, LengthKey(string)+3) |
         sub:   PrettyPara('( ', ' ) ', expr, Left) |
         opt:   PrettyPara('[ ', ' ] ', expr, Left) |
         mult:  IF Texinfo
                THEN
                   PrettyPara('@{ ', ' @} ', expr, Left)
                ELSE
                   PrettyPara('{ ', ' } ', expr, Left)
                END |
         m2 :   IF EmitCode
                THEN
                   NewLine(Left) ; WriteString('% ') ;
                   seentext := FALSE ;
                   curpos := 0 ;
                   WriteCodeHunkListIndent(code^.code, code^.indent, curpos, Left+2, seentext) ;
                   WriteString(' %') ;
                   NewLine(Left)
                END

         ELSE
         END ;
         PrettyFollow('<f:', ':f>', followinfo)
      END ;
      f := f^.next
   END
END PrettyCommentFactor ;


(*
   PeepTerm - returns the length of characters in term.
*)

PROCEDURE PeepTerm (t: TermDesc) : CARDINAL ;
VAR
   l: CARDINAL ;
BEGIN
   l := 0 ;
   WHILE t#NIL DO
      INC(l, PeepFactor(t^.factor)) ;
      IF t^.next#NIL
      THEN
         INC(l, 3)
      END ;
      t := t^.next
   END ;
   RETURN( l )
END PeepTerm ;


(*
   PeepExpression - returns the length of the expression.
*)

PROCEDURE PeepExpression (e: ExpressionDesc) : CARDINAL ;
BEGIN
   IF e=NIL
   THEN
      RETURN( 0 )
   ELSE
      RETURN( PeepTerm(e^.term) )
   END
END PeepExpression ;


(*
   PeepFactor - returns the length of character in the factor
*)

PROCEDURE PeepFactor (f: FactorDesc) : CARDINAL ;
VAR
   l: CARDINAL ;
BEGIN
   l := 0 ;
   WHILE f#NIL DO
      WITH f^ DO
         CASE type OF

         id  :  INC(l, LengthKey(ident^.name)+1) |
         lit :  INC(l, LengthKey(string)+3) |
         opt ,
         mult,
         sub :  INC(l, PeepExpression(expr)) |
         m2  :  (* empty *)

         ELSE
         END
      END ;
      f := f^.next
   END ;
   RETURN( l )
END PeepFactor ;


(*
   PrettyCommentTerm -
*)

PROCEDURE PrettyCommentTerm (t: TermDesc; Left: CARDINAL) ;
BEGIN
   WHILE t#NIL DO
      CheckNewLine(Left) ;
      PrettyCommentFactor(t^.factor, Left) ;
      IF t^.next#NIL
      THEN
         WriteString(' | ') ;
         INC(Indent, 3) ;
         IF PeepFactor(t^.factor)+Indent>BaseRightMargin
         THEN
            NewLine(Left)
         END
      END ;
      PrettyFollow('<t:', ':t>', t^.followinfo) ;
      t := t^.next
   END
END PrettyCommentTerm ;


(*
   PrettyCommentExpression -
*)

PROCEDURE PrettyCommentExpression (e: ExpressionDesc; Left: CARDINAL) ;
BEGIN
   IF e#NIL
   THEN
      PrettyCommentTerm(e^.term, Left) ;
      PrettyFollow('<e:', ':e>', e^.followinfo)
   END
END PrettyCommentExpression ;


(*
   PrettyCommentStatement -
*)

PROCEDURE PrettyCommentStatement (s: StatementDesc; Left: CARDINAL) ;
BEGIN
   IF s#NIL
   THEN
      PrettyCommentExpression(s^.expr, Left) ;
      PrettyFollow('<s:', ':s>', s^.followinfo)
   END
END PrettyCommentStatement ;


(*
   PrettyCommentProduction - generates the comment for rule, p.
*)

PROCEDURE PrettyCommentProduction (p: ProductionDesc) ;
VAR
   to: SetDesc ;
BEGIN
   IF p#NIL
   THEN
      BeginningOfLine := TRUE ;
      Indent          := 0 ;
      WriteString('(*') ; NewLine(3) ;
      WriteKey(GetDefinitionName(p)) ;
      WriteString(' := ') ;
      INC(Indent, LengthKey(GetDefinitionName(p))+4) ;
      PrettyCommentStatement(p^.statement, Indent) ;
      NewLine(0) ;
      IF ErrorRecovery
      THEN
         NewLine(3) ;
         WriteString('first  symbols:') ;
         EmitSet(p^.first, 0, 0) ;
         NewLine(3) ;
         PrettyFollow('<p:', ':p>', p^.followinfo) ;
         NewLine(3) ;
         CASE GetReachEnd(p^.followinfo) OF

         true   :  WriteString('reachend') |
         false  :  WriteString('cannot reachend') |
         unknown:  WriteString('unknown...')

         ELSE
         END ;
         NewLine(0)
      END ;
      WriteString('*)') ; NewLine(0) ;
   END
END PrettyCommentProduction ;


(*
   PrettyPrintProduction - pretty prints the ebnf rule, p.
*)

PROCEDURE PrettyPrintProduction (p: ProductionDesc) ;
VAR
   to: SetDesc ;
BEGIN
   IF p#NIL
   THEN
      BeginningOfLine := TRUE ;
      Indent          := 0 ;
      IF Texinfo
      THEN
         WriteString('@example') ; NewLine(0)
      END ;
      WriteKey(GetDefinitionName(p)) ;
      WriteString(' := ') ;
      INC(Indent, LengthKey(GetDefinitionName(p))+4) ;
      PrettyCommentStatement(p^.statement, Indent) ;
      IF p^.description#NulName
      THEN
         WriteKey(p^.description)
      END ;
      NewLine(0) ;
      WriteIndent(LengthKey(GetDefinitionName(p))+1) ;
      WriteString(' =: ') ;
      NewLine(0) ;
      IF Texinfo
      THEN
         WriteString('@findex ') ; WriteKey(GetDefinitionName(p)) ; WriteString(' (ebnf)') ; NewLine(0) ;
         WriteString('@end example') ; NewLine(0)
      END ;
      NewLine(0)
   END
END PrettyPrintProduction ;


(*
   EmitFileLineTag - emits a line and file tag using the C preprocessor syntax.
*)

PROCEDURE EmitFileLineTag (line: CARDINAL) ;
BEGIN
   IF (NOT SuppressFileLineTag) AND (line#LastLineNo)
   THEN
      LastLineNo := line ;
      IF NOT OnLineStart
      THEN
         WriteLn
      END ;
      WriteString('# ') ; WriteCard(line, 0) ; WriteString(' "') ; WriteString(FileName) ; Write('"') ;
      WriteLn ;
      OnLineStart := TRUE
   END
END EmitFileLineTag ;


(*
   EmitRule - generates a comment and code for rule, p.
*)

PROCEDURE EmitRule (p: ProductionDesc) ;
BEGIN
   IF PrettyPrint
   THEN
      PrettyPrintProduction(p)
   ELSE
      PrettyCommentProduction(p) ;
      IF ErrorRecovery
      THEN
         RecoverProduction(p)
      ELSE
         CodeProduction(p)
      END
   END
END EmitRule ;


(*
   CodeCondition -
*)

PROCEDURE CodeCondition (m: m2condition) ;
BEGIN
   CASE m OF

   m2if,
   m2none :  IndentString('IF ')    |
   m2elsif:  IndentString('ELSIF ') |
   m2while:  IndentString('WHILE ')

   ELSE
      Halt('unrecognised m2condition', __LINE__, __FILE__)
   END
END CodeCondition ;


(*
   CodeThenDo - codes a "THEN" or "DO" depending upon, m.
*)

PROCEDURE CodeThenDo (m: m2condition) ;
BEGIN
   CASE m OF

   m2if,
   m2none,
   m2elsif:  IF LastLineNo=0
             THEN
                WriteLn
             END ;
             IndentString('THEN') ;
             WriteLn |
   m2while:  WriteString(' DO') ;
             WriteLn

   ELSE
      Halt('unrecognised m2condition', __LINE__, __FILE__)
   END ;
   OnLineStart := TRUE
END CodeThenDo ;


(*
   CodeElseEnd - builds an ELSE END statement using string, end.
*)

PROCEDURE CodeElseEnd (end: ARRAY OF CHAR; consumed: BOOLEAN; f: FactorDesc; inopt: BOOLEAN) ;
BEGIN
   WriteLn ;
   OnLineStart := TRUE ;
   EmitFileLineTag(f^.line) ;
   IF NOT inopt
   THEN
      IndentString('ELSE') ; WriteLn ;
      INC(Indent, 3) ;
      IF consumed
      THEN
         IndentString('') ;
         WriteKey(ErrorProcArray) ;
         Write('(') ;
         WITH f^ DO
            CASE type OF

            id  :  Write("'") ; WriteKey(ident^.name) ; WriteString(' - expected') ; WriteString("') ;") |
            lit :  IF MakeKey("'")=string
                   THEN
                      Write('"') ;
                      KeyWord(string) ;
                      WriteString(' - expected') ; WriteString('") ;')
                   ELSIF MakeKey('"')=string
                   THEN
                      Write("'") ; KeyWord(string) ;
                      WriteString(' - expected') ; WriteString("') ;")
                   ELSE
                      Write('"') ; Write("'") ; KeyWord(string) ; WriteString("' - expected") ;
                      WriteString('") ;')
                   END

            ELSE
            END
         END ;
         WriteLn
      END ;
      IndentString('RETURN( FALSE )') ;
      DEC(Indent, 3) ;
      WriteLn
   END ;
   IndentString(end) ;
   WriteLn ;
   OnLineStart := TRUE
END CodeElseEnd ;


(*
   CodeEnd - codes a "END" depending upon, m.
*)

PROCEDURE CodeEnd (m: m2condition; t: TermDesc; consumed: BOOLEAN; f: FactorDesc; inopt: BOOLEAN) ;
BEGIN
   DEC(Indent, 3) ;
   WriteLn ;
   OnLineStart := TRUE ;
   CASE m OF

   m2none :  IF t=NIL
             THEN
                CodeElseEnd('END ;', consumed, f, inopt)
             END |
   m2if   :  IF t=NIL
             THEN
                CodeElseEnd('END ;  (* if *)', consumed, f, inopt)
             END |
   m2elsif:  IF t=NIL
             THEN
                CodeElseEnd('END ;  (* elsif *)', consumed, f, inopt)
             END |
   m2while:  IndentString('END ;  (* while *)')

   ELSE
      Halt('unrecognised m2condition', __LINE__, __FILE__)
   END ;
   OnLineStart := FALSE
END CodeEnd ;


(*
   EmitNonVarCode - writes out, code, providing it is not a variable declaration.
*)

PROCEDURE EmitNonVarCode (code: CodeDesc; curpos, left: CARDINAL) ;
VAR
   i       : CARDINAL ;
   t       : CodeHunk ;
   seentext: BOOLEAN ;
BEGIN
   t := code^.code ;
   IF (NOT FindStr(t, i, 'VAR')) AND EmitCode
   THEN
      seentext := FALSE ;
      curpos := 0 ;
      EmitFileLineTag(code^.line) ;
      IndentString('') ;
      WriteCodeHunkListIndent(code^.code, code^.indent, curpos, left, seentext) ;
      WriteString(' ;') ;
      WriteLn ;
      OnLineStart := TRUE
   END
END EmitNonVarCode ;


(*
   ChainOn -
*)

PROCEDURE ChainOn (codeStack, f: FactorDesc) : FactorDesc ;
VAR
   s: FactorDesc ;
BEGIN
   f^.pushed := NIL ;
   IF codeStack=NIL
   THEN
      RETURN( f )
   ELSE
      s := codeStack ;
      WHILE s^.pushed#NIL DO
         s := s^.pushed
      END ;
      s^.pushed := f ;
      RETURN( codeStack )
   END
END ChainOn ;


(*
   FlushCode -
*)

PROCEDURE FlushCode (VAR codeStack: FactorDesc) ;
BEGIN
   IF codeStack#NIL
   THEN
      NewLine(Indent) ; WriteString('(* begin flushing code *)') ;
      OnLineStart := FALSE ;
      WHILE codeStack#NIL DO
         NewLine(Indent) ; EmitNonVarCode(codeStack^.code, 0, Indent) ; NewLine(Indent) ;
         codeStack := codeStack^.pushed ;
         IF codeStack#NIL
         THEN
            WriteString(' (* again flushing code *)') ; WriteLn ;
            OnLineStart := TRUE
         END
      END ;
      NewLine(Indent) ;
      WriteString('(* end flushing code *)') ;
      OnLineStart := FALSE
   END
END FlushCode ;


(*
   CodeFactor -
*)

PROCEDURE CodeFactor (f: FactorDesc; t: TermDesc; l, n: m2condition; inopt, inwhile, consumed: BOOLEAN; codeStack: FactorDesc) ;
BEGIN
   IF f=NIL
   THEN
      IF (* ((l=m2elsif) OR (l=m2if) OR (l=m2none)) AND *) (NOT inwhile) AND (NOT inopt)
      THEN
         WriteLn ;
         IndentString('RETURN( TRUE )') ;
         OnLineStart := FALSE
      END
   ELSE
      WITH f^ DO
         EmitFileLineTag(line) ;
         CASE type OF

         id  :  FlushCode(codeStack) ;
                CodeCondition(n) ;
                WriteKey(ident^.name) ; WriteString('()') ;
                CodeThenDo(n) ;
                INC(Indent, 3) ;
                CodeFactor(f^.next, NIL, n, m2none, inopt, inwhile, TRUE, NIL) ;
                CodeEnd(n, t, consumed, f, inopt) |
         lit :  FlushCode(codeStack) ;
                CodeCondition(n) ;
                WriteKey(SymIsProc) ; Write('(') ;
                WriteKey(GetSymKey(Aliases, string)) ; Write(')') ;
                CodeThenDo(n) ;
                INC(Indent, 3) ;
                CodeFactor(f^.next, NIL, n, m2none, inopt, inwhile, TRUE, NIL) ;
                CodeEnd(n, t, consumed, f, inopt) |
         sub:   FlushCode(codeStack) ;
                CodeExpression(expr, m2none, inopt, inwhile, consumed, NIL) ;
                IF f^.next#NIL
                THEN
                   (*
                    *  the test above makes sure that we don't emit a RETURN( TRUE )
                    *  after a subexpression. Remember sub expressions are not conditional
                    *)
                   CodeFactor(f^.next, t, n, m2none, inopt, inwhile, TRUE, NIL)
                END |
         opt:   FlushCode(codeStack) ;
                CodeExpression(expr, m2if, TRUE, inwhile, FALSE, NIL) ;
                CodeFactor(f^.next, t, n, m2none, inopt, inwhile, consumed, NIL) |
         mult:  FlushCode(codeStack) ;
                CodeExpression(expr, m2while, FALSE, TRUE, consumed, NIL) ;
                CodeFactor(f^.next, t, n, m2none, inopt, inwhile, consumed, NIL) |
         m2 :   codeStack := ChainOn(codeStack, f) ;
                IF consumed OR (f^.next=NIL)
                THEN
                   FlushCode(codeStack)
                END ;
                CodeFactor(f^.next, t, n, m2none, inopt, inwhile, consumed, codeStack)

         ELSE
         END
      END
   END
END CodeFactor ;


(*
   CodeTerm -
*)

PROCEDURE CodeTerm (t: TermDesc; m: m2condition; inopt, inwhile, consumed: BOOLEAN; codeStack: FactorDesc) ;
VAR
   l: m2condition ;
BEGIN
   l := m ;
   WHILE t#NIL DO
      EmitFileLineTag(t^.line) ;
      IF (t^.factor^.type=m2) AND (m=m2elsif)
      THEN
         m := m2if ;
         IndentString('ELSE') ; WriteLn ;
         OnLineStart := TRUE ;
         INC(Indent, 3) ;
         CodeFactor(t^.factor, t^.next, m2none, m2none, inopt, inwhile, consumed, codeStack) ;
         DEC(Indent, 3) ;
         IndentString('END ;') ; WriteLn ;
         OnLineStart := TRUE
      ELSE
         CodeFactor(t^.factor, t^.next, m2none, m, inopt, inwhile, consumed, codeStack)
      END ;
      l := m ;
      IF t^.next#NIL
      THEN
         m := m2elsif
      END ;
      t := t^.next
   END
END CodeTerm ;


(*
   CodeExpression -
*)

PROCEDURE CodeExpression (e: ExpressionDesc; m: m2condition; inopt, inwhile, consumed: BOOLEAN; codeStack: FactorDesc) ;
BEGIN
   IF e#NIL
   THEN
      EmitFileLineTag(e^.line) ;
      CodeTerm(e^.term, m, inopt, inwhile, consumed, codeStack)
   END
END CodeExpression ;


(*
   CodeStatement -
*)

PROCEDURE CodeStatement (s: StatementDesc; m: m2condition) ;
BEGIN
   IF s#NIL
   THEN
      EmitFileLineTag(s^.line) ;
      CodeExpression(s^.expr, m, FALSE, FALSE, FALSE, NIL)
   END
END CodeStatement ;


(*
   CodeProduction - only encode grammer rules which are not special.
*)

PROCEDURE CodeProduction (p: ProductionDesc) ;
BEGIN
   IF (p#NIL) AND ((NOT p^.firstsolved) OR ((p^.statement#NIL) AND (p^.statement^.expr#NIL)))
   THEN
      BeginningOfLine := TRUE ;
      Indent          := 0 ;
      WriteLn ;
      EmitFileLineTag(p^.line) ;
      IndentString('PROCEDURE ') ;
      WriteKey(GetDefinitionName(p)) ;
      WriteString(' () : BOOLEAN ;') ;
      VarProduction(p) ;
      WriteLn ;
      OnLineStart := TRUE ;
      EmitFileLineTag(p^.line) ;
      IndentString('BEGIN') ; WriteLn ;
      OnLineStart := FALSE ;
      EmitFileLineTag(p^.line) ;
      Indent := 3 ;
      CodeStatement(p^.statement, m2none) ;
      WriteLn ;
      Indent := 0 ;
      IndentString('END ') ; WriteKey(GetDefinitionName(p)) ; WriteString(' ;') ;
      WriteLn ;
      WriteLn ;
      WriteLn
   END
END CodeProduction ;


(* and now for the production of code which will recover from syntax errors *)


(*
   RecoverCondition -
*)

PROCEDURE RecoverCondition (m: m2condition) ;
BEGIN
   CASE m OF

   m2if   :  IndentString('IF ')    |
   m2none :  IndentString('IF ')    |
   m2elsif:  IndentString('ELSIF ') |
   m2while:  IndentString('WHILE ')

   ELSE
      Halt('unrecognised m2condition', __LINE__, __FILE__)
   END
END RecoverCondition ;


(*
   ConditionIndent - returns the number of spaces indentation created via, m.
*)

PROCEDURE ConditionIndent (m: m2condition) : CARDINAL ;
BEGIN
   CASE m OF

   m2if   :  RETURN( 3 ) |
   m2none :  RETURN( 3 ) |
   m2elsif:  RETURN( 6 ) |
   m2while:  RETURN( 6 )

   ELSE
      Halt('unrecognised m2condition', __LINE__, __FILE__)
   END
END ConditionIndent ;


(*
   WriteGetTokenType - writes out the method of determining the token type.
*)

PROCEDURE WriteGetTokenType ;
BEGIN
   WriteKey(TokenTypeProc)
END WriteGetTokenType ;


(*
   NumberOfElements - returns the number of elements in set, to, which lie between low..high
*)

PROCEDURE NumberOfElements (to: SetDesc; low, high: WORD) : CARDINAL ;
VAR
   n: CARDINAL ;
BEGIN
   n := 0 ;
   WHILE to#NIL DO
      WITH to^ DO
         CASE type OF

         tokel:  IF (high=0) OR IsBetween(string, low, high)
                 THEN
                    INC(n)
                 END |
         litel:  IF (high=0) OR IsBetween(GetSymKey(Aliases, string), low, high)
                 THEN
                    INC(n)
                 END |
         idel :  WarnError('not expecting ident in first symbol list') ;
                 WasNoError := FALSE

         ELSE
            WarnError('unknown enuneration element') ;
            WasNoError := FALSE
         END
      END ;
      to := to^.next ;
   END ;
   RETURN( n )
END NumberOfElements ;


(*
   WriteElement - writes the literal name for element, e.
*)

PROCEDURE WriteElement (e: WORD) ;
BEGIN
   WriteKey(GetSymKey(ReverseValues, e))
END WriteElement ;


(*
   EmitIsInSet - writes out the equivalent of GetTokenType() IN { toset }
*)

PROCEDURE EmitIsInSet (to: SetDesc; low, high: Name) ;
BEGIN
   IF NumberOfElements(to, low, high)=1
   THEN
      WriteGetTokenType ; Write('=') ; EmitSet(to, low, high)
   ELSE
      WriteGetTokenType ;
      WriteString(' IN SetOfStop') ;
      IF LargestValue > MaxElementsInSet
      THEN
         WriteCard(CARDINAL(low) DIV MaxElementsInSet, 0)
      END ;
      WriteString(' {') ; EmitSet(to, low, high) ; WriteString('}')
   END
END EmitIsInSet ;


(*
   EmitIsInSubSet - writes out a test to see whether GetTokenype() is in { subset }
*)

PROCEDURE EmitIsInSubSet (to: SetDesc; low, high: WORD) ;
BEGIN
   IF NumberOfElements(to, low, high)=1
   THEN
      Write('(') ; EmitIsInSet(to, low, high) ; Write(')')
   ELSIF low=0
   THEN
      (* no need to check whether GetTokenType > low *)
      WriteString('((') ; WriteGetTokenType ; Write('<') ; WriteElement(INTEGER(high)+1) ;
      WriteString(') AND (') ; EmitIsInSet(to, low, high) ; WriteString('))')
   ELSIF CARDINAL(high)>LargestValue
   THEN
      (* no need to check whether GetTokenType < high *)
      WriteString('((') ; WriteGetTokenType ; WriteString('>=') ; WriteElement(low) ;
      WriteString(') AND (') ; EmitIsInSet(to, low, high) ; WriteString('))')
   ELSE
      WriteString('((') ; WriteGetTokenType ; WriteString('>=') ; WriteElement(low) ;
      WriteString(') AND (') ; WriteGetTokenType ; Write('<') ; WriteElement(INTEGER(high)+1) ;
      WriteString(') AND (') ; EmitIsInSet(to, low, high) ;
      WriteString('))')
   END
END EmitIsInSubSet ;


(*
   EmitIsInFirst -
*)

PROCEDURE EmitIsInFirst (to: SetDesc; m: m2condition) ;
VAR
   i    : CARDINAL ;
   first: BOOLEAN ;
BEGIN
   IF NumberOfElements(to, 0, 0)=1
   THEN
      (* only one element *)
      WriteGetTokenType ;
      Write('=') ;
      EmitSet(to, 0, 0)
   ELSE
      IF LargestValue<=MaxElementsInSet
      THEN
         Write('(') ; WriteGetTokenType ; WriteString(' IN ') ; EmitSetAsParameters(to) ; WriteString(')')
      ELSE
         i     := 0 ;
         first := TRUE ;
         REPEAT
            IF NOT IsEmptySet(to, i*MaxElementsInSet, (i+1)*MaxElementsInSet-1)
            THEN
               IF NOT first
               THEN
                  WriteString(' OR') ;
                  NewLine(Indent+ConditionIndent(m)) ;
                  DEC(Indent, ConditionIndent(m))
               END ;
               EmitIsInSubSet(to, i*MaxElementsInSet, (i+1)*MaxElementsInSet-1) ;
               first := FALSE
            END ;
            INC(i) ;
         UNTIL i*MaxElementsInSet>LargestValue
      END
   END
END EmitIsInFirst ;


(*
   FlushCode -
*)

PROCEDURE FlushRecoverCode (VAR codeStack: FactorDesc) ;
BEGIN
   IF codeStack#NIL
   THEN
      WHILE codeStack#NIL DO
         EmitNonVarCode(codeStack^.code, 0, Indent) ;
         codeStack := codeStack^.pushed
      END
   END
END FlushRecoverCode ;


(*
   RecoverFactor -
*)

PROCEDURE RecoverFactor (f: FactorDesc; m: m2condition; codeStack: FactorDesc) ;
VAR
   to: SetDesc ;
BEGIN
   IF f=NIL
   THEN
   ELSE
      EmitFileLineTag(f^.line) ;
      WITH f^ DO
         CASE type OF

         id  :  to := NIL ;
                CalcFirstFactor(f, NIL, to) ;
                IF (to#NIL) AND (m#m2none)
                THEN
                   RecoverCondition(m) ;
                   EmitIsInFirst(to, m) ;
                   CodeThenDo(m) ;
                   INC(Indent, 3)
                END ;
                FlushRecoverCode(codeStack) ;
                IndentString('') ;
                WriteKey(ident^.name) ; Write('(') ;
                EmitStopParametersAndFollow(f, m) ; WriteString(') ;') ; WriteLn ;
                RecoverFactor(f^.next, m2none, codeStack) ;
                IF (to#NIL) AND (m#m2none)
                THEN
                   DEC(Indent, 3)
                END |
         lit :  IF m=m2none
                THEN
                   FlushRecoverCode(codeStack) ;
                   IndentString('Expect(') ;
                   WriteKey(GetSymKey(Aliases, string)) ; WriteString(', ') ;
                   EmitStopParametersAndFollow(f, m) ; WriteString(') ;') ; WriteLn ;
                   RecoverFactor(f^.next, m2none, codeStack)
                ELSE
                   RecoverCondition(m) ;
                   WriteGetTokenType ;
                   Write('=') ;
                   WriteKey(GetSymKey(Aliases, string)) ;
                   CodeThenDo(m) ;
                   INC(Indent, 3) ;
                   IndentString('Expect(') ;
                   WriteKey(GetSymKey(Aliases, string)) ; WriteString(', ') ;
                   EmitStopParametersAndFollow(f, m) ; WriteString(') ;') ;
                   WriteLn ;
                   FlushRecoverCode(codeStack) ;
                   RecoverFactor(f^.next, m2none, codeStack) ;
                   DEC(Indent, 3)
                END |
         sub:   FlushRecoverCode(codeStack) ;
                RecoverExpression(expr, m2none, m) ;
                RecoverFactor(f^.next, m2none, codeStack) |
         opt:   FlushRecoverCode(codeStack) ;
                IF OptExpSeen(f)
                THEN
                   to := NIL ;
                   CalcFirstExpression(expr, NIL, to) ;
                   RecoverCondition(m) ;
                   EmitIsInFirst(to, m) ;
                   CodeThenDo(m) ;
                   INC(Indent, 3) ;
                   IndentString('(* seen optional [ | ] expression *)') ; WriteLn ;
                   stop();
                   RecoverExpression(expr, m2none, m2if) ;
                   IndentString('(* end of optional [ | ] expression *)') ; WriteLn ;
                   DEC(Indent, 3) ;
                   IndentString('END ;') ; WriteLn
                ELSE
                   RecoverExpression(expr, m2if, m)
                END ;
                RecoverFactor(f^.next, m2none, codeStack) |
         mult:  FlushRecoverCode(codeStack) ;
                IF OptExpSeen(f) OR (m=m2if) OR (m=m2elsif)
                THEN
                   to := NIL ;
                   CalcFirstExpression(expr, NIL, to) ;
                   RecoverCondition(m) ;
                   EmitIsInFirst(to, m) ;
                   CodeThenDo(m) ;
                   INC(Indent, 3) ;
                   IndentString('(* seen optional { | } expression *)') ; WriteLn ;
                   RecoverCondition(m2while) ;
                   EmitIsInFirst(to, m2while) ;
                   CodeThenDo(m2while) ;
                   INC(Indent, 3) ;
                   RecoverExpression(expr, m2none, m2while) ;
                   IndentString('(* end of optional { | } expression *)') ; WriteLn ;
                   DEC(Indent, 3) ;
                   IndentString('END ;') ; WriteLn ;
                   DEC(Indent, 3) ;
                   IF m=m2none
                   THEN
                      IndentString('END ;') ; WriteLn ;
                      DEC(Indent, 3)
                   END
                ELSE
                   RecoverExpression(expr, m2while, m)
                END ;
                RecoverFactor(f^.next, m2none, codeStack) |
         m2 :   codeStack := ChainOn(codeStack, f) ;
                IF f^.next=NIL
                THEN
                   FlushRecoverCode(codeStack)
                ELSE
                   RecoverFactor(f^.next, m, codeStack)   (* was m2none *)
                END

         ELSE
         END
      END
   END
END RecoverFactor ;


(*
   OptExpSeen - returns TRUE if we can see an optional expression in the factor.
                This is not the same as epsilon. Example { '+' } matches epsilon as
                well as { '+' | '-' } but OptExpSeen returns TRUE in the second case
                and FALSE in the first.
*)

PROCEDURE OptExpSeen (f: FactorDesc) : BOOLEAN ;
BEGIN
   IF f=NIL
   THEN
      RETURN( FALSE )
   ELSE
      WITH f^ DO
         CASE type OF

         id  ,
         lit :   RETURN( FALSE ) |
         sub :   RETURN( FALSE ) |  (* is this correct? *)
         opt ,
         mult:   RETURN( (expr#NIL) AND (expr^.term#NIL) AND (expr^.term^.next#NIL) ) |
         m2  :   RETURN( TRUE )

         ELSE
         END
      END
   END ;
   WarnError('all cases were not handled') ;
   WasNoError := FALSE
END OptExpSeen ;


(*
   RecoverTerm -
*)

PROCEDURE RecoverTerm (t: TermDesc; new, old: m2condition) ;
VAR
   LastWasM2Only,             (* does the factor only contain inline code? *)
   alternative  : BOOLEAN ;
   to           : SetDesc ;
BEGIN
   LastWasM2Only := (t^.factor^.type = m2) AND (t^.factor^.next = NIL) ;
   to := NIL ;
   CalcFirstTerm(t, NIL, to) ;
   alternative := FALSE ;
   IF t^.next#NIL
   THEN
      new := m2if
   END ;
   WHILE t#NIL DO
      EmitFileLineTag(t^.line) ;
      LastWasM2Only := (t^.factor^.type = m2) AND (t^.factor^.next = NIL) ;
      IF (t^.factor^.type=m2) AND (new=m2elsif)
      THEN
         new := m2if ;
         IndentString('ELSE') ; WriteLn ;
         INC(Indent, 3) ;
         RecoverFactor(t^.factor, m2none, NIL) ;
         alternative := FALSE
      ELSE
         RecoverFactor(t^.factor, new, NIL)
      END ;
      IF t^.next#NIL
      THEN
         new := m2elsif ;
         alternative := TRUE
      END ;
      t := t^.next
   END ;
   IF (new=m2if) OR (new=m2elsif)
   THEN
      IF alternative AND (old#m2while)
      THEN
         IndentString('ELSE') ; WriteLn ;
         INC(Indent, 3) ;
         IndentString('') ;
         WriteKey(ErrorProcArray) ;
         WriteString("('expecting one of: ") ;
         EmitSetName(to, 0, 0) ;
         WriteString("')") ;
         WriteLn ;
         DEC(Indent, 3)
      ELSIF LastWasM2Only
      THEN
         DEC(Indent, 3)
      END ;
      IndentString('END ;') ; WriteLn
   ELSIF new=m2while
   THEN
      IndentString('END (* while *) ;') ; WriteLn
   ELSIF LastWasM2Only
   THEN
      DEC(Indent, 3)
   END
END RecoverTerm ;


(*
   RecoverExpression -
*)

PROCEDURE RecoverExpression (e: ExpressionDesc; new, old: m2condition) ;
BEGIN
   IF e#NIL
   THEN
      EmitFileLineTag(e^.line) ;
      RecoverTerm(e^.term, new, old)
   END
END RecoverExpression ;


(*
   RecoverStatement -
*)

PROCEDURE RecoverStatement (s: StatementDesc; m: m2condition) ;
BEGIN
   IF s#NIL
   THEN
      EmitFileLineTag(s^.line) ;
      RecoverExpression(s^.expr, m, m2none)
   END
END RecoverStatement ;


(*
   EmitFirstFactor - generate a list of all first tokens between the range: low..high.
*)

PROCEDURE EmitFirstFactor (f: FactorDesc; low, high: CARDINAL) ;
BEGIN

END EmitFirstFactor ;


(*
   EmitStopParameters - generate the stop set.
*)

PROCEDURE EmitStopParameters (FormalParameters: BOOLEAN) ;
VAR
   i: CARDINAL ;
BEGIN
   IF LargestValue<=MaxElementsInSet
   THEN
      WriteString('stopset') ;
      IF FormalParameters
      THEN
         WriteString(': SetOfStop')
      END
   ELSE
      i := 0 ;
      REPEAT
         WriteString('stopset') ; WriteCard(i, 0) ;
         IF FormalParameters
         THEN
            WriteString(': SetOfStop') ; WriteCard(i, 0)
         END ;
         INC(i) ;
         IF i*MaxElementsInSet<LargestValue
         THEN
            IF FormalParameters
            THEN
               WriteString('; ')
            ELSE
               WriteString(', ')
            END
         END
      UNTIL i*MaxElementsInSet>=LargestValue ;
   END
END EmitStopParameters ;


(*
   IsBetween - returns TRUE if the value of the token, string, is
               in the range: low..high
*)

PROCEDURE IsBetween (string: Name; low, high: WORD) : BOOLEAN ;
BEGIN
   RETURN( (GetSymKey(Values, string) >= low) AND (GetSymKey(Values, string) <= high) )
END IsBetween ;


(*
   IsEmptySet - returns TRUE if no elements exist in set, to, with values, low..high.
*)

PROCEDURE IsEmptySet (to: SetDesc; low, high: WORD) : BOOLEAN ;
BEGIN
   WHILE to#NIL DO
      WITH to^ DO
         CASE type OF

         tokel:  IF IsBetween(string, low, high)
                 THEN
                    RETURN( FALSE )
                 END |
         litel:  IF IsBetween(GetSymKey(Aliases, string), low, high)
                 THEN
                    RETURN( FALSE )
                 END |
         idel :  WarnError('not expecting ident in first symbol list') ;
                 WasNoError := FALSE

         ELSE
            WarnError('unknown enuneration element') ;
            WasNoError := FALSE
         END
      END ;
      to := to^.next ;
   END ;
   RETURN( TRUE )
END IsEmptySet ;


(*
   EmitSet - emits the tokens in the set, to, which have values low..high
*)

PROCEDURE EmitSet (to: SetDesc; low, high: WORD) ;
VAR
   first: BOOLEAN ;
BEGIN
   first := TRUE ;
   WHILE to#NIL DO
      WITH to^ DO
         CASE type OF

         tokel:  IF (high=0) OR IsBetween(string, low, high)
                 THEN
                    IF NOT first
                    THEN
                       WriteString(', ')
                    END ;
                    WriteKey(string) ;
                    first := FALSE
                 END |
         litel:  IF (high=0) OR IsBetween(GetSymKey(Aliases, string), low, high)
                 THEN
                    IF NOT first
                    THEN
                       WriteString(', ')
                    END ;
                    WriteKey(GetSymKey(Aliases, string)) ;
                    first := FALSE
                 END |
         idel :  WarnError('not expecting ident in first symbol list') ;
                 WasNoError := FALSE

         ELSE
            WarnError('unknown enuneration element') ;
            WasNoError := FALSE
         END
      END ;
      to := to^.next
   END
END EmitSet ;


(*
   EmitSetName - emits the tokens in the set, to, which have values low..high, using
                 their names.
*)

PROCEDURE EmitSetName (to: SetDesc; low, high: WORD) ;
BEGIN
   WHILE to#NIL DO
      WITH to^ DO
         CASE type OF

         tokel:  IF (high=0) OR IsBetween(string, low, high)
                 THEN
                    IF MakeKey("'")=GetSymKey(ReverseAliases, string)
                    THEN
                       WriteString('single quote')
                    ELSE
                       KeyWord(GetSymKey(ReverseAliases, string))
                    END
                 END |
         litel:  IF (high=0) OR IsBetween(GetSymKey(Aliases, string), low, high)
                 THEN
                    WriteKey(string)
                 END |
         idel :  WarnError('not expecting ident in first symbol list') ;
                 WasNoError := FALSE

         ELSE
            WarnError('unknown enuneration element') ;
            WasNoError := FALSE
         END
      END ;
      to := to^.next ;
      IF to#NIL
      THEN
         Write(' ')
      END
   END
END EmitSetName ;


(*
   EmitStopParametersAndSet - generates the stop parameters together with a set
                              inclusion of all the symbols in set, to.
*)

PROCEDURE EmitStopParametersAndSet (to: SetDesc) ;
VAR
   i : CARDINAL ;
BEGIN
   IF LargestValue<=MaxElementsInSet
   THEN
      WriteString('stopset') ;
      IF (to#NIL) AND (NumberOfElements(to, 0, MaxElementsInSet-1)>0)
      THEN
         WriteString(' + SetOfStop') ;
         Write('{') ;
         EmitSet(to, 0, MaxElementsInSet-1) ;
         Write('}')
      END
   ELSE
      i := 0 ;
      REPEAT
         WriteString('stopset') ; WriteCard(i, 0) ;
         IF (to#NIL) AND (NumberOfElements(to, i*MaxElementsInSet, (i+1)*MaxElementsInSet-1)>0)
         THEN
            WriteString(' + SetOfStop') ; WriteCard(i, 0) ;
            Write('{') ;
            EmitSet(to, i*MaxElementsInSet, (i+1)*MaxElementsInSet-1) ;
            Write('}')
         END ;
         INC(i) ;
         IF i*MaxElementsInSet<LargestValue
         THEN
            WriteString(', ')
         END
      UNTIL i*MaxElementsInSet>=LargestValue
   END
END EmitStopParametersAndSet ;


(*
   EmitSetAsParameters - generates the first symbols as parameters to a set function.
*)

PROCEDURE EmitSetAsParameters (to: SetDesc) ;
VAR
   i : CARDINAL ;
BEGIN
   IF LargestValue<=MaxElementsInSet
   THEN
      Write('{') ;
      EmitSet(to, 0, MaxElementsInSet-1)
   ELSE
      i := 0 ;
      REPEAT
         Write('{') ;
         EmitSet(to, i*MaxElementsInSet, (i+1)*MaxElementsInSet-1) ;
         INC(i) ;
         IF (i+1)*MaxElementsInSet>LargestValue
         THEN
            WriteString('}, ')
         END
      UNTIL (i+1)*MaxElementsInSet>=LargestValue ;
   END ;
   Write('}')
END EmitSetAsParameters ;


(*
   EmitStopParametersAndFollow - generates the stop parameters together with a set
                                 inclusion of all the follow symbols for subsequent
                                 sentances.
*)

PROCEDURE EmitStopParametersAndFollow (f: FactorDesc; m: m2condition) ;
VAR
   to: SetDesc ;
BEGIN
   to := NIL ;
(*
   IF m=m2while
   THEN
      CalcFirstFactor(f, NIL, to)
   END ;
*)
   CollectFollow(to, f^.followinfo) ;
   EmitStopParametersAndSet(to) ;
   IF Debugging
   THEN
      WriteLn ;
      WriteString('factor is: ') ;
      PrettyCommentFactor(f, StrLen('factor is: ')) ;
      WriteLn ;
      WriteString('follow set:') ;
      EmitSet(to, 0, 0) ;
      WriteLn
   END
END EmitStopParametersAndFollow ;


(*
   EmitFirstAsParameters -
*)

PROCEDURE EmitFirstAsParameters (f: FactorDesc) ;
VAR
   to: SetDesc ;
BEGIN
   to := NIL ;
   CalcFirstFactor(f, NIL, to) ;
   EmitSetAsParameters(to)
END EmitFirstAsParameters ;


(*
   RecoverProduction - only encode grammer rules which are not special.
                       Generate error recovery code.
*)

PROCEDURE RecoverProduction (p: ProductionDesc) ;
BEGIN
   IF (p#NIL) AND ((NOT p^.firstsolved) OR ((p^.statement#NIL) AND (p^.statement^.expr#NIL)))
   THEN
      BeginningOfLine := TRUE ;
      Indent := 0 ;
      WriteLn ;
      OnLineStart := FALSE ;
      EmitFileLineTag(p^.line) ;
      IndentString('PROCEDURE ') ;
      WriteKey(GetDefinitionName(p)) ;
      WriteString(' (') ;
      EmitStopParameters(TRUE) ;
      WriteString(') ;') ;
      VarProduction(p) ;
      WriteLn ;
      OnLineStart := FALSE ;
      EmitFileLineTag(p^.line) ;
      Indent := 0 ;
      IndentString('BEGIN') ; WriteLn ;
      OnLineStart := FALSE ;
      EmitFileLineTag(p^.line) ;
      Indent := 3 ;
      RecoverStatement(p^.statement, m2none) ;
      Indent := 0 ;
      IndentString('END ') ; WriteKey(GetDefinitionName(p)) ; WriteString(' ;') ;
      WriteLn ;
      WriteLn ;
      WriteLn
   END
END RecoverProduction ;


(*
   IsWhite - returns TRUE if, ch, is a space or a tab.
*)

PROCEDURE IsWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch=' ') OR (ch=tab) OR (ch=lf) )
END IsWhite ;


(*
   FindStr - returns TRUE if, str, was seen inside the code hunk
*)

PROCEDURE FindStr (VAR code: CodeHunk; VAR i: CARDINAL; str: ARRAY OF CHAR) : BOOLEAN ;
VAR
   j, k: CARDINAL ;
   t   : CodeHunk ;
BEGIN
   t := code ;
   k := StrLen(code^.codetext)+1 ;
   WHILE t#NIL DO
      REPEAT
         WHILE (k>0) AND IsWhite(t^.codetext[k-1]) DO
            DEC(k)
         END ;
         IF k=0
         THEN
            t := t^.next ;
            k := MaxCodeHunkLength+1
         END
      UNTIL (t=NIL) OR (NOT IsWhite(t^.codetext[k-1])) ;

      (* found another word check it *)

      IF t#NIL
      THEN
         j := StrLen(str) ;
         i := k ;
         WHILE (t#NIL) AND (j>0) AND ((str[j-1]=t^.codetext[k-1]) OR
                                      (IsWhite(str[j-1]) AND IsWhite(t^.codetext[k-1]))) DO
            DEC(j) ;
            DEC(k) ;
            IF j=0
            THEN
               (* found word remember position *)
               code := t
            END ;
            IF k=0
            THEN
               t := t^.next ;
               k := MaxCodeHunkLength+1
            END
         END ;
         IF k>0
         THEN
            DEC(k)
         ELSE
            t := t^.next
         END
      END ;
   END ;
   RETURN( (t=NIL) AND (j=0) )
END FindStr ;


(*
   WriteUpto -
*)

PROCEDURE WriteUpto (code, upto: CodeHunk; limit: CARDINAL) ;
BEGIN
   IF code#upto
   THEN
      WriteUpto(code^.next, upto, limit) ;
      WriteString(code^.codetext)
   ELSE
      WHILE (limit<=MaxCodeHunkLength) AND (code^.codetext[limit]#nul) DO
         Write(code^.codetext[limit]) ;
         INC(limit)
      END
   END
END WriteUpto ;


(*
   CheckForVar - checks for any local variables which need to be emitted during
                 this production.
*)

PROCEDURE CheckForVar (code: CodeHunk) ;
VAR
   i: CARDINAL ;
   t: CodeHunk ;
BEGIN
   t := code ;
   IF FindStr(t, i, 'VAR') AND EmitCode
   THEN
      IF NOT EmittedVar
      THEN
         WriteLn ;
         Indent := 0 ;
         IndentString('VAR') ;
         INC(Indent, 3) ;
         WriteLn ;
         EmittedVar := TRUE ;
      END ;
      WriteUpto(code, t, i)
   END
END CheckForVar ;


(*
   VarFactor -
*)

PROCEDURE VarFactor (f: FactorDesc) ;
BEGIN
   WHILE f#NIL DO
      WITH f^ DO
         CASE type OF

         id  :  |
         lit :  |
         sub ,
         opt ,
         mult:  VarExpression(expr) |
         m2  :  CheckForVar(code^.code)

         ELSE
         END
      END ;
      f := f^.next
   END
END VarFactor ;


(*
   VarTerm -
*)

PROCEDURE VarTerm (t: TermDesc) ;
BEGIN
   WHILE t#NIL DO
      VarFactor(t^.factor) ;
      t := t^.next
   END
END VarTerm ;


(*
   VarExpression -
*)

PROCEDURE VarExpression (e: ExpressionDesc) ;
BEGIN
   IF e#NIL
   THEN
      VarTerm(e^.term)
   END
END VarExpression ;


(*
   VarStatement -
*)

PROCEDURE VarStatement (s: StatementDesc) ;
BEGIN
   IF s#NIL
   THEN
      VarExpression(s^.expr)
   END
END VarStatement ;


(*
   VarProduction - writes out all variable declarations.
*)

PROCEDURE VarProduction (p: ProductionDesc) ;
BEGIN
   EmittedVar := FALSE ;
   IF p#NIL
   THEN
      VarStatement(p^.statement)
   END
END VarProduction ;


(*
   In - returns TRUE if token, s, is already in the set, to.
*)

PROCEDURE In (to: SetDesc; s: Name) : BOOLEAN ;
BEGIN
   WHILE to#NIL DO
      WITH to^ DO
         CASE type OF

         idel  :  IF s=ident^.name
                  THEN
                     RETURN( TRUE )
                  END |
         tokel,
         litel :  IF s=string
                  THEN
                     RETURN( TRUE )
                  END

         ELSE
            WarnError('internal error CASE type not known') ;
            WasNoError := FALSE
         END
      END ;
      to := to^.next
   END ;
   RETURN( FALSE )
END In ;


(*
   IntersectionIsNil - given two set lists, s1, s2, return TRUE if the
                       s1 * s2 = {}
*)

PROCEDURE IntersectionIsNil (s1, s2: SetDesc) : BOOLEAN ;
BEGIN
   WHILE s1#NIL DO
      WITH s1^ DO
         CASE type OF

         idel :  IF In(s2, ident^.name)
                 THEN
                    RETURN( FALSE )
                 END |
         tokel,
         litel:  IF In(s2, string)
                 THEN
                    RETURN( FALSE )
                 END

         ELSE
            WarnError('internal error CASE type not known') ;
            WasNoError := FALSE
         END
      END ;
      s1 := s1^.next
   END ;
   RETURN( TRUE )
END IntersectionIsNil ;


(*
   AddSet - adds a first symbol to a production.
*)

PROCEDURE AddSet (VAR to: SetDesc; s: Name) ;
VAR
   d: SetDesc ;
BEGIN
   IF NOT In(to, s)
   THEN
      d := NewSetDesc() ;
      WITH d^ DO
         type   := tokel ;
         string := s ;
         next   := to ;
      END ;
      to := d ;
      Finished := FALSE
   END
END AddSet ;


(*
   OrSet -
*)

PROCEDURE OrSet (VAR to: SetDesc; from: SetDesc) ;
BEGIN
   WHILE from#NIL DO
      WITH from^ DO
         CASE type OF

         tokel:  AddSet(to, string) |
         litel:  AddSet(to, GetSymKey(Aliases, string)) |
         idel :  WarnError('not expecting ident in first symbol list') ;
                 WasNoError := FALSE

         ELSE
            Halt('unknown element in enumeration type', __LINE__, __FILE__)
         END
      END ;
      from := from^.next
   END
END OrSet ;


(*
   CalcFirstFactor -
*)

PROCEDURE CalcFirstFactor (f: FactorDesc; from: ProductionDesc; VAR to: SetDesc) ;
BEGIN
   WHILE f#NIL DO
      WITH f^ DO
         CASE type OF

         id  :  IF ident^.definition=NIL
                THEN
                   WarnError1("no rule found for an 'ident' called '%s'", ident^.name) ;
                   HALT
                END ;
                OrSet(to, ident^.definition^.first) ;
                IF GetReachEnd(ident^.definition^.followinfo)=false
                THEN
                   RETURN
                END |
         lit :  IF GetSymKey(Aliases, string)=NulKey
                THEN
                   WarnError1("unknown token for '%s'", string) ;
                   WasNoError := FALSE
                ELSE
                   AddSet(to, GetSymKey(Aliases, string))
                END ;
                RETURN |
         sub ,
         opt ,
         mult:  CalcFirstExpression(expr, from, to) |
         m2  :

         ELSE
         END
      END ;
      f := f^.next
   END
END CalcFirstFactor ;


(*
   CalcFirstTerm -
*)

PROCEDURE CalcFirstTerm (t: TermDesc; from: ProductionDesc; VAR to: SetDesc) ;
BEGIN
   WHILE t#NIL DO
      CalcFirstFactor(t^.factor, from, to) ;
      t := t^.next
   END
END CalcFirstTerm ;


(*
   CalcFirstExpression -
*)

PROCEDURE CalcFirstExpression (e: ExpressionDesc; from: ProductionDesc; VAR to: SetDesc) ;
BEGIN
   IF e#NIL
   THEN
      CalcFirstTerm(e^.term, from, to)
   END
END CalcFirstExpression ;


(*
   CalcFirstStatement -
*)

PROCEDURE CalcFirstStatement (s: StatementDesc; from: ProductionDesc; VAR to: SetDesc) ;
BEGIN
   IF s#NIL
   THEN
      CalcFirstExpression(s^.expr, from, to)
   END
END CalcFirstStatement ;


(*
   CalcFirstProduction - calculates all of the first symbols for the grammer
*)

PROCEDURE CalcFirstProduction (p: ProductionDesc; from: ProductionDesc; VAR to: SetDesc) ;
VAR
   s: SetDesc ;
BEGIN
   IF p#NIL
   THEN
      IF p^.firstsolved
      THEN
         s := p^.first ;
         WHILE s#NIL DO
            CASE s^.type OF

            idel :  CalcFirstProduction(s^.ident^.definition, from, to) |
            tokel,
            litel:  AddSet(to, s^.string)

            ELSE
            END ;
            s := s^.next
         END
      ELSE
         CalcFirstStatement(p^.statement, from, to)
      END
   END
END CalcFirstProduction ;


(*
   WorkOutFollow -
*)

PROCEDURE WorkOutFollowFactor (f: FactorDesc; VAR followset: SetDesc; after: SetDesc) ;
VAR
   foundepsilon,
   canreachend : TraverseResult ;
BEGIN
   foundepsilon := true ;
   canreachend  := true ;
   WHILE (f#NIL) AND (foundepsilon=true) DO
      WITH f^ DO
         CASE type OF

         id  :  IF ident^.definition=NIL
                THEN
                   WarnError1("no rule found for an 'ident' called '%s'", ident^.name) ;
                   HALT
                END ;
                OrSet(followset, ident^.definition^.first) |
         lit :  AddSet(followset, GetSymKey(Aliases, string)) |
         sub :  WorkOutFollowExpression(expr, followset, NIL) |
         opt :  WorkOutFollowExpression(expr, followset, NIL) |
         mult:  WorkOutFollowExpression(expr, followset, NIL) |
         m2  :

         ELSE
         END
      END ;
      IF GetEpsilon(f^.followinfo)=unknown
      THEN
         WarnError('internal error: epsilon unknown') ;
         PrettyCommentFactor(f, 3) ;
         WasNoError := FALSE
      END ;
      foundepsilon := GetEpsilon(f^.followinfo) ;
      canreachend := GetReachEnd(f^.followinfo) ;  (* only goes from FALSE -> TRUE *)
      f := f^.next
   END ;
   IF canreachend=true
   THEN
      OrSet(followset, after)
   END
END WorkOutFollowFactor ;


(*
   WorkOutFollowTerm -
*)

PROCEDURE WorkOutFollowTerm (t: TermDesc; VAR followset: SetDesc; after: SetDesc) ;
BEGIN
   IF t#NIL
   THEN
      WHILE t#NIL DO
         WITH t^ DO
            WorkOutFollowFactor(factor, followset, after) ;   (*  { '|' Term } *)
         END ;
         t := t^.next
      END
   END
END WorkOutFollowTerm ;


(*
   WorkOutFollowExpression -
*)

PROCEDURE WorkOutFollowExpression (e: ExpressionDesc; VAR followset: SetDesc; after: SetDesc) ;
BEGIN
   IF e#NIL
   THEN
      WITH e^ DO
         WorkOutFollowTerm(term, followset, after)
      END
   END
END WorkOutFollowExpression ;


(*
   CollectFollow - collects the follow set from, f, into, to.
*)

PROCEDURE CollectFollow (VAR to: SetDesc; f: FollowDesc) ;
BEGIN
   OrSet(to, f^.follow)
END CollectFollow ;


(*
   CalcFollowFactor -
*)

PROCEDURE CalcFollowFactor (f: FactorDesc; after: SetDesc) ;
BEGIN
   WHILE f#NIL DO
      WITH f^ DO
         CASE type OF

         id  :  WorkOutFollowFactor(next, followinfo^.follow, after) |
         lit :  WorkOutFollowFactor(next, followinfo^.follow, after) |
         opt ,
         sub :  CalcFirstFactor(next, NIL, followinfo^.follow) ;
                IF (next=NIL) OR (GetReachEnd(next^.followinfo)=true)
                THEN
                   OrSet(followinfo^.follow, after) ;
                   CalcFollowExpression(expr, followinfo^.follow)
                ELSE
                   CalcFollowExpression(expr, followinfo^.follow)
                END |
         mult:  CalcFirstFactor(f, NIL, followinfo^.follow) ;
                (* include first as we may repeat this sentance *)
                IF Debugging
                THEN
                   WriteLn ;
                   WriteString('found mult: and first is: ') ; EmitSet(followinfo^.follow, 0, 0) ; WriteLn
                END ;
                IF (next=NIL) OR (GetReachEnd(next^.followinfo)=true)
                THEN
                   OrSet(followinfo^.follow, after) ;
                   CalcFollowExpression(expr, followinfo^.follow)
                ELSE
                   CalcFollowExpression(expr, followinfo^.follow)
                END

         ELSE
         END
      END ;
      f := f^.next
   END
END CalcFollowFactor ;


(*
   CalcFollowTerm -
*)

PROCEDURE CalcFollowTerm (t: TermDesc; after: SetDesc) ;
BEGIN
   IF t#NIL
   THEN
      WHILE t#NIL DO
         WITH t^ DO
            CalcFollowFactor(factor, after) ;   (*  { '|' Term } *)
         END ;
         t := t^.next
      END
   END
END CalcFollowTerm ;


(*
   CalcFollowExpression -
*)

PROCEDURE CalcFollowExpression (e: ExpressionDesc; after: SetDesc) ;
BEGIN
   IF e#NIL
   THEN
      WITH e^ DO
         CalcFollowTerm(term, after)
      END
   END
END CalcFollowExpression ;


(*
   CalcFollowStatement - given a bnf statement generate the follow set.
*)

PROCEDURE CalcFollowStatement (s: StatementDesc) ;
BEGIN
   IF s#NIL
   THEN
      WITH s^ DO
         CalcFollowExpression(expr, NIL)
      END
   END
END CalcFollowStatement ;


(*
   CalcFollowProduction -
*)

PROCEDURE CalcFollowProduction (p: ProductionDesc) ;
BEGIN
   IF p#NIL
   THEN
      WITH p^ DO
         CalcFollowStatement(statement)
      END
   END
END CalcFollowProduction ;


(*
   CalcEpsilonFactor -
*)

PROCEDURE CalcEpsilonFactor (f: FactorDesc) ;
BEGIN
   WHILE f#NIL DO
      WITH f^ DO
         CASE type OF

         id  :  AssignEpsilon(GetEpsilon(ident^.definition^.followinfo)#unknown,
                              followinfo, GetEpsilon(ident^.definition^.followinfo)) |
         lit :  AssignEpsilon(TRUE, followinfo, false) |
         sub :  CalcEpsilonExpression(expr) ;
                AssignEpsilon(GetEpsilon(expr^.followinfo)#unknown,
                              followinfo, GetEpsilon(expr^.followinfo)) |
         m2  :  AssignEpsilon(TRUE, followinfo, true) |
         opt ,
         mult:  CalcEpsilonExpression(expr) ;
                AssignEpsilon(TRUE, followinfo, true)

         ELSE
         END
      END ;
      f := f^.next
   END
END CalcEpsilonFactor ;


(*
   CalcEpsilonTerm -
*)

PROCEDURE CalcEpsilonTerm (t: TermDesc) ;
BEGIN
   IF t#NIL
   THEN
      WHILE t#NIL DO
         WITH t^ DO
            IF factor#NIL
            THEN
               CASE GetReachEnd(factor^.followinfo) OF

               true :  AssignEpsilon(TRUE, followinfo, true) |
               false:  AssignEpsilon(TRUE, followinfo, false) |
               unknown:

               ELSE
               END
            END ;
            CalcEpsilonFactor(factor)    (*  { '|' Term } *)
         END ;
         t := t^.next
      END
   END
END CalcEpsilonTerm ;


(*
   CalcEpsilonExpression -
*)

PROCEDURE CalcEpsilonExpression (e: ExpressionDesc) ;
VAR
   t     : TermDesc ;
   result: TraverseResult ;
BEGIN
   IF e#NIL
   THEN
      CalcEpsilonTerm(e^.term) ;
      IF GetEpsilon(e^.followinfo)=unknown
      THEN
         result := unknown ;
         WITH e^ DO
            t := term ;
            WHILE t#NIL DO
               IF GetEpsilon(t^.followinfo)#unknown
               THEN
                  stop
               END ;
               CASE GetEpsilon(t^.followinfo) OF

               unknown: |
               true   : result := true |
               false  : IF result#true
                        THEN
                           result := false
                        END

               ELSE
               END ;
               t := t^.next
            END
         END ;
         AssignEpsilon(result#unknown, e^.followinfo, result)
      END
   END
END CalcEpsilonExpression ;


(*
   CalcEpsilonStatement - given a bnf statement generate the follow set.
*)

PROCEDURE CalcEpsilonStatement (s: StatementDesc) ;
BEGIN
   IF s#NIL
   THEN
      WITH s^ DO
         IF expr#NIL
         THEN
            AssignEpsilon(GetEpsilon(expr^.followinfo)#unknown,
                          followinfo, GetEpsilon(expr^.followinfo))
         END ;
         CalcEpsilonExpression(expr)
      END
   END
END CalcEpsilonStatement ;


(*
   CalcEpsilonProduction -
*)

PROCEDURE CalcEpsilonProduction (p: ProductionDesc) ;
BEGIN
   IF p#NIL
   THEN
(*
      IF p^.statement^.ident^.name=MakeKey('DefinitionModule')
      THEN
         stop
      END ;
*)

      IF Debugging
      THEN
         WriteKey(p^.statement^.ident^.name) ;
         WriteString('  calculating epsilon') ;
         WriteLn
      END ;

      WITH p^ DO
         AssignEpsilon(GetEpsilon(statement^.followinfo)#unknown,
                       followinfo, GetEpsilon(statement^.followinfo)) ;
         CalcEpsilonStatement(statement)
      END
   END
END CalcEpsilonProduction ;


(*
   CalcReachEndFactor -
*)

PROCEDURE CalcReachEndFactor (f: FactorDesc) : TraverseResult ;
VAR
   canreachend,
   result     : TraverseResult ;
BEGIN
   IF f=NIL
   THEN
      RETURN( true )   (* we have reached the end of this factor list *)
   ELSE
      WITH f^ DO
         (* we need to traverse all factors even if we can short cut the answer to this list of factors *)
         result := CalcReachEndFactor(next) ;
         CASE type OF

         id  :  IF ident^.definition=NIL
                THEN
                   WarnError1('definition for %s is absent (assuming epsilon is false for this production)', ident^.name) ;
                   result := false
                ELSIF result#false
                THEN
                   CASE GetReachEnd(ident^.definition^.followinfo) OF

                   false  :  result := false |
                   true   :  |
                   unknown:  result := unknown

                   ELSE
                   END
                END |
         lit :  result := false |
         sub :  CalcReachEndExpression(expr) ;
                IF (expr#NIL) AND (result=true)
                THEN
                   result := GetReachEnd(expr^.followinfo)
                END |
         mult,
         opt :  IF expr#NIL
                THEN
                   (* not interested in the result as expression is optional *)
                   CalcReachEndExpression(expr)
                END |
         m2  :

         ELSE
         END ;
         AssignReachEnd(result#unknown, followinfo, result)
      END ;
      RETURN( result )
   END
END CalcReachEndFactor ;


(*
   CalcReachEndTerm -
*)

PROCEDURE CalcReachEndTerm (t: TermDesc) : TraverseResult ;
VAR
   canreachend,
   result     : TraverseResult ;
BEGIN
   IF t#NIL
   THEN
      canreachend := false ;
      WHILE t#NIL DO
         WITH t^ DO
            result := CalcReachEndFactor(factor) ;
            AssignReachEnd(result#unknown, followinfo, result) ;
            CASE result OF

            true   :  canreachend := true |
            false  :  |
            unknown:  IF canreachend=false
                      THEN
                         canreachend := unknown
                      END

            ELSE
            END
         END ;
         t := t^.next    (*  { '|' Term } *)
      END ;
      RETURN( canreachend )
   END
END CalcReachEndTerm ;


(*
   CalcReachEndExpression -
*)

PROCEDURE CalcReachEndExpression (e: ExpressionDesc) ;
VAR
   result: TraverseResult ;
BEGIN
   IF e=NIL
   THEN
      (* no expression, thus reached the end of this sentance *)
   ELSE
      WITH e^ DO
         result := CalcReachEndTerm(term) ;
         AssignReachEnd(result#unknown, followinfo, result)
      END
   END
END CalcReachEndExpression ;


(*
   CalcReachEndStatement -
*)

PROCEDURE CalcReachEndStatement (s: StatementDesc) ;
BEGIN
   IF s#NIL
   THEN
      WITH s^ DO
         IF expr#NIL
         THEN
            CalcReachEndExpression(expr) ;
            AssignReachEnd(GetReachEnd(expr^.followinfo)#unknown,
                           followinfo, GetReachEnd(expr^.followinfo))
         END
      END
   END
END CalcReachEndStatement ;


PROCEDURE stop ; BEGIN END stop ;

(*
   CalcReachEndProduction -
*)

PROCEDURE CalcReachEndProduction (p: ProductionDesc) ;
BEGIN
   IF p#NIL
   THEN
      WITH p^ DO
         CalcReachEndStatement(statement) ;
         IF GetReachEnd(followinfo)#unknown
         THEN
            IF Debugging
            THEN
               WriteString('already calculated reach end for: ') ;
               WriteKey(p^.statement^.ident^.name) ; WriteString(' its value is ') ;
               IF GetReachEnd(followinfo)=true
               THEN
                  WriteString('reachable')
               ELSE
                  WriteString('non reachable')
               END ;
               WriteLn
            END
         END ;
         AssignReachEnd(GetReachEnd(statement^.followinfo)#unknown, followinfo, GetReachEnd(statement^.followinfo)) ;
      END
   END
END CalcReachEndProduction ;


(*
   EmptyFactor -
*)

PROCEDURE EmptyFactor (f: FactorDesc) : BOOLEAN ;
BEGIN
   WHILE f#NIL DO
      WITH f^ DO
         CASE type OF

         id  :  IF NOT EmptyProduction(ident^.definition)
                THEN
                   RETURN( FALSE )
                END |
         lit :  RETURN( FALSE ) |
         sub :  IF NOT EmptyExpression(expr)
                THEN
                   RETURN( FALSE )
                END |
         opt ,
         mult:  RETURN( TRUE ) |
         m2  :

         ELSE
         END
      END ;
      f := f^.next
   END ;
   RETURN( TRUE )
END EmptyFactor ;


(*
   EmptyTerm - returns TRUE if the term maybe empty.
*)

PROCEDURE EmptyTerm (t: TermDesc) : BOOLEAN ;
BEGIN
   WHILE t#NIL DO
      IF EmptyFactor(t^.factor)
      THEN
         RETURN( TRUE )
      ELSE
         t := t^.next
      END
   END ;
   RETURN( FALSE )
END EmptyTerm ;


(*
   EmptyExpression -
*)

PROCEDURE EmptyExpression (e: ExpressionDesc) : BOOLEAN ;
BEGIN
   IF e=NIL
   THEN
      RETURN( TRUE )
   ELSE
      RETURN( EmptyTerm(e^.term) )
   END
END EmptyExpression ;


(*
   EmptyStatement - returns TRUE if statement, s, is empty.
*)

PROCEDURE EmptyStatement (s: StatementDesc) : BOOLEAN ;
BEGIN
   IF s=NIL
   THEN
      RETURN( TRUE )
   ELSE
      RETURN( EmptyExpression(s^.expr) )
   END
END EmptyStatement ;


(*
   EmptyProduction - returns if production, p, maybe empty.
*)

PROCEDURE EmptyProduction (p: ProductionDesc) : BOOLEAN ;
BEGIN
   IF p=NIL
   THEN
      WarnError('unknown production') ;
      RETURN( TRUE )
   ELSIF (p^.firstsolved) AND (p^.first#NIL)
   THEN
      (* predefined but first set to something - thus not empty *)
      RETURN( FALSE )
   ELSE
      RETURN( EmptyStatement(p^.statement) )
   END
END EmptyProduction ;


(*
   EmitFDLNotice -
*)

PROCEDURE EmitFDLNotice ;
BEGIN
   WriteString('@c Copyright (C) 2000-2019 Free Software Foundation, Inc.') ; WriteLn ;
   WriteLn ;
   WriteString('@c This file is part of GCC.') ; WriteLn ;
   WriteString('@c Permission is granted to copy, distribute and/or modify this document') ; WriteLn ;
   WriteString('@c under the terms of the GNU Free Documentation License, Version 1.2 or') ; WriteLn ;
   WriteString('@c any later version published by the Free Software Foundation.') ; WriteLn
END EmitFDLNotice ;


(*
   EmitRules - generates the BNF rules.
*)

PROCEDURE EmitRules ;
BEGIN
   IF Texinfo AND FreeDocLicense
   THEN
      EmitFDLNotice
   END ;
   ForeachRuleDo(EmitRule)
END EmitRules ;


(*
   DescribeElement -
*)

PROCEDURE DescribeElement (name: WORD) ;
VAR
   lit: Name ;
BEGIN
   IF InitialElement
   THEN
      InitialElement := FALSE
   ELSE
      WriteString(' |')
   END ;
   WriteLn ;
   Indent := 3 ;
   IndentString('') ;
   WriteKey(name) ;
   WriteString(': ') ;
   lit := GetSymKey(ReverseAliases, name) ;
   IF MakeKey('"')=lit
   THEN
      WriteString('str := ConCat(ConCatChar(ConCatChar(InitString("syntax error, found ') ;
      Write("'") ; WriteString('"), ') ;
      Write("'") ; Write('"') ; Write("'") ; WriteString("), ") ;
      Write('"') ; Write("'") ; Write('"') ; WriteString("), Mark(str))")
   ELSIF MakeKey("'")=lit
   THEN
      WriteString("str := ConCat(ConCatChar(ConCatChar(InitString('syntax error, found ") ;
      Write('"') ; WriteString("'), ") ;
      Write('"') ; Write("'") ; Write('"') ; WriteString('), ') ;
      Write("'") ; Write('"') ; Write("'") ; WriteString('), Mark(str))')
   ELSE
      WriteString("str := ConCat(InitString(") ; Write('"') ;
      WriteString("syntax error, found ") ; KeyWord(lit) ; WriteString('"), Mark(str))')
   END
END DescribeElement ;


(*
   EmitInTestStop - construct a test for stop element, name.
*)

PROCEDURE EmitInTestStop (name: Name) ;
VAR
   i, value: CARDINAL ;
BEGIN
   IF LargestValue<=MaxElementsInSet
   THEN
      WriteKey(name) ; WriteString(' IN stopset')
   ELSE
      value := GetSymKey(Values, name) ;
      i := value DIV MaxElementsInSet ;
      WriteKey(name) ; WriteString(' IN stopset') ; WriteCard(i, 0)
   END
END EmitInTestStop ;


(*
   DescribeStopElement -
*)

PROCEDURE DescribeStopElement (name: WORD) ;
VAR
   lit: Name ;
BEGIN
   Indent := 3 ;
   IndentString('IF ') ; EmitInTestStop(name) ; WriteLn ;
   IndentString('THEN') ; WriteLn ;
   Indent := 6 ;
   lit := GetSymKey(ReverseAliases, name) ;
   IF (lit=NulName) OR (lit=MakeKey(''))
   THEN
      IndentString('(* ') ;
      WriteKey(name) ;
      WriteString(' has no token name (needed to generate error messages) *)')
   ELSIF MakeKey("'")=lit
   THEN
      IndentString('message := ConCatChar(ConCatChar(ConCatChar(ConCatChar(ConCatChar(message, ') ;
      WriteString("' '), ") ;
      Write("'") ; Write('"') ; WriteString("'), ") ;
      Write('"') ; Write("'") ; WriteString('"), ') ;
      Write("'") ; Write('"') ; WriteString("'), ',') ; INC(n) ; ")
   ELSIF MakeKey('"')=lit
   THEN
      IndentString("message := ConCatChar(ConCatChar(ConCatChar(ConCatChar(ConCatChar(message, ") ;
      WriteString('" "), ') ;
      Write('"') ; Write("`") ; WriteString('"), ') ;
      Write("'") ; Write('"') ; WriteString("'), ") ;
      Write('"') ; Write("'") ; WriteString('"), ",") ; INC(n) ; ')
   ELSE
      IndentString("message := ConCat(ConCatChar(message, ' ") ; WriteString("'), ") ;
      WriteString('Mark(InitString("') ; KeyWord(lit) ; Write('"') ;
      WriteString('))) ; INC(n)')
   END ;
   WriteLn ;
   Indent := 3 ;
   IndentString('END ;') ; WriteLn
END DescribeStopElement ;


(*
   EmitDescribeStop -
*)

PROCEDURE EmitDescribeStop ;
BEGIN
   WriteLn ;
   Indent := 0 ;
   IndentString('(*') ;
   Indent := 3 ;
   WriteLn ;
   IndentString('DescribeStop - issues a message explaining what tokens were expected') ;
   WriteLn ;
   WriteString('*)') ;
   WriteLn ;
   WriteLn ;
   Indent := 0 ;
   IndentString('PROCEDURE DescribeStop (') ; EmitStopParameters(TRUE) ; WriteString(') : String ;') ;
   WriteLn ;
   IndentString('VAR') ; WriteLn ;
   Indent := 3 ;
   IndentString('n      : CARDINAL ;') ; WriteLn ;
   IndentString('str,') ; WriteLn ;
   IndentString('message: String ;') ; WriteLn ;
   Indent := 0 ;
   IndentString('BEGIN') ; WriteLn ;
   Indent := 3 ;
   IndentString('n := 0 ;') ; WriteLn ;
   IndentString("message := InitString('') ;") ;
   WriteLn ;
   ForeachNodeDo(Aliases, DescribeStopElement) ; WriteLn ;
   Indent := 3 ;
   IndentString('IF n=0') ; WriteLn ;
   IndentString('THEN') ; WriteLn ;
   Indent := 6 ;
   IndentString("str := InitString(' syntax error') ; ") ; WriteLn ;
   IndentString('message := KillString(message) ; ') ; WriteLn ;
   Indent := 3 ;
   IndentString('ELSIF n=1') ; WriteLn ;
   IndentString('THEN') ; WriteLn ;
   Indent := 6 ;
   IndentString("str := ConCat(message, Mark(InitString(' missing '))) ;") ; WriteLn ;
   Indent := 3 ;
   IndentString('ELSE') ; WriteLn ;
   Indent := 6 ;
   IndentString("str := ConCat(InitString(' expecting one of'), message) ;") ; WriteLn ;
   IndentString("message := KillString(message) ;") ; WriteLn ;
   Indent := 3 ;
   IndentString('END ;') ; WriteLn ;
   IndentString('RETURN( str )') ; WriteLn ;
   Indent := 0 ;
   IndentString('END DescribeStop ;') ; WriteLn ;
   WriteLn
END EmitDescribeStop ;


(*
   EmitDescribeError -
*)

PROCEDURE EmitDescribeError ;
BEGIN
   WriteLn ;
   Indent := 0 ;
   IndentString('(*') ; WriteLn ;
   Indent := 3 ;
   IndentString('DescribeError - issues a message explaining what tokens were expected') ; WriteLn ;
   Indent := 0 ;
   IndentString('*)') ;
   WriteLn ;
   WriteLn ;
   IndentString('PROCEDURE DescribeError (') ; EmitStopParameters(TRUE) ; WriteString(') ;') ;
   WriteLn ;
   IndentString('VAR') ; WriteLn ;
   Indent := 3 ;
   IndentString('str: String ;') ; WriteLn ;
   Indent := 0 ;
   IndentString('BEGIN') ; WriteLn ;
   Indent := 3 ;
   IndentString("str := InitString('') ;") ; WriteLn ;
   (* was
   IndentString('str := DescribeStop(') ; EmitStopParameters(FALSE) ; WriteString(') ;') ; WriteLn ;
   *)
   IndentString('CASE ') ; WriteGetTokenType ; WriteString(' OF') ; NewLine(3) ;
   InitialElement := TRUE ;
   ForeachNodeDo(Aliases, DescribeElement) ;
   WriteLn ;
   Indent := 3 ;
   IndentString('ELSE') ; WriteLn ;
   IndentString('END ;') ; WriteLn ;
   IndentString('') ;
   WriteKey(ErrorProcString) ; WriteString('(str) ;') ; WriteLn ;
   Indent := 0 ;
   IndentString('END DescribeError ;') ; WriteLn
END EmitDescribeError ;


(*
   EmitSetTypes - write out the set types used during error recovery
*)

PROCEDURE EmitSetTypes ;
VAR
   i, j, m, n: CARDINAL ;
BEGIN
   WriteString('(*') ; NewLine(3) ;
   WriteString('expecting token set defined as an enumerated type') ; NewLine(3) ;
   WriteString('(') ;
   i := 0 ;
   WHILE i<LargestValue DO
      WriteKey(GetSymKey(ReverseValues, WORD(i))) ;
      INC(i) ;
      IF i<LargestValue
      THEN
         WriteString(', ')
      END
   END ;
   WriteString(') ;') ; NewLine(0) ;
   WriteString('*)') ; NewLine(0) ;
   WriteString('TYPE') ; NewLine(3) ;
   IF LargestValue>MaxElementsInSet
   THEN
      i := 0 ;
      n := LargestValue DIV MaxElementsInSet ;
      WHILE i<=n DO
         j := (i*MaxElementsInSet) ;
         IF LargestValue<(i+1)*MaxElementsInSet-1
         THEN
            m := LargestValue-1
         ELSE
            m := (i+1)*MaxElementsInSet-1
         END ;
         WriteString('stop') ; WriteCard(i, 0) ;
         WriteString(' = [') ;
         WriteKey(GetSymKey(ReverseValues, WORD(j))) ;
         WriteString('..') ;
         WriteKey(GetSymKey(ReverseValues, WORD(m))) ;
         WriteString('] ;') ;
         NewLine(3) ;
         WriteString('SetOfStop') ; WriteCard(i, 0) ;
         WriteString(' = SET OF stop') ; WriteCard(i, 0) ;
         WriteString(' ;') ;
         NewLine(3) ;
         INC(i)
      END
   ELSE
      WriteString('SetOfStop') ;
      WriteString(' = SET OF [') ;
      WriteKey(GetSymKey(ReverseValues, WORD(0))) ;
      WriteString('..') ;
      WriteKey(GetSymKey(ReverseValues, WORD(LargestValue-1))) ;
      WriteString('] ;')
   END ;
   NewLine(0)
END EmitSetTypes ;


(*
   EmitSupport - generates the support routines.
*)

PROCEDURE EmitSupport ;
BEGIN
   IF ErrorRecovery
   THEN
      EmitSetTypes ;
      EmitDescribeStop ;
      EmitDescribeError
   END
END EmitSupport ;


(*
   DisposeSetDesc - dispose of the set list, s.
*)

PROCEDURE DisposeSetDesc (VAR s: SetDesc) ;
VAR
   h, n: SetDesc ;
BEGIN
   IF s#NIL
   THEN
      h := s ;
      n := s^.next ;
      REPEAT
         DISPOSE(h) ;
         h := n ;
         IF n#NIL
         THEN
            n := n^.next
         END
      UNTIL h=NIL ;
      s := NIL
   END
END DisposeSetDesc ;


(*
   OptionalFactor -
*)

PROCEDURE OptionalFactor (f: FactorDesc) : BOOLEAN ;
BEGIN
   WHILE f#NIL DO
      WITH f^ DO
         CASE type OF

         id  :  |
         lit :  |
         sub ,
         opt ,
         mult:  IF OptionalExpression(expr)
                THEN
                   RETURN( TRUE )
                END |
         m2  :

         ELSE
         END
      END ;
      f := f^.next
   END ;
   RETURN( FALSE )
END OptionalFactor ;


(*
   OptionalTerm - returns TRUE if the term maybe empty.
*)

PROCEDURE OptionalTerm (t: TermDesc) : BOOLEAN ;
VAR
   u, v    : TermDesc ;
   tov, tou: SetDesc ;
BEGIN
   u := t ;
   WHILE u#NIL DO
      IF OptionalFactor(u^.factor)
      THEN
         RETURN( TRUE )
      END ;
      v := t ;
      tou := NIL ;
      CalcFirstFactor(u^.factor, NIL, tou) ;
      WHILE v#NIL DO
         IF v#u
         THEN
            tov := NIL ;
            CalcFirstFactor(v^.factor, NIL, tov) ;
            IF IntersectionIsNil(tov, tou)
            THEN
               DisposeSetDesc(tov) ;
            ELSE
               WriteString('problem with two first sets. Set 1: ') ;
               EmitSet(tou, 0, 0) ; WriteLn ;
               WriteString('                             Set 2: ') ;
               EmitSet(tov, 0, 0) ; WriteLn ;
               DisposeSetDesc(tou) ;
               DisposeSetDesc(tov) ;
               RETURN( TRUE )
            END
         END ;
         v := v^.next
      END ;
      DisposeSetDesc(tou) ;
      u := u^.next
   END ;
   RETURN( FALSE )
END OptionalTerm ;


(*
   OptionalExpression -
*)

PROCEDURE OptionalExpression (e: ExpressionDesc) : BOOLEAN ;
BEGIN
   IF e=NIL
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( OptionalTerm(e^.term) )
   END
END OptionalExpression ;


(*
   OptionalStatement - returns FALSE if statement, s, does not have a optional ambiguity.
*)

PROCEDURE OptionalStatement (s: StatementDesc) : BOOLEAN ;
BEGIN
   IF s=NIL
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( OptionalExpression(s^.expr) )
   END
END OptionalStatement ;


(*
   OptionalProduction -
*)

PROCEDURE OptionalProduction (p: ProductionDesc) : BOOLEAN ;
BEGIN
   IF p=NIL
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( OptionalStatement(p^.statement) )
   END
END OptionalProduction ;


(*
   CheckFirstFollow -
*)

PROCEDURE CheckFirstFollow (f: FactorDesc; after: FactorDesc) : BOOLEAN ;
VAR
   first, follow: SetDesc ;
BEGIN
   first := NIL ;
   CalcFirstFactor(f, NIL, first) ;
   follow := NIL ;
   follow := GetFollow(f^.followinfo) ;
   IF IntersectionIsNil(first, follow)
   THEN
      DisposeSetDesc(first) ;
      DisposeSetDesc(follow) ;
      RETURN( FALSE )
   ELSE
      PrettyCommentFactor(f, 3) ;
      NewLine(3) ;
      WriteString('first: ') ;
      EmitSet(first, 0, 0) ;
      NewLine(3) ;
      WriteString('follow: ') ;
      EmitSet(follow, 0, 0) ;
      NewLine(3) ;
      DisposeSetDesc(first) ;
      DisposeSetDesc(follow) ;
      RETURN( TRUE )
   END
END CheckFirstFollow ;


(*
   ConstrainedEmptyFactor -
*)

PROCEDURE ConstrainedEmptyFactor (f: FactorDesc) : BOOLEAN ;
BEGIN
   WHILE f#NIL DO
      WITH f^ DO
         CASE type OF

         id  :  |
         lit :  |
         sub ,
         opt ,
         mult:  IF ConstrainedEmptyExpression(expr)
                THEN
                   RETURN( TRUE )
                END |
         m2  :

         ELSE
         END
      END ;
      IF (f^.type#m2) AND EmptyFactor(f) AND CheckFirstFollow(f, f^.next)
      THEN
         RETURN( TRUE )
      END ;
      f := f^.next
   END ;
   RETURN( FALSE )
END ConstrainedEmptyFactor ;


(*
   ConstrainedEmptyTerm - returns TRUE if the term maybe empty.
*)

PROCEDURE ConstrainedEmptyTerm (t: TermDesc) : BOOLEAN ;
VAR
   first, follow: SetDesc ;
BEGIN
   WHILE t#NIL DO
      IF ConstrainedEmptyFactor(t^.factor)
      THEN
         RETURN( TRUE )
      ELSIF (t^.factor^.type#m2) AND EmptyFactor(t^.factor) AND CheckFirstFollow(t^.factor, t^.factor^.next)
      THEN
         RETURN( TRUE )
      END ;
      t := t^.next
   END ;
   RETURN( FALSE )
END ConstrainedEmptyTerm ;


(*
   ConstrainedEmptyExpression -
*)

PROCEDURE ConstrainedEmptyExpression (e: ExpressionDesc) : BOOLEAN ;
BEGIN
   IF e=NIL
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( ConstrainedEmptyTerm(e^.term) )
   END
END ConstrainedEmptyExpression ;


(*
   ConstrainedEmptyStatement - returns FALSE if statement, s, does not have a optional ambiguity.
*)

PROCEDURE ConstrainedEmptyStatement (s: StatementDesc) : BOOLEAN ;
BEGIN
   IF s=NIL
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( ConstrainedEmptyExpression(s^.expr) )
   END
END ConstrainedEmptyStatement ;


(*
   ConstrainedEmptyProduction - returns TRUE if a problem exists with, p.
*)

PROCEDURE ConstrainedEmptyProduction (p: ProductionDesc) : BOOLEAN ;
BEGIN
   IF p=NIL
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( ConstrainedEmptyStatement(p^.statement) )
   END
END ConstrainedEmptyProduction ;


(*
   TestForLALR1 -
*)

PROCEDURE TestForLALR1 (p: ProductionDesc) ;
BEGIN
   IF OptionalProduction(p)
   THEN
      WarnError1('production %s has two optional sentances using | which both have the same start symbols',
                 p^.statement^.ident^.name) ;
      WasNoError := FALSE ;
      PrettyCommentProduction(p)
   END ;
(*
   IF ConstrainedEmptyProduction(p)
   THEN
      WarnError1('production %s has an empty sentance and the first and follow symbols intersect',
                 p^.statement^.ident^.name) ;
      WasNoError := FALSE
   END
*)
END TestForLALR1 ;


(*
   DoEpsilon - runs the epsilon interrelated rules
*)

PROCEDURE DoEpsilon (p: ProductionDesc) ;
BEGIN
   CalcEpsilonProduction(p) ;
   CalcReachEndProduction(p)
END DoEpsilon ;


(*
   CheckComplete - checks that production, p, is complete.
*)

PROCEDURE CheckComplete (p: ProductionDesc) ;
BEGIN
   IF GetReachEnd(p^.followinfo)=unknown
   THEN
      PrettyCommentProduction(p) ;
      WarnError1('cannot determine epsilon, probably a left recursive rule in %s and associated rules (hint rewrite using ebnf and eliminate left recursion)',
                 p^.statement^.ident^.name) ;
      WasNoError := FALSE
   END
END CheckComplete ;


(*
   PostProcessRules - backpatch the ident to rule definitions and emit comments and code.
*)

PROCEDURE PostProcessRules ;
BEGIN
   ForeachRuleDo(BackPatchIdentToDefinitions) ;
   IF NOT WasNoError
   THEN
      HALT
   END ;
   WhileNotCompleteDo(DoEpsilon) ;
   IF NOT WasNoError
   THEN
      HALT
   END ;
   ForeachRuleDo(CheckComplete) ;
   IF NOT WasNoError
   THEN
      HALT
   END ;
   WhileNotCompleteDo(CalculateFirstAndFollow) ;
   IF NOT WasNoError
   THEN
      HALT
   END ;
   ForeachRuleDo(TestForLALR1) ;
   IF NOT WasNoError
   THEN
      ForeachRuleDo(PrettyCommentProduction)
   END
END PostProcessRules ;


(*
   DisplayHelp - display a summary help and then exit (0).
*)

PROCEDURE DisplayHelp ;
BEGIN
   WriteString('Usage: ppg [-l] [-c] [-d] [-e] [-k] [-t] [-k] [-p] [-t] [-f] filename') ; WriteLn ;
   WriteString('   -l             suppress file and line source information') ; WriteLn ;
   WriteString('   -c             do not generate any Modula-2 code within the parser rules') ; WriteLn ;
   WriteString('   -h or --help   generate this help message') ; WriteLn ;
   WriteString('   -e             do not generate a parser with error recovery') ; WriteLn ;
   WriteString('   -k             generate keyword errors with GCC formatting directives') ; WriteLn ;
   WriteString('   -d             generate internal debugging information') ; WriteLn ;
   WriteString('   -p             only display the ebnf rules') ; WriteLn ;
   WriteString('   -t             generate texinfo formating for pretty printing (-p)') ; WriteLn ;
   WriteString('   -f             generate GNU Free Documentation header before pretty printing in texinfo') ; WriteLn ;
   exit (0)
END DisplayHelp ;



(*
   ParseArgs -
*)

PROCEDURE ParseArgs ;
VAR
   n, i: CARDINAL ;
BEGIN
   ErrorRecovery     := TRUE ;  (* DefaultRecovery ; *)
   Debugging         := FALSE ;
   PrettyPrint       := FALSE ;
   KeywordFormatting := FALSE ;
   i := 1 ;
   n := Narg() ;
   WHILE i<n DO
      IF GetArg(FileName, i)
      THEN
         IF StrEqual(FileName, '-e')
         THEN
            ErrorRecovery := FALSE
         ELSIF StrEqual(FileName, '-d')
         THEN
            Debugging := TRUE ;
            SetDebugging(TRUE)
         ELSIF StrEqual(FileName, '-c')
         THEN
            EmitCode := FALSE
         ELSIF StrEqual(FileName, '-k')
         THEN
            KeywordFormatting := TRUE
         ELSIF StrEqual(FileName, '-l')
         THEN
            SuppressFileLineTag := TRUE
         ELSIF StrEqual(FileName, '-h') OR StrEqual(FileName, '--help')
         THEN
            DisplayHelp
         ELSIF StrEqual(FileName, '-p')
         THEN
            PrettyPrint := TRUE
         ELSIF StrEqual(FileName, '-t')
         THEN
            Texinfo := TRUE
         ELSIF StrEqual(FileName, '-f')
         THEN
            FreeDocLicense := TRUE
         ELSIF OpenSource(FileName)
         THEN
            AdvanceToken
         ELSE
            WriteString('cannot open ') ; WriteString(FileName) ; WriteString(' for reading') ; WriteLn
         END
      END ;
      INC (i)
   END ;
   IF n=1
   THEN
      DisplayHelp
   END
END ParseArgs ;


(*
   Init - initialize the modules data structures
*)

PROCEDURE Init ;
BEGIN
   WasNoError        := TRUE ;
   Texinfo           := FALSE ;
   FreeDocLicense    := FALSE ;
   EmitCode          := TRUE ;
   LargestValue      := 0 ;
   HeadProduction    := NIL ;
   CurrentProduction := NIL ;
   InitTree(Aliases) ;
   InitTree(ReverseAliases) ;
   InitTree(Values) ;
   InitTree(ReverseValues) ;
   LastLineNo        := 0 ;
   CodePrologue      := NIL ;
   CodeEpilogue      := NIL ;
   CodeDeclaration   := NIL ;
   ErrorProcArray    := MakeKey('Error') ;
   ErrorProcString   := MakeKey('ErrorS') ;
   TokenTypeProc     := MakeKey('GetCurrentTokenType()') ;
   SymIsProc         := MakeKey('SymIs') ;
   OnLineStart       := TRUE ;
   ParseArgs ;
   WasNoError := Main() ;             (* this line will be manipulated by sed in buildpg *)
   IF WasNoError
   THEN
      PostProcessRules ;
      IF WasNoError
      THEN
         IF Debugging
         THEN
            EmitRules
         ELSIF PrettyPrint
         THEN
            EmitRules
         ELSE
            WriteString('(* it is advisable not to edit this file as it was automatically generated from the grammer file ') ;
            WriteString(FileName) ; WriteString(' *)') ; WriteLn ;
            OnLineStart := FALSE ;
            EmitFileLineTag(LinePrologue) ;
            BeginningOfLine := TRUE ;
            WriteCodeHunkList(CodePrologue) ;
            EmitSupport ;
            EmitFileLineTag(LineDeclaration) ;
            WriteCodeHunkList(CodeDeclaration) ;
            EmitRules ;
            (* code rules *)
            EmitFileLineTag(LineEpilogue) ;
            WriteCodeHunkList(CodeEpilogue)
         END
      END
   END
END Init ;


BEGIN
   Init
END ppg.
(*
 * Local variables:
 *  compile-command: "gm2 -I../gm2-libs:. -fbounds -freturn -c -g ppg.mod"
 * End:
 *)
