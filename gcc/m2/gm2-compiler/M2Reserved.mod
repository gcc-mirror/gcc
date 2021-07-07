(* M2Reserved.mod determines is a token is a reserved word.

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

IMPLEMENTATION MODULE M2Reserved ;


FROM SymbolKey IMPORT SymbolTree, InitTree, PutSymKey, GetSymKey ;
FROM NameKey IMPORT MakeKey, NulName ;
FROM ASCII IMPORT nul ;

CONST
   eof = 032C ;

VAR
   NameTotok,
   tokToName: SymbolTree ;


(*
   AddKeyword - adds the Name and enumerated value of a keyword
                into the binary tree.
*)

PROCEDURE AddKeyword (n: Name; tok: toktype) ;
BEGIN
   PutSymKey(NameTotok, n, tok) ;
   PutSymKey(tokToName, VAL(Name, tok), n)
END AddKeyword ;


PROCEDURE Init ;
VAR
   a: ARRAY [0..1] OF CHAR ;
BEGIN
   InitTree(NameTotok) ;
   InitTree(tokToName) ;

   NulTok := NulName ;

   PlusTok := MakeKey('+') ;
   AddKeyword(PlusTok, plustok) ;

   MinusTok := MakeKey('-') ;
   AddKeyword(MinusTok, minustok) ;

   TimesTok := MakeKey('*') ;
   AddKeyword(TimesTok, timestok) ;

   DivideTok := MakeKey('/') ;
   AddKeyword(DivideTok, dividetok) ;

   BecomesTok := MakeKey(':=') ;
   AddKeyword(BecomesTok, becomestok) ;

   AmbersandTok := MakeKey('&') ;
   AddKeyword(AmbersandTok, ambersandtok) ;

   PeriodTok := MakeKey('.') ;
   AddKeyword(PeriodTok, periodtok) ;

   CommaTok := MakeKey(',') ;
   AddKeyword(CommaTok, commatok) ;

   SemiColonTok := MakeKey(';') ;
   AddKeyword(SemiColonTok, semicolontok) ;

   LParaTok := MakeKey('(') ;
   AddKeyword(LParaTok, lparatok) ;

   LSBraTok := MakeKey('[') ;
   AddKeyword(LSBraTok, lsbratok) ;

   LCBraTok := MakeKey('{') ;
   AddKeyword(LCBraTok, lcbratok) ;

   UpArrowTok := MakeKey('^') ;
   AddKeyword(UpArrowTok, uparrowtok) ;

   SingleQuoteTok := MakeKey("'") ;
   AddKeyword(SingleQuoteTok, singlequotetok) ;

   EqualTok := MakeKey('=') ;
   AddKeyword(EqualTok, equaltok) ;

   HashTok := MakeKey('#') ;
   AddKeyword(HashTok, hashtok) ;

   LessTok := MakeKey('<') ;
   AddKeyword(LessTok, lesstok) ;

   GreaterTok := MakeKey('>') ;
   AddKeyword(GreaterTok, greatertok) ;

   LessGreaterTok := MakeKey('<>') ;
   AddKeyword(LessGreaterTok, lessgreatertok) ;

   LessEqualTok := MakeKey('<=') ;
   AddKeyword(LessEqualTok, lessequaltok) ;

   GreaterEqualTok := MakeKey('>=') ;
   AddKeyword(GreaterEqualTok, greaterequaltok) ;

   LDirectiveTok := MakeKey('<*') ;
   AddKeyword(LDirectiveTok, ldirectivetok) ;

   RDirectiveTok := MakeKey('*>') ;
   AddKeyword(RDirectiveTok, rdirectivetok) ;

   PeriodPeriodTok := MakeKey('..') ;
   AddKeyword(PeriodPeriodTok, periodperiodtok) ;

   ColonTok := MakeKey(':') ;
   AddKeyword(ColonTok, colontok) ;

   RParaTok := MakeKey(')') ;
   AddKeyword(RParaTok, rparatok) ;

   RSBraTok := MakeKey(']') ;
   AddKeyword(RSBraTok, rsbratok) ;

   RCBraTok := MakeKey('}') ;
   AddKeyword(RCBraTok, rcbratok) ;

   BarTok := MakeKey('|') ;
   AddKeyword(BarTok, bartok) ;

   DoubleQuotesTok := MakeKey('"') ;
   AddKeyword(DoubleQuotesTok, doublequotestok) ;


   AndTok := MakeKey('AND') ;
   AddKeyword(AndTok, andtok) ;

   ArrayTok := MakeKey('ARRAY') ;
   AddKeyword(ArrayTok, arraytok) ;

   BeginTok := MakeKey('BEGIN') ;
   AddKeyword(BeginTok, begintok) ;

   ByTok := MakeKey('BY') ;
   AddKeyword(ByTok, bytok) ;

   CaseTok := MakeKey('CASE') ;
   AddKeyword(CaseTok, casetok) ;

   ConstTok := MakeKey('CONST') ;
   AddKeyword(ConstTok, consttok) ;

   DefinitionTok := MakeKey('DEFINITION') ;
   AddKeyword(DefinitionTok, definitiontok) ;

   DivTok := MakeKey('DIV') ;
   AddKeyword(DivTok, divtok) ;

   DoTok := MakeKey('DO') ;
   AddKeyword(DoTok, dotok) ;

   ElseTok := MakeKey('ELSE') ;
   AddKeyword(ElseTok, elsetok) ;

   ElsifTok := MakeKey('ELSIF') ;
   AddKeyword(ElsifTok, elsiftok) ;

   EndTok := MakeKey('END') ;
   AddKeyword(EndTok, endtok) ;

   ExitTok := MakeKey('EXIT') ;
   AddKeyword(ExitTok, exittok) ;

   ExceptTok := MakeKey('EXCEPT') ;
   AddKeyword(ExceptTok, excepttok) ;

   ExportTok := MakeKey('EXPORT') ;
   AddKeyword(ExportTok, exporttok) ;

   FinallyTok := MakeKey('FINALLY') ;
   AddKeyword(FinallyTok, finallytok) ;

   ForTok := MakeKey('FOR') ;
   AddKeyword(ForTok, fortok) ;

   FromTok := MakeKey('FROM') ;
   AddKeyword(FromTok, fromtok) ;

   IfTok := MakeKey('IF') ;
   AddKeyword(IfTok, iftok) ;

   ImplementationTok := MakeKey('IMPLEMENTATION') ;
   AddKeyword(ImplementationTok, implementationtok) ;

   ImportTok := MakeKey('IMPORT') ;
   AddKeyword(ImportTok, importtok) ;

   InTok := MakeKey('IN') ;
   AddKeyword(InTok, intok) ;

   LoopTok := MakeKey('LOOP') ;
   AddKeyword(LoopTok, looptok) ;

   ModTok := MakeKey('MOD') ;
   AddKeyword(ModTok, modtok) ;

   ModuleTok := MakeKey('MODULE') ;
   AddKeyword(ModuleTok, moduletok) ;

   NotTok := MakeKey('NOT') ;
   AddKeyword(NotTok, nottok) ;

   OfTok := MakeKey('OF') ;
   AddKeyword(OfTok, oftok) ;

   OrTok := MakeKey('OR') ;
   AddKeyword(OrTok, ortok) ;
(*
   PackedTok := MakeKey('PACKED') ;
   AddKeyword(PackedTok, packedtok) ;
*)
   PackedSetTok := MakeKey('PACKEDSET') ;
   AddKeyword(PackedSetTok, packedsettok) ;

   PointerTok := MakeKey('POINTER') ;
   AddKeyword(PointerTok, pointertok) ;

   ProcedureTok := MakeKey('PROCEDURE') ;
   AddKeyword(ProcedureTok, proceduretok) ;

   QualifiedTok := MakeKey('QUALIFIED') ;
   AddKeyword(QualifiedTok, qualifiedtok) ;

   UnQualifiedTok := MakeKey('UNQUALIFIED') ;
   AddKeyword(UnQualifiedTok, unqualifiedtok) ;

   RecordTok := MakeKey('RECORD') ;
   AddKeyword(RecordTok, recordtok) ;

   RemTok := MakeKey('REM') ;
   AddKeyword(RemTok, remtok) ;

   RepeatTok := MakeKey('REPEAT') ;
   AddKeyword(RepeatTok, repeattok) ;

   RetryTok := MakeKey('RETRY') ;
   AddKeyword(RetryTok, retrytok) ;

   ReturnTok := MakeKey('RETURN') ;
   AddKeyword(ReturnTok, returntok) ;

   SetTok := MakeKey('SET') ;
   AddKeyword(SetTok, settok) ;

   ThenTok := MakeKey('THEN') ;
   AddKeyword(ThenTok, thentok) ;

   ToTok := MakeKey('TO') ;
   AddKeyword(ToTok, totok) ;

   TypeTok := MakeKey('TYPE') ;
   AddKeyword(TypeTok, typetok) ;

   UntilTok := MakeKey('UNTIL') ;
   AddKeyword(UntilTok, untiltok) ;

   VarTok := MakeKey('VAR') ;
   AddKeyword(VarTok, vartok) ;

   WhileTok := MakeKey('WHILE') ;
   AddKeyword(WhileTok, whiletok) ;

   WithTok := MakeKey('WITH') ;
   AddKeyword(WithTok, withtok) ;

   AsmTok := MakeKey('ASM') ;
   AddKeyword(AsmTok, asmtok) ;

   VolatileTok := MakeKey('VOLATILE') ;
   AddKeyword(VolatileTok, volatiletok) ;

   DateTok := MakeKey('__DATE__') ;     (* C compatible preprocessor primatives *)
   AddKeyword(DateTok, datetok) ;

   LineTok := MakeKey('__LINE__') ;
   AddKeyword(LineTok, linetok) ;

   FileTok := MakeKey('__FILE__') ;
   AddKeyword(FileTok, filetok) ;

   AttributeTok := MakeKey('__ATTRIBUTE__') ; (* GCC extension incorporated into gm2 *)
   AddKeyword(AttributeTok, attributetok) ;

   BuiltinTok := MakeKey('__BUILTIN__') ; (* GCC extension incorporated into gm2 *)
   AddKeyword(BuiltinTok, builtintok) ;

   InlineTok := MakeKey('__INLINE__') ; (* GCC extension incorporated into gm2 *)
   AddKeyword(InlineTok, inlinetok) ;

   a[0] := eof ;
   a[1] := nul ;
   EofTok := MakeKey(a)              (* Not a reserved token *)
END Init ;


(*
   IsReserved - returns TRUE if the symbol, Name, is a reserved word.
                If TRUE it also sets tok to the appropriate enumerated
                value. It will set tok to eoftok if appropriate.
*)

PROCEDURE IsReserved (n: Name; VAR tok: toktype) : BOOLEAN ;
VAR
   t: CARDINAL ;
BEGIN
   t := GetSymKey(NameTotok, n) ;
   IF t=0
   THEN
      (* eoftok is not a reserved word *)
      IF n=EofTok
      THEN
         tok := eoftok
      END ;
      RETURN( FALSE )
   ELSE
      tok := VAL(toktype, t) ;
      RETURN( TRUE )
   END
END IsReserved ;


(*
   tokToTok - returns a Tok given the enumerated variable, t.
*)

PROCEDURE tokToTok (t: toktype) : Name ;
BEGIN
   RETURN( GetSymKey(tokToName, VAL(Name, t)) )
END tokToTok ;


BEGIN
   Init
END M2Reserved.
