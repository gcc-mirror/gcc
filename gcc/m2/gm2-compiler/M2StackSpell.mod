(* M2StackSpell.mod maintain a stack of scopes used in spell checks.

Copyright (C) 2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaiusmod2@gmail.com>.

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

IMPLEMENTATION MODULE M2StackSpell ;

FROM SymbolTable IMPORT NulSym, IsModule, IsDefImp, IsRecord,
                        IsEnumeration, IsProcedure, GetNth,
                        GetSymName, GetSym, GetLocalSym, GetScope,
                        UnknownReported, IsUnknown,
                        GetUnknownOnImport, GetUnknownDeclScope,
                        ForeachExportedDo,
                        ForeachProcedureDo, ForeachLocalSymDo,
                        ForeachFieldEnumerationDo ;

FROM SymbolKey IMPORT PerformOperation ;
FROM DynamicStrings IMPORT InitStringCharStar, InitString, Mark, string, ConCat ;
FROM FormatStrings IMPORT Sprintf1, Sprintf2, Sprintf3 ;
FROM NameKey IMPORT KeyToCharStar, NulName ;
FROM M2MetaError IMPORT MetaErrorStringT0 ;

FROM M2StackWord IMPORT StackOfWord, PushWord, PopWord,
                        InitStackWord, KillStackWord,
                        NoOfItemsInStackWord, PeepWord ;

FROM CDataTypes IMPORT ConstCharStar ;
FROM M2Batch IMPORT GetModuleNo ;

IMPORT m2spellcheck ;
FROM m2spellcheck IMPORT Candidates ;


VAR
   DefaultStack: StackOfWord ;


(*
   GetRecordField - return the record field containing fieldName.
                    An error is generated if the fieldName is not
                    found in record.
*)

PROCEDURE GetRecordField (tokno: CARDINAL;
                          record: CARDINAL;
                          fieldName: Name) : CARDINAL ;
VAR
   str       : String ;
   sym       : CARDINAL ;
   recordName: Name ;
   content   : ConstCharStar ;
   cand      : Candidates ;
   fieldStr,
   recordStr,
   contentStr: String ;
BEGIN
   sym := GetLocalSym (record, fieldName) ;
   IF sym = NulSym
   THEN
      recordName := GetSymName (record) ;
      content := NIL ;
      cand := m2spellcheck.InitCandidates () ;
      IF PushCandidates (cand, record) > 0
      THEN
         content := m2spellcheck.FindClosestCharStar (cand,
                                                      KeyToCharStar (fieldName))
      END ;
      fieldStr := Mark (InitStringCharStar (KeyToCharStar (fieldName))) ;
      recordStr := Mark (InitStringCharStar (KeyToCharStar (recordName))) ;
      IF content = NIL
      THEN
         str := Sprintf2 (Mark (InitString ("field %s does not exist within record %s")),
                          fieldStr, recordStr)
      ELSE
         contentStr := Mark (InitStringCharStar (content)) ;
         str := Sprintf3 (Mark (InitString ("field %s does not exist within record %s, did you mean %s?")),
                          fieldStr, recordStr, contentStr)
      END ;
      MetaErrorStringT0 (tokno, str) ;
      m2spellcheck.KillCandidates (cand)
   END ;
   RETURN sym
END GetRecordField ;


(*
   CandidatePushName - push a symbol name to the candidate list.
*)

PROCEDURE CandidatePushName (cand: Candidates; sym: CARDINAL) ;
VAR
   str: String ;
BEGIN
   str := InitStringCharStar (KeyToCharStar (GetSymName (sym))) ;
   m2spellcheck.Push (cand, string (str)) ;
   INC (PushCount)
END CandidatePushName ;


(*
   GetDefModuleSpellHint - return a string describing a spelling
                           hint for the definition module name
                           similiar to defimp.  The premise is that
                           defimp has been misspelt.  NIL is returned
                           if no hint can be given.
*)

PROCEDURE GetDefModuleSpellHint (defimp: CARDINAL) : String ;
VAR
   i        : CARDINAL ;
   sym      : CARDINAL ;
   cand     : Candidates ;
   misspell,
   content  : ConstCharStar ;
   HintStr  : String ;
BEGIN
   HintStr := NIL ;
   IF GetSymName (defimp) # NulName
   THEN
      misspell := KeyToCharStar (GetSymName (defimp)) ;
      i := 1 ;
      sym := GetModuleNo (i) ;
      cand := m2spellcheck.InitCandidates () ;
      WHILE sym # NulSym DO
         IF sym # defimp
         THEN
            CandidatePushName (cand, sym)
         END ;
         INC (i) ;
         sym := GetModuleNo (i)
      END ;
      content := m2spellcheck.FindClosestCharStar (cand, misspell) ;
      HintStr := BuildHintStr (HintStr, content) ;
      m2spellcheck.KillCandidates (cand)
   END ;
   RETURN AddPunctuation (HintStr, '?')
END GetDefModuleSpellHint ;


(*
   Push - push a scope onto the spelling stack.
          sym might be a ModSym, DefImpSym or a varsym
          of a record type denoting a with statement.
*)

PROCEDURE Push (sym: CARDINAL) ;
BEGIN
   PushWord (DefaultStack, sym)
END Push ;


(*
   Pop - remove the top scope from the spelling stack.
*)

PROCEDURE Pop ;
BEGIN
   IF PopWord (DefaultStack) = 0
   THEN
   END
END Pop ;


VAR
   PushCount    : CARDINAL ;
   PushCandidate: Candidates ;


(*
   PushName - push a name to the candidate vec.
*)

PROCEDURE PushName (sym: CARDINAL) ;
VAR
   str: String ;
BEGIN
   str := InitStringCharStar (KeyToCharStar (GetSymName (sym))) ;
   m2spellcheck.Push (PushCandidate, string (str)) ;
   (* str := KillString (str) *)
   INC (PushCount)
END PushName ;


(*
   ForeachRecordFieldDo -
*)

PROCEDURE ForeachRecordFieldDo (record: CARDINAL; op: PerformOperation) ;
VAR
   i    : CARDINAL ;
   field: CARDINAL ;
BEGIN
   i := 1 ;
   REPEAT
      field := GetNth (record, i) ;
      IF field # NulSym
      THEN
         op (field)
      END ;
      INC (i)
   UNTIL field = NulSym
END ForeachRecordFieldDo ;


(*
   PushCandidates -
*)

PROCEDURE PushCandidates (cand: Candidates; sym: CARDINAL) : CARDINAL ;
BEGIN
   PushCount := 0 ;
   PushCandidate := cand ;
   IF IsModule (sym) OR IsDefImp (sym)
   THEN
      ForeachProcedureDo (sym, PushName) ;
      ForeachLocalSymDo (sym, PushName)
   ELSIF IsEnumeration (sym)
   THEN
      ForeachFieldEnumerationDo (sym, PushName)
   ELSIF IsRecord (sym)
   THEN
      ForeachRecordFieldDo (sym, PushName)
   END ;
   RETURN PushCount
END PushCandidates ;


(*
   BuildHintStr - create the did you mean hint and return it
                  if HintStr is NIL.  Otherwise append a hint
                  to HintStr.  If content is NIL then return NIL.
*)

PROCEDURE BuildHintStr (HintStr: String; content: ConstCharStar) : String ;
VAR
   str: String ;
BEGIN
   IF content # NIL
   THEN
      str := InitStringCharStar (content) ;
      IF HintStr = NIL
      THEN
         RETURN Sprintf1 (Mark (InitString (", did you mean %s")), str)
      ELSE
         RETURN Sprintf2 (Mark (InitString ("%s or %s")), HintStr, str)
      END
   END ;
   RETURN NIL
END BuildHintStr ;


(*
   CheckForHintStr - lookup a spell hint matching misspelt.  If one exists
                     then append it to HintStr.  Return HintStr.
*)

PROCEDURE CheckForHintStr (sym: CARDINAL;
                           HintStr, misspelt: String) : String ;
VAR
   cand   : Candidates ;
   content: ConstCharStar ;
BEGIN
   IF IsModule (sym) OR IsDefImp (sym) OR IsProcedure (sym) OR
      IsRecord (sym) OR IsEnumeration (sym)
   THEN
      cand := m2spellcheck.InitCandidates () ;
      IF PushCandidates (cand, sym) > 1
      THEN
         content := m2spellcheck.FindClosestCharStar (cand, string (misspelt))
      ELSE
         content := NIL
      END ;
      m2spellcheck.KillCandidates (cand) ;
      HintStr := BuildHintStr (HintStr, content)
   END ;
   RETURN HintStr
END CheckForHintStr ;


(*
   AddPunctuation - adds punct to the end of str providing that str is non NIL.
*)

PROCEDURE AddPunctuation (str: String; punct: ARRAY OF CHAR) : String ;
BEGIN
   IF str = NIL
   THEN
      RETURN NIL
   ELSE
      RETURN ConCat (str, Mark (InitString (punct)))
   END
END AddPunctuation ;


(*
   GetSpellHint - return a string describing a spelling hint.
*)

PROCEDURE GetSpellHint (unknown: CARDINAL) : String ;
BEGIN
   IF IsUnknown (unknown) AND
      GetUnknownOnImport (unknown) AND
      (GetUnknownDeclScope (unknown) # GetScope (unknown))
   THEN
      (* It was created during an import statement.  *)
      RETURN GetExportedSpellHint (unknown, GetUnknownDeclScope (unknown))
   END ;
   RETURN GetScopeSpellHint (unknown)
END GetSpellHint ;


(*
   GetExportedSpellHint - return a string describing a spelling hint
                          using the module exported identifiers.
*)

PROCEDURE GetExportedSpellHint (unknown, module: CARDINAL) : String ;
VAR
   content : ConstCharStar ;
   misspell,
   HintStr : String ;
BEGIN
   misspell := InitStringCharStar (KeyToCharStar (GetSymName (unknown))) ;
   HintStr := NIL ;
   PushCount := 0 ;
   PushCandidate := m2spellcheck.InitCandidates () ;
   ForeachExportedDo (module, PushName) ;
   ForeachLocalSymDo (module, PushName) ;
   IF PushCount > 0
   THEN
      content := m2spellcheck.FindClosestCharStar (PushCandidate,
                                                   string (misspell)) ;
      HintStr := BuildHintStr (HintStr, content)
   END ;
   m2spellcheck.KillCandidates (PushCandidate) ;
   RETURN AddPunctuation (HintStr, '?')
END GetExportedSpellHint ;


(*
   GetScopeSpellHint - return a string describing a spelling hint
                       using the visible scopes.
*)

PROCEDURE GetScopeSpellHint (unknown: CARDINAL) : String ;
VAR
   i, n     : CARDINAL ;
   sym      : CARDINAL ;
   misspell,
   HintStr  : String ;
BEGIN
   misspell := InitStringCharStar (KeyToCharStar (GetSymName (unknown))) ;
   HintStr := NIL ;
   n := NoOfItemsInStackWord (DefaultStack) ;
   i := 1 ;
   WHILE (i <= n) AND (HintStr = NIL) DO
      sym := PeepWord (DefaultStack, i) ;
      HintStr := CheckForHintStr (sym, HintStr, misspell) ;
      IF IsModule (sym) OR IsDefImp (sym)
      THEN
         (* Cannot see beyond a module scope.  *)
         RETURN AddPunctuation (HintStr, '?')
      END ;
      INC (i)
   END ;
   RETURN AddPunctuation (HintStr, '?')
END GetScopeSpellHint ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   DefaultStack := InitStackWord ()
END Init ;


BEGIN
   Init
END M2StackSpell.
