(* M2Students.mod checks for new programmer errors.

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

IMPLEMENTATION MODULE M2Students ;


FROM SymbolTable IMPORT FinalSymbol, IsVar, IsProcedure, IsModule,
                        GetMainModule, IsType, NulSym, IsRecord, GetSymName, GetNth, GetNthProcedure, GetDeclaredMod, NoOfParam ;
FROM NameKey IMPORT GetKey, WriteKey, MakeKey, IsSameExcludingCase, NulName, makekey, KeyToCharStar ;
FROM M2MetaError IMPORT MetaErrorString0, MetaError2 ;
FROM Lists IMPORT List, InitList, IsItemInList, IncludeItemIntoList ;
FROM M2Reserved IMPORT IsReserved, toktype ;
FROM DynamicStrings IMPORT String, InitString, KillString, ToUpper, InitStringCharStar, string, Mark, ToUpper, Dup ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2 ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM ASCII IMPORT nul ;
FROM M2Options IMPORT StudentChecking ;


VAR
   ErrantNames,
   ErrantSymbols: List ;


(*
   IsNotADuplicate - returns TRUE if either s1 or s2 have not been reported before.
*)

PROCEDURE IsNotADuplicate (s1, s2: CARDINAL) : BOOLEAN ;
BEGIN
   IF (NOT IsItemInList(ErrantSymbols, s1)) AND (NOT IsItemInList(ErrantSymbols, s2))
   THEN
      IncludeItemIntoList(ErrantSymbols, s1) ;
      IncludeItemIntoList(ErrantSymbols, s2) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsNotADuplicate ;


(*
   IsNotADuplicateName - returns TRUE if name has not been reported before.
*)

PROCEDURE IsNotADuplicateName (name: Name) : BOOLEAN ;
BEGIN
   IF NOT IsItemInList(ErrantNames, name)
   THEN
      IncludeItemIntoList(ErrantNames, name) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsNotADuplicateName ;


(*
   CheckForVariableThatLooksLikeKeyword - checks for a identifier that looks the same
                                          as a keyword except for its case.
*)

PROCEDURE CheckForVariableThatLooksLikeKeyword (name: Name) ;
BEGIN
   IF StudentChecking
   THEN
      PerformVariableKeywordCheck (name)
   END
END CheckForVariableThatLooksLikeKeyword ;


(*
   PerformVariableKeywordCheck - performs the check and constructs the metaerror notes if appropriate.
*)

PROCEDURE PerformVariableKeywordCheck (name: Name) ;
VAR
   upper : Name ;
   token : toktype ;
   orig,
   upperS: String ;
BEGIN
   orig := InitStringCharStar (KeyToCharStar (name)) ;
   upperS := ToUpper (Dup (orig)) ;
   upper := makekey (string (upperS)) ;
   IF IsReserved (upper, token)
   THEN
      IF IsNotADuplicateName (name)
      THEN
         MetaErrorString0 (Sprintf2 (Mark (InitString ('either the identifier has the same name as a keyword or alternatively a keyword has the wrong case ({%%K%s} and {!%%O:{%%K%s}})')),
                                     upperS, orig)) ;
         MetaErrorString0 (Sprintf1 (Mark (InitString ('the symbol name {!%%O:{%%K%s}} is legal as an identifier, however as such it might cause confusion and is considered bad programming practice')), orig))
      END
   END ;
   upperS := KillString (upperS) ;
   orig := KillString (orig)
END PerformVariableKeywordCheck ;


(*
   CheckAsciiName - checks to see whether ascii names, s1, and, s2, are similar.
*)

PROCEDURE CheckAsciiName (previous, s1, newblock, s2: CARDINAL) ;
VAR
   a1, a2: Name ;
BEGIN
   a1 := GetSymName (s1) ;
   a2 := GetSymName (s2) ;
   IF (a1 = a2) AND (a1 # NulName)
   THEN
      IF IsNotADuplicate (s1, s2)
      THEN
         MetaError2 ('identical symbol name in two different scopes, scope {%1Oad} has symbol {%2Mad}', previous, s1) ;
         MetaError2 ('identical symbol name in two different scopes, scope {%1Oad} has symbol {%2Mad}', newblock, s2)
      END
   ELSIF IsSameExcludingCase (a1, a2)
   THEN
      IF IsNotADuplicate (s1, s2)
      THEN
         MetaError2 ('very similar symbol names (different case) in two different scopes, scope {%1ORad} has symbol {%2Mad}', previous, s1) ;
         MetaError2 ('very similar symbol names (different case) in two different scopes, scope {%1OCad} has symbol {%2Mad}', newblock, s2)
      END
   END
END CheckAsciiName ;


(*
   CheckProcedure - checks the procedure, p, for symbols which look like, s.
*)

PROCEDURE CheckProcedure (m, p: CARDINAL) ;
VAR
   i, n1,
   j, n2: CARDINAL ;
BEGIN
   IF p#NulSym
   THEN
      i := 1 ;   (* I would have used NoOfParam(p)+1 but Stuart wants parameters checked as well - maybe he is right *)
      REPEAT
         n1 := GetNth(p, i) ;
         IF n1#NulSym
         THEN
            IF IsVar(n1) OR IsType(n1) OR IsProcedure(n1) OR IsRecord(n1)
            THEN
               j := 1 ;
               REPEAT
                  n2 := GetNth(m, j) ;
                  IF n2#NulSym
                  THEN
                     IF IsVar(n2) OR IsType(n2) OR IsProcedure(n2) OR IsRecord(n2)
                     THEN
                        CheckAsciiName(m, n2, p, n1)
                     END
                  END ;
                  INC(j)
               UNTIL n2=NulSym
            END
         END ;
         INC(i)
      UNTIL n1=NulSym
   END
END CheckProcedure ;


(*
   CheckModule - checks the module, m, for symbols which look like, s.
*)

PROCEDURE CheckModule (m, s: CARDINAL) ;
VAR
   i, n: CARDINAL ;
BEGIN
   IF m#NulSym
   THEN
      i := 1 ;
      REPEAT
         n := GetNth(m, i) ;
         IF n#NulSym
         THEN
            IF (n#NulSym) AND (n#s)
            THEN
               IF IsVar(n) OR IsType(n) OR IsProcedure(n) OR IsRecord(n)
               THEN
                  CheckAsciiName(m, s, m, n)
               END
            END
         END ;
         INC(i)
      UNTIL n=NulSym
   END
END CheckModule ;


(*
   StudentVariableCheck - checks to see that variables are quite different from keywords and
                          issues an message if they are not. It ignores case so to catch
                          1st and 2nd semester programming errors.
*)

PROCEDURE StudentVariableCheck ;
VAR
   i, n, m: CARDINAL ;
BEGIN
   m := GetMainModule() ;
   (* first check global scope *)
   i := 1 ;
   REPEAT
      n := GetNth(m, i) ;
      IF n#NulSym
      THEN
         IF IsVar(n) OR IsType(n) OR IsProcedure(n) OR IsRecord(n)
         THEN
            CheckModule(m, n)
         END
      END ;
      INC(i)
   UNTIL n=NulSym ;
   (* now check local scope *)
   i := 1 ;
   REPEAT
      n := GetNthProcedure(m, i) ;
      IF n#NulSym
      THEN
         IF IsProcedure(n)
         THEN
            CheckProcedure(m, n)
         END
      END ;
      INC(i)
   UNTIL n=NulSym
END StudentVariableCheck ;


BEGIN
   InitList(ErrantSymbols) ;
   InitList(ErrantNames)
END M2Students.
