(* M2SSA.mod discover very obvious single assignment temporaries.

Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2SSA ;


FROM M2Debug IMPORT Assert ;

FROM NameKey IMPORT Name, WriteKey, MakeKey, GetKey ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;

FROM M2Error IMPORT InternalError ;
FROM M2Batch IMPORT GetModuleNo ;
FROM M2Quiet IMPORT qprintf1 ;
FROM M2Scope IMPORT ScopeBlock, InitScopeBlock, KillScopeBlock ;
FROM M2StackWord IMPORT StackOfWord, InitStackWord, KillStackWord, PushWord, PopWord, PeepWord ;
FROM M2Options IMPORT CompilerDebugging ;
FROM Lists IMPORT InitList, KillList, List, IncludeItemIntoList, IsItemInList ;
FROM M2Printf IMPORT printf0, printf1, printf2 ;

FROM SymbolTable IMPORT GetSymName,
                        GetProcedureQuads, GetModuleQuads,
                        GetModule, GetNthProcedure,
                        GetSubrange, GetModuleScope,
                        PutProcedureReachable, IsProcedureReachable,
                        PutProcedureStartQuad, PutProcedureEndQuad,
                        PutProcedureScopeQuad,
                        PutNeedSavePriority,
                        IsProcedure, GetPriority,
                        GetDeclaredMod, GetFirstUsed,
                        GetType, GetNth, GetWriteQuads, GetReadQuads, GetMode,
                        IsExportQualified, IsExportUnQualified, IsExported,
                        ForeachProcedureDo, ForeachInnerModuleDo,
                        IsModuleWithinProcedure, IsTemporary,
                        PutVariableSSA,
                        NulSym ;

FROM M2Quads IMPORT QuadOperator, GetQuad, GetFirstQuad, GetNextQuad,
                    PutQuad, SubQuad, Opposite, IsReferenced,
                    GetRealQuad ;

VAR
   stack  : StackOfWord ;
   visited: List ;


(*
   writtenOnce - return TRUE if variable is written to once.
*)

PROCEDURE writtenOnce (variable: CARDINAL) : BOOLEAN ;
VAR
   writeStart, writeEnd: CARDINAL ;
   readStart, readEnd  : CARDINAL ;
BEGIN
   GetWriteQuads (variable, GetMode (variable), writeStart, writeEnd) ;
   GetReadQuads (variable, GetMode (variable), readStart, readEnd) ;
   RETURN (writeStart = writeEnd) AND ((readStart > writeStart) OR (readStart = 0))
END writtenOnce ;


(*
   DetermineSSA - performs a trivial check to see if the temporary is written to
                  once.
*)

PROCEDURE DetermineSSA (variable: CARDINAL) ;
VAR
   name: Name ;
BEGIN
   IF EnableSSA AND IsTemporary (variable)
   THEN
      name := GetSymName (variable) ;
      IF writtenOnce (variable)
      THEN
         PutVariableSSA (variable, TRUE) ;
         IF CompilerDebugging
         THEN
            printf1 ("  temporary:  %a  SSA found\n", name)
         END
      ELSE
         IF CompilerDebugging
         THEN
            printf1 ("  temporary:  %a  not SSA\n", name)
         END
      END
   END
END DetermineSSA ;


(*
   DiscoverSSATemporaries -
*)

PROCEDURE DiscoverSSATemporaries (scope: CARDINAL) ;
VAR
   n,
   variable: CARDINAL ;
BEGIN
   IF CompilerDebugging
   THEN
      printf1 ("examining scope %d\n", scope)
   END ;
   n := 1 ;
   variable := GetNth (scope, n) ;
   WHILE variable # NulSym DO
      DetermineSSA (variable) ;
      INC (n) ;
      variable := GetNth (scope, n)
   END
END DiscoverSSATemporaries ;


(*
   DiscoverSSA - perform a very simple check to determine whether a
                 temporary is a single use write.
*)

PROCEDURE DiscoverSSA (scope: CARDINAL) ;
VAR
   sb: ScopeBlock ;
BEGIN
   IF NOT IsItemInList (visited, scope)
   THEN
      IncludeItemIntoList (visited, scope) ;
      sb := InitScopeBlock (scope) ;
      PushWord (stack, scope) ;
      IF CompilerDebugging
      THEN
         printf1 ("DiscoverSSA %d\n", scope)
      END ;

      IF CompilerDebugging
      THEN
         printf0 ("ForeachInnerModuleDo\n")
      END ;
      ForeachInnerModuleDo(scope, DiscoverSSA) ;
      IF CompilerDebugging
      THEN
         printf0 ("ForeachProcedureDo\n")
      END ;
      ForeachProcedureDo(scope, DiscoverSSA) ;
      DiscoverSSATemporaries (scope) ;

      Assert (PopWord (stack) = scope) ;
      KillScopeBlock (sb)
   END
END DiscoverSSA ;


BEGIN
   stack := InitStackWord () ;
   InitList (visited)
END M2SSA.
