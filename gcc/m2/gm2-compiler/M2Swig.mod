(* M2Swig.mod generates a swig interface file for the main module.

Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Swig ;

FROM Storage IMPORT ALLOCATE ;
FROM M2Options IMPORT GenerateSwig ;
FROM SFIO IMPORT OpenToWrite ;
FROM FIO IMPORT File, Close ;
FROM NameKey IMPORT Name, KeyToCharStar ;
FROM M2Error IMPORT InternalError ;
FROM M2Printf IMPORT fprintf0, fprintf1, fprintf2, fprintf3, fprintf4 ;
FROM M2AsmUtil IMPORT GetFullScopeAsmName ;
FROM SYSTEM IMPORT WORD ;

FROM DynamicStrings IMPORT String, InitString, InitStringCharStar, ConCat, Mark,
                           KillString ;

FROM Lists IMPORT List, InitList, KillList, IsItemInList,
                  IncludeItemIntoList, RemoveItemFromList,
                  ForeachItemInListDo, NoOfItemsInList,
                  GetItemFromList ;

FROM M2Quads IMPORT IsProcedureScope ;
FROM M2System IMPORT IsSystemType, Address, Byte, Loc, Word ;
FROM M2Bitset IMPORT Bitset ;
FROM Indexing IMPORT Index, InitIndex, KillIndex, HighIndice, PutIndice, GetIndice ;
FROM M2Scope IMPORT ScopeBlock, InitScopeBlock, KillScopeBlock ;

FROM M2Base IMPORT IsBaseType, Char, Cardinal, Integer, Real, LongReal, ShortReal,
                   LongCard, ShortCard, LongInt, ShortInt, Boolean ;

FROM SymbolTable IMPORT GetSymName, IsType, IsProcedure, IsConst, IsVar,
                        GetType, GetNthParam, IsUnbounded, GetMode, ModeOfAddr,
                        NoOfParam, IsConstString, IsConstLit, IsPointer,
                        IsExported, ForeachExportedDo, IsUnboundedParam,
                        IsParameter, IsParameterUnbounded, IsParameterVar,
                        GetParameterShadowVar, GetReadQuads, GetWriteQuads,
                        NulSym ;

FROM M2BasicBlock IMPORT BasicBlock, InitBasicBlocks, KillBasicBlocks,
                         ForeachBasicBlockDo ;


TYPE
   UnboundedSig = POINTER TO RECORD
                                type: CARDINAL ;
                                name: Name ;
                             END ;

VAR
   includedArray: BOOLEAN ;
   uKey         : Index ;
   mainModule   : CARDINAL ;
   Done,
   ToDo         : List ;
   f            : File ;
   name         : String ;


(*
   DoExported - includes, sym, into the, ToDo, list.
*)

PROCEDURE DoExported (sym: CARDINAL) ;
BEGIN
   IncludeItemIntoList(ToDo, sym)
END DoExported ;


(*
   MoveToDone - moves a sym to the, Done, list,
                providing that it is not already on it.
                It returns TRUE if the lists were modified.
*)

PROCEDURE MoveToDone (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsItemInList(Done, sym)
   THEN
      RETURN( FALSE )
   ELSIF IsItemInList(ToDo, sym)
   THEN
      RemoveItemFromList(ToDo, sym) ;
      IncludeItemIntoList(Done, sym) ;
      RETURN( TRUE )
   END ;
   IncludeItemIntoList(Done, sym) ;
   RETURN( TRUE )
END MoveToDone ;


(*
   MoveToToDo - moves a sym to the, ToDo, list,
                providing that it is not already on it.
                It returns TRUE if the lists were modified.
*)

PROCEDURE MoveToToDo (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsItemInList(Done, sym)
   THEN
      InternalError ('not expecting to get here')
   ELSIF IsItemInList(ToDo, sym)
   THEN
      RETURN( FALSE )
   ELSE
      IncludeItemIntoList(ToDo, sym) ;
      RETURN( TRUE )
   END
END MoveToToDo ;


(*
   Trybase - returns TRUE
*)

PROCEDURE TryBase (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF (sym=Cardinal) OR (sym=Integer) OR (sym=LongInt) OR
      (sym=LongCard) OR (sym=Char) OR (sym=ShortCard) OR
      (sym=ShortInt) OR (sym=Real) OR (sym=LongReal) OR
      (sym=ShortReal) OR (sym=Boolean)
   THEN
      RETURN( MoveToDone(sym) )
   ELSE
      RETURN( FALSE )
   END
END TryBase ;


(*
   TrySystem - returns TRUE if sym can be moved to the done list.
*)

PROCEDURE TrySystem (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF (sym=Bitset) OR (sym=Address) OR (sym=Byte) OR (sym=Loc) OR
      (sym=Word)
   THEN
      RETURN( MoveToDone(sym) )
   ELSE
      RETURN( FALSE )
   END
END TrySystem ;


(*
   TryMove - tries to move sym to the done queue as long
             as type is known.
*)

PROCEDURE TryMove (sym, type: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsItemInList(Done, type)
   THEN
      IF MoveToDone(sym)
      THEN
         RETURN( TRUE )
      END
   ELSE
      IF MoveToToDo(sym)
      THEN
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END TryMove ;


(*
   TryType -
*)

PROCEDURE TryType (sym: CARDINAL) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   type := GetType(sym) ;
   result := TryDependents(type) ;
   IF TryMove(sym, type)
   THEN
      RETURN( TRUE )
   ELSE
      RETURN( result )
   END
END TryType ;


(*
   TryVar -
*)

PROCEDURE TryVar (sym: CARDINAL) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   type := GetType(sym) ;
   result := TryDependents(type) ;
   IF TryMove(sym, type)
   THEN
      RETURN( TRUE )
   ELSE
      RETURN( result )
   END
END TryVar ;


(*
   TryProcedure -
*)

PROCEDURE TryProcedure (sym: CARDINAL) : BOOLEAN ;
VAR
   son,
   p, i,
   type  : CARDINAL ;
   solved,
   result: BOOLEAN ;
BEGIN
   type := GetType(sym) ;
   result := FALSE ;
   solved := TRUE ;
   IF type#NulSym
   THEN
      IF TryDependents(type)
      THEN
         result := TRUE
      END ;
      IF NOT IsItemInList(Done, type)
      THEN
         solved := FALSE
      END
   END ;
   p := NoOfParam(sym) ;
   i := 1 ;
   WHILE i<=p DO
      son := GetNthParam(sym, i) ;
      IF TryDependents(son)
      THEN
         result := TRUE
      END ;
      IF NOT IsItemInList(Done, son)
      THEN
         solved := FALSE
      END ;
      INC(i)
   END ;
   IF solved
   THEN
      IF MoveToDone(sym)
      THEN
         RETURN( TRUE )
      END
   ELSE
      IF MoveToToDo(sym)
      THEN
         RETURN( TRUE )
      END
   END ;
   RETURN( result )
END TryProcedure ;


(*
   TryUnbounded -
*)

PROCEDURE TryUnbounded (sym: CARDINAL) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   type := GetType(sym) ;
   result := TryDependents(type) ;
   IF TryMove(sym, type)
   THEN
      RETURN( TRUE )
   ELSE
      RETURN( result )
   END
END TryUnbounded ;


(*
   TryParameter -
*)

PROCEDURE TryParameter (sym: CARDINAL) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   type := GetType(sym) ;
   result := TryDependents(type) ;
   IF TryMove(sym, type)
   THEN
      RETURN( TRUE )
   ELSE
      RETURN( result )
   END
END TryParameter ;


(*
   TryDependents - returns TRUE if any alteration occurred to any
                   of the lists.
*)

PROCEDURE TryDependents (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsBaseType(sym)
   THEN
      RETURN( TryBase(sym) )
   ELSIF IsSystemType(sym)
   THEN
      RETURN( TrySystem(sym) )
   ELSIF IsType(sym)
   THEN
      RETURN( TryType(sym) )
   ELSIF IsParameter(sym)
   THEN
      RETURN( TryParameter(sym) )
   ELSIF IsProcedure(sym)
   THEN
      RETURN( TryProcedure(sym) )
   ELSIF IsConstString(sym)
   THEN
      RETURN( MoveToDone(sym) )
   ELSIF IsConstLit(sym)
   THEN
      RETURN( MoveToDone(sym) )
   ELSIF IsVar(sym) AND (GetMode(sym)=ImmediateValue)
   THEN
      RETURN( MoveToDone(sym) )
   ELSIF IsVar(sym)
   THEN
      RETURN( TryVar(sym) )
   ELSIF IsUnbounded(sym)
   THEN
      RETURN( TryUnbounded(sym) )
   ELSE
      RETURN( FALSE )
   END
END TryDependents ;


(*
   DoResolveOrder - resolves the declaration order for swig (C).
*)

PROCEDURE DoResolveOrder ;
VAR
   sym,
   i, n    : CARDINAL ;
   movement: BOOLEAN ;
BEGIN
   REPEAT
      n := NoOfItemsInList(ToDo) ;
      movement := FALSE ;
      i := 1 ;
      WHILE (i<=n) AND (NOT movement) DO
         sym := GetItemFromList(ToDo, i) ;
         movement := TryDependents(sym) ;
         INC(i)
      END
   UNTIL NOT movement
END DoResolveOrder ;


(*
   DoName -
*)

PROCEDURE DoName (sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   n := GetFullScopeAsmName(sym) ;
   fprintf1(f, "%a", n)
END DoName ;


(*
   DoParamName -
*)

PROCEDURE DoParamName (sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   n := GetSymName(sym) ;
   fprintf1(f, "%a", n)
END DoParamName ;


(*
   DoVar -
*)

PROCEDURE DoVar (sym: CARDINAL) ;
BEGIN
   fprintf0(f, 'extern "C" ') ;
   DoType(GetType(sym)) ;
   fprintf0(f, ' ') ;
   DoName(sym) ;
   fprintf0(f, ';\n')
END DoVar ;


(*
   DoType -
*)

PROCEDURE DoType (sym: CARDINAL) ;
BEGIN
   IF IsPointer(sym)
   THEN
      DoType(GetType(sym)) ;
      fprintf0(f, ' *')
   ELSIF sym=Cardinal
   THEN
      fprintf0(f, "unsigned int")
   ELSIF sym=Integer
   THEN
      fprintf0(f, "int")
   ELSIF sym=Boolean
   THEN
      fprintf0(f, "unsigned int")
   ELSIF sym=LongInt
   THEN
      fprintf0(f, "long long int")
   ELSIF sym=LongCard
   THEN
      fprintf0(f, "long long unsigned int")
   ELSIF sym=Char
   THEN
      fprintf0(f, "char")
   ELSIF sym=ShortCard
   THEN
      fprintf0(f, "short unsigned int")
   ELSIF sym=ShortInt
   THEN
      fprintf0(f, "short int")
   ELSIF sym=Real
   THEN
      fprintf0(f, "double")
   ELSIF sym=LongReal
   THEN
      fprintf0(f, "long double")
   ELSIF sym=ShortReal
   THEN
      fprintf0(f, "float")
   ELSIF sym=Bitset
   THEN
      fprintf0(f, "unsigned int")
   ELSIF sym=Address
   THEN
      fprintf0(f, "void *")
   ELSIF sym=Byte
   THEN
      fprintf0(f, "unsigned char")
   ELSIF sym=Loc
   THEN
      fprintf0(f, "unsigned char")
   ELSIF sym=Word
   THEN
      fprintf0(f, "unsigned int")
   END
END DoType ;


(*
   DoUnbounded -
*)

PROCEDURE DoUnbounded (sym: CARDINAL) ;
VAR
   n   : Name ;
   type: CARDINAL ;
BEGIN
   type := GetType(sym) ;
   DoType(GetType(type)) ;
   n := GetSymName(sym) ;
   fprintf2(f, ' *_m2_address_%a, int _m2_high_%a', n, n)
END DoUnbounded ;


VAR
   FirstBasicBlock,
   Input,
   Output,
   InOut,
   CanGuess,
   IsKnown        : BOOLEAN ;
   rs, ws         : CARDINAL ;


(*
   DoBasicBlock -
*)

PROCEDURE DoBasicBlock (start, end: CARDINAL) ;
BEGIN
   IF IsProcedureScope(start)
   THEN
      (* skip this basic block, as this will not modify the parameter *)
      RETURN
   ELSIF IsKnown OR CanGuess
   THEN
      (* already resolved *)
      RETURN
   ELSE
      IF (ws=0) AND (rs=0)
      THEN
         FirstBasicBlock := FALSE
      ELSIF rs=0
      THEN
         (* only written *)
         IF ws<=end
         THEN
            Output := TRUE ;
            IF FirstBasicBlock
            THEN
               IsKnown := TRUE
            ELSE
               CanGuess := TRUE
            END ;
            FirstBasicBlock := FALSE
         END
      ELSIF ws=0
      THEN
         (* only read *)
         Input := TRUE ;
         IF (rs<=end) AND FirstBasicBlock
         THEN
            IsKnown := TRUE
         ELSE
            CanGuess := TRUE
         END ;
         FirstBasicBlock := FALSE
      ELSIF rs<=ws
      THEN
         (* read before write *)
         InOut := TRUE ;
         IF (rs<=end) AND (ws<=end) AND FirstBasicBlock
         THEN
            IsKnown := TRUE
         ELSE
            CanGuess := TRUE
         END ;
         FirstBasicBlock := FALSE
      ELSE
         (* must be written before read *)
         Output := TRUE ;
         IF (rs<=end) AND (ws<=end) AND FirstBasicBlock
         THEN
            IsKnown := TRUE
         ELSE
            CanGuess := TRUE
         END ;
         FirstBasicBlock := FALSE
      END
   END
END DoBasicBlock ;


(*
   DetermineParameter -
*)

PROCEDURE DetermineParameter (procedure, param: CARDINAL) ;
VAR
   sb: ScopeBlock ;
   bb: BasicBlock ;
   we,
   re: CARDINAL ;
BEGIN
   sb := InitScopeBlock(procedure) ;
   bb := InitBasicBlocks(sb) ;
   Input := FALSE ;
   Output := FALSE ;
   InOut := FALSE ;
   CanGuess := FALSE ;
   IsKnown := FALSE ;
   FirstBasicBlock := TRUE ;
   GetReadQuads(param, RightValue, rs, re) ;
   GetWriteQuads(param, RightValue, ws, we) ;
   ForeachBasicBlockDo(bb, DoBasicBlock) ;
   KillBasicBlocks(bb) ;
   KillScopeBlock(sb)
END DetermineParameter ;


(*
   PrintDirection -
*)

PROCEDURE PrintDirection ;
BEGIN
   IF Input
   THEN
      fprintf0(f, 'INPUT')
   ELSIF Output
   THEN
      fprintf0(f, 'OUTPUT')
   ELSE
      fprintf0(f, 'INOUT')
   END
END PrintDirection ;


(*
   CalculateVarDirective -
*)

PROCEDURE CalculateVarDirective (procedure, param: CARDINAL; annotate: BOOLEAN) ;
VAR
   sym: CARDINAL ;
BEGIN
   sym := GetParameterShadowVar(param) ;
   IF sym=NulSym
   THEN
      InternalError ('why did we get here')
   ELSE
      DetermineParameter(procedure, sym) ;
      IF annotate
      THEN
         DoParamName(sym) ;
         IF IsKnown
         THEN
            fprintf0(f, ' is known to be an ') ;
            PrintDirection
         ELSIF CanGuess
         THEN
            fprintf0(f, ' is guessed to be an ') ;
            PrintDirection
         ELSE
            fprintf0(f, ' is unknown')
         END
      ELSE
         fprintf0(f, '*') ;
         IF IsKnown OR CanGuess
         THEN
            PrintDirection
         ELSE
            DoParamName(sym)
         END
      END
   END
END CalculateVarDirective ;


(*
   AnnotateProcedure -
*)

PROCEDURE AnnotateProcedure (sym: CARDINAL) ;
VAR
   son, p, i: CARDINAL ;
   needComma: BOOLEAN ;
BEGIN
   fprintf0(f, '/*  parameter: ') ;
   p := NoOfParam(sym) ;
   i := 1 ;
   needComma := FALSE ;
   WHILE i<=p DO
      son := GetNthParam(sym, i) ;
      IF IsParameterVar(son)
      THEN
         IF needComma
         THEN
            fprintf0(f, ', ')
         END ;
         CalculateVarDirective(sym, son, TRUE) ;
         needComma := TRUE
      END ;
      INC(i)
   END ;
   fprintf0(f, ' */\n\n')
END AnnotateProcedure ;


(*
   DoProcedure -
*)

PROCEDURE DoProcedure (sym: CARDINAL) : BOOLEAN ;
VAR
   son,
   p, i : CARDINAL ;
   found: BOOLEAN ;
BEGIN
   found := FALSE ;
   fprintf0(f, 'extern "C" ') ;
   IF GetType(sym)=NulSym
   THEN
      fprintf0(f, 'void') ;
   ELSE
      DoType(GetType(sym))
   END ;
   fprintf0(f, ' ') ;
   DoName(sym) ;
   fprintf0(f, ' (') ;
   p := NoOfParam(sym) ;
   IF p=0
   THEN
      fprintf0(f, 'void') ;
   ELSE
      i := 1 ;
      WHILE i<=p DO
         son := GetNthParam(sym, i) ;
         IF IsUnboundedParam(sym, i)
         THEN
            DoUnbounded(son)
         ELSE
            DoType(GetType(son)) ;
            fprintf0(f, ' ') ;
            IF IsParameterVar(son)
            THEN
               found := TRUE ;
               CalculateVarDirective(sym, son, FALSE)
            ELSE
               DoParamName(son)
            END
         END ;
         IF i<p
         THEN
            fprintf0(f, ', ')
         END ;
         INC(i)
      END
   END ;
   fprintf0(f, ');\n') ;
   RETURN( found )
END DoProcedure ;


(*
   DoWriteSymbol -
*)

PROCEDURE DoWriteSymbol (sym: CARDINAL) ;
BEGIN
   IF IsBaseType(sym)
   THEN
   ELSIF IsSystemType(sym)
   THEN
   ELSIF IsType(sym)
   THEN
   ELSIF IsProcedure(sym)
   THEN
      IF DoProcedure(sym)
      THEN
         AnnotateProcedure(sym)
      END
   ELSIF IsConstString(sym)
   THEN
   ELSIF IsConstLit(sym)
   THEN
   ELSIF IsVar(sym) AND (GetMode(sym)=ImmediateValue)
   THEN
   ELSIF IsVar(sym)
   THEN
      DoVar(sym)
   END
END DoWriteSymbol ;


(*
   DoCheckExported -
*)

PROCEDURE DoCheckExported (sym: WORD) ;
BEGIN
   IF IsExported(mainModule, sym)
   THEN
      DoWriteSymbol(sym)
   END
END DoCheckExported ;


(*
   IsUnique - returns TRUE if the combination of, n, and, t,
              is unique.
*)

PROCEDURE IsUnique (n: Name; t: CARDINAL) : BOOLEAN ;
VAR
   p   : UnboundedSig ;
   h, i: CARDINAL ;
BEGIN
   i := 1 ;
   h := HighIndice(uKey) ;
   WHILE i<=h DO
      p := GetIndice(uKey, i) ;
      IF (p^.type=t) AND (p^.name=n)
      THEN
         RETURN( FALSE )
      END ;
      INC(i)
   END ;
   INC(h) ;
   NEW(p) ;
   WITH p^ DO
      type := t ;
      name := n
   END ;
   PutIndice(uKey, h, p) ;
   RETURN( TRUE )
END IsUnique ;


(*
   IsTypeUnique - returns TRUE if type, t, has not been entered yet.
*)

PROCEDURE IsTypeUnique (t: CARDINAL) : BOOLEAN ;
VAR
   p   : UnboundedSig ;
   h, i: CARDINAL ;
BEGIN
   i := 1 ;
   h := HighIndice(uKey) ;
   WHILE i<=h DO
      p := GetIndice(uKey, i) ;
      IF p^.type=t
      THEN
         RETURN( FALSE )
      END ;
      INC(i)
   END ;
   RETURN( TRUE )
END IsTypeUnique ;


(*
   DoCheckUnbounded -
*)

PROCEDURE DoCheckUnbounded (sym: WORD) ;
VAR
   name      : Name ;
   type      : CARDINAL ;
   typeUnique: BOOLEAN ;
BEGIN
   IF IsParameter(sym) AND IsParameterUnbounded(sym)
   THEN
      name := GetSymName(sym) ;
      type := GetType(GetType(sym)) ;
      typeUnique := IsTypeUnique(type) ;
      IF IsUnique(name, type)
      THEN
         IF NOT includedArray
         THEN
            includedArray := TRUE ;
            fprintf0(f, '%include "carrays.i"\n')
         END ;
         fprintf0(f, '%') ;
         fprintf0(f, 'apply (char *STRING, int LENGTH) { (') ;
         DoUnbounded(sym) ;
         fprintf0(f, ') };\n') ;
         IF typeUnique
         THEN
            fprintf0(f, '%array_functions(') ;
            DoType(type) ;
            fprintf0(f, ', ') ;
            DoType(type) ;
            fprintf0(f, 'Array);\n')
         END
      END
   END
END DoCheckUnbounded ;


(*
   DoWriteFile -
*)

PROCEDURE DoWriteFile (sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   mainModule := sym ;
   n := GetSymName(sym) ;
   fprintf0(f, '/* automatically generated by gm2 -fswig */\n') ;
   fprintf0(f, '%') ;
   fprintf1(f, 'module %a\n\n', n) ;
   fprintf0(f, '%') ;
   fprintf1(f, 'include exception.i\n\n', n) ;
   fprintf0(f, '%') ;
   fprintf0(f, 'exception {\n') ;
   fprintf0(f, '  try {\n') ;
   fprintf0(f, '     $action\n') ;
   fprintf0(f, '  } catch (int i) {\n') ;
   fprintf0(f, '     return NULL;\n') ;
   fprintf0(f, '  }\n') ;
   fprintf0(f, '}\n\n') ;
   ForeachItemInListDo(Done, DoCheckUnbounded) ;
   fprintf0(f, '\n%{\n') ;
   ForeachItemInListDo(Done, DoCheckExported) ;
   fprintf0(f, '%}\n\n') ;
   ForeachItemInListDo(Done, DoCheckExported)
END DoWriteFile ;


(*
   DoGenerateSwig -
*)

PROCEDURE DoGenerateSwig (sym: CARDINAL) ;
BEGIN
   Init ;
   name := ConCat (InitStringCharStar (KeyToCharStar (GetSymName (sym))),
                   Mark (InitString ('.i'))) ;
   f := OpenToWrite (name) ;
   ForeachExportedDo (sym, DoExported) ;
   DoResolveOrder ;
   DoWriteFile (sym) ;
   Close (f) ;
   name := KillString (name) ;
   Kill
END DoGenerateSwig ;


(*
   GenerateSwigFile - if the -fswig option was specified then generate
                      a swig interface file for the main module.
*)

PROCEDURE GenerateSwigFile (sym: CARDINAL) ;
BEGIN
   IF GenerateSwig
   THEN
      DoGenerateSwig(sym)
   END
END GenerateSwigFile ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   InitList(Done) ;
   InitList(ToDo) ;
   uKey := InitIndex(1) ;
   includedArray := FALSE
END Init ;


(*
   Kill -
*)

PROCEDURE Kill ;
BEGIN
   KillList(Done) ;
   KillList(ToDo) ;
   uKey := KillIndex(uKey)
END Kill ;


END M2Swig.
