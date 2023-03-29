(* M2Scope.mod derive the subset of quadruples for each scope.

Copyright (C) 2003-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Scope ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Debug IMPORT Assert ;
FROM NameKey IMPORT Name ;

FROM SymbolTable IMPORT IsProcedure, IsDefImp, GetProcedureQuads, GetScope,
                        GetProcedureScope, IsModule, IsModuleWithinProcedure,
                        GetSymName, GetErrorScope, NulSym ;

FROM M2Options IMPORT DisplayQuadruples ;
FROM M2Printf IMPORT printf0, printf1 ;
FROM M2Quads IMPORT QuadOperator, GetFirstQuad, GetNextQuad, GetQuad, DisplayQuadRange ;
FROM M2StackWord IMPORT StackOfWord, InitStackWord, KillStackWord,
                        PopWord, PushWord, PeepWord ;
IMPORT M2Error ;


CONST
   Debugging = FALSE ;

TYPE
   scopeKind = (unsetscope, ignorescope, procedurescope, modulescope, definitionscope, implementationscope, programscope) ;

   ScopeBlock = POINTER TO RECORD
                   scopeSym : CARDINAL ;
                   kindScope: scopeKind ;
                   low, high: CARDINAL ;
                   next     : ScopeBlock ;
                END ;

VAR
   FreeList: ScopeBlock ;


(*
   New -
*)

PROCEDURE New (VAR sb: ScopeBlock) ;
BEGIN
   IF FreeList = NIL
   THEN
      NEW (sb)
   ELSE
      sb := FreeList ;
      FreeList := FreeList^.next
   END
END New ;


(*
   Dispose -
*)

PROCEDURE Dispose (VAR sb: ScopeBlock) ;
BEGIN
   sb^.next := FreeList ;
   FreeList := sb ;
   sb := NIL
END Dispose ;


(*
   SetScope - assigns the scopeSym and kindScope.
*)

PROCEDURE SetScope (sb: ScopeBlock; sym: CARDINAL; kindScope: scopeKind) ;
BEGIN
   sb^.scopeSym := sym ;
   sb^.kindScope := kindScope
END SetScope ;


(*
   AddToRange - returns a ScopeBlock pointer to the last block. The,
                quad, will be added to the end of sb or a later block
                if First is TRUE.
*)

PROCEDURE AddToRange (sb: ScopeBlock;
                      First: BOOLEAN; quad: CARDINAL) : ScopeBlock ;
BEGIN
   IF First
   THEN
      IF sb^.high=0
      THEN
         sb^.high := sb^.low
      END ;
      sb^.next := InitScopeBlock (NulSym) ;
      sb := sb^.next
   END ;
   IF sb^.low=0
   THEN
      sb^.low := quad
   END ;
   sb^.high := quad ;
   RETURN sb
END AddToRange ;


(*
   GetGlobalQuads -
*)

PROCEDURE GetGlobalQuads (sb: ScopeBlock; scope: CARDINAL) : ScopeBlock ;
VAR
   prev,
   nb           : ScopeBlock ;
   NestedLevel,
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   First        : BOOLEAN ;
   start, end   : CARDINAL ;
BEGIN
   NestedLevel := 0 ;
   prev := NIL ;
   First := FALSE ;
   IF (GetScope(scope)#NulSym) AND
      (IsProcedure(GetScope(scope)) OR
       (IsModule(scope) AND IsModuleWithinProcedure(scope)))
   THEN
      GetProcedureQuads (GetProcedureScope (scope), i, start, end) ;
      GetQuad (i, op, op1, op2, op3) ;
      WHILE (op#ModuleScopeOp) OR (op3#scope) DO
         i := GetNextQuad (i) ;
         GetQuad (i, op, op1, op2, op3)
      END ;
      end := i ;
      GetQuad (end, op, op1, op2, op3) ;
      WHILE (op#FinallyEndOp) OR (op3#scope) DO
         end := GetNextQuad (end) ;
         GetQuad (end, op, op1, op2, op3)
      END
   ELSE
      i := GetFirstQuad () ;
      end := 0
   END ;
   nb := sb ;
   sb^.low := 0 ;
   sb^.high := 0 ;
   LOOP
      IF i=0
      THEN
         IF Debugging
         THEN
            DisplayScope (sb)
         END ;
         RETURN sb
      END ;
      GetQuad (i, op, op1, op2, op3) ;
      IF op=ProcedureScopeOp
      THEN
         INC (NestedLevel)
      ELSIF op=ReturnOp
      THEN
         IF NestedLevel>0
         THEN
            DEC (NestedLevel)
         END ;
         IF NestedLevel=0
         THEN
            First := TRUE
         END
      ELSIF NestedLevel=0
      THEN
         IF op=StartDefFileOp
         THEN
            nb := AddToRange (nb, TRUE, i) ;
            SetScope (nb, op3, definitionscope) ;
            prev := nb
         ELSIF (op=StartModFileOp) OR (op=InitStartOp)
         THEN
            nb := AddToRange (nb, TRUE, i) ;
            IF IsDefImp (op3)
            THEN
               SetScope (nb, op3, implementationscope)
            ELSE
               SetScope (nb, op3, programscope)
            END ;
            prev := nb
         ELSE
            nb := AddToRange (nb, First, i) ;
            IF op = InitEndOp
            THEN
               IF IsDefImp (op3)
               THEN
                  SetScope (nb, op3, implementationscope)
               ELSE
                  SetScope (nb, op3, programscope)
               END ;
               prev := nb
            ELSIF First
            THEN
               Assert (prev # NIL) ;
               SetScope (nb, prev^.scopeSym, prev^.kindScope)
            END
         END ;
         First := FALSE
      END ;
      IF i=end
      THEN
         IF Debugging
         THEN
            DisplayScope (sb)
         END ;
         RETURN sb
      END ;
      i := GetNextQuad (i)
   END
END GetGlobalQuads ;


(*
   GetProcQuads -
*)

PROCEDURE GetProcQuads (sb: ScopeBlock;
                        proc: CARDINAL) : ScopeBlock ;
VAR
   nb           : ScopeBlock ;
   scope, start,
   end, i, last : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   First        : BOOLEAN ;
   s            : StackOfWord ;
   n            : Name ;
BEGIN
   s := InitStackWord () ;
   IF Debugging
   THEN
      n := GetSymName (proc) ;
      printf1("GetProcQuads for %a\n", n)
   END ;
   Assert(IsProcedure(proc)) ;
   GetProcedureQuads(proc, scope, start, end) ;
   IF Debugging
   THEN
      printf1(" proc %d\n", proc) ;
      printf1(" scope %d\n", scope) ;
      printf1(" start %d\n", start) ;
      printf1(" end %d\n", end)
   END ;
   PushWord(s, 0) ;
   First := FALSE ;
   i := scope ;
   last := scope ;
   nb := sb ;
   sb^.low := scope ;
   sb^.high := 0 ;
   SetScope (sb, proc, procedurescope) ;
   WHILE (i<=end) AND (start#0) DO
      GetQuad (i, op, op1, op2, op3) ;
      IF (op=ProcedureScopeOp) OR (op=ModuleScopeOp)
      THEN
         IF (PeepWord(s, 1)=proc) AND (op3=proc)
         THEN
            nb := AddToRange (nb, First, last) ;
            First := FALSE
         END ;
         PushWord (s,  op3) ;
         IF op=ProcedureScopeOp
         THEN
            SetScope (nb, proc, procedurescope)
         ELSE
            SetScope (nb, proc, modulescope)
         END
      ELSIF (op=ReturnOp) OR (op=FinallyEndOp)
      THEN
         op3 := PopWord (s) ;
         IF PeepWord (s, 1) = proc
         THEN
            First := TRUE
         END
      ELSE
         IF PeepWord (s, 1) = proc
         THEN
            nb := AddToRange (nb, First, i) ;
            First := FALSE
         END
      END ;
      last := i ;
      i := GetNextQuad (i)
   END ;
   IF start<=nb^.high
   THEN
      nb^.high := end
   ELSE
      nb^.next := InitScopeBlock (NulSym) ;
      nb := nb^.next ;
      SetScope (nb, proc, unsetscope) ;
      WITH nb^ DO
         low := start ;
         high := end
      END
   END ;
   s := KillStackWord (s) ;
   RETURN sb
END GetProcQuads ;


(*
   DisplayScope -
*)

PROCEDURE DisplayScope (sb: ScopeBlock) ;
VAR
   name: Name ;
BEGIN
   WITH sb^ DO
      printf0 ("scope: ") ;
      CASE sb^.kindScope OF

      unsetscope    :  printf0 ("unset") |
      ignorescope   :  printf0 ("ignore") |
      procedurescope     :  name := GetSymName (scopeSym) ;
                       printf1 ("procedure %a", name) |
      modulescope        :  name := GetSymName (scopeSym) ;
                       printf1 ("inner module %a", name) |
      definitionscope    :  name := GetSymName (scopeSym) ;
                       printf1 ("definition module %a", name) |
      implementationscope:  name := GetSymName (scopeSym) ;
                       printf1 ("implementation module %a", name) |
      programscope       :  name := GetSymName (scopeSym) ;
                       printf1 ("program module %a", name)

      END ;
      printf0 ("\n") ;
      DisplayQuadRange (low, high) ;
      IF next#NIL
      THEN
         DisplayScope (next)
      END
   END
END DisplayScope ;


(*
   InitScopeBlock -
*)

PROCEDURE InitScopeBlock (scope: CARDINAL) : ScopeBlock ;
VAR
   sb: ScopeBlock ;
BEGIN
   New (sb) ;
   WITH sb^ DO
      next := NIL ;
      kindScope := unsetscope ;
      IF scope=NulSym
      THEN
         low := 0 ;
         high := 0
      ELSE
         IF IsProcedure (scope)
         THEN
            sb := GetProcQuads (sb, scope)
         ELSE
            sb := GetGlobalQuads (sb, scope) ;
         END ;
         IF DisplayQuadruples
         THEN
            DisplayScope (sb)
         END
      END
   END ;
   RETURN sb
END InitScopeBlock ;


(*
   KillScopeBlock - destroys the ScopeBlock sb and assign sb to NIL.
*)

PROCEDURE KillScopeBlock (VAR sb: ScopeBlock) ;
VAR
   t: ScopeBlock ;
BEGIN
   t := sb ;
   WHILE t # NIL DO
      sb := t ;
      t := t^.next ;
      Dispose (sb) ;
   END ;
   sb := NIL
END KillScopeBlock ;


(*
   ForeachScopeBlockDo -
*)

PROCEDURE ForeachScopeBlockDo (sb: ScopeBlock; p: ScopeProcedure) ;
BEGIN
   IF DisplayQuadruples
   THEN
      printf0 ("ForeachScopeBlockDo\n")
   END ;
   WHILE sb#NIL DO
      WITH sb^ DO
         IF DisplayQuadruples
         THEN
            DisplayScope (sb)
         END ;
         enter (sb) ;
         IF (low # 0) AND (high # 0)
         THEN
            p (low, high)
         END ;
         leave (sb)
      END ;
      sb := sb^.next
   END ;
   IF DisplayQuadruples
   THEN
      printf0 ("end ForeachScopeBlockDo\n\n")
   END ;
END ForeachScopeBlockDo ;


(*
   enter -
*)

PROCEDURE enter (sb: ScopeBlock) ;
BEGIN
   WITH sb^ DO
      CASE kindScope OF

      unsetscope,
      ignorescope        : |
      procedurescope     ,
      modulescope        ,
      definitionscope    ,
      implementationscope,
      programscope       : M2Error.EnterErrorScope (GetErrorScope (scopeSym))

      END
   END
END enter ;


(*
   leave -
*)

PROCEDURE leave (sb: ScopeBlock) ;
BEGIN
   CASE sb^.kindScope OF

   unsetscope,
   ignorescope   : |

   ELSE
      M2Error.LeaveErrorScope
   END
END leave ;



(*
   Init - initializes the global variables for this module.
*)

PROCEDURE Init ;
BEGIN
   FreeList := NIL
END Init ;


BEGIN
   Init
END M2Scope.
