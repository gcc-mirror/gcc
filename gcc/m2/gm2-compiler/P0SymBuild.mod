(* P0SymBuild.mod pass 0 symbol creation.

Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE P0SymBuild ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Printf IMPORT printf0, printf1, printf2 ;
FROM Lists IMPORT List, InitList, KillList, IncludeItemIntoList, RemoveItemFromList, NoOfItemsInList, GetItemFromList, IsItemInList ;
FROM M2Batch IMPORT MakeDefinitionSource, MakeProgramSource, MakeImplementationSource ;
FROM SymbolTable IMPORT NulSym, MakeInnerModule, SetCurrentModule, SetFileModule, MakeError ;
FROM NameKey IMPORT Name, NulName ;
FROM M2Quads IMPORT PushT, PushTF, PopT, PopTF, PopN, OperandT, PopTtok, PushTtok ;
FROM M2Reserved IMPORT ImportTok ;
FROM M2Debug IMPORT Assert ;
FROM M2MetaError IMPORT MetaErrorT1, MetaErrorT2, MetaError1, MetaError2 ;
FROM M2LexBuf IMPORT GetTokenNo, UnknownTokenNo ;


CONST
   Debugging = FALSE ;

TYPE
   Kind = (module, program, defimp, inner, procedure, universe, unknown) ;

   BlockInfoPtr = POINTER TO BlockInfo ;
   BlockInfo    = RECORD
                     name           : Name ;
                     kind           : Kind ;
                     sym            : CARDINAL ;
                     level          : CARDINAL ;
                     token          : CARDINAL ;      (* where the block starts.  *)
                     LocalModules,                    (* locally declared modules at the current level  *)
                     ImportedModules: List ;          (* current list of imports for the scanned module *)
                     toPC,
                     toReturn,
                     toNext,                          (* next in same level *)
                     toUp,                            (* return to outer level *)
                     toDown         : BlockInfoPtr ;  (* first of the inner level *)
                  END ;

VAR
   headBP,
   curBP : BlockInfoPtr ;
   Level : CARDINAL ;


(*
   nSpaces -
*)

PROCEDURE nSpaces (n: CARDINAL) ;
BEGIN
   WHILE n>0 DO
      printf0(" ") ;
      DEC(n)
   END
END nSpaces ;


(*
   DisplayB -
*)

PROCEDURE DisplayB (b: BlockInfoPtr) ;
BEGIN
   CASE b^.kind OF

   program  :  printf1("MODULE %a ;\n", b^.name) |
   defimp   :  printf1("DEFIMP %a ;\n", b^.name) |
   inner    :  printf1("INNER MODULE %a ;\n", b^.name) |
   procedure:  printf1("PROCEDURE %a ;\n", b^.name)

   ELSE
      HALT
   END
END DisplayB ;


(*
   DisplayBlock -
*)

PROCEDURE DisplayBlock (b: BlockInfoPtr; l: CARDINAL) ;
VAR
   a: BlockInfoPtr ;
BEGIN
   nSpaces(l) ;
   DisplayB(b) ;
   a := b^.toDown ;
   INC(l, 3) ;
   WHILE a#NIL DO
      DisplayBlock(a, l) ;
      a := a^.toNext
   END ;
   DEC(l, 3) ;
   nSpaces(l) ;
   printf1("END %a\n", b^.name)
END DisplayBlock ;


(*
   pc - an interactive debugging aid callable from gdb.
*)

PROCEDURE pc ;
BEGIN
   DisplayB(curBP)
END pc ;


(*
   Display -
*)

PROCEDURE Display ;
VAR
   b: BlockInfoPtr ;
BEGIN
   printf0("Universe of Modula-2 modules\n") ;
   IF headBP#NIL
   THEN
      b := headBP^.toDown ;
      WHILE b#NIL DO
         DisplayBlock(b, 0) ;
         b := b^.toNext
      END
   END
END Display ;


(*
   addDown - adds, b, to the down link of, a.
*)

PROCEDURE addDown (a, b: BlockInfoPtr) ;
BEGIN
   IF a^.toDown=NIL
   THEN
      a^.toDown := b
   ELSE
      a := a^.toDown ;
      WHILE a^.toNext#NIL DO
         a := a^.toNext
      END ;
      a^.toNext := b
   END
END addDown ;


(*
   GraftBlock - add a new block, b, into the tree in the correct order.
*)

PROCEDURE GraftBlock (b: BlockInfoPtr) ;
BEGIN
   Assert(curBP#NIL) ;
   Assert(ABS(Level-curBP^.level)<=1) ;
   CASE Level-curBP^.level OF

   -1:  (* returning up to the outer scope *)
        curBP := curBP^.toUp ;
        Assert(curBP^.toNext=NIL) ;
        curBP^.toNext := b |
    0:  (* add toNext *)
        Assert(curBP^.toNext=NIL) ;
        curBP^.toNext := b ;
        b^.toUp := curBP^.toUp |
   +1:  (* insert down a level *)
        b^.toUp := curBP ;  (* save return value *)
        addDown(curBP, b)

   ELSE
      HALT
   END ;
   curBP := b
END GraftBlock ;


(*
   BeginBlock - denotes the start of the next block.  We remember all imports and
                local modules and procedures created in this block.
*)

PROCEDURE BeginBlock (n: Name; k: Kind; s: CARDINAL; tok: CARDINAL) ;
VAR
   b: BlockInfoPtr ;
BEGIN
   NEW(b) ;
   WITH b^ DO
      name := n ;
      kind := k ;
      sym := s ;
      InitList(LocalModules) ;
      InitList(ImportedModules) ;
      toPC := NIL ;
      toReturn := NIL ;
      toNext := NIL ;
      toDown := NIL ;
      toUp := NIL ;
      level := Level ;
      token := tok
   END ;
   GraftBlock(b)
END BeginBlock ;


(*
   InitUniverse -
*)

PROCEDURE InitUniverse ;
BEGIN
   NEW(curBP) ;
   WITH curBP^ DO
      name := NulName ;
      kind := universe ;
      sym := NulSym ;
      InitList(LocalModules) ;
      InitList(ImportedModules) ;
      toNext := NIL ;
      toDown := NIL ;
      toUp := curBP ;
      level := Level
   END ;
   headBP := curBP
END InitUniverse ;


(*
   FlushImports -
*)

PROCEDURE FlushImports (b: BlockInfoPtr) ;
VAR
   i, n   : CARDINAL ;
   modname: Name ;
   sym    : CARDINAL ;
BEGIN
   WITH b^ DO
      i := 1 ;
      n := NoOfItemsInList(ImportedModules) ;
      WHILE i<=n DO
         modname := GetItemFromList(ImportedModules, i) ;
         sym := MakeDefinitionSource(GetTokenNo(), modname) ;
         INC(i)
      END
   END
END FlushImports ;


(*
   EndBlock - shutdown the module and create definition symbols for all imported
              modules.
*)

PROCEDURE EndBlock ;
BEGIN
   FlushImports(curBP) ;
   curBP := curBP^.toUp ;
   DEC(Level) ;
   IF Level=0
   THEN
      FlushImports(curBP)
   END
END EndBlock ;


(*
   RegisterLocalModule - register, n, as a local module.
*)

PROCEDURE RegisterLocalModule (n: Name) ;
BEGIN
   (* printf1('seen local module %a\n', n) ; *)
   WITH curBP^ DO
      IncludeItemIntoList(LocalModules, n) ;
      RemoveItemFromList(ImportedModules, n)
   END
END RegisterLocalModule ;


(*
   RegisterImport - register, n, as a module imported from either a local scope or definition module.
*)

PROCEDURE RegisterImport (n: Name) ;
VAR
   bp: BlockInfoPtr ;
BEGIN
   (* printf1('register import from module %a\n', n) ; *)
   Assert(curBP#NIL) ;
   Assert(curBP^.toUp#NIL) ;
   bp := curBP^.toUp ;   (* skip over current module *)
   WITH bp^ DO
      IF NOT IsItemInList(LocalModules, n)
      THEN
         IncludeItemIntoList(ImportedModules, n)
      END
   END
END RegisterImport ;


(*
   RegisterImports -
*)

PROCEDURE RegisterImports ;
VAR
   i, n: CARDINAL ;
BEGIN
   PopT(n) ;       (* n   = # of the Ident List *)
   IF OperandT(n+1)=ImportTok
   THEN
      (* Ident list contains Module Names *)
      i := 1 ;
      WHILE i<=n DO
         RegisterImport(OperandT(n+1-i)) ;
         INC(i)
      END
   ELSE
      (* Ident List contains list of objects *)
      RegisterImport(OperandT(n+1))
   END ;
   PopN(n+1)   (* clear stack *)
END RegisterImports ;


(*
   RegisterInnerImports -
*)

PROCEDURE RegisterInnerImports ;
VAR
   n: CARDINAL ;
BEGIN
   PopT(n) ;       (* n   = # of the Ident List *)
   IF OperandT(n+1)=ImportTok
   THEN
      (* Ident list contains list of objects, which will be seen outside the scope of this module *)
(*
      i := 1 ;
      WHILE i<=n DO
         RegisterImport(OperandT(n+1-i)) ;
         INC(i)
      END
*)
   ELSE
      (* Ident List contains list of objects, but we are importing directly from a module OperandT(n+1) *)
      RegisterImport(OperandT(n+1))
   END ;
   PopN(n+1)   (* clear stack *)
END RegisterInnerImports ;


(*
   RegisterProgramModule - register the top of stack as a program module.
*)

PROCEDURE RegisterProgramModule ;
VAR
   n  : Name ;
   sym: CARDINAL ;
   tok: CARDINAL ;
BEGIN
   Assert(Level=0) ;
   INC(Level) ;
   PopTtok (n, tok) ;
   PushTtok (n, tok) ;
   sym := MakeProgramSource(tok, n) ;
   SetCurrentModule(sym) ;
   SetFileModule(sym) ;
   BeginBlock(n, program, sym, tok)
END RegisterProgramModule ;


(*
   RegisterImplementationModule - register the top of stack as an implementation module.
*)

PROCEDURE RegisterImplementationModule ;
VAR
   n  : Name ;
   sym: CARDINAL ;
   tok: CARDINAL ;
BEGIN
   Assert(Level=0) ;
   INC(Level) ;
   PopTtok (n, tok) ;
   PushTtok (n, tok) ;
   sym := MakeImplementationSource(tok, n) ;
   SetCurrentModule(sym) ;
   SetFileModule(sym) ;
   BeginBlock(n, defimp, sym, tok)
END RegisterImplementationModule ;


(*
   RegisterDefinitionModule - register the top of stack as a definition module.
*)

PROCEDURE RegisterDefinitionModule ;
VAR
   n  : Name ;
   sym: CARDINAL ;
   tok: CARDINAL ;
BEGIN
   Assert(Level=0) ;
   INC(Level) ;
   PopTtok (n, tok) ;
   PushTtok (n, tok) ;
   sym := MakeDefinitionSource(tok, n) ;
   SetCurrentModule(sym) ;
   SetFileModule(sym) ;
   BeginBlock(n, defimp, sym, tok)
END RegisterDefinitionModule ;


(*
   RegisterInnerModule - register the top of stack as an inner module, this module name
                         will be removed from the list of outstanding imports in the
                         current module block.
*)

PROCEDURE RegisterInnerModule ;
VAR
   n  : Name ;
   tok: CARDINAL ;
BEGIN
   INC(Level) ;
   PopTtok (n, tok) ;
   PushTtok (n, tok) ;
   RegisterLocalModule(n) ;
   BeginBlock(n, inner, NulSym, tok)
END RegisterInnerModule ;


(*
   RegisterProcedure - register the top of stack as a procedure.
*)

PROCEDURE RegisterProcedure ;
VAR
   n  : Name ;
   tok: CARDINAL ;
BEGIN
   INC (Level) ;
   PopTtok (n, tok) ;
   PushTtok (n, tok) ;
   BeginBlock (n, procedure, NulSym, tok)
END RegisterProcedure ;


(*
   EndBuildProcedure - ends building a Procedure.
*)

PROCEDURE EndProcedure ;
VAR
   NameEnd, NameStart: Name ;
   end, start        : CARDINAL ;
BEGIN
   PopTtok (NameEnd, end) ;
   PopTtok (NameStart, start) ;
   Assert (start # UnknownTokenNo) ;
   Assert (end # UnknownTokenNo) ;
   IF NameEnd # NameStart
   THEN
      IF NameEnd = NulName
      THEN
         MetaErrorT1 (start,
                      'procedure name at beginning {%1Ea} does not match the name at end',
                      MakeError (start, NameStart)) ;
         MetaError1 ('procedure name at end does not match the name at beginning {%1Ea}',
                     MakeError (start, NameStart))
      ELSE
         MetaErrorT2 (start,
                      'procedure name at beginning {%1Ea} does not match the name at end {%2a}',
                      MakeError (start, curBP^.name), MakeError (end, NameEnd)) ;
         MetaErrorT2 (end,
                      'procedure name at end {%1Ea} does not match the name at beginning {%2Ea}',
                      MakeError (end, NameEnd), MakeError (start, curBP^.name))
      END
   END ;
   EndBlock
END EndProcedure ;


(*
   EndModule -
*)

PROCEDURE EndModule ;
VAR
   NameEnd, NameStart: Name ;
   end, start        : CARDINAL ;
BEGIN
   PopTtok (NameEnd, end) ;
   PopTtok (NameStart, start) ;
   Assert (start # UnknownTokenNo) ;
   Assert (end # UnknownTokenNo) ;
   IF NameEnd # NameStart
   THEN
      IF NameEnd = NulName
      THEN
         MetaErrorT1 (start,
                      'module name at beginning {%1Ea} does not match the name at end',
                      MakeError (start, NameStart)) ;
         MetaError1 ('module name at end does not match the name at beginning {%1Ea}',
                     MakeError (start, NameStart))
      ELSE
         MetaErrorT2 (start,
                      'module name at beginning {%1Ea} does not match the name at end {%2a}',
                      MakeError (start, curBP^.name), MakeError (end, NameEnd)) ;
         MetaErrorT2 (end,
                      'module name at end {%1Ea} does not match the name at beginning {%2Ea}',
                      MakeError (end, NameEnd), MakeError (start, curBP^.name))
      END
   END ;
   EndBlock
END EndModule ;


(*
   DeclareModules - declare all inner modules seen at the current block level.
*)

PROCEDURE DeclareModules ;
VAR
   b: BlockInfoPtr ;
   s: CARDINAL ;
BEGIN
   b := curBP^.toDown ;
   WHILE b#NIL DO
      IF b^.kind=inner
      THEN
         IF Debugging
         THEN
            printf1("***  declaring inner module %a\n", b^.name)
         END ;
         s := MakeInnerModule(curBP^.token, b^.name)
      END ;
      b := b^.toNext
   END
END DeclareModules ;


(*
   MoveNext -
*)

PROCEDURE MoveNext ;
VAR
   b: BlockInfoPtr ;
BEGIN
   IF curBP^.toNext#NIL
   THEN
      b := curBP^.toUp ;
      (* moving to next *)
      curBP := curBP^.toNext ;
      (* remember our return *)
      curBP^.toUp := b
   END
END MoveNext ;


(*
   MoveDown -
*)

PROCEDURE MoveDown ;
VAR
   b: BlockInfoPtr ;
BEGIN
   (* move down a level *)
   (* remember where we came from *)
   b := curBP ;
   curBP := curBP^.toDown ;
   curBP^.toUp := b
END MoveDown ;


(*
   MoveUp -
*)

PROCEDURE MoveUp ;
BEGIN
   (* move up to the outer scope *)
   curBP := curBP^.toUp ;
END MoveUp ;


(*
   Move -
*)

PROCEDURE Move ;
VAR
   b: BlockInfoPtr ;
BEGIN
   IF Level=curBP^.level
   THEN
      b := curBP^.toReturn ;
      (* moving to next *)
      curBP := curBP^.toNext ;
      (* remember our return *)
      curBP^.toReturn := b
   ELSE
      WHILE Level#curBP^.level DO
         IF Level<curBP^.level
         THEN
            (* move up to the outer scope *)
            b := curBP ;
            curBP := curBP^.toReturn ;
            curBP^.toPC := b^.toNext   (* remember where we reached *)
         ELSE
            (* move down a level *)
            (* remember where we came from *)
            b := curBP ;
            IF curBP^.toPC=NIL
            THEN
               Assert(curBP^.toDown#NIL) ;
               curBP^.toPC := curBP^.toDown
            END ;
            Assert(curBP^.toPC#NIL) ;
            curBP := curBP^.toPC ;
            curBP^.toReturn := b
         END
      END
   END
END Move ;


(*
   EnterBlock -
*)

PROCEDURE EnterBlock (n: Name) ;
BEGIN
   Assert(curBP#NIL) ;
   INC(Level) ;
   Move ;
   IF Debugging
   THEN
      nSpaces(Level*3) ;
      IF n=curBP^.name
      THEN
         printf1('block %a\n', n)
      ELSE
         printf2('seen block %a but tree has recorded %a\n', n, curBP^.name)
      END
   END ;
   Assert((n=curBP^.name) OR (curBP^.name=NulName)) ;
   DeclareModules
END EnterBlock ;


(*
   LeaveBlock -
*)

PROCEDURE LeaveBlock ;
BEGIN
   IF Debugging
   THEN
      printf1('leaving block %a ', curBP^.name)
   END ;
   DEC(Level) ;
   Move
END LeaveBlock ;


(*
   P0Init -
*)

PROCEDURE P0Init ;
BEGIN
   headBP := NIL ;
   curBP := NIL ;
   Level := 0 ;
   InitUniverse
END P0Init ;


(*
   P1Init -
*)

PROCEDURE P1Init ;
BEGIN
   IF Debugging
   THEN
      Display
   END ;
   (* curBP := headBP^.toDown ; *)
   curBP := headBP ;
   Assert(curBP#NIL) ;
   curBP^.toPC := curBP^.toDown ;
   curBP^.toReturn := curBP ;
   Level := 0
END P1Init ;


END P0SymBuild.
