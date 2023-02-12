(* M2Batch.mod implements a queue for modules pending compilation.

Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Batch ;


FROM M2Debug IMPORT Assert ;
FROM SymbolTable IMPORT MakeModule, MakeDefImp, IsModule, IsDefImp, GetScope, GetLocalSym, GetCurrentScope, GetSym, NulSym ;
FROM NameKey IMPORT GetKey, WriteKey ;
FROM M2Printf IMPORT printf2 ;
FROM M2Error IMPORT InternalError ;
FROM M2MetaError IMPORT MetaError1 ;
FROM Indexing IMPORT Index, InitIndex, GetIndice, HighIndice, RemoveIndiceFromIndex, IncludeIndiceIntoIndex, InBounds ;
FROM Lists IMPORT List, InitList, IncludeItemIntoList, RemoveItemFromList,
                  GetItemFromList, NoOfItemsInList ;
FROM Storage IMPORT ALLOCATE ;
FROM DynamicStrings IMPORT String ;
FROM M2Pass IMPORT IsPass1, IsPass2, IsPass3, IsPassC ;


TYPE
   Module = POINTER TO RECORD
                          SymNo  : CARDINAL ;
                          Key    : Name ;
                          DefFile,
                          ModFile: String ;
                       END ;

VAR
   SeenList    : Index ;
   PendingQueue: List ;


(*
   MakeProgramSource - is given a Name, n, which is used to create a program module.
                       The program module will be placed onto the compilation
                       pending queue if it has not yet been compiled.
                       If the module has been compiled then no action is taken.
                       The Module Sym is returned.
*)

PROCEDURE MakeProgramSource (tok: CARDINAL; n: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := Get (n) ;
   IF Sym = NulSym
   THEN
      Assert ((NOT IsPass1 ()) AND (NOT IsPass2 ()) AND (NOT IsPass3 ()) AND (NOT IsPassC ())) ;
      (* Neither been compiled or on the Pending Queue *)
      Sym := MakeModule (tok, n) ;
      Put (Sym, n) ;
      Push (Sym)
   END ;
   RETURN Sym
END MakeProgramSource ;


(*
   MakeDefinitionSource - is given a Name, n, which is used to create a Definition
                          module.
                          The Definition Module will be placed onto the
                          compilation pending queue if it has not yet been
                          compiled.
                          If the module has been compiled then no action is
                          taken. The Module Sym is returned.
*)

PROCEDURE MakeDefinitionSource (tok: CARDINAL; n: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := Get (n) ;
   IF Sym = NulSym
   THEN
      Assert ((NOT IsPass1 ()) AND (NOT IsPass2 ()) AND (NOT IsPass3 ()) AND (NOT IsPassC ())) ;
      (* Neither been compiled or on the Pending Queue *)
      Sym := MakeDefImp (tok, n) ;
      Put (Sym, n) ;
      Push (Sym)
   END ;
   RETURN Sym
END MakeDefinitionSource ;


(*
   MakeImplementationSource - is given a Name, n, which is used to create an
                              implementation module.
                              The implementation Module will be placed onto
                              the compilation pending
                              queue if it has not yet been compiled.
                              If the module has been compiled then no
                              action is taken. The Module Sym is returned.
*)

PROCEDURE MakeImplementationSource (tok: CARDINAL; n: Name) : CARDINAL ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := Get (n) ;
   IF Sym = NulSym
   THEN
      Assert ((NOT IsPass1 ()) AND (NOT IsPass2 ()) AND (NOT IsPass3 ()) AND (NOT IsPassC ())) ;
      (* Neither been compiled or on the Pending Queue *)
      Sym := MakeDefImp (tok, n) ;
      Put (Sym, n) ;
      Push (Sym)
   END ;
   RETURN Sym
END MakeImplementationSource ;


(*
   GetSource - returns with the symbol Sym of the next module to be compiled.
               If Sym returns with value 0 then no module should be compiled.
*)

PROCEDURE GetSource () : CARDINAL ;
BEGIN
   RETURN Pop ()
END GetSource ;


(*
   GetModuleNo - returns with symbol number of the nth module read during Pass 1.
*)

PROCEDURE GetModuleNo (nth: CARDINAL) : CARDINAL ;
VAR
   m: Module ;
BEGIN
   Assert (nth#0) ;
   IF InBounds (SeenList, nth)
   THEN
      m := GetIndice (SeenList, nth) ;
      RETURN m^.SymNo
   ELSE
      RETURN NulSym
   END
END GetModuleNo ;


(*
   IsModuleKnown - returns TRUE if the Name n matches a module.
*)

PROCEDURE IsModuleKnown (n: Name) : BOOLEAN ;
BEGIN
   RETURN Get (n) # NulSym
END IsModuleKnown ;


(*
   Get - returns the module symbol matching name n.
*)

PROCEDURE Get (n: Name) : CARDINAL ;
VAR
   i, no: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice (SeenList) ;
   WHILE i <= no DO
      m := GetIndice (SeenList, i) ;
      WITH m^ DO
         IF Key = n
         THEN
            RETURN SymNo
         ELSE
            INC (i)
         END
      END
   END ;
   RETURN NulSym
END Get ;


PROCEDURE Put (Sym: CARDINAL; n: Name) ;
VAR
   m: Module ;
BEGIN
   NEW (m) ;
   IncludeIndiceIntoIndex (SeenList, m) ;
   WITH m^ DO
      SymNo   := Sym ;
      Key     := n ;
      DefFile := NIL ;
      ModFile := NIL
   END
END Put ;


PROCEDURE Push (Sym: CARDINAL) ;
BEGIN
   IncludeItemIntoList (PendingQueue, Sym)
END Push ;


PROCEDURE Pop () : CARDINAL ;
VAR
   n  : CARDINAL ;
   Sym: CARDINAL ;
BEGIN
   n := NoOfItemsInList (PendingQueue) ;
   IF n = 0
   THEN
      RETURN NulSym
   ELSE
      Sym := GetItemFromList (PendingQueue, n) ;
      RemoveItemFromList (PendingQueue, Sym) ;
      RETURN Sym
   END
END Pop ;


(*
   DisplayModules - a debugging routine to textually emit the names of modules in the SeenList.
*)

PROCEDURE DisplayModules ;
VAR
   m   : Module ;
   n, i: CARDINAL ;
BEGIN
   i := 1 ;
   n := HighIndice (SeenList) ;
   WHILE i<=n DO
      m := GetIndice (SeenList, i) ;
      WITH m^ DO
         printf2 ('Module %a %d\n', Key, i)
      END ;
      INC (i)
   END
END DisplayModules ;


(*
   AssociateDefinition - associate the source file, filename, with the definition module,
                         Sym.
*)

PROCEDURE AssociateDefinition (filename: String; Sym: CARDINAL) : String ;
VAR
   no, i: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice (SeenList) ;
   WHILE i <= no DO
      m := GetIndice (SeenList, i) ;
      WITH m^ DO
         IF SymNo = Sym
         THEN
            DefFile := filename ;
            RETURN filename
         ELSE
            INC (i)
         END
      END
   END ;
   InternalError ('failed to find module sym')
END AssociateDefinition ;


(*
   GetDefinitionModuleFile - returns the filename associated with the definition module, Sym.
                             It may return a temporary preprocessed file.
*)

PROCEDURE GetDefinitionModuleFile (Sym: CARDINAL) : String ;
VAR
   no, i: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice (SeenList) ;
   WHILE i <= no DO
      m := GetIndice (SeenList, i) ;
      WITH m^ DO
         IF SymNo = Sym
         THEN
            RETURN DefFile
         ELSE
            INC (i)
         END
      END
   END ;
   RETURN NIL
END GetDefinitionModuleFile ;


(*
   AssociateModule - associate the source file, filename, with the implementation/program
                     module, Sym.
*)

PROCEDURE AssociateModule (filename: String; Sym: CARDINAL) : String ;
VAR
   no, i: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice (SeenList) ;
   WHILE i<=no DO
      m := GetIndice (SeenList, i) ;
      WITH m^ DO
         IF SymNo = Sym
         THEN
            ModFile := filename ;
            RETURN filename
         ELSE
            INC (i)
         END
      END
   END ;
   InternalError ('failed to find module sym')
END AssociateModule ;


(*
   GetModuleFile - returns the filename associated with the implementation/program module, Sym.
                   It may return a temporary preprocessed file.
*)

PROCEDURE GetModuleFile (Sym: CARDINAL) : String ;
VAR
   no, i: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice (SeenList) ;
   WHILE i <= no DO
      m := GetIndice (SeenList, i) ;
      WITH m^ DO
         IF SymNo = Sym
         THEN
            RETURN ModFile
         ELSE
            INC (i)
         END
      END
   END ;
   RETURN NIL
END GetModuleFile ;


(*
   ForeachSourceModuleDo - for each source file call procedure, p.
*)

PROCEDURE ForeachSourceModuleDo (p: DoProcedure) ;
VAR
   i, no: CARDINAL ;
   m    : Module ;
BEGIN
   i := 1 ;
   no := HighIndice (SeenList) ;
   WHILE i<=no DO
      m := GetIndice (SeenList, i) ;
      WITH m^ DO
         IF ModFile # NIL
         THEN
            p (SymNo)
         END
      END ;
      INC (i)
   END
END ForeachSourceModuleDo ;


(*
   IsSourceSeen - returns TRUE if the source for the program module or
                  implementation module has been seen.
*)

PROCEDURE IsSourceSeen (sym: CARDINAL) : BOOLEAN ;
BEGIN
   Assert (IsModule (sym) OR IsDefImp (sym)) ;
   RETURN GetModuleFile (sym) # NIL
END IsSourceSeen ;


(*
   IsModuleSeen - returns TRUE if the source for module, name, has been seen.
*)

PROCEDURE IsModuleSeen (n: Name) : BOOLEAN ;
BEGIN
   RETURN Get (n) # NulSym
END IsModuleSeen ;


(*
   LookupModule - looks up a module in the current scope, if a module does not exist
                  then it creates a DefImp module.
*)

PROCEDURE LookupModule (tok: CARDINAL; n: Name) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   sym := GetSym (n) ;
   IF sym = NulSym
   THEN
      RETURN MakeDefinitionSource (tok, n)
   ELSIF IsModule (sym) OR IsDefImp (sym)
   THEN
      RETURN sym
   ELSE
      RETURN MakeDefinitionSource (tok, n)
   END
END LookupModule ;


(*
   LookupOuterModule - looks up a module in the order of: current scope, then outer scope, finally if a
                       module does not exist then it creates a DefImp module.
*)

PROCEDURE LookupOuterModule (tok: CARDINAL; n: Name) : CARDINAL ;
VAR
   outer: CARDINAL ;
   sym  : CARDINAL ;
BEGIN
   sym := GetSym (n) ;
   IF sym = NulSym
   THEN
      outer := GetScope (GetCurrentScope ()) ;
      IF outer # NulSym
      THEN
         sym := GetLocalSym (outer, n)
      END ;
      IF sym = NulSym
      THEN
         (* not a local module, so it must be refering to a definition module.  *)
         sym := MakeDefinitionSource (tok, n)
      END
   END ;
   IF IsModule (sym) OR IsDefImp (sym)
   THEN
      RETURN sym
   ELSE
      RETURN MakeDefinitionSource (tok, n)
   END
END LookupOuterModule ;


BEGIN
   InitList (PendingQueue) ;
   SeenList := InitIndex (1)
END M2Batch.
