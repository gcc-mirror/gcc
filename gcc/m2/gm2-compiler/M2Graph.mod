(* M2Graph.mod maintains the dependancy graph depth.

Copyright (C) 2022-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Graph ;


FROM Storage IMPORT ALLOCATE ;
FROM StrLib IMPORT StrEqual, StrCopy ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NameKey IMPORT Name, WriteKey ;
FROM Lists IMPORT InitList, KillList, IncludeItemIntoList, RemoveItemFromList ;
FROM Indexing IMPORT Index, HighIndice, IncludeIndiceIntoIndex, InitIndex, KillIndex, GetIndice ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3 ;
FROM SymbolTable IMPORT GetSymName, GetLibName, IsDefinitionForC, IsModule ;


CONST
   Debugging = FALSE ;

TYPE
   state = (initial, started, ordered) ;

   node = POINTER TO RECORD
             moduleSym: CARDINAL ;  (* SymbolTable entry for module.  *)
             deps     : Index ;
             nstate   : state ;
          END ;

   Graph = POINTER TO RECORD
                         nodes: Index ;
                      END ;


(*
   InitGraph - creates and returns an empty graph.
*)

PROCEDURE InitGraph () : Graph ;
VAR
   g: Graph ;
BEGIN
   NEW (g) ;
   g^.nodes := InitIndex (1) ;
   RETURN g
END InitGraph ;


(*
   KillNode - deletes the dynamic storage associated with nptr.
*)

PROCEDURE KillNode (nptr: node) ;
BEGIN
   nptr^.deps := KillIndex (nptr^.deps)
END KillNode ;


(*
   KillGraph - deletes graph and all nodes.
*)

PROCEDURE KillGraph (VAR g: Graph) ;
VAR
   i, n: CARDINAL ;
   nptr: node ;
BEGIN
   n := HighIndice (g^.nodes) ;
   i := 1 ;
   WHILE i <= n DO
      nptr := GetIndice (g^.nodes, i) ;
      KillNode (nptr) ;
      INC (i)
   END ;
   g := NIL
END KillGraph ;


(*
   initNode - create a new node in graph and return the node.
*)

PROCEDURE initNode (graph: Graph; moduleSym: CARDINAL) : node ;
VAR
   nptr: node ;
BEGIN
   NEW (nptr) ;
   nptr^.moduleSym := moduleSym ;
   nptr^.deps := InitIndex (1) ;
   nptr^.nstate := initial ;
   IncludeIndiceIntoIndex (graph^.nodes, nptr) ;
   RETURN nptr
END initNode ;


(*
   getNode - returns a node from graph representing moduleSym.
             If the node does not exist it is created.
*)

PROCEDURE getNode (graph: Graph; moduleSym: CARDINAL) : node ;
VAR
   i, n: CARDINAL ;
   nptr: node ;
BEGIN
   i := 1 ;
   n := HighIndice (graph^.nodes) ;
   WHILE i <= n DO
      nptr := GetIndice (graph^.nodes, i) ;
      IF nptr^.moduleSym = moduleSym
      THEN
         RETURN nptr
      END ;
      INC (i)
   END ;
   RETURN initNode (graph, moduleSym)
END getNode ;


(*
   createDependent - mptr imports from dptr.
*)

PROCEDURE createDependent (mptr, dptr: node) ;
BEGIN
   IncludeIndiceIntoIndex (mptr^.deps, dptr)
END createDependent ;


(*
   AddDependent - adds moduleSym <- dependSym into the graph.
*)

PROCEDURE AddDependent (graph: Graph; moduleSym, dependSym: CARDINAL) ;
VAR
   mptr, dptr: node ;
BEGIN
   IF (IsModule (moduleSym) OR (NOT IsDefinitionForC (moduleSym))) AND
      (IsModule (dependSym) OR (NOT IsDefinitionForC (dependSym)))
   THEN
      mptr := getNode (graph, moduleSym) ;
      dptr := getNode (graph, dependSym) ;
      createDependent (mptr, dptr)
   END
END AddDependent ;


(*
   SortGraph - returns a List containing the sorted graph.
*)

PROCEDURE SortGraph (g: Graph; topModule: CARDINAL) : List ;
VAR
   sorted: List ;
   nptr  : node ;
BEGIN
   InitList (sorted) ;
   setNodesInitial (g) ;
   nptr := getNode (g, topModule) ;
   resolveImports (sorted, nptr) ;
   RemoveItemFromList (sorted, topModule) ;
   IncludeItemIntoList (sorted, topModule) ;  (* Ensure topModule is last.  *)
   RETURN sorted
END SortGraph ;


(*
   resolveImports - recursively resolve imports using ISO Modula-2
                    rules for the order of module initialization.
*)

PROCEDURE resolveImports (sorted: List; nptr: node) ;
VAR
   i, n: CARDINAL ;
   libname,
   name   : Name ;
BEGIN
   IF nptr^.nstate = initial
   THEN
      nptr^.nstate := started ;
      name := GetSymName (nptr^.moduleSym) ;
      libname := GetLibName (nptr^.moduleSym) ;
      i := 1 ;
      n := HighIndice (nptr^.deps) ;
      IF Debugging
      THEN
         printf3 ("resolving %a [%a] %d dependents\n", name, libname, n)
      END ;
      WHILE i <= n DO
         resolveImports (sorted, GetIndice (nptr^.deps, i)) ;
         INC (i)
      END ;
      nptr^.nstate := ordered ;
      IncludeItemIntoList (sorted, nptr^.moduleSym)
   END
END resolveImports ;


(*
   setNodesInitial - changes the state of all nodes in graph to initial.
*)

PROCEDURE setNodesInitial (g: Graph) ;
VAR
   i, n: CARDINAL ;
   nptr: node ;
BEGIN
   i := 1 ;
   n := HighIndice (g^.nodes) ;
   WHILE i <= n DO
      nptr := GetIndice (g^.nodes, i) ;
      nptr^.nstate := initial ;
      INC (i)
   END
END setNodesInitial ;


END M2Graph.
