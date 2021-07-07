(* M2Depth.mod maintains the dependancy graph depth.

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

IMPLEMENTATION MODULE M2Depth ;


FROM Storage IMPORT ALLOCATE ;
FROM StrLib IMPORT StrEqual, StrCopy ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NameKey IMPORT Name, WriteKey ;


CONST
   MaxNoOfSons  =  100 ;
   MaxNoOfFiles = 1000 ;

TYPE
   PtrToNode = POINTER TO Node ;
   Node      = RECORD
                  Index    : CARDINAL ;  (* Index into Source *)
                  NoOfSons : CARDINAL ;
                  Sons     : ARRAY [0..MaxNoOfSons] OF PtrToNode ;
                  Father   : PtrToNode ; (* Father of this node *)
               END ;

   (* A Son field element may be NIL, in which case the son has been *)
   (* replaced in another part of the dependancy tree.               *)

   Source    = RECORD
                  name       : Name ;
                  Depth      : CARDINAL ;
                  SourceNode : PtrToNode ;  (* Node which corresponds to *)
                                            (* source.                   *)
               END ;


VAR
   FileNo   : CARDINAL ;
   Files    : ARRAY [1..MaxNoOfFiles] OF Source ;


PROCEDURE NewNode (VAR n: PtrToNode) ;
BEGIN
   NEW( n ) ;
   WITH n^ DO
      NoOfSons := 0
   END
END NewNode ;


PROCEDURE NewSource (VAR s: CARDINAL) ;
BEGIN
   IF FileNo=MaxNoOfFiles
   THEN
      WriteString('too many source files, increase MaxNoOfFiles in M2Depth.mod') ;
      WriteLn ;
      HALT
   ELSE
      INC( FileNo ) ;
      s := FileNo
   END
END NewSource ;


PROCEDURE GetModuleID (name: Name) : CARDINAL ;
VAR
   i  : CARDINAL ;
   Mod: PtrToNode ;
BEGIN
   i := 1 ;
   WHILE i<=FileNo DO
      IF name=Files[i].name
      THEN
         RETURN( i )
      ELSE
         INC(i)
      END
   END ;
   NewNode( Mod ) ;
   Mod^.Father := NIL ;
   NewSource( Mod^.Index ) ;
   WITH Files[Mod^.Index] DO
      Depth := 0 ;
      SourceNode := Mod
   END ;
   Files[Mod^.Index].name := name ;
   RETURN( Mod^.Index )
END GetModuleID ;


(*
   MakeDependant - makes DepandantName a son of ModuleName.
*)

PROCEDURE MakeDependant (ModuleName, DependantName: Name) ;
VAR
   DependantId,
   ModuleId   : CARDINAL ;
BEGIN
   ModuleId := GetModuleID(ModuleName) ;
   DependantId := GetModuleID(DependantName) ;
   (*
   WriteString('Making ') ; WriteKey(ModuleName) ;
   WriteString(' <--- ') ; WriteKey(DependantName) ; WriteLn ;
   *)
   CreateSon( Files[DependantId].SourceNode, Files[ModuleId].SourceNode,
              GetNodeDepth(Files[ModuleId].SourceNode)+1 ) ;
END MakeDependant ;


(*
   GetDepth - returns the depth of a module.
*)

PROCEDURE GetDepth (ModuleName: Name) : CARDINAL ;
VAR
   ModuleId: CARDINAL ;
BEGIN
   ModuleId := GetModuleID(ModuleName) ;
   RETURN( GetNodeDepth(Files[ModuleId].SourceNode) )
END GetDepth ;


PROCEDURE CreateSon (son, father: PtrToNode; Level: CARDINAL) ;
BEGIN
   IF GetNodeDepth(son)<Level
   THEN
      (*
         Move SubTree son[i] furthur down the Tree to
         be a son of father. But since son is currently
         higher up the Tree we must make certain that father is
         not a 'IsSubNode' of son.
      *)
      IF IsSubNode( son, father )
      THEN
         (*
         WriteString('Found Circular Import: ') ; WriteLn ;
         DisplayTree
         *)
      ELSE
         (*
         WriteString('Moving subtree') ; WriteLn ;
         *)
         (*
            Move son SubTree to be a son of father.
            Need to delete son from its father.
         *)
         DeleteFromFather( son ) ;

         (* Delete Pseudo sons - only need proper son if available *)
         DeleteSons( father, son ) ;

         (*
            Now need to increase the Depth of son and his children.
            All need to increased by Level-GetNodeDepth(son)
         *)
         (* IncreaseLevel( son, Level-GetNodeDepth(son), Level ) ; *)

         (* Now add Files[i].SourceNode to be a son of father.     *)
         AddSon( father, son ) ;
         son^.Father := father ;

      END
   ELSE
      (*
         sons Depth is greater than fathers therefore add son to be
         a child of father.
      *)
      IF NOT IsSon( father, son )
      THEN
         AddSon( father, son ) ;
         son^.Father := father  (* added this ? *)
      END
   END
END CreateSon ;


(*
   GetNodeDepth - returns the depth of node son.
*)

PROCEDURE GetNodeDepth (son: PtrToNode) : CARDINAL ;
VAR
   Top: PtrToNode ;
   i  : CARDINAL ;
BEGIN
   (* RETURN( Files[son^.Index].Depth ) *)
   Top := Files[1].SourceNode ;
   i := 1 ;
   WHILE (son#Top) AND (son#NIL) DO
      IF son=NIL
      THEN
         WriteString('Son does not have a father') ; WriteLn ; HALT
      ELSE
         INC(i) ;
         son := son^.Father
      END
   END ;
   RETURN( i )
END GetNodeDepth ;


(*
   IsSon - returns true if son is a direct son of father.
*)

PROCEDURE IsSon (father, son: PtrToNode) : BOOLEAN ;
VAR
   i: CARDINAL ;
   t: PtrToNode ;
BEGIN
   i := 1 ;
   WHILE i<=father^.NoOfSons DO
      t := father^.Sons[i] ;
      IF (t#NIL) AND (t=son)
      THEN
         RETURN( TRUE )
      END ;
      INC(i)
   END ;
   RETURN( FALSE )
END IsSon ;


PROCEDURE DeleteSons (father, son: PtrToNode) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE (i<=father^.NoOfSons) DO
      IF father^.Sons[i]=son
      THEN
         father^.Sons[i] := NIL
      END ;
      INC(i)
   END
END DeleteSons ;


(*
   IsSubNode - returns TRUE if son is a desendant of father, otherwise
               FALSE is returned.
*)

PROCEDURE IsSubNode (father, son: PtrToNode) : BOOLEAN ;
VAR
   i     : CARDINAL ;
   found : BOOLEAN ;
   t     : PtrToNode ;
BEGIN
   ENTER ;
   IF father=son
   THEN
      found := TRUE
   ELSE
      i := 1 ;
      found := FALSE ;
      WHILE (i<=father^.NoOfSons) AND (NOT found) DO
         t := father^.Sons[i] ;
         IF (t#NIL) AND (t^.Father=father)
         THEN
            found := IsSubNode( t, son )
         END ;
         INC( i )
      END
   END ;
 ; LEAVE ;
   RETURN( found )
END IsSubNode ;


(*
   DeleteFromFather - deletes all entries for son from its Father.
*)

PROCEDURE DeleteFromFather (son: PtrToNode) ;
VAR
   father: PtrToNode ;
   i     : CARDINAL ;
BEGIN
   father := son^.Father ;
   IF father#NIL
   THEN
      i := 1 ;
      WHILE i<=father^.NoOfSons DO
         IF father^.Sons[i]=son
         THEN
            father^.Sons[i] := NIL
         END ;
         INC(i)
      END
   END
END DeleteFromFather ;


(*
   IncreaseLevel - adds inc to the Depth field for all n and its subtree,
                   providing that the subtree has a Depth > Threshold.
*)

PROCEDURE IncreaseLevel (n: PtrToNode ; inc, Threshold: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   ENTER ;
   INC( Files[n^.Index].Depth, inc ) ;
   i := 1 ;
   WHILE i<=n^.NoOfSons DO
      IF (n^.Sons[i]#NIL) AND (GetNodeDepth(n^.Sons[i])>Threshold)
      THEN
         IncreaseLevel( n^.Sons[i], inc, Threshold )
      END ;
      INC( i )
   END
 ; LEAVE
END IncreaseLevel ;


(* AddSon --- adds son to be a son of father, it only alters the Son array *)
(* and the NoOfSons but no other fields. This procedure should be used     *)
(* since it reuses maybe NIL fields within the Son ARRAY. Caused by moving *)
(* subtrees of the dependancy graph.                                       *)

PROCEDURE AddSon (father, son: PtrToNode) ;
VAR
  i    : CARDINAL ;
  Found: BOOLEAN ;
BEGIN
   Found := FALSE ;
   i := 1 ;
   WHILE (i<=father^.NoOfSons) AND (father^.Sons[i]#NIL) AND (NOT Found) DO
      Found := (father^.Sons[i]=son) ;
      INC(i)
   END ;
   IF NOT Found
   THEN
      IF i<=father^.NoOfSons
      THEN
         father^.Sons[i] := son   (* Resuse a NIL field *)
      ELSIF father^.NoOfSons<MaxNoOfSons
      THEN
         (* Add son at the end of the ARRAY *)
         INC( father^.NoOfSons ) ;
         father^.Sons[father^.NoOfSons] := son
      ELSE
         (* Run out of space -- Error *)
         WriteString('Too many sons --- Too many imports in one file') ; WriteLn ;
         WriteString('Increase MaxNoOfSons in M2Depth') ; WriteLn ;
         HALT
      END
   END
END AddSon ;


PROCEDURE DisplayTree ;
BEGIN
   DisplaySource ;
   (* DisplayTreeFile(1) *)
END DisplayTree ;


PROCEDURE DisplayTreeFile (f: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   WITH Files[f] DO
      i := 0 ;
      WHILE i<Depth DO
         WriteString('  ') ;
         INC(i)
      END ;
      WriteKey(name) ; WriteLn ;
      i := 1 ;
      WITH SourceNode^ DO
         WHILE i<=NoOfSons DO
            IF (Sons[i]#NIL) AND (Depth<GetNodeDepth(Sons[i]))
            THEN
               DisplayTreeFile(Sons[i]^.Index)
            END ;
            INC(i)
         END
      END
   END
END DisplayTreeFile ;


PROCEDURE DisplayGraph ;
VAR
   i   : CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=FileNo DO
      WITH Files[i] DO
         IF SourceNode#NIL
         THEN
            DisplaySons(SourceNode)
         END
      END ;
      INC(i)
   END
END DisplayGraph ;


PROCEDURE DisplaySource ;
VAR
   i, j: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=FileNo DO
      WITH Files[i] DO
         j := 0 ;
         WHILE j<Depth DO
            WriteString('  ') ;
            INC(j)
         END ;
         WriteKey(name) ; WriteLn ;
(*
      WriteCard(i, 3) ;
      WriteKey(Files[i].name) ; WriteString(' : Depth') ;
      WriteCard( Files[i].Depth, 6 ) ;
      WriteLn ;
*)
      END ;
      INC(i)
   END
END DisplaySource ;


PROCEDURE DisplaySons (n: PtrToNode) ;
VAR
   i, j: CARDINAL ;
   t   : PtrToNode ;
BEGIN
   WriteKey(Files[n^.Index].name) ;
   IF n^.NoOfSons>0
   THEN
      WriteString(' --> ') ;
      i := 1 ;
      WHILE i<=n^.NoOfSons DO
         t := n^.Sons[i] ;
         IF t#NIL
         THEN
            j := t^.Index ;
            WriteString('  ') ;
            WriteKey(Files[j].name)
         END ;
         INC( i )
      END
   END ;
   WriteLn ;
END DisplaySons ;


VAR
   LEVEL : CARDINAL ;


PROCEDURE ENTER ;
BEGIN
   INC(LEVEL) ;
   IF LEVEL=10000
   THEN
      WriteString('stack overflow') ;
      DisplayTree ;
      HALT
   END
END ENTER ;


PROCEDURE LEAVE ;
BEGIN
   DEC(LEVEL)
END LEAVE ;


BEGIN
   LEVEL := 0 ;
   FileNo := 0 ;
END M2Depth.
