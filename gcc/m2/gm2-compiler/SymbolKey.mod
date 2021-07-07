(* SymbolKey.mod binary tree operations for storing symbols.

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

IMPLEMENTATION MODULE SymbolKey ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM NameKey IMPORT WriteKey ;
FROM Debug IMPORT Halt ;


TYPE
   SymbolTree = POINTER TO Node ;
   Node       = RECORD
                   KeyName : Name ;   (* The sorted entity *)
                   KeySym  : WORD ;   (* The value entity  *)
                   Left    : SymbolTree ;
                   Right   : SymbolTree ;
                END ;


PROCEDURE stop ;
BEGIN
END stop ;

PROCEDURE InitTree (VAR t: SymbolTree) ;
BEGIN
   NEW(t) ;
   WITH t^ DO
      Left := NIL ;
      Right := NIL
   END
END InitTree ;


(*
    we used to get problems compiling KillTree below - so it was split
    into the two procedures below.


PROCEDURE KillTree (VAR t: SymbolTree) ;
BEGIN
   IF t#NIL
   THEN
      Kill(t) ;  (* Would like to place Kill in here but the compiler *)
                 (* gives a type incompatible error... so i've split  *)
                 (* the procedure into two. - Problem i think with    *)
                 (* VAR t at the top?                                 *)
      t := NIL
   END
END KillTree ;


PROCEDURE Kill (t: SymbolTree) ;
BEGIN
   IF t#NIL
   THEN
      Kill(t^.Left) ;
      Kill(t^.Right) ;
      DISPOSE(t)
   END
END Kill ;
*)


PROCEDURE KillTree (VAR t: SymbolTree) ;
BEGIN
   IF t#NIL
   THEN
      KillTree(t^.Left) ;
      KillTree(t^.Right) ;
      DISPOSE(t) ;
      t := NIL
   END
END KillTree ;


(*
   ContainsSymKey - return TRUE if tree, t, contains an entry for, NameKey.
*)

PROCEDURE ContainsSymKey (t: SymbolTree; NameKey: Name) : BOOLEAN ;
VAR
   father,
   child : SymbolTree ;
BEGIN
   FindNodeAndParentInTree(t, NameKey, child, father) ;
   RETURN child#NIL
END ContainsSymKey ;


PROCEDURE GetSymKey (t: SymbolTree; NameKey: Name) : WORD ;
VAR
   father,
   child : SymbolTree ;
BEGIN
   FindNodeAndParentInTree(t, NameKey, child, father) ;
   IF child=NIL
   THEN
      RETURN( NulKey )
   ELSE
      RETURN( child^.KeySym )
   END
END GetSymKey ;


PROCEDURE PutSymKey (t: SymbolTree; NameKey: Name; SymKey: WORD) ;
VAR
   father,
   child : SymbolTree ;
BEGIN
   FindNodeAndParentInTree(t, NameKey, child, father) ;
   IF child=NIL
   THEN
      (* no child found, now is NameKey less than father or greater? *)
      IF father=t
      THEN
         (* empty tree, add it to the left branch of t *)
         NEW(child) ;
         father^.Left := child
      ELSE
         IF NameKey<father^.KeyName
         THEN
            NEW(child) ;
            father^.Left := child
         ELSIF NameKey>father^.KeyName
         THEN
            NEW(child) ;
            father^.Right := child
         END
      END ;
      WITH child^ DO
         Right   := NIL ;
         Left    := NIL ;
         KeySym  := SymKey ;
         KeyName := NameKey
      END
   ELSE
      Halt('symbol already stored', __LINE__, __FILE__)
   END
END PutSymKey ;


(*
   DelSymKey - deletes an entry in the binary tree.

               NB in order for this to work we must ensure that the InitTree sets
               both Left and Right to NIL.
*)

PROCEDURE DelSymKey (t: SymbolTree; NameKey: Name) ;
VAR
   i, child, father: SymbolTree ;
BEGIN
   FindNodeAndParentInTree(t, NameKey, child, father) ;  (* find father and child of the node *)
   IF (child#NIL) AND (child^.KeyName=NameKey)
   THEN
      (* Have found the node to be deleted *)
      IF father^.Right=child
      THEN
         (* Node is child and this is greater than the father. *)
         (* Greater being on the right.                        *)
         (* Connect child^.Left onto the father^.Right.        *)
         (* Connect child^.Right onto the end of the right     *)
         (* most branch of child^.Left.                        *)
         IF child^.Left#NIL
         THEN
            (* Scan for Right most node of child^.Left *)
            i := child^.Left ;
            WHILE i^.Right#NIL DO
               i := i^.Right
            END ;
            i^.Right      := child^.Right ;
            father^.Right := child^.Left
         ELSE
            (* No child^.Left node therefore link over child   *)
            (* (as in a single linked list) to child^.Right    *)
            father^.Right := child^.Right
         END ;
         DISPOSE(child)
      ELSE
         (* Assert that father^.Left=child will always be true *)
         (* Perform exactly the mirror image of the above code *)

         (* Connect child^.Right onto the father^.Left.        *)
         (* Connect child^.Left onto the end of the Left most  *)
         (* branch of child^.Right                             *)
         IF child^.Right#NIL
         THEN
            (* Scan for Left most node of child^.Right *)
            i := child^.Right ;
            WHILE i^.Left#NIL DO
               i := i^.Left
            END ;
            i^.Left      := child^.Left ;
            father^.Left := child^.Right
         ELSE
            (* No child^.Right node therefore link over c      *)
            (* (as in a single linked list) to child^.Left.    *)
            father^.Left := child^.Left
         END ;
         DISPOSE(child)
      END
   ELSE
      Halt('trying to delete a symbol that is not in the tree - the compiler never expects this to occur',
            __LINE__, __FILE__)
   END
END DelSymKey ;


(*
   FindNodeAndParentInTree - find a node, child, in a binary tree, t, with name equal to n.
                             if an entry is found, father is set to the node above child.
*)

PROCEDURE FindNodeAndParentInTree (t: SymbolTree; n: Name;
                                   VAR child, father: SymbolTree) ;
BEGIN
   (* remember to skip the sentinal value and assign father and child *)
   father := t ;
   IF t=NIL
   THEN
      Halt('parameter t should never be NIL', __LINE__, __FILE__)
   END ;
   child := t^.Left ;
   IF child#NIL
   THEN
      REPEAT
         IF n<child^.KeyName
         THEN
            father := child ;
            child := child^.Left
         ELSIF n>child^.KeyName
         THEN
            father := child ;
            child := child^.Right
         END
      UNTIL (child=NIL) OR (n=child^.KeyName)
   END
END FindNodeAndParentInTree ;


(*
   IsEmptyTree - returns true if SymbolTree, t, is empty.
*)

PROCEDURE IsEmptyTree (t: SymbolTree) : BOOLEAN ;
BEGIN
   RETURN( t^.Left=NIL )
END IsEmptyTree ;


(*
   DoesTreeContainAny - returns true if SymbolTree, t, contains any
                        symbols which in turn return true when procedure,
                        P, is called with a symbol as its parameter.
                        The SymbolTree root is empty apart from the field,
                        Left, hence we need two procedures.
*)

PROCEDURE DoesTreeContainAny (t: SymbolTree; P: IsSymbol) : BOOLEAN ;
BEGIN
   RETURN( SearchForAny(t^.Left, P) )
END DoesTreeContainAny ;


(*
   SearchForAny - performs the search required for DoesTreeContainAny.
                  The root node always contains a nul data value,
                  therefore we must skip over it.
*)

PROCEDURE SearchForAny (t: SymbolTree; P: IsSymbol) : BOOLEAN ;
BEGIN
   IF t=NIL
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( P(t^.KeySym) OR SearchForAny(t^.Left, P) OR
                              SearchForAny(t^.Right, P)
            )
   END
END SearchForAny ;


(*
   ForeachNodeDo - for each node in SymbolTree, t, a procedure, P,
                   is called with the node symbol as its parameter.
                   The tree root node only contains a legal Left pointer,
                   therefore we need two procedures to examine this tree.
*)

PROCEDURE ForeachNodeDo (t: SymbolTree; P: PerformOperation) ;
BEGIN
   SearchAndDo(t^.Left, P)
END ForeachNodeDo ;


(*
   SearchAndDo - searches all the nodes in SymbolTree, t, and
                 calls procedure, P, with a node as its parameter.
                 It traverse the tree in order.
*)

PROCEDURE SearchAndDo (t: SymbolTree; P: PerformOperation) ;
BEGIN
   IF t#NIL
   THEN
      WITH t^ DO
         SearchAndDo(Right, P) ;
         P(KeySym) ;
         SearchAndDo(Left, P)
      END
   END
END SearchAndDo ;


END SymbolKey.
