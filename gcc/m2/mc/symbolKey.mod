(* symbolKey.mod provides binary tree operations for storing symbols.

Copyright (C) 2015-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

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

IMPLEMENTATION MODULE symbolKey ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM Debug IMPORT Halt ;

FROM nameKey IMPORT writeKey ;


TYPE
   symbolTree = POINTER TO RECORD
                   name : Name ;   (* The sorted entity *)
                   key  : ADDRESS ;   (* The value entity  *)
                   left,
                   right: symbolTree ;
                END ;


PROCEDURE initTree () : symbolTree ;
VAR
   t: symbolTree ;
BEGIN
   NEW (t) ;
   WITH t^ DO
      left := NIL ;
      right := NIL
   END ;
   RETURN t
END initTree ;


PROCEDURE killTree (VAR t: symbolTree) ;
BEGIN
   IF t#NIL
   THEN
      killTree (t^.left) ;
      killTree (t^.right) ;
      DISPOSE (t) ;
      t := NIL
   END
END killTree ;


PROCEDURE getSymKey (t: symbolTree; name: Name) : ADDRESS ;
VAR
   father,
   child : symbolTree ;
BEGIN
   IF t=NIL
   THEN
      RETURN NulKey
   ELSE
      findNodeAndParentInTree (t, name, child, father) ;
      IF child=NIL
      THEN
         RETURN NulKey
      ELSE
         RETURN child^.key
      END
   END
END getSymKey ;


PROCEDURE putSymKey (t: symbolTree; name: Name; key: ADDRESS) ;
VAR
   father,
   child : symbolTree ;
BEGIN
   findNodeAndParentInTree (t, name, child, father) ;
   IF child=NIL
   THEN
      (* no child found, now is name less than father or greater? *)
      IF father=t
      THEN
         (* empty tree, add it to the left branch of t *)
         NEW(child) ;
         father^.left := child
      ELSE
         IF name<father^.name
         THEN
            NEW (child) ;
            father^.left := child
         ELSIF name>father^.name
         THEN
            NEW (child) ;
            father^.right := child
         END
      END ;
      WITH child^ DO
         right   := NIL ;
         left    := NIL
      END ;
      child^.key  := key ;
      child^.name := name
   ELSE
      Halt ('symbol already stored', __FILE__, __FUNCTION__, __LINE__)
   END
END putSymKey ;


(*
   delSymKey - deletes an entry in the binary tree.

               NB in order for this to work we must ensure that the InitTree sets
               both left and right to NIL.
*)

PROCEDURE delSymKey (t: symbolTree; name: Name) ;
VAR
   i, child, father: symbolTree ;
BEGIN
   findNodeAndParentInTree (t, name, child, father) ;  (* find father and child of the node *)
   IF (child#NIL) AND (child^.name=name)
   THEN
      (* Have found the node to be deleted *)
      IF father^.right=child
      THEN
         (* Node is child and this is greater than the father. *)
         (* Greater being on the right.                        *)
         (* Connect child^.left onto the father^.right.        *)
         (* Connect child^.right onto the end of the right     *)
         (* most branch of child^.left.                        *)
         IF child^.left#NIL
         THEN
            (* Scan for right most node of child^.left *)
            i := child^.left ;
            WHILE i^.right#NIL DO
               i := i^.right
            END ;
            i^.right      := child^.right ;
            father^.right := child^.left
         ELSE
            (* No child^.left node therefore link over child   *)
            (* (as in a single linked list) to child^.right    *)
            father^.right := child^.right
         END ;
         DISPOSE (child)
      ELSE
         (* Assert that father^.left=child will always be true *)
         (* Perform exactly the mirror image of the above code *)

         (* Connect child^.right onto the father^.left.        *)
         (* Connect child^.left onto the end of the left most  *)
         (* branch of child^.right                             *)
         IF child^.right#NIL
         THEN
            (* Scan for left most node of child^.right *)
            i := child^.right ;
            WHILE i^.left#NIL DO
               i := i^.left
            END ;
            i^.left      := child^.left ;
            father^.left := child^.right
         ELSE
            (* No child^.right node therefore link over c      *)
            (* (as in a single linked list) to child^.left.    *)
            father^.left := child^.left
         END ;
         DISPOSE (child)
      END
   ELSE
      Halt ('trying to delete a symbol that is not in the tree - the compiler never expects this to occur',
            __FILE__, __FUNCTION__, __LINE__)
   END
END delSymKey ;


(*
   findNodeAndParentInTree - find a node, child, in a binary tree, t, with name equal to n.
                             if an entry is found, father is set to the node above child.
*)

PROCEDURE findNodeAndParentInTree (t: symbolTree; n: Name;
                                   VAR child, father: symbolTree) ;
BEGIN
   (* remember to skip the sentinal value and assign father and child *)
   father := t ;
   IF t=NIL
   THEN
      Halt ('parameter t should never be NIL', __FILE__, __FUNCTION__, __LINE__)
   END ;
   child := t^.left ;
   IF child#NIL
   THEN
      REPEAT
         IF n<child^.name
         THEN
            father := child ;
            child := child^.left
         ELSIF n>child^.name
         THEN
            father := child ;
            child := child^.right
         END
      UNTIL (child=NIL) OR (n=child^.name)
   END
END findNodeAndParentInTree ;


(*
   isEmptyTree - returns true if symbolTree, t, is empty.
*)

PROCEDURE isEmptyTree (t: symbolTree) : BOOLEAN ;
BEGIN
   RETURN t^.left=NIL
END isEmptyTree ;


(*
   doesTreeContainAny - returns true if symbolTree, t, contains any
                        symbols which in turn return true when procedure,
                        p, is called with a symbol as its parameter.
                        The symbolTree root is empty apart from the field,
                        left, hence we need two procedures.
*)

PROCEDURE doesTreeContainAny (t: symbolTree; p: isSymbol) : BOOLEAN ;
BEGIN
   RETURN searchForAny (t^.left, p)
END doesTreeContainAny ;


(*
   searchForAny - performs the search required for doesTreeContainAny.
                  The root node always contains a nul data value,
                  therefore we must skip over it.
*)

PROCEDURE searchForAny (t: symbolTree; p: isSymbol) : BOOLEAN ;
BEGIN
   IF t=NIL
   THEN
      RETURN FALSE
   ELSE
      RETURN p (t^.key) OR
             searchForAny (t^.left, p) OR
             searchForAny (t^.right, p)
   END
END searchForAny ;


(*
   foreachNodeDo - for each node in symbolTree, t, a procedure, p,
                   is called with the node symbol as its parameter.
                   The tree root node only contains a legal left pointer,
                   therefore we need two procedures to examine this tree.
*)

PROCEDURE foreachNodeDo (t: symbolTree; p: performOperation) ;
BEGIN
   searchAndDo (t^.left, p)
END foreachNodeDo ;


(*
   searchAndDo - searches all the nodes in symbolTree, t, and
                 calls procedure, p, with a node as its parameter.
                 It traverse the tree in order.
*)

PROCEDURE searchAndDo (t: symbolTree; p: performOperation) ;
BEGIN
   IF t#NIL
   THEN
      WITH t^ DO
         searchAndDo (right, p) ;
         p (key) ;
         searchAndDo (left, p)
      END
   END
END searchAndDo ;


END symbolKey.
