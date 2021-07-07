(* RTentity.mod implements a grouping of different opaque types.

Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE RTentity ;

FROM libc IMPORT malloc, free ;
FROM M2RTS IMPORT Halt ;
FROM RTco IMPORT init, initSemaphore, wait, signal ;

TYPE
   Group = POINTER TO RECORD
                         left, right:  Group ;
                         entity     :  SYSTEM.ADDRESS ;
                         entityKey  :  CARDINAL ;
                      END ;


PROCEDURE InitGroup () : Group ;
VAR
   g: Group ;
BEGIN
   checkInitialised ;
   wait (mutex) ;
   g := malloc (SIZE (g^)) ;
   WITH g^ DO
      left := NIL ;
      right := NIL ;
      entity := NIL ;
      entityKey := 0
   END ;
   signal (mutex) ;
   RETURN g
END InitGroup ;


(*
   killGroup -
*)

PROCEDURE killGroup (g: Group) : Group ;
BEGIN
   IF g # NIL
   THEN
      WITH g^ DO
         left := killGroup (left) ;
         right := killGroup (right)
      END ;
      free (g)
   END ;
   RETURN NIL
END killGroup ;



PROCEDURE KillGroup (g: Group) : Group ;
BEGIN
   wait (mutex) ;
   g := killGroup (g) ;
   signal (mutex) ;
   RETURN g
END KillGroup ;


PROCEDURE GetKey (g: Group; a: SYSTEM.ADDRESS) : CARDINAL ;
VAR
   parent,
   child : Group ;
BEGIN
   wait (mutex) ;
   findChildAndParent (g, a, child, parent) ;
   signal (mutex) ;
   IF child = NIL
   THEN
      RETURN 0
   ELSE
      RETURN child^.entityKey
   END
END GetKey ;


PROCEDURE PutKey (g: Group; a: SYSTEM.ADDRESS; key: CARDINAL) ;
VAR
   parent,
   child : Group ;
BEGIN
   wait (mutex) ;
   findChildAndParent (g, a, child, parent) ;
   IF child = NIL
   THEN
      (* no child found, now is, a, less than parent or greater? *)
      IF parent = g
      THEN
         (* empty tree, add it to the left branch of t *)
         child := malloc (SIZE (child^)) ;
         parent^.left := child
      ELSE
         (* parent is a leaf node *)
         IF a < parent^.entity
         THEN
            child := malloc (SIZE (child^)) ;
            parent^.left := child
         ELSIF a > parent^.entity
         THEN
            child := malloc (SIZE (child^)) ;
            parent^.right := child
         END
      END ;
      WITH child^ DO
         right     := NIL ;
         left      := NIL ;
         entity    := a ;
         entityKey := key
      END
   ELSE
      Halt (__FILE__, __LINE__, __FUNCTION__,
            'internal runtime error, entity already stored')
   END ;
   signal (mutex)
END PutKey ;


PROCEDURE IsIn (g: Group; a: SYSTEM.ADDRESS) : BOOLEAN ;
VAR
   child, parent: Group ;
BEGIN
   wait (mutex) ;
   findChildAndParent (g, a, child, parent) ;
   signal (mutex) ;
   RETURN child # NIL
END IsIn ;


(*
   DelKey - deletes an entry in the binary tree.

            NB in order for this to work we must
            ensure that the InitGroup sets
            both left and right to NIL.
*)

PROCEDURE DelKey (g: Group; a: SYSTEM.ADDRESS) ;
VAR
   i, child, parent: Group ;
BEGIN
   wait (mutex) ;
   (* find parent and child of the node *)
   findChildAndParent (g, a, child, parent) ;
   IF (child # NIL) AND (child^.entity = a)
   THEN
      (* Have found the node to be deleted *)
      IF parent^.right = child
      THEN
         (* Node is child and this is greater than the parent. *)
         (* Greater being on the right.                        *)
         (* Connect child^.left onto the parent^.right.        *)
         (* Connect child^.right onto the end of the right     *)
         (* most branch of child^.left.                        *)
         IF child^.left # NIL
         THEN
            (* Scan for right most node of child^.left *)
            i := child^.left ;
            WHILE i^.right # NIL DO
               i := i^.right
            END ;
            i^.right      := child^.right ;
            parent^.right := child^.left
         ELSE
            (* No child^.left node therefore link over child   *)
            (* (as in a single linked list) to child^.right    *)
            parent^.right := child^.right
         END ;
         free (child)
      ELSE
         (* Assert that parent^.left=child will always be true *)
         (* Perform exactly the mirror image of the above code *)

         (* Connect child^.right onto the parent^.left.        *)
         (* Connect child^.left onto the end of the left most  *)
         (* branch of child^.right                             *)
         IF child^.right # NIL
         THEN
            (* Scan for left most node of child^.right *)
            i := child^.right ;
            WHILE i^.left # NIL DO
               i := i^.left
            END ;
            i^.left      := child^.left ;
            parent^.left := child^.right
         ELSE
            (* No child^.right node therefore link over c      *)
            (* (as in a single linked list) to child^.left.    *)
            parent^.left := child^.left
         END ;
         free (child)
      END
   ELSE
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'internal runtime error, trying to delete an entity which is not in the tree')
   END ;
   signal (mutex)
END DelKey ;


(*
   findChildAndParent - find a node, child, in a binary tree, t, with name
                        equal to n.  If an entry is found, parent is set
                        to the node above child.
*)

PROCEDURE findChildAndParent (t: Group; a: SYSTEM.ADDRESS;
                              VAR child, parent: Group) ;
BEGIN
   (* remember to skip the sentinal value and assign parent and child *)
   parent := t ;
   IF t = NIL
   THEN
      Halt (__FILE__, __LINE__, __FUNCTION__,
            'internal runtime error, RTentity is either corrupt or the module storage has not been initialized yet')
   END ;
   child := t^.left ;
   IF child # NIL
   THEN
      REPEAT
         IF a < child^.entity
         THEN
            parent := child ;
            child := child^.left
         ELSIF a > child^.entity
         THEN
            parent := child ;
            child := child^.right
         END
      UNTIL (child = NIL) OR (a = child^.entity)
   END
END findChildAndParent ;


(*
   checkInitialised -
*)

PROCEDURE checkInitialised ;
VAR
   result: INTEGER ;
BEGIN
   IF NOT initialized
   THEN
      initialized := TRUE ;
      result := init () ;
      mutex := initSemaphore (1)
   END
END checkInitialised ;


VAR
   initialized: BOOLEAN ;
   mutex      : INTEGER ;
BEGIN
   initialized := FALSE
END RTentity.
