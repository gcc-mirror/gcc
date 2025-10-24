(* BinDict.mod provides a generic binary dictionary.

Copyright (C) 2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaiusmod2@gmail.com>.

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

IMPLEMENTATION MODULE BinDict ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;


TYPE
   Dictionary = POINTER TO RECORD
                              content    : Node ;
                              compare    : Compare ;
                              deleteKey,
                              deleteValue: Delete
                           END ;

   Node = POINTER TO RECORD
                        dict : Dictionary ;
                        left,
                        right: Node ;
                        key,
                        value: ADDRESS ;
                     END ;


(*
   Init - create and return a new binary dictionary which will use
          the compare procedure to order the contents as they are
          added.
*)

PROCEDURE Init (KeyCompare: Compare; KeyDelete,
                ValueDelete: Delete) : Dictionary ;
VAR
   dict: Dictionary ;
BEGIN
   NEW (dict) ;
   WITH dict^ DO
      content := NIL ;
      compare := KeyCompare ;
      deleteKey := KeyDelete ;
      deleteValue := ValueDelete
   END ;
   RETURN dict
END Init ;


(*
   Kill - delete the dictionary and its contents.
          dict is assigned to NIL.
*)

PROCEDURE Kill (VAR dict: Dictionary) ;
BEGIN
   PostOrder (dict, DeleteNode) ;
   DISPOSE (dict) ;
   dict := NIL
END Kill ;


(*
   DeleteNode - deletes node dict, key and value.
*)

PROCEDURE DeleteNode (node: Node) ;
BEGIN
   IF node # NIL
   THEN
      WITH node^ DO
         dict^.deleteKey (key) ;
         dict^.deleteValue (value)
      END ;
      DISPOSE (node)
   END
END DeleteNode ;


(*
   Insert - insert key value pair into the dictionary.
*)

PROCEDURE Insert (dict: Dictionary; key, value: ADDRESS) ;
BEGIN
   dict^.content := InsertNode (dict, dict^.content, key, value)
END Insert ;


(*
   InsertNode - insert the key value pair as a new node in the
                binary tree within dict.
*)

PROCEDURE InsertNode (dict: Dictionary;
                      node: Node;
                      key, value: ADDRESS) : Node ;
BEGIN
   IF node = NIL
   THEN
      RETURN ConsNode (dict, key, value, NIL, NIL)
   ELSE
      CASE dict^.compare (key, node^.key) OF

       0:  HALT |  (* Not expecting to replace a key value.  *)
      -1:  RETURN ConsNode (dict, node^.key, node^.value,
                            InsertNode (dict, node^.left,
                                        key, value), node^.right) |
      +1:  RETURN ConsNode (dict, node^.key, node^.value,
                            node^.left,
                            InsertNode (dict, node^.right,
                                        key, value))
      END
   END
END InsertNode ;


(*
   ConsNode - return a new node containing the pairing key and value.
              The new node fields are assigned left, right and dict.
*)

PROCEDURE ConsNode (dict: Dictionary;
                    key, value: ADDRESS;
                    left, right: Node) : Node ;
VAR
   node: Node ;
BEGIN
   NEW (node) ;
   node^.key := key ;
   node^.value := value ;
   node^.left := left ;
   node^.right := right ;
   node^.dict := dict ;
   RETURN node
END ConsNode ;


(*
   KeyExist - return TRUE if dictionary contains an entry key.
              It compares the content and not the address pointer.
*)

PROCEDURE KeyExist (dict: Dictionary; key: ADDRESS) : BOOLEAN ;
BEGIN
   RETURN KeyExistNode (dict^.content, key)
END KeyExist ;


(*
   KeyExistNode - return TRUE if the binary tree under node contains
                  key.
*)

PROCEDURE KeyExistNode (node: Node; key: ADDRESS) : BOOLEAN ;
BEGIN
   IF node # NIL
   THEN
      CASE node^.dict^.compare (key, node^.key) OF

       0:  RETURN TRUE |
      -1:  RETURN KeyExistNode (node^.left, key) |
      +1:  RETURN KeyExistNode (node^.right, key)

      END
   END ;
   RETURN FALSE
END KeyExistNode ;


(*
   Value - return the value from node.
*)

PROCEDURE Value (node: Node) : ADDRESS ;
BEGIN
   RETURN node^.value
END Value ;


(*
   Key - return the key from node.
*)

PROCEDURE Key (node: Node) : ADDRESS ;
BEGIN
   RETURN node^.value
END Key ;


(*
   Get - return the value associated with the key or NIL
         if it does not exist.
*)

PROCEDURE Get (dict: Dictionary; key: ADDRESS) : ADDRESS ;
BEGIN
   RETURN GetNode (dict^.content, key)
END Get ;


(*
   GetNode - return the value in binary node tree which
             is associated with key.
*)

PROCEDURE GetNode (node: Node; key: ADDRESS) : ADDRESS ;
BEGIN
   IF node # NIL
   THEN
      CASE node^.dict^.compare (key, node^.key) OF

       0: RETURN node^.value |
      +1: RETURN GetNode (node^.right, key) |
      -1: RETURN GetNode (node^.left, key)

      END
   END ;
   RETURN NIL
END GetNode ;


(*
   PostOrder - visit each dictionary entry in post order.
*)

PROCEDURE PostOrder (dict: Dictionary; visit: VisitNode) ;
BEGIN
   IF dict # NIL
   THEN
      PostOrderNode (dict^.content, visit)
   END
END PostOrder ;


(*
   PostOrderNode - visit the tree node in post order.
*)

PROCEDURE PostOrderNode (node: Node; visit: VisitNode) ;
BEGIN
   IF node # NIL
   THEN
      PostOrderNode (node^.left, visit) ;
      PostOrderNode (node^.right, visit) ;
      visit (node)
   END
END PostOrderNode ;


END BinDict.
