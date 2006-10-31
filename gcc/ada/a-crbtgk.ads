------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--        A D A . C O N T A I N E R S . R E D _ B L A C K _ T R E E S .     --
--                          G E N E R I C _ K E Y S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  Tree_Type is used to implement ordered containers. This package declares
--  the tree operations that depend on keys.

with Ada.Containers.Red_Black_Trees.Generic_Operations;

generic
   with package Tree_Operations is new Generic_Operations (<>);

   use Tree_Operations.Tree_Types;

   type Key_Type (<>) is limited private;

   with function Is_Less_Key_Node
     (L : Key_Type;
      R : Node_Access) return Boolean;

   with function Is_Greater_Key_Node
     (L : Key_Type;
      R : Node_Access) return Boolean;

package Ada.Containers.Red_Black_Trees.Generic_Keys is
   pragma Pure;

   generic
      with function New_Node return Node_Access;
   procedure Generic_Insert_Post
     (Tree   : in out Tree_Type;
      Y      : Node_Access;
      Before : Boolean;
      Z      : out Node_Access);
   --  Completes an insertion after the insertion position has been
   --  determined. On output Z contains a pointer to the newly inserted
   --  node, allocated using New_Node. If Tree is busy then
   --  Program_Error is raised. If Y is null, then Tree must be empty.
   --  Otherwise Y denotes the insertion position, and Before specifies
   --  whether the new node is Y's left (True) or right (False) child.

   generic
      with procedure Insert_Post
        (T : in out Tree_Type;
         Y : Node_Access;
         B : Boolean;
         Z : out Node_Access);

   procedure Generic_Conditional_Insert
     (Tree     : in out Tree_Type;
      Key      : Key_Type;
      Node     : out Node_Access;
      Inserted : out Boolean);
   --  Inserts a new node in Tree, but only if the tree does not already
   --  contain Key. Generic_Conditional_Insert first searches for a key
   --  equivalent to Key in Tree. If an equivalent key is found, then on
   --  output Node designates the node with that key and Inserted is
   --  False; there is no allocation and Tree is not modified. Otherwise
   --  Node designates a new node allocated using Insert_Post, and
   --  Inserted is True.

   generic
      with procedure Insert_Post
        (T : in out Tree_Type;
         Y : Node_Access;
         B : Boolean;
         Z : out Node_Access);

   procedure Generic_Unconditional_Insert
     (Tree : in out Tree_Type;
      Key  : Key_Type;
      Node : out Node_Access);
   --  Inserts a new node in Tree. On output Node designates the new
   --  node, which is allocated using Insert_Post. The node is inserted
   --  immediately after already-existing equivalent keys.

   generic
      with procedure Insert_Post
        (T : in out Tree_Type;
         Y : Node_Access;
         B : Boolean;
         Z : out Node_Access);

      with procedure Unconditional_Insert_Sans_Hint
        (Tree    : in out Tree_Type;
         Key     : Key_Type;
         Node    : out Node_Access);

   procedure Generic_Unconditional_Insert_With_Hint
     (Tree : in out Tree_Type;
      Hint : Node_Access;
      Key  : Key_Type;
      Node : out Node_Access);
   --  Inserts a new node in Tree near position Hint, to avoid having to
   --  search from the root for the insertion position. If Hint is null
   --  then Generic_Unconditional_Insert_With_Hint attempts to insert
   --  the new node after Tree.Last. If Hint is non-null then if Key is
   --  less than Hint, it attempts to insert the new node immediately
   --  prior to Hint. Otherwise it attempts to insert the node
   --  immediately following Hint. We say "attempts" above to emphasize
   --  that insertions always preserve invariants with respect to key
   --  order, even when there's a hint. So if Key can't be inserted
   --  immediately near Hint, then the new node is inserted in the
   --  normal way, by searching for the correct position starting from
   --  the root.

   generic
      with procedure Insert_Post
        (T : in out Tree_Type;
         Y : Node_Access;
         B : Boolean;
         Z : out Node_Access);

      with procedure Conditional_Insert_Sans_Hint
        (Tree     : in out Tree_Type;
         Key      : Key_Type;
         Node     : out Node_Access;
         Inserted : out Boolean);

   procedure Generic_Conditional_Insert_With_Hint
     (Tree     : in out Tree_Type;
      Position : Node_Access;       -- the hint
      Key      : Key_Type;
      Node     : out Node_Access;
      Inserted : out Boolean);
   --  Inserts a new node in Tree if the tree does not already contain
   --  Key, using Position as a hint about where to insert the new node.
   --  See Generic_Unconditional_Insert_With_Hint for more details about
   --  hint semantics.

   function Find
     (Tree : Tree_Type;
      Key  : Key_Type) return Node_Access;
   --  Searches Tree for the smallest node equivalent to Key

   function Ceiling
     (Tree : Tree_Type;
      Key  : Key_Type) return Node_Access;
   --  Searches Tree for the smallest node equal to or greater than Key

   function Floor
     (Tree : Tree_Type;
      Key  : Key_Type) return Node_Access;
   --  Searches Tree for the largest node less than or equal to Key

   function Upper_Bound
     (Tree : Tree_Type;
      Key  : Key_Type) return Node_Access;
   --  Searches Tree for the smallest node greater than Key

   generic
      with procedure Process (Node : Node_Access);
   procedure Generic_Iteration
     (Tree : Tree_Type;
      Key  : Key_Type);
   --  Calls Process for each node in Tree equivalent to Key, in order
   --  from earliest in range to latest.

   generic
      with procedure Process (Node : Node_Access);
   procedure Generic_Reverse_Iteration
     (Tree : Tree_Type;
      Key  : Key_Type);
   --  Calls Process for each node in Tree equivalent to Key, but in
   --  order from largest in range to earliest.

end Ada.Containers.Red_Black_Trees.Generic_Keys;
