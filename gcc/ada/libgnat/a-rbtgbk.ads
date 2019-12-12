------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--            ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_BOUNDED_KEYS           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  Tree_Type is used to implement ordered containers. This package declares
--  the tree operations that depend on keys.

with Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations;

generic
   with package Tree_Operations is new Generic_Bounded_Operations (<>);

   use Tree_Operations.Tree_Types, Tree_Operations.Tree_Types.Implementation;

   type Key_Type (<>) is limited private;

   with function Is_Less_Key_Node
     (L : Key_Type;
      R : Node_Type) return Boolean;

   with function Is_Greater_Key_Node
     (L : Key_Type;
      R : Node_Type) return Boolean;

package Ada.Containers.Red_Black_Trees.Generic_Bounded_Keys is
   pragma Pure;

   generic
      with function New_Node return Count_Type;

   procedure Generic_Insert_Post
     (Tree   : in out Tree_Type'Class;
      Y      : Count_Type;
      Before : Boolean;
      Z      : out Count_Type);
   --  Completes an insertion after the insertion position has been
   --  determined. On output Z contains the index of the newly inserted
   --  node, allocated using Allocate. If Tree is busy then
   --  Program_Error is raised. If Y is 0, then Tree must be empty.
   --  Otherwise Y denotes the insertion position, and Before specifies
   --  whether the new node is Y's left (True) or right (False) child.

   generic
      with procedure Insert_Post
        (T : in out Tree_Type'Class;
         Y : Count_Type;
         B : Boolean;
         Z : out Count_Type);

   procedure Generic_Conditional_Insert
     (Tree     : in out Tree_Type'Class;
      Key      : Key_Type;
      Node     : out Count_Type;
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
        (T : in out Tree_Type'Class;
         Y : Count_Type;
         B : Boolean;
         Z : out Count_Type);

   procedure Generic_Unconditional_Insert
     (Tree : in out Tree_Type'Class;
      Key  : Key_Type;
      Node : out Count_Type);
   --  Inserts a new node in Tree. On output Node designates the new
   --  node, which is allocated using Insert_Post. The node is inserted
   --  immediately after already-existing equivalent keys.

   generic
      with procedure Insert_Post
        (T : in out Tree_Type'Class;
         Y : Count_Type;
         B : Boolean;
         Z : out Count_Type);

      with procedure Unconditional_Insert_Sans_Hint
        (Tree    : in out Tree_Type'Class;
         Key     : Key_Type;
         Node    : out Count_Type);

   procedure Generic_Unconditional_Insert_With_Hint
     (Tree : in out Tree_Type'Class;
      Hint : Count_Type;
      Key  : Key_Type;
      Node : out Count_Type);
   --  Inserts a new node in Tree near position Hint, to avoid having to
   --  search from the root for the insertion position. If Hint is 0
   --  then Generic_Unconditional_Insert_With_Hint attempts to insert
   --  the new node after Tree.Last. If Hint is non-zero then if Key is
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
        (T : in out Tree_Type'Class;
         Y : Count_Type;
         B : Boolean;
         Z : out Count_Type);

      with procedure Conditional_Insert_Sans_Hint
        (Tree     : in out Tree_Type'Class;
         Key      : Key_Type;
         Node     : out Count_Type;
         Inserted : out Boolean);

   procedure Generic_Conditional_Insert_With_Hint
     (Tree     : in out Tree_Type'Class;
      Position : Count_Type;       -- the hint
      Key      : Key_Type;
      Node     : out Count_Type;
      Inserted : out Boolean);
   --  Inserts a new node in Tree if the tree does not already contain
   --  Key, using Position as a hint about where to insert the new node.
   --  See Generic_Unconditional_Insert_With_Hint for more details about
   --  hint semantics.

   function Find
     (Tree : Tree_Type'Class;
      Key  : Key_Type) return Count_Type;
   --  Searches Tree for the smallest node equivalent to Key

   function Ceiling
     (Tree : Tree_Type'Class;
      Key  : Key_Type) return Count_Type;
   --  Searches Tree for the smallest node equal to or greater than Key

   function Floor
     (Tree : Tree_Type'Class;
      Key  : Key_Type) return Count_Type;
   --  Searches Tree for the largest node less than or equal to Key

   function Upper_Bound
     (Tree : Tree_Type'Class;
      Key  : Key_Type) return Count_Type;
   --  Searches Tree for the smallest node greater than Key

   generic
      with procedure Process (Index : Count_Type);
   procedure Generic_Iteration
     (Tree : Tree_Type'Class;
      Key  : Key_Type);
   --  Calls Process for each node in Tree equivalent to Key, in order
   --  from earliest in range to latest.

   generic
      with procedure Process (Index : Count_Type);
   procedure Generic_Reverse_Iteration
     (Tree : Tree_Type'Class;
      Key  : Key_Type);
   --  Calls Process for each node in Tree equivalent to Key, but in
   --  order from largest in range to earliest.

end Ada.Containers.Red_Black_Trees.Generic_Bounded_Keys;
