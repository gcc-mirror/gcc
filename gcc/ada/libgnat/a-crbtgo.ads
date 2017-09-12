------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--             ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_OPERATIONS            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2017, Free Software Foundation, Inc.         --
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

--  Tree_Type is used to implement the ordered containers. This package
--  declares the tree operations that do not depend on keys.

with Ada.Streams; use Ada.Streams;

generic
   with package Tree_Types is new Generic_Tree_Types (<>);
   use Tree_Types, Tree_Types.Implementation;

   with function  Parent (Node : Node_Access) return Node_Access is <>;
   with procedure Set_Parent (Node : Node_Access; Parent : Node_Access) is <>;
   with function  Left (Node : Node_Access) return Node_Access is <>;
   with procedure Set_Left (Node : Node_Access; Left : Node_Access) is <>;
   with function  Right (Node : Node_Access) return Node_Access is <>;
   with procedure Set_Right (Node : Node_Access; Right : Node_Access) is <>;
   with function  Color (Node : Node_Access) return Color_Type is <>;
   with procedure Set_Color (Node : Node_Access; Color : Color_Type) is <>;

package Ada.Containers.Red_Black_Trees.Generic_Operations is
   pragma Pure;

   function Min (Node : Node_Access) return Node_Access;
   --  Returns the smallest-valued node of the subtree rooted at Node

   function Max (Node : Node_Access) return Node_Access;
   --  Returns the largest-valued node of the subtree rooted at Node

   --  NOTE: The Check_Invariant operation was used during early
   --  development of the red-black tree. Now that the tree type
   --  implementation has matured, we don't really need Check_Invariant
   --  anymore.

   --  procedure Check_Invariant (Tree : Tree_Type);

   function Vet (Tree : Tree_Type; Node : Node_Access) return Boolean;
   --  Inspects Node to determine (to the extent possible) whether
   --  the node is valid; used to detect if the node is dangling.

   function Next (Node : Node_Access) return Node_Access;
   --  Returns the smallest node greater than Node

   function Previous (Node : Node_Access) return Node_Access;
   --  Returns the largest node less than Node

   generic
      with function Is_Equal (L, R : Node_Access) return Boolean;
   function Generic_Equal (Left, Right : Tree_Type) return Boolean;
   --  Uses Is_Equal to perform a node-by-node comparison of the
   --  Left and Right trees; processing stops as soon as the first
   --  non-equal node is found.

   procedure Delete_Node_Sans_Free
     (Tree : in out Tree_Type;
      Node : Node_Access);
   --  Removes Node from Tree without deallocating the node. If Tree
   --  is busy then Program_Error is raised.

   generic
      with procedure Free (X : in out Node_Access);
   procedure Generic_Delete_Tree (X : in out Node_Access);
   --  Deallocates the tree rooted at X, calling Free on each node

   generic
      with function Copy_Node (Source : Node_Access) return Node_Access;
      with procedure Delete_Tree (X : in out Node_Access);
   function Generic_Copy_Tree (Source_Root : Node_Access) return Node_Access;
   --  Copies the tree rooted at Source_Root, using Copy_Node to copy each
   --  node of the source tree. If Copy_Node propagates an exception
   --  (e.g. Storage_Error), then Delete_Tree is first used to deallocate
   --  the target tree, and then the exception is propagated.

   generic
      with function Copy_Tree (Root : Node_Access) return Node_Access;
   procedure Generic_Adjust (Tree : in out Tree_Type);
   --  Used to implement controlled Adjust. On input to Generic_Adjust, Tree
   --  holds a bitwise (shallow) copy of the source tree (as would be the case
   --  when controlled Adjust is called). On output, Tree holds its own (deep)
   --  copy of the source tree, which is constructed by calling Copy_Tree.

   generic
      with procedure Delete_Tree (X : in out Node_Access);
   procedure Generic_Clear (Tree : in out Tree_Type);
   --  Clears Tree by deallocating all of its nodes. If Tree is busy then
   --  Program_Error is raised.

   generic
      with procedure Clear (Tree : in out Tree_Type);
   procedure Generic_Move (Target, Source : in out Tree_Type);
   --  Moves the tree belonging to Source onto Target. If Source is busy then
   --  Program_Error is raised. Otherwise Target is first cleared (by calling
   --  Clear, to deallocate its existing tree), then given the Source tree, and
   --  then finally Source is cleared (by setting its pointers to null).

   generic
      with procedure Process (Node : Node_Access) is <>;
   procedure Generic_Iteration (Tree : Tree_Type);
   --  Calls Process for each node in Tree, in order from smallest-valued
   --  node to largest-valued node.

   generic
      with procedure Process (Node : Node_Access) is <>;
   procedure Generic_Reverse_Iteration (Tree : Tree_Type);
   --  Calls Process for each node in Tree, in order from largest-valued
   --  node to smallest-valued node.

   generic
      with procedure Write_Node
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Access);
   procedure Generic_Write
     (Stream : not null access Root_Stream_Type'Class;
      Tree   : Tree_Type);
   --  Used to implement stream attribute T'Write. Generic_Write
   --  first writes the number of nodes into Stream, then calls
   --  Write_Node for each node in Tree.

   generic
      with procedure Clear (Tree : in out Tree_Type);
      with function Read_Node
        (Stream : not null access Root_Stream_Type'Class) return Node_Access;
   procedure Generic_Read
     (Stream : not null access Root_Stream_Type'Class;
      Tree   : in out Tree_Type);
   --  Used to implement stream attribute T'Read. Generic_Read
   --  first clears Tree. It then reads the number of nodes out of
   --  Stream, and calls Read_Node for each node in Stream.

   procedure Rebalance_For_Insert
     (Tree : in out Tree_Type;
      Node : Node_Access);
   --  This rebalances Tree to complete the insertion of Node (which
   --  must already be linked in at its proper insertion position).

end Ada.Containers.Red_Black_Trees.Generic_Operations;
