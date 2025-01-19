------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--         ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_BOUNDED_OPERATIONS        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2025, Free Software Foundation, Inc.         --
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
   with package Tree_Types is new Generic_Bounded_Tree_Types (<>);
   use Tree_Types, Tree_Types.Implementation;

   with function  Parent (Node : Node_Type) return Count_Type is <>;

   with procedure Set_Parent
     (Node   : in out Node_Type;
      Parent : Count_Type) is <>;

   with function  Left (Node : Node_Type) return Count_Type is <>;

   with procedure Set_Left
     (Node : in out Node_Type;
      Left : Count_Type) is <>;

   with function  Right (Node : Node_Type) return Count_Type is <>;

   with procedure Set_Right
     (Node  : in out Node_Type;
      Right : Count_Type) is <>;

   with function  Color (Node : Node_Type) return Color_Type is <>;

   with procedure Set_Color
     (Node  : in out Node_Type;
      Color : Color_Type) is <>;

package Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations is
   pragma Annotate (CodePeer, Skip_Analysis);
   pragma Pure;

   function Min (Tree : Tree_Type'Class; Node : Count_Type) return Count_Type;
   --  Returns the smallest-valued node of the subtree rooted at Node

   function Max (Tree : Tree_Type'Class; Node : Count_Type) return Count_Type;
   --  Returns the largest-valued node of the subtree rooted at Node

   function Vet (Tree : Tree_Type'Class; Index : Count_Type) return Boolean
     with Inline;
   --  Inspects Node to determine (to the extent possible) whether
   --  the node is valid; used to detect if the node is dangling.

   function Next
     (Tree : Tree_Type'Class;
      Node : Count_Type) return Count_Type;
   --  Returns the smallest node greater than Node

   function Previous
     (Tree : Tree_Type'Class;
      Node : Count_Type) return Count_Type;
   --  Returns the largest node less than Node

   generic
      with function Is_Equal (L, R : Node_Type) return Boolean;
   function Generic_Equal (Left, Right : Tree_Type'Class) return Boolean;
   --  Uses Is_Equal to perform a node-by-node comparison of the
   --  Left and Right trees; processing stops as soon as the first
   --  non-equal node is found.

   procedure Delete_Node_Sans_Free
     (Tree : in out Tree_Type'Class; Node : Count_Type);
   --  Removes Node from Tree without deallocating the node. If Tree
   --  is busy then Program_Error is raised.

   procedure Clear_Tree (Tree : in out Tree_Type'Class);
   --  Clears Tree by deallocating all of its nodes. If Tree is busy then
   --  Program_Error is raised.

   generic
      with procedure Process (Node : Count_Type) is <>;
   procedure Generic_Iteration (Tree : Tree_Type'Class);
   --  Calls Process for each node in Tree, in order from smallest-valued
   --  node to largest-valued node.

   generic
      with procedure Process (Node : Count_Type) is <>;
   procedure Generic_Reverse_Iteration (Tree : Tree_Type'Class);
   --  Calls Process for each node in Tree, in order from largest-valued
   --  node to smallest-valued node.

   generic
      with procedure Write_Node
        (Stream : not null access Root_Stream_Type'Class;
         Node   : Node_Type);
   procedure Generic_Write
     (Stream : not null access Root_Stream_Type'Class;
      Tree   : Tree_Type'Class);
   --  Used to implement stream attribute T'Write. Generic_Write
   --  first writes the number of nodes into Stream, then calls
   --  Write_Node for each node in Tree.

   generic
      with procedure Allocate
        (Tree : in out Tree_Type'Class;
         Node : out Count_Type);
   procedure Generic_Read
     (Stream : not null access Root_Stream_Type'Class;
      Tree   : in out Tree_Type'Class);
   --  Used to implement stream attribute T'Read. Generic_Read
   --  first clears Tree. It then reads the number of nodes out of
   --  Stream, and calls Read_Node for each node in Stream.

   procedure Rebalance_For_Insert
     (Tree : in out Tree_Type'Class;
      Node : Count_Type);
   --  This rebalances Tree to complete the insertion of Node (which
   --  must already be linked in at its proper insertion position).

   generic
      with procedure Set_Element (Node : in out Node_Type);
   procedure Generic_Allocate
     (Tree : in out Tree_Type'Class;
      Node : out Count_Type);
   --  Claim a node from the free store. Generic_Allocate first
   --  calls Set_Element on the potential node, and then returns
   --  the node's index as the value of the Node parameter.

   procedure Free (Tree : in out Tree_Type'Class; X : Count_Type);
   --  Return a node back to the free store, from where it had
   --  been previously claimed via Generic_Allocate.

end Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations;
