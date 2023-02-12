------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--           ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_SET_OPERATIONS          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2023, Free Software Foundation, Inc.         --
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
--  set-based tree operations.

with Ada.Containers.Red_Black_Trees.Generic_Operations;

generic
   with package Tree_Operations is new Generic_Operations (<>);

   use Tree_Operations.Tree_Types, Tree_Operations.Tree_Types.Implementation;

   with procedure Insert_With_Hint
     (Dst_Tree : in out Tree_Type;
      Dst_Hint : Node_Access;
      Src_Node : Node_Access;
      Dst_Node : out Node_Access);

   with function Copy_Tree (Source_Root : Node_Access)
       return Node_Access;

   with procedure Delete_Tree (X : in out Node_Access);

   with function Is_Less (Left, Right : Node_Access) return Boolean;

   with procedure Free (X : in out Node_Access);

package Ada.Containers.Red_Black_Trees.Generic_Set_Operations is
   pragma Pure;

   procedure Union (Target : in out Tree_Type; Source : Tree_Type);
   --  Attempts to insert each element of Source in Target. If Target is
   --  busy then Program_Error is raised. We say "attempts" here because
   --  if these are unique-element sets, then the insertion should fail
   --  (not insert a new item) when the insertion item from Source is
   --  equivalent to an item already in Target. If these are multisets
   --  then of course the attempt should always succeed.

   function Union (Left, Right : Tree_Type) return Tree_Type;
   --  Makes a copy of Left, and attempts to insert each element of
   --  Right into the copy, then returns the copy.

   procedure Intersection (Target : in out Tree_Type; Source : Tree_Type);
   --  Removes elements from Target that are not equivalent to items in
   --  Source. If Target is busy then Program_Error is raised.

   function Intersection (Left, Right : Tree_Type) return Tree_Type;
   --  Returns a set comprising all the items in Left equivalent to items in
   --  Right.

   procedure Difference (Target : in out Tree_Type; Source : Tree_Type);
   --  Removes elements from Target that are equivalent to items in Source. If
   --  Target is busy then Program_Error is raised.

   function Difference (Left, Right : Tree_Type) return Tree_Type;
   --  Returns a set comprising all the items in Left not equivalent to items
   --  in Right.

   procedure Symmetric_Difference
     (Target : in out Tree_Type;
      Source : Tree_Type);
   --  Removes from Target elements that are equivalent to items in Source, and
   --  inserts into Target items from Source not equivalent elements in
   --  Target. If Target is busy then Program_Error is raised.

   function Symmetric_Difference (Left, Right : Tree_Type) return Tree_Type;
   --  Returns a set comprising the union of the elements in Left not
   --  equivalent to items in Right, and the elements in Right not equivalent
   --  to items in Left.

   function Is_Subset (Subset : Tree_Type; Of_Set : Tree_Type) return Boolean;
   --  Returns False if Subset contains at least one element not equivalent to
   --  any item in Of_Set; returns True otherwise.

   function Overlap (Left, Right : Tree_Type) return Boolean;
   --  Returns True if at least one element of Left is equivalent to an item in
   --  Right; returns False otherwise.

end Ada.Containers.Red_Black_Trees.Generic_Set_Operations;
