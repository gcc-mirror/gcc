------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       ADA.CONTAINERS.RED_BLACK_TREES.GENERIC_BOUNDED_SET_OPERATIONS      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2020, Free Software Foundation, Inc.         --
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

with Ada.Containers.Red_Black_Trees.Generic_Bounded_Operations;

generic
   with package Tree_Operations is new Generic_Bounded_Operations (<>);

   type Set_Type is new Tree_Operations.Tree_Types.Tree_Type with private;

   use Tree_Operations.Tree_Types, Tree_Operations.Tree_Types.Implementation;

   with procedure Assign (Target : in out Set_Type; Source : Set_Type);

   with procedure Insert_With_Hint
     (Dst_Set  : in out Set_Type;
      Dst_Hint : Count_Type;
      Src_Node : Node_Type;
      Dst_Node : out Count_Type);

   with function Is_Less (Left, Right : Node_Type) return Boolean;

package Ada.Containers.Red_Black_Trees.Generic_Bounded_Set_Operations is
   pragma Pure;

   procedure Set_Union (Target : in out Set_Type; Source : Set_Type);
   --  Attempts to insert each element of Source in Target. If Target is
   --  busy then Program_Error is raised. We say "attempts" here because
   --  if these are unique-element sets, then the insertion should fail
   --  (not insert a new item) when the insertion item from Source is
   --  equivalent to an item already in Target. If these are multisets
   --  then of course the attempt should always succeed.

   function Set_Union (Left, Right : Set_Type) return Set_Type;
   --  Makes a copy of Left, and attempts to insert each element of
   --  Right into the copy, then returns the copy.

   procedure Set_Intersection (Target : in out Set_Type; Source : Set_Type);
   --  Removes elements from Target that are not equivalent to items in
   --  Source. If Target is busy then Program_Error is raised.

   function Set_Intersection (Left, Right : Set_Type) return Set_Type;
   --  Returns a set comprising all the items in Left equivalent to items in
   --  Right.

   procedure Set_Difference (Target : in out Set_Type; Source : Set_Type);
   --  Removes elements from Target that are equivalent to items in Source. If
   --  Target is busy then Program_Error is raised.

   function Set_Difference (Left, Right : Set_Type) return Set_Type;
   --  Returns a set comprising all the items in Left not equivalent to items
   --  in Right.

   procedure Set_Symmetric_Difference
     (Target : in out Set_Type;
      Source : Set_Type);
   --  Removes from Target elements that are equivalent to items in Source,
   --  and inserts into Target items from Source not equivalent elements in
   --  Target. If Target is busy then Program_Error is raised.

   function Set_Symmetric_Difference (Left, Right : Set_Type) return Set_Type;
   --  Returns a set comprising the union of the elements in Left not
   --  equivalent to items in Right, and the elements in Right not equivalent
   --  to items in Left.

   function Set_Subset (Subset : Set_Type; Of_Set : Set_Type) return Boolean;
   --  Returns False if Subset contains at least one element not equivalent to
   --  any item in Of_Set; returns True otherwise.

   function Set_Overlap (Left, Right : Set_Type) return Boolean;
   --  Returns True if at least one element of Left is equivalent to an item in
   --  Right; returns False otherwise.

end Ada.Containers.Red_Black_Trees.Generic_Bounded_Set_Operations;
