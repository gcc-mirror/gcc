------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                         --
--                                                                          --
--         A D A . C O N T A I N E R S . S T A B L E _ S O R T I N G        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2023, AdaCore                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Stable_Sorting package

--  This package provides a generic stable sorting procedure that is
--  intended for use by the various doubly linked list container generics.
--  If a stable array sorting algorithm with better-than-quadratic worst
--  case execution time is ever needed, then it could also reside here.

private package Ada.Containers.Stable_Sorting is
   pragma Annotate (CodePeer, Skip_Analysis);
   pragma Pure;
   pragma Remote_Types;

   --  Stable sorting algorithms with N-log-N worst case execution time.

   generic
      type Node_Ref is private; -- access value or array index
      Nil : Node_Ref;
   package List_Descriptors is

      type List_Descriptor is
         record
            First, Last : Node_Ref := Nil;
            Length      : Count_Type := 0;
         end record;

      --  We use a nested generic here so that the inner generic can
      --  refer to the List_Descriptor type.

      generic
         with function Next (N : Node_Ref) return Node_Ref is <>;
         with procedure Set_Next (N : Node_Ref; Next : Node_Ref) is <>;
         with procedure Set_Prev (N : Node_Ref; Prev : Node_Ref) is <>;
         with function "<" (L, R : Node_Ref) return Boolean is <>;

         with procedure Update_Container (List : List_Descriptor) is <>;
      procedure Doubly_Linked_List_Sort (List : List_Descriptor);

   end List_Descriptors;

end Ada.Containers.Stable_Sorting;
