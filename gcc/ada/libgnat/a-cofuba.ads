------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_BASE                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2016-2022, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
------------------------------------------------------------------------------
--  Functional containers are neither controlled nor limited. This is safe, as
--  no primitives are provided to modify them.
--  Memory allocated inside functional containers is never reclaimed.

pragma Ada_2012;

--  To allow reference counting on the base container

private with Ada.Finalization;

private generic
   type Index_Type is (<>);
   --  To avoid Constraint_Error being raised at run time, Index_Type'Base
   --  should have at least one more element at the low end than Index_Type.

   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Functional_Base with SPARK_Mode => Off is

   subtype Extended_Index is Index_Type'Base range
     Index_Type'Pred (Index_Type'First) .. Index_Type'Last;

   type Container is private;

   function "=" (C1 : Container; C2 : Container) return Boolean;
   --  Return True if C1 and C2 contain the same elements at the same position

   function Length (C : Container) return Count_Type;
   --  Number of elements stored in C

   function Get (C : Container; I : Index_Type) return Element_Type;
   --  Access to the element at index I in C

   function Set
     (C : Container;
      I : Index_Type;
      E : Element_Type) return Container;
   --  Return a new container which is equal to C except for the element at
   --  index I, which is set to E.

   function Add
     (C : Container;
      I : Index_Type;
      E : Element_Type) return Container;
   --  Return a new container that is C with E inserted at index I

   function Remove (C : Container; I : Index_Type) return Container;
   --  Return a new container that is C without the element at index I

   function Find (C : Container; E : Element_Type) return Extended_Index;
   --  Return the first index for which the element stored in C is I. If there
   --  are no such indexes, return Extended_Index'First.

   --------------------
   -- Set Operations --
   --------------------

   function "<=" (C1 : Container; C2 : Container) return Boolean;
   --  Return True if every element of C1 is in C2

   function Num_Overlaps (C1 : Container; C2 : Container) return Count_Type;
   --  Return the number of elements that are in both C1 and C2

   function Union (C1 : Container; C2 : Container) return Container;
   --  Return a container which is C1 plus all the elements of C2 that are not
   --  in C1.

   function Intersection (C1 : Container; C2 : Container) return Container;
   --  Return a container which is C1 minus all the elements that are also in
   --  C2.

private

   --  Theoretically, each operation on a functional container implies the
   --  creation of a new container i.e. the copy of the array itself and all
   --  the elements in it. In the implementation, most of these copies are
   --  avoided by sharing between the containers.
   --
   --  A container stores its last used index. So, when adding an
   --  element at the end of the container, the exact same array can be reused.
   --  As a functionnal container cannot be modifed once created, there is no
   --  risk of unwanted modifications.
   --
   --                 _1_2_3_
   --  S             :    end       => [1, 2, 3]
   --                      |
   --                 |1|2|3|4|.|.|
   --                        |
   --  Add (S, 4, 4) :      end     => [1, 2, 3, 4]
   --
   --  The elements are also shared between containers as much as possible. For
   --  example, when something is added in the middle, the array is changed but
   --  the elementes are reused.
   --
   --                  _1_2_3_4_
   --  S             : |1|2|3|4|    => [1, 2, 3, 4]
   --                   |  \ \ \
   --  Add (S, 2, 5) : |1|5|2|3|4|  => [1, 5, 2, 3, 4]
   --
   --  To make this sharing possible, both the elements and the arrays are
   --  stored inside dynamically allocated access types which shall be
   --  deallocated when they are no longer used. The memory is managed using
   --  reference counting both at the array and at the element level.

   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

   type Reference_Count_Type is new Natural;

   type Element_Access is access all Element_Type;

   type Refcounted_Element is record
      Reference_Count : Reference_Count_Type;
      E_Access        : Element_Access;
   end record;

   type Refcounted_Element_Access is access Refcounted_Element;

   type Controlled_Element_Access is new Ada.Finalization.Controlled
   with record
      Ref : Refcounted_Element_Access := null;
   end record;

   function Element_Init (E : Element_Type) return Controlled_Element_Access;
   --  Use to initialize a refcounted element

   type Element_Array is
     array (Positive_Count_Type range <>) of Controlled_Element_Access;

   type Element_Array_Access_Base is access Element_Array;

   subtype Element_Array_Access is Element_Array_Access_Base;

   type Array_Base is record
     Reference_Count : Reference_Count_Type;
     Max_Length      : Count_Type;
     Elements        : Element_Array_Access;
   end record;

   type Array_Base_Access is access Array_Base;

   type Array_Base_Controlled_Access is new Ada.Finalization.Controlled
   with record
      Base : Array_Base_Access;
   end record;

   overriding procedure Adjust
     (Controlled_Base : in out Array_Base_Controlled_Access);

   overriding procedure Finalize
     (Controlled_Base : in out Array_Base_Controlled_Access);

   overriding procedure Adjust
     (Ctrl_E : in out Controlled_Element_Access);

   overriding procedure Finalize
     (Ctrl_E : in out Controlled_Element_Access);

   function Content_Init (L : Count_Type := 0)
                          return Array_Base_Controlled_Access;
   --  Used to initialize the content of an array base with length L

   type Container is record
      Length          : Count_Type := 0;
      Controlled_Base : Array_Base_Controlled_Access := Content_Init;
   end record;

end Ada.Containers.Functional_Base;
