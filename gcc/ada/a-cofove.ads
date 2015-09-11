------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--         A D A . C O N T A I N E R S . F O R M A L _ V E C T O R S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2015, Free Software Foundation, Inc.         --
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

--  This spec is derived from package Ada.Containers.Bounded_Vectors in the Ada
--  2012 RM. The modifications are meant to facilitate formal proofs by making
--  it easier to express properties, and by making the specification of this
--  unit compatible with SPARK 2014. Note that the API of this unit may be
--  subject to incompatible changes as SPARK 2014 evolves.

generic
   type Index_Type is range <>;
   type Element_Type is private;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   Bounded : Boolean := True;
   --  If True, the containers are bounded; the initial capacity is the maximum
   --  size, and heap allocation will be avoided. If False, the containers can
   --  grow via heap allocation.

package Ada.Containers.Formal_Vectors with
  SPARK_Mode
is
   pragma Annotate (GNATprove, External_Axiomatization);

   subtype Extended_Index is Index_Type'Base
   range Index_Type'First - 1 ..
     Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   subtype Capacity_Range is
     Count_Type range 0 .. Count_Type (Index_Type'Last - Index_Type'First + 1);

   type Vector (Capacity : Capacity_Range) is limited private with
     Default_Initial_Condition => Is_Empty (Vector);
   --  In the bounded case, Capacity is the capacity of the container, which
   --  never changes. In the unbounded case, Capacity is the initial capacity
   --  of the container, and operations such as Reserve_Capacity and Append can
   --  increase the capacity. The capacity never shrinks, except in the case of
   --  Clear.
   --
   --  Note that all objects of type Vector are constrained, including in the
   --  unbounded case; you can't assign from one object to another if the
   --  Capacity is different.

   function Empty_Vector return Vector;

   function "=" (Left, Right : Vector) return Boolean with
     Global => null;

   function To_Vector
     (New_Item : Element_Type;
      Length   : Capacity_Range) return Vector
   with
     Global => null;

   function Capacity (Container : Vector) return Capacity_Range with
     Global => null,
     Post => Capacity'Result >= Container.Capacity;

   procedure Reserve_Capacity
     (Container : in out Vector;
      Capacity  : Capacity_Range)
   with
     Global => null,
     Pre    => (if Bounded then Capacity <= Container.Capacity);

   function Length (Container : Vector) return Capacity_Range with
     Global => null;

   function Is_Empty (Container : Vector) return Boolean with
     Global => null;

   procedure Clear (Container : in out Vector) with
     Global => null;
   --  Note that this reclaims storage in the unbounded case. You need to call
   --  this before a container goes out of scope in order to avoid storage
   --  leaks. In addition, "X := ..." can leak unless you Clear(X) first.

   procedure Assign (Target : in out Vector; Source : Vector) with
     Global => null,
     Pre    => (if Bounded then Length (Source) <= Target.Capacity);

   function Copy
     (Source   : Vector;
      Capacity : Capacity_Range := 0) return Vector
   with
     Global => null,
     Pre    => (if Bounded then (Capacity = 0 or Length (Source) <= Capacity));

   function Element
     (Container : Vector;
      Index     : Index_Type) return Element_Type
   with
     Global => null,
     Pre    => Index in First_Index (Container) .. Last_Index (Container);

   procedure Replace_Element
     (Container : in out Vector;
      Index     : Index_Type;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Index in First_Index (Container) .. Last_Index (Container);

   procedure Append
     (Container : in out Vector;
      New_Item  : Vector)
   with
     Global => null,
     Pre    => (if Bounded then
                 Length (Container) + Length (New_Item) <= Container.Capacity);

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => (if Bounded then
                  Length (Container) < Container.Capacity);

   procedure Delete_Last
     (Container : in out Vector)
   with
     Global => null;

   procedure Reverse_Elements (Container : in out Vector) with
     Global => null;

   procedure Swap (Container : in out Vector; I, J : Index_Type) with
     Global => null,
     Pre    => I in First_Index (Container) .. Last_Index (Container)
      and then J in First_Index (Container) .. Last_Index (Container);

   function First_Index (Container : Vector) return Index_Type with
     Global => null;

   function First_Element (Container : Vector) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container);

   function Last_Index (Container : Vector) return Extended_Index with
     Global => null;

   function Last_Element (Container : Vector) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container);

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index
   with
     Global => null;

   function Reverse_Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'Last) return Extended_Index
   with
     Global => null;

   function Contains
     (Container : Vector;
      Item      : Element_Type) return Boolean
   with
     Global => null;

   function Has_Element
     (Container : Vector;
      Position  : Extended_Index) return Boolean
   with
     Global => null;

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   package Generic_Sorting is

      function Is_Sorted (Container : Vector) return Boolean with
        Global => null;

      procedure Sort (Container : in out Vector) with
        Global => null;

   end Generic_Sorting;

   function First_To_Previous
     (Container : Vector;
      Current   : Index_Type) return Vector
   with
     Ghost,
     Global => null,
     Pre    => Current in First_Index (Container) .. Last_Index (Container);

   function Current_To_Last
     (Container : Vector;
      Current   : Index_Type) return Vector
   with
     Ghost,
     Global => null,
     Pre    => Current in First_Index (Container) .. Last_Index (Container);
   --  First_To_Previous returns a container containing all elements preceding
   --  Current (excluded) in Container. Current_To_Last returns a container
   --  containing all elements following Current (included) in Container.
   --  These two new functions can be used to express invariant properties in
   --  loops which iterate over containers. First_To_Previous returns the part
   --  of the container already scanned and Current_To_Last the part not
   --  scanned yet.

private
   pragma SPARK_Mode (Off);

   pragma Inline (First_Index);
   pragma Inline (Last_Index);
   pragma Inline (Element);
   pragma Inline (First_Element);
   pragma Inline (Last_Element);
   pragma Inline (Replace_Element);
   pragma Inline (Contains);

   subtype Array_Index is Capacity_Range range 1 .. Capacity_Range'Last;
   type Elements_Array is array (Array_Index range <>) of Element_Type;
   function "=" (L, R : Elements_Array) return Boolean is abstract;

   type Elements_Array_Ptr is access all Elements_Array;

   type Vector (Capacity : Capacity_Range) is limited record
      --  In the bounded case, the elements are stored in Elements. In the
      --  unbounded case, the elements are initially stored in Elements, until
      --  we run out of room, then we switch to Elements_Ptr.
      Last         : Extended_Index := No_Index;
      Elements_Ptr : Elements_Array_Ptr := null;
      Elements     : aliased Elements_Array (1 .. Capacity);
   end record;

   --  The primary reason Vector is limited is that in the unbounded case, once
   --  Elements_Ptr is in use, assignment statements won't work. "X := Y;" will
   --  cause X and Y to share state; that is, X.Elements_Ptr = Y.Elements_Ptr,
   --  so for example "Append (X, ...);" will modify BOTH X and Y. That would
   --  allow SPARK to "prove" things that are false. We could fix that by
   --  making Vector a controlled type, and override Adjust to make a deep
   --  copy, but finalization is not allowed in SPARK.
   --
   --  Note that (unfortunately) this means that 'Old and 'Loop_Entry are not
   --  allowed on Vectors.

   function Empty_Vector return Vector is
     ((Capacity => 0, others => <>));

end Ada.Containers.Formal_Vectors;
