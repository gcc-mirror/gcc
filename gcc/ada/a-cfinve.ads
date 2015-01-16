------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.FORMAL_INDEFINITE_VECTORS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
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

--  Similar to Ada.Containers.Formal_Vectors. The main difference is that
--  Element_Type may be indefinite (but not an unconstrained array). In
--  addition, this is simplified by removing less-used functionality.

with Ada.Containers.Bounded_Holders;
with Ada.Containers.Formal_Vectors;

generic
   type Index_Type is range <>;
   type Element_Type (<>) is private;
   Max_Size_In_Storage_Elements : Natural :=
                                    Element_Type'Max_Size_In_Storage_Elements;
   --  This has the same meaning as in Ada.Containers.Bounded_Holders, with the
   --  same restrictions.

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   Bounded : Boolean := True;
   --  If True, the containers are bounded; the initial capacity is the maximum
   --  size, and heap allocation will be avoided. If False, the containers can
   --  grow via heap allocation.

package Ada.Containers.Formal_Indefinite_Vectors with
  SPARK_Mode => On
is
   pragma Annotate (GNATprove, External_Axiomatization);

   subtype Extended_Index is Index_Type'Base
   range Index_Type'First - 1 ..
     Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   subtype Capacity_Range is
     Count_Type range 0 .. Count_Type (Index_Type'Last - Index_Type'First + 1);

   type Vector (Capacity : Capacity_Range) is limited private with
     Default_Initial_Condition;

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
     Post   => Capacity'Result >= Container.Capacity;

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
   --  leaks.

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
     Pre    => (if Bounded
                then Length (Container) + Length (New_Item) <=
                                                       Container.Capacity);

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => (if Bounded
                then Length (Container) < Container.Capacity);

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
     (Container : Vector; Position : Extended_Index) return Boolean with
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
      Current : Index_Type) return Vector
   with
     Ghost,
     Global => null;

   function Current_To_Last
     (Container : Vector;
      Current : Index_Type) return Vector
   with
     Ghost,
     Global => null;

private
   pragma SPARK_Mode (Off);

   pragma Inline (First_Index);
   pragma Inline (Last_Index);
   pragma Inline (Element);
   pragma Inline (First_Element);
   pragma Inline (Last_Element);
   pragma Inline (Replace_Element);
   pragma Inline (Contains);

   --  The implementation method is to instantiate Bounded_Holders to get a
   --  definite type for Element_Type, and then use that Holder type to
   --  instantiate Formal_Vectors. All the operations are just wrappers.

   package Holders is new Bounded_Holders
     (Element_Type, Max_Size_In_Storage_Elements, "=");
   use Holders;

   package Def is new Formal_Vectors (Index_Type, Holder, "=", Bounded);
   use Def;

   --  ????Assert that Def subtypes have the same range

   type Vector (Capacity : Capacity_Range) is limited record
      V : Def.Vector (Capacity);
   end record;

   function Empty_Vector return Vector is
     ((Capacity => 0, V => Def.Empty_Vector));

end Ada.Containers.Formal_Indefinite_Vectors;
