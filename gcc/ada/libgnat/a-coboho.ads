------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       A D A . C O N T A I N E R S . B O U N D E D _ H O L D E R S        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2015-2021, Free Software Foundation, Inc.       --
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

private with System;
private with Ada.Strings.Text_Output;

generic
   type Element_Type (<>) is private;
   Max_Size_In_Storage_Elements : Natural :=
     Element_Type'Max_Size_In_Storage_Elements;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Bounded_Holders is
   pragma Annotate (CodePeer, Skip_Analysis);

   --  This package is patterned after Ada.Containers.Indefinite_Holders. It is
   --  used to treat indefinite subtypes as definite, but without using heap
   --  allocation. For example, you might like to say:
   --
   --     type A is array (...) of T'Class; -- illegal
   --
   --  Instead, you can instantiate this package with Element_Type => T'Class,
   --  and say:
   --
   --     type A is array (...) of Holder;
   --
   --  Each object of type Holder is allocated Max_Size_In_Storage_Elements
   --  bytes. If you try to create a holder from an object of type Element_Type
   --  that is too big, an exception is raised (assuming assertions are
   --  enabled). This applies to To_Holder and Set. If you pass an Element_Type
   --  object that is smaller than Max_Size_In_Storage_Elements, it works fine,
   --  but some space is wasted.
   --
   --  NOTE: If assertions are disabled, and you try to use an Element that is
   --  too big, execution is erroneous, and anything can happen, such as
   --  overwriting arbitrary memory locations.
   --
   --  Element_Type must not be an unconstrained array type. It can be a
   --  class-wide type or a type with non-defaulted discriminants.
   --
   --  The 'Size of each Element_Type object must be a multiple of
   --  System.Storage_Unit; e.g. creating Holders from 5-bit objects won't
   --  work.

   type Holder is private;

   function "=" (Left, Right : Holder) return Boolean;

   function To_Holder (New_Item : Element_Type) return Holder;
   function "+" (New_Item : Element_Type) return Holder renames To_Holder;

   function Get (Container : Holder) return Element_Type;

   procedure Set (Container : in out Holder; New_Item  : Element_Type);

private

   --  The implementation uses low-level tricks (Address clauses and unchecked
   --  conversions of access types) to treat the elements as storage arrays.

   pragma Assert (Element_Type'Alignment <= Standard'Maximum_Alignment);
   --  This prevents elements with a user-specified Alignment that is too big

   type Storage_Element is mod 2 ** System.Storage_Unit;
   type Storage_Array is array (Positive range <>) of Storage_Element;
   type Holder is record
      Data : Storage_Array (1 .. Max_Size_In_Storage_Elements);
   end record
     with Alignment => Standard'Maximum_Alignment, Put_Image => Put_Image;
   --  We would like to say "Alignment => Element_Type'Alignment", but that
   --  is illegal because it's not static, so we use the maximum possible
   --  (default) alignment instead.

   procedure Put_Image
     (S : in out Ada.Strings.Text_Output.Sink'Class; V : Holder);

   type Element_Access is access all Element_Type;
   pragma Assert (Element_Access'Size = Standard'Address_Size,
                  "cannot instantiate with an array type");
   --  If Element_Access is a fat pointer, Element_Type must be an
   --  unconstrained array, which is not allowed. Arrays won't work, because
   --  the 'Address of an array points to the first element, thus losing the
   --  bounds.

   pragma No_Strict_Aliasing (Element_Access);
   --  Needed because we are unchecked-converting from Address to
   --  Element_Access (see package body), which is a violation of the
   --  normal aliasing rules enforced by gcc.

end Ada.Containers.Bounded_Holders;
