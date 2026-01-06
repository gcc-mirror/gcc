------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                ADA.CONTAINERS.BOUNDED_INDEFINITE_HOLDERS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2026, Free Software Foundation, Inc.         --
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

with System.Storage_Elements; use System.Storage_Elements;

private with Ada.Finalization;
private with Ada.Streams;
private with Ada.Strings.Text_Buffers;
private with System.Storage_Pools.Subpools;

generic
   type Element_Type (<>) is private;
   Max_Element_Size_In_Storage_Elements : Storage_Count;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Bounded_Indefinite_Holders is
   pragma Annotate (CodePeer, Skip_Analysis);
   pragma Preelaborate (Bounded_Indefinite_Holders);
   pragma Remote_Types (Bounded_Indefinite_Holders);

   type Holder is tagged private
   with
     Preelaborable_Initialization => Element_Type'Preelaborable_Initialization;

   Empty_Holder : constant Holder;

   function "=" (Left, Right : Holder) return Boolean;

   function To_Holder (New_Item : Element_Type) return Holder;

   function Is_Empty (Container : Holder) return Boolean;

   procedure Clear (Container : in out Holder);

   function Element (Container : Holder) return Element_Type;

   procedure Replace_Element
     (Container : in out Holder;
      New_Item  : Element_Type);

   procedure Query_Element
     (Container : Holder;
      Process   : not null access procedure (Element : Element_Type));

   procedure Update_Element
     (Container : in out Holder;
      Process   : not null access procedure (Element : in out Element_Type));

   type Constant_Reference_Type
      (Element : not null access constant Element_Type) is limited private
   with
      Implicit_Dereference => Element;

   type Reference_Type
     (Element : not null access Element_Type) is limited private
   with
      Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Holder) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out Holder) return Reference_Type;
   pragma Inline (Reference);

   procedure Assign (Target : in out Holder; Source : Holder);

   function Copy (Source : Holder) return Holder;

   procedure Move (Target : in out Holder; Source : in out Holder);

   procedure Swap (Left, Right : in out Holder);

private

   use Ada.Finalization;
   use Ada.Streams;
   use System.Storage_Pools.Subpools;

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Holder);

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Holder);

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; V : Holder);

   package Subpool_Support is

      type Holder_Pool_Type is
        limited new Root_Storage_Pool_With_Subpools with null record;

      type Holder_Subpool is
        limited new Root_Subpool with record
         Start : System.Address := System.Null_Address;
      end record;

      overriding
      function Create_Subpool (Pool : in out Holder_Pool_Type)
        return not null Subpool_Handle
        ; --  tbd with No_Return;
      --  We never use this one.  It will raise Program_Error

      overriding
      procedure Allocate_From_Subpool
        (Pool : in out Holder_Pool_Type;
         Storage_Address : out System.Address;
         Size_In_Storage_Elements : Storage_Count;
         Alignment : Storage_Count;
         Subpool : not null Subpool_Handle);

      overriding
      procedure Deallocate_Subpool
        (Pool : in out Holder_Pool_Type;
         Subpool : in out Subpool_Handle);

      function Create_Subpool
        (Pool : in out Holder_Pool_Type'Class; Container : Holder)
        return not null Subpool_Handle;

      The_Storage_Pool : Holder_Pool_Type;
      --  The one and only object of this type ever created.
   end Subpool_Support;

   use Subpool_Support;

   type Element_Access is access Element_Type
     with Storage_Pool => Subpool_Support.The_Storage_Pool,
          Size => Standard'Address_Size;
   --  Size specification needed to ensure contiguous bounds if Element_Type
   --  turns out to be an unconstrained array subtype. We do not want a
   --  fat-pointer representation in that case.

   pragma No_Strict_Aliasing (Element_Access);
   --  Needed because we are unchecked-converting from Address to
   --  Element_Access (see package body), which is a violation of the
   --  normal aliasing rules enforced by gcc.

   Worst_Case_Alignment : constant Storage_Count :=
     Storage_Count'Max (Holder_Subpool'Alignment,
       Storage_Count'Max (System.Address'Alignment,
         Element_Type'Alignment));

   --  Convert Element_Type'Size from bits to bytes, rounding up
   Element_Size_In_Storage_Elements : constant Long_Integer :=
     Long_Integer ((Element_Type'Size / System.Storage_Unit) +
       Boolean'Pos (Element_Type'Size mod System.Storage_Unit /= 0));

   --  An upper bound on additional storage required for an allocator for data
   --  other than the allocated object itself. This includes things like
   --  array bounds (if Element_Type is an unconstrained array subtype),
   --  finalization-related linkage (if Element_Type requires
   --  finalization), alignment-related gaps between such prefix info and the
   --  allocated object, etc. This does not include alignment-related
   --  overhead except for aforementioned possibility of an alignment-related
   --  gap between some prefix data and the object itself.

   pragma Warnings (Off); -- avoid warnings for exceptions raised in dead code

   function Max_Allocation_Overhead_In_Storage_Elements return Storage_Count is
     (if Element_Size_In_Storage_Elements >= Long_Integer (Integer'Last) then
         --  If the more precise computation in the else-arm (below) could
         --  overflow or return the wrong answer then return a guess.
         --  We get a multiplier of 6 by adding 2 for finalization-linkage
         --  and 4 for array bounds. If we have an unconstrained array subtype
         --  with a controlled element type and with multiple dimensions each
         --  indexed by Long_Long_Integer, then this guess could be too small.
         System.Address'Max_Size_In_Storage_Elements * 6
      else
         Storage_Count (Element_Type'Max_Size_In_Storage_Elements -
           Element_Size_In_Storage_Elements));
   --
   --  ???  It would be helpful if GNAT provided this value as an attribute so
   --  that we would not have to deal with the "huge" case here. Instead, we
   --  use a very imprecise "hugeness" test; in the "huge" case, we return an
   --  estimate. If the estimate turns out to be too small, then it is
   --  possible for the size check in Allocate_From_Subpool to fail even
   --  though the earlier (earlier at run-time) size check in Replace_Element
   --  passed. A GNAT-defined attribute could eliminate this issue.

   pragma Warnings (On);

   --  Compute extra amount needed for space requested for an allocator
   --  (specifically, in a call to Allocate_From_Subpool) in addition to
   --  the space required for the allocated object itself.
   Extra_Storage : constant Storage_Count :=
     Holder_Subpool'Max_Size_In_Storage_Elements +
     Worst_Case_Alignment * 2 +
     Max_Allocation_Overhead_In_Storage_Elements;

   subtype Bound_Range is Storage_Count range
     0 ..  Max_Element_Size_In_Storage_Elements + Extra_Storage;

   type Storage_Wrapper (Bound : Bound_Range := 0) is record
      Storage : aliased Storage_Array (1 .. Bound);
         --  Should allocate space for case when Bound = Bound_Range'Last
         --  but we actually leave Bound at zero so assignment
         --  is faster (this wouldn't work if the compiler didn't
         --  allocate the "max" for types with defaulted discriminants).
   end record;

   type Holder is new Ada.Finalization.Controlled with record
      Busy : Natural := 0;
      Handle : Subpool_Handle;
      Element : Element_Access;
      Wrapper : Storage_Wrapper;
   end record
     with Put_Image => Put_Image, Read => Read, Write => Write;

   overriding procedure Adjust (Container : in out Holder);
   overriding procedure Finalize (Container : in out Holder);

   type Holder_Access is access all Holder;
   for Holder_Access'Storage_Size use 0;

   --  Instead of declaring Reference_Control_Type as a controlled type,
   --  we could use the GNAT-defined Finalizable aspect instead.
   --  But we would not want to make this change only in this unit - many
   --  of the container generics declare a Reference_Control_Type type.
   --  In particular, we want to minimize differences between this unit
   --  and the corresponding unbounded unit (Ada.Indefinite_Holders).

   type Reference_Control_Type is new Controlled with record
      Container : Holder_Access;
   end record;

   overriding procedure Finalize (Control : in out Reference_Control_Type);
   pragma Inline (Finalize);

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is
   record
      Control : Reference_Control_Type :=
        (raise Program_Error with "default initialized reference");
      --  The RM says, "The default initialization of an object of
      --  type Constant_Reference_Type or Reference_Type propagates
      --  Program_Error."
   end record;

   type Reference_Type
     (Element : not null access Element_Type) is
   record
      Control : Reference_Control_Type :=
        (raise Program_Error with "default initialized reference");
      --  The RM says, "The default initialization of an object of
      --  type Constant_Reference_Type or Reference_Type propagates
      --  Program_Error."
   end record;

   --  The following four streaming-related subprograms could be
   --  deleted (the two reference types are limited as a result of
   --  AI22-0082, so streaming operations are not available for them).
   --  But we do not want to perform this cleanup only in this unit - the
   --  same change should be made for all the container generics.
   --  In particular, we want to minimize differences between this unit
   --  and the corresponding unbounded unit (Ada.Indefinite_Holders).

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Constant_Reference_Type);

   for Constant_Reference_Type'Read use Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Reference_Type);

   for Reference_Type'Read use Read;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Constant_Reference_Type);

   for Constant_Reference_Type'Write use Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Reference_Type);

   for Reference_Type'Write use Write;

   Empty_Holder : constant Holder := (Controlled with
     Busy => 0,
     Handle => null,
     Element => null,
     Wrapper => (Bound => 0, others => <>));

end Ada.Containers.Bounded_Indefinite_Holders;
