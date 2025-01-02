------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . P A C K _ 1 0 0                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with System.Unsigned_Types;

package body System.Pack_100 is

   --  The high-level idea of the implementation is to overlay a record
   --  containing components of the same size as that of the component
   --  of the array, in order words 100 bits, and to access the indexed
   --  component of the array through the appropriate selected component
   --  of the record.

   --  The record must be of a fixed size for technical reasons, so we
   --  effectively overlay a series of contiguous records containing 8
   --  components (so that their size in bits is a multiple of a byte)
   --  at the start of the array and access the component in the last
   --  of them. However, this component in the last record may also be
   --  mapped to the last component of the array, which means that the
   --  generated code cannot safely access past it (or its last byte).
   --  That's why the last record of the series is shortened, so the
   --  accessed component is always the last component of the record.

   --  A (0)                                               A (N)
   --  |                                                     |
   --  V                                                     V
   --  ---------------------------------------------------------------
   --  | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
   --  ---------------------------------------------------------------
   --
   --                                                    component K
   --                                                        |
   --                                                        V
   --  ---------------------------------------------------------
   --  |               |               |               |       |
   --  ---------------------------------------------------------
   --         |               |               |             |
   --      Cluster7        Cluster7        Cluster7     ClusterK
   --
   --  where the number of Cluster7 is N / 8 and K is N mod 8.

   subtype Bit_Order is System.Bit_Order;
   Reverse_Bit_Order : constant Bit_Order :=
     Bit_Order'Val (1 - Bit_Order'Pos (System.Default_Bit_Order));

   subtype Ofs is System.Storage_Elements.Storage_Offset;
   subtype Uns is System.Unsigned_Types.Unsigned;
   subtype N07 is System.Unsigned_Types.Unsigned range 0 .. 7;

   use type System.Storage_Elements.Storage_Offset;
   use type System.Unsigned_Types.Unsigned;

   --------------
   -- Cluster0 --
   --------------

   type Cluster0 is record
      E0 : Bits_100;
   end record;

   for Cluster0 use record
      E0 at 0 range 0 * Bits .. 0 * Bits + Bits - 1;
   end record;

   for Cluster0'Size use Bits * (1 + 0);

   for Cluster0'Alignment use Integer'Min (Standard'Maximum_Alignment,
     1 +
     1 * Boolean'Pos (Bits mod 2 = 0) +
     2 * Boolean'Pos (Bits mod 4 = 0));
   --  Use maximum possible alignment, given the bit field size, since this
   --  will result in the most efficient code possible for the field.

   package AAC0 is new Address_To_Access_Conversions (Cluster0);
   --  We convert addresses to access values and dereference them instead of
   --  directly using overlays in order to work around the implementation of
   --  the RM 13.3(19) clause, which would pessimize the generated code.

   type Rev_Cluster0 is new Cluster0
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC0 is new Address_To_Access_Conversions (Rev_Cluster0);

   --  The following declarations are for the case where the address
   --  passed to GetU_100 or SetU_100 is not guaranteed to be aligned.
   --  These routines are used when the packed array is itself a
   --  component of a packed record, and therefore may not be aligned.

   type Cluster0U is new Cluster0;
   for Cluster0U'Alignment use 1;

   package AAC0U is new Address_To_Access_Conversions (Cluster0U);

   type Rev_Cluster0U is new Cluster0U
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC0U is new Address_To_Access_Conversions (Rev_Cluster0U);

   --------------
   -- Cluster1 --
   --------------

   type Cluster1 is record
      E0, E1 : Bits_100;
   end record;

   for Cluster1 use record
      E0 at 0 range 0 * Bits .. 0 * Bits + Bits - 1;
      E1 at 0 range 1 * Bits .. 1 * Bits + Bits - 1;
   end record;

   for Cluster1'Size use Bits * (1 + 1);

   for Cluster1'Alignment use Integer'Min (Standard'Maximum_Alignment,
     1 +
     1 * Boolean'Pos (Bits mod 2 = 0) +
     2 * Boolean'Pos (Bits mod 4 = 0));
   --  Use maximum possible alignment, given the bit field size, since this
   --  will result in the most efficient code possible for the field.

   package AAC1 is new Address_To_Access_Conversions (Cluster1);
   --  We convert addresses to access values and dereference them instead of
   --  directly using overlays in order to work around the implementation of
   --  the RM 13.3(19) clause, which would pessimize the generated code.

   type Rev_Cluster1 is new Cluster1
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC1 is new Address_To_Access_Conversions (Rev_Cluster1);

   --  The following declarations are for the case where the address
   --  passed to GetU_100 or SetU_100 is not guaranteed to be aligned.
   --  These routines are used when the packed array is itself a
   --  component of a packed record, and therefore may not be aligned.

   type Cluster1U is new Cluster1;
   for Cluster1U'Alignment use 1;

   package AAC1U is new Address_To_Access_Conversions (Cluster1U);

   type Rev_Cluster1U is new Cluster1U
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC1U is new Address_To_Access_Conversions (Rev_Cluster1U);

   --------------
   -- Cluster2 --
   --------------

   type Cluster2 is record
      E0, E1, E2 : Bits_100;
   end record;

   for Cluster2 use record
      E0 at 0 range 0 * Bits .. 0 * Bits + Bits - 1;
      E1 at 0 range 1 * Bits .. 1 * Bits + Bits - 1;
      E2 at 0 range 2 * Bits .. 2 * Bits + Bits - 1;
   end record;

   for Cluster2'Size use Bits * (1 + 2);

   for Cluster2'Alignment use Integer'Min (Standard'Maximum_Alignment,
     1 +
     1 * Boolean'Pos (Bits mod 2 = 0) +
     2 * Boolean'Pos (Bits mod 4 = 0));
   --  Use maximum possible alignment, given the bit field size, since this
   --  will result in the most efficient code possible for the field.

   package AAC2 is new Address_To_Access_Conversions (Cluster2);
   --  We convert addresses to access values and dereference them instead of
   --  directly using overlays in order to work around the implementation of
   --  the RM 13.3(19) clause, which would pessimize the generated code.

   type Rev_Cluster2 is new Cluster2
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC2 is new Address_To_Access_Conversions (Rev_Cluster2);

   --  The following declarations are for the case where the address
   --  passed to GetU_100 or SetU_100 is not guaranteed to be aligned.
   --  These routines are used when the packed array is itself a
   --  component of a packed record, and therefore may not be aligned.

   type Cluster2U is new Cluster2;
   for Cluster2U'Alignment use 1;

   package AAC2U is new Address_To_Access_Conversions (Cluster2U);

   type Rev_Cluster2U is new Cluster2U
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC2U is new Address_To_Access_Conversions (Rev_Cluster2U);

   --------------
   -- Cluster3 --
   --------------

   type Cluster3 is record
      E0, E1, E2, E3 : Bits_100;
   end record;

   for Cluster3 use record
      E0 at 0 range 0 * Bits .. 0 * Bits + Bits - 1;
      E1 at 0 range 1 * Bits .. 1 * Bits + Bits - 1;
      E2 at 0 range 2 * Bits .. 2 * Bits + Bits - 1;
      E3 at 0 range 3 * Bits .. 3 * Bits + Bits - 1;
   end record;

   for Cluster3'Size use Bits * (1 + 3);

   for Cluster3'Alignment use Integer'Min (Standard'Maximum_Alignment,
     1 +
     1 * Boolean'Pos (Bits mod 2 = 0) +
     2 * Boolean'Pos (Bits mod 4 = 0));
   --  Use maximum possible alignment, given the bit field size, since this
   --  will result in the most efficient code possible for the field.

   package AAC3 is new Address_To_Access_Conversions (Cluster3);
   --  We convert addresses to access values and dereference them instead of
   --  directly using overlays in order to work around the implementation of
   --  the RM 13.3(19) clause, which would pessimize the generated code.

   type Rev_Cluster3 is new Cluster3
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC3 is new Address_To_Access_Conversions (Rev_Cluster3);

   --  The following declarations are for the case where the address
   --  passed to GetU_100 or SetU_100 is not guaranteed to be aligned.
   --  These routines are used when the packed array is itself a
   --  component of a packed record, and therefore may not be aligned.

   type Cluster3U is new Cluster3;
   for Cluster3U'Alignment use 1;

   package AAC3U is new Address_To_Access_Conversions (Cluster3U);

   type Rev_Cluster3U is new Cluster3U
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC3U is new Address_To_Access_Conversions (Rev_Cluster3U);

   --------------
   -- Cluster4 --
   --------------

   type Cluster4 is record
      E0, E1, E2, E3, E4 : Bits_100;
   end record;

   for Cluster4 use record
      E0 at 0 range 0 * Bits .. 0 * Bits + Bits - 1;
      E1 at 0 range 1 * Bits .. 1 * Bits + Bits - 1;
      E2 at 0 range 2 * Bits .. 2 * Bits + Bits - 1;
      E3 at 0 range 3 * Bits .. 3 * Bits + Bits - 1;
      E4 at 0 range 4 * Bits .. 4 * Bits + Bits - 1;
   end record;

   for Cluster4'Size use Bits * (1 + 4);

   for Cluster4'Alignment use Integer'Min (Standard'Maximum_Alignment,
     1 +
     1 * Boolean'Pos (Bits mod 2 = 0) +
     2 * Boolean'Pos (Bits mod 4 = 0));
   --  Use maximum possible alignment, given the bit field size, since this
   --  will result in the most efficient code possible for the field.

   package AAC4 is new Address_To_Access_Conversions (Cluster4);
   --  We convert addresses to access values and dereference them instead of
   --  directly using overlays in order to work around the implementation of
   --  the RM 13.3(19) clause, which would pessimize the generated code.

   type Rev_Cluster4 is new Cluster4
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC4 is new Address_To_Access_Conversions (Rev_Cluster4);

   --  The following declarations are for the case where the address
   --  passed to GetU_100 or SetU_100 is not guaranteed to be aligned.
   --  These routines are used when the packed array is itself a
   --  component of a packed record, and therefore may not be aligned.

   type Cluster4U is new Cluster4;
   for Cluster4U'Alignment use 1;

   package AAC4U is new Address_To_Access_Conversions (Cluster4U);

   type Rev_Cluster4U is new Cluster4U
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC4U is new Address_To_Access_Conversions (Rev_Cluster4U);

   --------------
   -- Cluster5 --
   --------------

   type Cluster5 is record
      E0, E1, E2, E3, E4, E5 : Bits_100;
   end record;

   for Cluster5 use record
      E0 at 0 range 0 * Bits .. 0 * Bits + Bits - 1;
      E1 at 0 range 1 * Bits .. 1 * Bits + Bits - 1;
      E2 at 0 range 2 * Bits .. 2 * Bits + Bits - 1;
      E3 at 0 range 3 * Bits .. 3 * Bits + Bits - 1;
      E4 at 0 range 4 * Bits .. 4 * Bits + Bits - 1;
      E5 at 0 range 5 * Bits .. 5 * Bits + Bits - 1;
   end record;

   for Cluster5'Size use Bits * (1 + 5);

   for Cluster5'Alignment use Integer'Min (Standard'Maximum_Alignment,
     1 +
     1 * Boolean'Pos (Bits mod 2 = 0) +
     2 * Boolean'Pos (Bits mod 4 = 0));
   --  Use maximum possible alignment, given the bit field size, since this
   --  will result in the most efficient code possible for the field.

   package AAC5 is new Address_To_Access_Conversions (Cluster5);
   --  We convert addresses to access values and dereference them instead of
   --  directly using overlays in order to work around the implementation of
   --  the RM 13.3(19) clause, which would pessimize the generated code.

   type Rev_Cluster5 is new Cluster5
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC5 is new Address_To_Access_Conversions (Rev_Cluster5);

   --  The following declarations are for the case where the address
   --  passed to GetU_100 or SetU_100 is not guaranteed to be aligned.
   --  These routines are used when the packed array is itself a
   --  component of a packed record, and therefore may not be aligned.

   type Cluster5U is new Cluster5;
   for Cluster5U'Alignment use 1;

   package AAC5U is new Address_To_Access_Conversions (Cluster5U);

   type Rev_Cluster5U is new Cluster5U
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC5U is new Address_To_Access_Conversions (Rev_Cluster5U);

   --------------
   -- Cluster6 --
   --------------

   type Cluster6 is record
      E0, E1, E2, E3, E4, E5, E6 : Bits_100;
   end record;

   for Cluster6 use record
      E0 at 0 range 0 * Bits .. 0 * Bits + Bits - 1;
      E1 at 0 range 1 * Bits .. 1 * Bits + Bits - 1;
      E2 at 0 range 2 * Bits .. 2 * Bits + Bits - 1;
      E3 at 0 range 3 * Bits .. 3 * Bits + Bits - 1;
      E4 at 0 range 4 * Bits .. 4 * Bits + Bits - 1;
      E5 at 0 range 5 * Bits .. 5 * Bits + Bits - 1;
      E6 at 0 range 6 * Bits .. 6 * Bits + Bits - 1;
   end record;

   for Cluster6'Size use Bits * (1 + 6);

   for Cluster6'Alignment use Integer'Min (Standard'Maximum_Alignment,
     1 +
     1 * Boolean'Pos (Bits mod 2 = 0) +
     2 * Boolean'Pos (Bits mod 4 = 0));
   --  Use maximum possible alignment, given the bit field size, since this
   --  will result in the most efficient code possible for the field.

   package AAC6 is new Address_To_Access_Conversions (Cluster6);
   --  We convert addresses to access values and dereference them instead of
   --  directly using overlays in order to work around the implementation of
   --  the RM 13.3(19) clause, which would pessimize the generated code.

   type Rev_Cluster6 is new Cluster6
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC6 is new Address_To_Access_Conversions (Rev_Cluster6);

   --  The following declarations are for the case where the address
   --  passed to GetU_100 or SetU_100 is not guaranteed to be aligned.
   --  These routines are used when the packed array is itself a
   --  component of a packed record, and therefore may not be aligned.

   type Cluster6U is new Cluster6;
   for Cluster6U'Alignment use 1;

   package AAC6U is new Address_To_Access_Conversions (Cluster6U);

   type Rev_Cluster6U is new Cluster6U
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC6U is new Address_To_Access_Conversions (Rev_Cluster6U);

   --------------
   -- Cluster7 --
   --------------

   type Cluster7 is record
      E0, E1, E2, E3, E4, E5, E6, E7 : Bits_100;
   end record;

   for Cluster7 use record
      E0 at 0 range 0 * Bits .. 0 * Bits + Bits - 1;
      E1 at 0 range 1 * Bits .. 1 * Bits + Bits - 1;
      E2 at 0 range 2 * Bits .. 2 * Bits + Bits - 1;
      E3 at 0 range 3 * Bits .. 3 * Bits + Bits - 1;
      E4 at 0 range 4 * Bits .. 4 * Bits + Bits - 1;
      E5 at 0 range 5 * Bits .. 5 * Bits + Bits - 1;
      E6 at 0 range 6 * Bits .. 6 * Bits + Bits - 1;
      E7 at 0 range 7 * Bits .. 7 * Bits + Bits - 1;
   end record;

   for Cluster7'Size use Bits * (1 + 7);

   for Cluster7'Alignment use Integer'Min (Standard'Maximum_Alignment,
     1 +
     1 * Boolean'Pos (Bits mod 2 = 0) +
     2 * Boolean'Pos (Bits mod 4 = 0));
   --  Use maximum possible alignment, given the bit field size, since this
   --  will result in the most efficient code possible for the field.

   package AAC7 is new Address_To_Access_Conversions (Cluster7);
   --  We convert addresses to access values and dereference them instead of
   --  directly using overlays in order to work around the implementation of
   --  the RM 13.3(19) clause, which would pessimize the generated code.

   type Rev_Cluster7 is new Cluster7
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC7 is new Address_To_Access_Conversions (Rev_Cluster7);

   --  The following declarations are for the case where the address
   --  passed to GetU_100 or SetU_100 is not guaranteed to be aligned.
   --  These routines are used when the packed array is itself a
   --  component of a packed record, and therefore may not be aligned.

   type Cluster7U is new Cluster7;
   for Cluster7U'Alignment use 1;

   package AAC7U is new Address_To_Access_Conversions (Cluster7U);

   type Rev_Cluster7U is new Cluster7U
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;

   package Rev_AAC7U is new Address_To_Access_Conversions (Rev_Cluster7U);

   ------------
   -- Get_100 --
   ------------

   function Get_100
     (Arr     : System.Address;
      N       : Natural;
      Rev_SSO : Boolean) return Bits_100
   is
      A   : constant System.Address          := Arr + Bits * Ofs (Uns (N) / 8);
      C0  : constant AAC0.Object_Pointer     := AAC0.To_Pointer (A);
      C1  : constant AAC1.Object_Pointer     := AAC1.To_Pointer (A);
      C2  : constant AAC2.Object_Pointer     := AAC2.To_Pointer (A);
      C3  : constant AAC3.Object_Pointer     := AAC3.To_Pointer (A);
      C4  : constant AAC4.Object_Pointer     := AAC4.To_Pointer (A);
      C5  : constant AAC5.Object_Pointer     := AAC5.To_Pointer (A);
      C6  : constant AAC6.Object_Pointer     := AAC6.To_Pointer (A);
      C7  : constant AAC7.Object_Pointer     := AAC7.To_Pointer (A);
      RC0 : constant Rev_AAC0.Object_Pointer := Rev_AAC0.To_Pointer (A);
      RC1 : constant Rev_AAC1.Object_Pointer := Rev_AAC1.To_Pointer (A);
      RC2 : constant Rev_AAC2.Object_Pointer := Rev_AAC2.To_Pointer (A);
      RC3 : constant Rev_AAC3.Object_Pointer := Rev_AAC3.To_Pointer (A);
      RC4 : constant Rev_AAC4.Object_Pointer := Rev_AAC4.To_Pointer (A);
      RC5 : constant Rev_AAC5.Object_Pointer := Rev_AAC5.To_Pointer (A);
      RC6 : constant Rev_AAC6.Object_Pointer := Rev_AAC6.To_Pointer (A);
      RC7 : constant Rev_AAC7.Object_Pointer := Rev_AAC7.To_Pointer (A);

   begin
      return
         (if Rev_SSO then
            (case N07 (Uns (N) mod 8) is
               when 0 => RC0.E0,
               when 1 => RC1.E1,
               when 2 => RC2.E2,
               when 3 => RC3.E3,
               when 4 => RC4.E4,
               when 5 => RC5.E5,
               when 6 => RC6.E6,
               when 7 => RC7.E7)

         else
            (case N07 (Uns (N) mod 8) is
               when 0 => C0.E0,
               when 1 => C1.E1,
               when 2 => C2.E2,
               when 3 => C3.E3,
               when 4 => C4.E4,
               when 5 => C5.E5,
               when 6 => C6.E6,
               when 7 => C7.E7)
         );
   end Get_100;

   -------------
   -- GetU_100 --
   -------------

   function GetU_100
     (Arr     : System.Address;
      N       : Natural;
      Rev_SSO : Boolean) return Bits_100
   is
      A   : constant System.Address          := Arr + Bits * Ofs (Uns (N) / 8);
      C0  : constant AAC0U.Object_Pointer     := AAC0U.To_Pointer (A);
      C1  : constant AAC1U.Object_Pointer     := AAC1U.To_Pointer (A);
      C2  : constant AAC2U.Object_Pointer     := AAC2U.To_Pointer (A);
      C3  : constant AAC3U.Object_Pointer     := AAC3U.To_Pointer (A);
      C4  : constant AAC4U.Object_Pointer     := AAC4U.To_Pointer (A);
      C5  : constant AAC5U.Object_Pointer     := AAC5U.To_Pointer (A);
      C6  : constant AAC6U.Object_Pointer     := AAC6U.To_Pointer (A);
      C7  : constant AAC7U.Object_Pointer     := AAC7U.To_Pointer (A);
      RC0 : constant Rev_AAC0U.Object_Pointer := Rev_AAC0U.To_Pointer (A);
      RC1 : constant Rev_AAC1U.Object_Pointer := Rev_AAC1U.To_Pointer (A);
      RC2 : constant Rev_AAC2U.Object_Pointer := Rev_AAC2U.To_Pointer (A);
      RC3 : constant Rev_AAC3U.Object_Pointer := Rev_AAC3U.To_Pointer (A);
      RC4 : constant Rev_AAC4U.Object_Pointer := Rev_AAC4U.To_Pointer (A);
      RC5 : constant Rev_AAC5U.Object_Pointer := Rev_AAC5U.To_Pointer (A);
      RC6 : constant Rev_AAC6U.Object_Pointer := Rev_AAC6U.To_Pointer (A);
      RC7 : constant Rev_AAC7U.Object_Pointer := Rev_AAC7U.To_Pointer (A);

   begin
      return
         (if Rev_SSO then
            (case N07 (Uns (N) mod 8) is
               when 0 => RC0.E0,
               when 1 => RC1.E1,
               when 2 => RC2.E2,
               when 3 => RC3.E3,
               when 4 => RC4.E4,
               when 5 => RC5.E5,
               when 6 => RC6.E6,
               when 7 => RC7.E7)

         else
            (case N07 (Uns (N) mod 8) is
               when 0 => C0.E0,
               when 1 => C1.E1,
               when 2 => C2.E2,
               when 3 => C3.E3,
               when 4 => C4.E4,
               when 5 => C5.E5,
               when 6 => C6.E6,
               when 7 => C7.E7)
         );
   end GetU_100;

   ------------
   -- Set_100 --
   ------------

   procedure Set_100
     (Arr     : System.Address;
      N       : Natural;
      E       : Bits_100;
      Rev_SSO : Boolean)
   is
      A   : constant System.Address          := Arr + Bits * Ofs (Uns (N) / 8);
      C0  : constant AAC0.Object_Pointer     := AAC0.To_Pointer (A);
      C1  : constant AAC1.Object_Pointer     := AAC1.To_Pointer (A);
      C2  : constant AAC2.Object_Pointer     := AAC2.To_Pointer (A);
      C3  : constant AAC3.Object_Pointer     := AAC3.To_Pointer (A);
      C4  : constant AAC4.Object_Pointer     := AAC4.To_Pointer (A);
      C5  : constant AAC5.Object_Pointer     := AAC5.To_Pointer (A);
      C6  : constant AAC6.Object_Pointer     := AAC6.To_Pointer (A);
      C7  : constant AAC7.Object_Pointer     := AAC7.To_Pointer (A);
      RC0 : constant Rev_AAC0.Object_Pointer := Rev_AAC0.To_Pointer (A);
      RC1 : constant Rev_AAC1.Object_Pointer := Rev_AAC1.To_Pointer (A);
      RC2 : constant Rev_AAC2.Object_Pointer := Rev_AAC2.To_Pointer (A);
      RC3 : constant Rev_AAC3.Object_Pointer := Rev_AAC3.To_Pointer (A);
      RC4 : constant Rev_AAC4.Object_Pointer := Rev_AAC4.To_Pointer (A);
      RC5 : constant Rev_AAC5.Object_Pointer := Rev_AAC5.To_Pointer (A);
      RC6 : constant Rev_AAC6.Object_Pointer := Rev_AAC6.To_Pointer (A);
      RC7 : constant Rev_AAC7.Object_Pointer := Rev_AAC7.To_Pointer (A);

   begin
      if Rev_SSO then
         case N07 (Uns (N) mod 8) is
            when 0 => RC0.E0 := E;
            when 1 => RC1.E1 := E;
            when 2 => RC2.E2 := E;
            when 3 => RC3.E3 := E;
            when 4 => RC4.E4 := E;
            when 5 => RC5.E5 := E;
            when 6 => RC6.E6 := E;
            when 7 => RC7.E7 := E;
         end case;
      else
         case N07 (Uns (N) mod 8) is
            when 0 => C0.E0 := E;
            when 1 => C1.E1 := E;
            when 2 => C2.E2 := E;
            when 3 => C3.E3 := E;
            when 4 => C4.E4 := E;
            when 5 => C5.E5 := E;
            when 6 => C6.E6 := E;
            when 7 => C7.E7 := E;
         end case;
      end if;
   end Set_100;

   -------------
   -- SetU_100 --
   -------------

   procedure SetU_100
     (Arr     : System.Address;
      N       : Natural;
      E       : Bits_100;
      Rev_SSO : Boolean)
   is
      A   : constant System.Address          := Arr + Bits * Ofs (Uns (N) / 8);
      C0  : constant AAC0U.Object_Pointer     := AAC0U.To_Pointer (A);
      C1  : constant AAC1U.Object_Pointer     := AAC1U.To_Pointer (A);
      C2  : constant AAC2U.Object_Pointer     := AAC2U.To_Pointer (A);
      C3  : constant AAC3U.Object_Pointer     := AAC3U.To_Pointer (A);
      C4  : constant AAC4U.Object_Pointer     := AAC4U.To_Pointer (A);
      C5  : constant AAC5U.Object_Pointer     := AAC5U.To_Pointer (A);
      C6  : constant AAC6U.Object_Pointer     := AAC6U.To_Pointer (A);
      C7  : constant AAC7U.Object_Pointer     := AAC7U.To_Pointer (A);
      RC0 : constant Rev_AAC0U.Object_Pointer := Rev_AAC0U.To_Pointer (A);
      RC1 : constant Rev_AAC1U.Object_Pointer := Rev_AAC1U.To_Pointer (A);
      RC2 : constant Rev_AAC2U.Object_Pointer := Rev_AAC2U.To_Pointer (A);
      RC3 : constant Rev_AAC3U.Object_Pointer := Rev_AAC3U.To_Pointer (A);
      RC4 : constant Rev_AAC4U.Object_Pointer := Rev_AAC4U.To_Pointer (A);
      RC5 : constant Rev_AAC5U.Object_Pointer := Rev_AAC5U.To_Pointer (A);
      RC6 : constant Rev_AAC6U.Object_Pointer := Rev_AAC6U.To_Pointer (A);
      RC7 : constant Rev_AAC7U.Object_Pointer := Rev_AAC7U.To_Pointer (A);

   begin
      if Rev_SSO then
         case N07 (Uns (N) mod 8) is
            when 0 => RC0.E0 := E;
            when 1 => RC1.E1 := E;
            when 2 => RC2.E2 := E;
            when 3 => RC3.E3 := E;
            when 4 => RC4.E4 := E;
            when 5 => RC5.E5 := E;
            when 6 => RC6.E6 := E;
            when 7 => RC7.E7 := E;
         end case;
      else
         case N07 (Uns (N) mod 8) is
            when 0 => C0.E0 := E;
            when 1 => C1.E1 := E;
            when 2 => C2.E2 := E;
            when 3 => C3.E3 := E;
            when 4 => C4.E4 := E;
            when 5 => C5.E5 := E;
            when 6 => C6.E6 := E;
            when 7 => C7.E7 := E;
         end case;
      end if;
   end SetU_100;

end System.Pack_100;
