------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . A U X _ D E C                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS For A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains definitions that are designed to be compatible
--  with the extra definitions in package System for DEC Ada implementations.

--  These definitions can be used directly by withing this package, or merged
--  with System using pragma Extend_System (Aux_DEC)

with Ada.Unchecked_Conversion;

package System.Aux_DEC is
   pragma Preelaborate;

   subtype Short_Address is Address;
   --  In some versions of System.Aux_DEC, notably that for VMS on the
   --  ia64, there are two address types (64-bit and 32-bit), and the
   --  name Short_Address is used for the short address form. To avoid
   --  difficulties (in regression tests and elsewhere) with units that
   --  reference Short_Address, it is provided for other targets as a
   --  synonum for the normal Address type, and, as in the case where
   --  the lengths are different, Address and Short_Address can be
   --  freely inter-converted.

   type Integer_8  is range -2 **  (8 - 1) .. +2 **  (8 - 1) - 1;
   for Integer_8'Size  use  8;

   type Integer_16 is range -2 ** (16 - 1) .. +2 ** (16 - 1) - 1;
   for Integer_16'Size use 16;

   type Integer_32 is range -2 ** (32 - 1) .. +2 ** (32 - 1) - 1;
   for Integer_32'Size use 32;

   type Integer_64 is range -2 ** (64 - 1) .. +2 ** (64 - 1) - 1;
   for Integer_64'Size use 64;

   type Largest_Integer is range Min_Int .. Max_Int;

   type AST_Handler is private;

   No_AST_Handler : constant AST_Handler;

   type Type_Class is
     (Type_Class_Enumeration,
      Type_Class_Integer,
      Type_Class_Fixed_Point,
      Type_Class_Floating_Point,
      Type_Class_Array,
      Type_Class_Record,
      Type_Class_Access,
      Type_Class_Task,             -- also in Ada 95 protected
      Type_Class_Address);

   function "not" (Left        : Largest_Integer) return Largest_Integer;
   function "and" (Left, Right : Largest_Integer) return Largest_Integer;
   function "or"  (Left, Right : Largest_Integer) return Largest_Integer;
   function "xor" (Left, Right : Largest_Integer) return Largest_Integer;

   Address_Zero : constant Address;
   No_Addr      : constant Address;
   Address_Size : constant := Standard'Address_Size;

   function "+" (Left : Address; Right : Integer) return Address;
   function "+" (Left : Integer; Right : Address) return Address;
   function "-" (Left : Address; Right : Address) return Integer;
   function "-" (Left : Address; Right : Integer) return Address;

   generic
      type Target is private;
   function Fetch_From_Address (A : Address) return Target;

   generic
      type Target is private;
   procedure Assign_To_Address (A : Address; T : Target);

   --  Floating point type declarations for VAX floating point data types

   pragma Warnings (Off);

   type F_Float is digits 6;
   pragma Float_Representation (VAX_Float, F_Float);

   type D_Float is digits 9;
   pragma Float_Representation (Vax_Float, D_Float);

   type G_Float is digits 15;
   pragma Float_Representation (Vax_Float, G_Float);

   --  Floating point type declarations for IEEE floating point data types

   type IEEE_Single_Float is digits 6;
   pragma Float_Representation (IEEE_Float, IEEE_Single_Float);

   type IEEE_Double_Float is digits 15;
   pragma Float_Representation (IEEE_Float, IEEE_Double_Float);

   pragma Warnings (On);

   Non_Ada_Error : exception;

   --  Hardware-oriented types and functions

   type Bit_Array is array (Integer range <>) of Boolean;
   pragma Pack (Bit_Array);

   subtype Bit_Array_8  is Bit_Array (0 ..  7);
   subtype Bit_Array_16 is Bit_Array (0 .. 15);
   subtype Bit_Array_32 is Bit_Array (0 .. 31);
   subtype Bit_Array_64 is Bit_Array (0 .. 63);

   type Unsigned_Byte is range 0 .. 255;
   for  Unsigned_Byte'Size use 8;

   function "not" (Left        : Unsigned_Byte) return Unsigned_Byte;
   function "and" (Left, Right : Unsigned_Byte) return Unsigned_Byte;
   function "or"  (Left, Right : Unsigned_Byte) return Unsigned_Byte;
   function "xor" (Left, Right : Unsigned_Byte) return Unsigned_Byte;

   function To_Unsigned_Byte (X : Bit_Array_8) return Unsigned_Byte;
   function To_Bit_Array_8   (X : Unsigned_Byte) return Bit_Array_8;

   type Unsigned_Byte_Array is array (Integer range <>) of Unsigned_Byte;

   type Unsigned_Word is range 0 .. 65535;
   for  Unsigned_Word'Size use 16;

   function "not" (Left        : Unsigned_Word) return Unsigned_Word;
   function "and" (Left, Right : Unsigned_Word) return Unsigned_Word;
   function "or"  (Left, Right : Unsigned_Word) return Unsigned_Word;
   function "xor" (Left, Right : Unsigned_Word) return Unsigned_Word;

   function To_Unsigned_Word (X : Bit_Array_16) return Unsigned_Word;
   function To_Bit_Array_16  (X : Unsigned_Word) return Bit_Array_16;

   type Unsigned_Word_Array is array (Integer range <>) of Unsigned_Word;

   type Unsigned_Longword is range -2_147_483_648 .. 2_147_483_647;
   for  Unsigned_Longword'Size use 32;

   function "not" (Left        : Unsigned_Longword) return Unsigned_Longword;
   function "and" (Left, Right : Unsigned_Longword) return Unsigned_Longword;
   function "or"  (Left, Right : Unsigned_Longword) return Unsigned_Longword;
   function "xor" (Left, Right : Unsigned_Longword) return Unsigned_Longword;

   function To_Unsigned_Longword (X : Bit_Array_32) return Unsigned_Longword;
   function To_Bit_Array_32 (X : Unsigned_Longword) return Bit_Array_32;

   type Unsigned_Longword_Array is
      array (Integer range <>) of Unsigned_Longword;

   type Unsigned_32 is range 0 .. 4_294_967_295;
   for  Unsigned_32'Size use 32;

   function "not" (Left        : Unsigned_32) return Unsigned_32;
   function "and" (Left, Right : Unsigned_32) return Unsigned_32;
   function "or"  (Left, Right : Unsigned_32) return Unsigned_32;
   function "xor" (Left, Right : Unsigned_32) return Unsigned_32;

   function To_Unsigned_32 (X : Bit_Array_32) return Unsigned_32;
   function To_Bit_Array_32 (X : Unsigned_32) return Bit_Array_32;

   type Unsigned_Quadword is record
      L0 : Unsigned_Longword;
      L1 : Unsigned_Longword;
   end record;

   for Unsigned_Quadword'Size      use 64;
   for Unsigned_Quadword'Alignment use
     Integer'Min (8, Standard'Maximum_Alignment);

   function "not" (Left        : Unsigned_Quadword) return Unsigned_Quadword;
   function "and" (Left, Right : Unsigned_Quadword) return Unsigned_Quadword;
   function "or"  (Left, Right : Unsigned_Quadword) return Unsigned_Quadword;
   function "xor" (Left, Right : Unsigned_Quadword) return Unsigned_Quadword;

   function To_Unsigned_Quadword (X : Bit_Array_64) return Unsigned_Quadword;
   function To_Bit_Array_64 (X : Unsigned_Quadword) return Bit_Array_64;

   type Unsigned_Quadword_Array is
      array (Integer range <>) of Unsigned_Quadword;

   function To_Address (X : Integer) return Address;
   pragma Pure_Function (To_Address);

   function To_Address_Long (X : Unsigned_Longword) return Address;
   pragma Pure_Function (To_Address_Long);

   function To_Integer (X : Address) return Integer;

   function To_Unsigned_Longword (X : Address)     return Unsigned_Longword;
   function To_Unsigned_Longword (X : AST_Handler) return Unsigned_Longword;

   --  Conventional names for static subtypes of type UNSIGNED_LONGWORD

   subtype Unsigned_1  is Unsigned_Longword range 0 .. 2** 1-1;
   subtype Unsigned_2  is Unsigned_Longword range 0 .. 2** 2-1;
   subtype Unsigned_3  is Unsigned_Longword range 0 .. 2** 3-1;
   subtype Unsigned_4  is Unsigned_Longword range 0 .. 2** 4-1;
   subtype Unsigned_5  is Unsigned_Longword range 0 .. 2** 5-1;
   subtype Unsigned_6  is Unsigned_Longword range 0 .. 2** 6-1;
   subtype Unsigned_7  is Unsigned_Longword range 0 .. 2** 7-1;
   subtype Unsigned_8  is Unsigned_Longword range 0 .. 2** 8-1;
   subtype Unsigned_9  is Unsigned_Longword range 0 .. 2** 9-1;
   subtype Unsigned_10 is Unsigned_Longword range 0 .. 2**10-1;
   subtype Unsigned_11 is Unsigned_Longword range 0 .. 2**11-1;
   subtype Unsigned_12 is Unsigned_Longword range 0 .. 2**12-1;
   subtype Unsigned_13 is Unsigned_Longword range 0 .. 2**13-1;
   subtype Unsigned_14 is Unsigned_Longword range 0 .. 2**14-1;
   subtype Unsigned_15 is Unsigned_Longword range 0 .. 2**15-1;
   subtype Unsigned_16 is Unsigned_Longword range 0 .. 2**16-1;
   subtype Unsigned_17 is Unsigned_Longword range 0 .. 2**17-1;
   subtype Unsigned_18 is Unsigned_Longword range 0 .. 2**18-1;
   subtype Unsigned_19 is Unsigned_Longword range 0 .. 2**19-1;
   subtype Unsigned_20 is Unsigned_Longword range 0 .. 2**20-1;
   subtype Unsigned_21 is Unsigned_Longword range 0 .. 2**21-1;
   subtype Unsigned_22 is Unsigned_Longword range 0 .. 2**22-1;
   subtype Unsigned_23 is Unsigned_Longword range 0 .. 2**23-1;
   subtype Unsigned_24 is Unsigned_Longword range 0 .. 2**24-1;
   subtype Unsigned_25 is Unsigned_Longword range 0 .. 2**25-1;
   subtype Unsigned_26 is Unsigned_Longword range 0 .. 2**26-1;
   subtype Unsigned_27 is Unsigned_Longword range 0 .. 2**27-1;
   subtype Unsigned_28 is Unsigned_Longword range 0 .. 2**28-1;
   subtype Unsigned_29 is Unsigned_Longword range 0 .. 2**29-1;
   subtype Unsigned_30 is Unsigned_Longword range 0 .. 2**30-1;
   subtype Unsigned_31 is Unsigned_Longword range 0 .. 2**31-1;

   --  Function for obtaining global symbol values

   function Import_Value         (Symbol : String) return Unsigned_Longword;
   function Import_Address       (Symbol : String) return Address;
   function Import_Largest_Value (Symbol : String) return Largest_Integer;

   pragma Import (Intrinsic, Import_Value);
   pragma Import (Intrinsic, Import_Address);
   pragma Import (Intrinsic, Import_Largest_Value);

   --  For the following declarations, note that the declaration without
   --  a Retry_Count parameter means to retry infinitely. A value of zero
   --  for the Retry_Count parameter means do not retry.

   --  Interlocked-instruction procedures

   procedure Clear_Interlocked
     (Bit       : in out Boolean;
      Old_Value : out Boolean);

   procedure Set_Interlocked
     (Bit       : in out Boolean;
      Old_Value : out Boolean);

   type Aligned_Word is record
      Value : Short_Integer;
   end record;

   for Aligned_Word'Alignment use
     Integer'Min (2, Standard'Maximum_Alignment);

   procedure Clear_Interlocked
     (Bit          : in out Boolean;
      Old_Value    : out Boolean;
      Retry_Count  : Natural;
      Success_Flag : out Boolean);

   procedure Set_Interlocked
     (Bit          : in out Boolean;
      Old_Value    : out Boolean;
      Retry_Count  : Natural;
      Success_Flag : out Boolean);

   procedure Add_Interlocked
     (Addend       : Short_Integer;
      Augend       : in out Aligned_Word;
      Sign         : out Integer);

   type Aligned_Integer is record
      Value : Integer;
   end record;

   for Aligned_Integer'Alignment use
     Integer'Min (4, Standard'Maximum_Alignment);

   type Aligned_Long_Integer is record
      Value : Long_Integer;
   end record;

   for Aligned_Long_Integer'Alignment use
     Integer'Min (8, Standard'Maximum_Alignment);

   --  For the following declarations, note that the declaration without
   --  a Retry_Count parameter mean to retry infinitely. A value of zero
   --  for the Retry_Count means do not retry.

   procedure Add_Atomic
     (To           : in out Aligned_Integer;
      Amount       : Integer);

   procedure Add_Atomic
     (To           : in out Aligned_Integer;
      Amount       : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean);

   procedure Add_Atomic
     (To           : in out Aligned_Long_Integer;
      Amount       : Long_Integer);

   procedure Add_Atomic
     (To           : in out Aligned_Long_Integer;
      Amount       : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean);

   procedure And_Atomic
     (To           : in out Aligned_Integer;
      From         : Integer);

   procedure And_Atomic
     (To           : in out Aligned_Integer;
      From         : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean);

   procedure And_Atomic
     (To           : in out Aligned_Long_Integer;
      From         : Long_Integer);

   procedure And_Atomic
     (To           : in out Aligned_Long_Integer;
      From         : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean);

   procedure Or_Atomic
     (To           : in out Aligned_Integer;
      From         : Integer);

   procedure Or_Atomic
     (To           : in out Aligned_Integer;
      From         : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean);

   procedure Or_Atomic
     (To           : in out Aligned_Long_Integer;
      From         : Long_Integer);

   procedure Or_Atomic
     (To           : in out Aligned_Long_Integer;
      From         : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean);

   type Insq_Status is
     (Fail_No_Lock, OK_Not_First, OK_First);

   for Insq_Status use
     (Fail_No_Lock => -1,
      OK_Not_First => 0,
      OK_First     => +1);

   type Remq_Status is (
     Fail_No_Lock,
     Fail_Was_Empty,
     OK_Not_Empty,
     OK_Empty);

   for Remq_Status use
     (Fail_No_Lock   => -1,
      Fail_Was_Empty => 0,
      OK_Not_Empty   => +1,
      OK_Empty       => +2);

   procedure Insqhi
     (Item   : Address;
      Header : Address;
      Status : out Insq_Status);

   procedure Remqhi
     (Header : Address;
      Item   : out Address;
      Status : out Remq_Status);

   procedure Insqti
     (Item   : Address;
      Header : Address;
      Status : out Insq_Status);

   procedure Remqti
     (Header : Address;
      Item   : out Address;
      Status : out Remq_Status);

private

   Address_Zero : constant Address := Null_Address;
   No_Addr      : constant Address := Null_Address;

   --  An AST_Handler value is from a typing point of view simply a pointer
   --  to a procedure taking a single 64bit parameter. However, this
   --  is a bit misleading, because the data that this pointer references is
   --  highly stylized. See body of System.AST_Handling for full details.

   type AST_Handler is access procedure (Param : Long_Integer);
   No_AST_Handler : constant AST_Handler := null;

   --  Other operators have incorrect profiles. It would be nice to make
   --  them intrinsic, since the backend can handle them, but the front
   --  end is not prepared to deal with them, so at least inline them.

   pragma Inline_Always ("+");
   pragma Inline_Always ("-");
   pragma Inline_Always ("not");
   pragma Inline_Always ("and");
   pragma Inline_Always ("or");
   pragma Inline_Always ("xor");

   --  Other inlined subprograms

   pragma Inline_Always (Fetch_From_Address);
   pragma Inline_Always (Assign_To_Address);

   --  Synchronization related subprograms. Mechanism is explicitly set
   --  so that the critical parameters are passed by reference.
   --  Without this, the parameters are passed by copy, creating load/store
   --  race conditions. We also inline them, since this seems more in the
   --  spirit of the original (hardware intrinsic) routines.

   pragma Export_Procedure
     (Clear_Interlocked,
      External        => "system__aux_dec__clear_interlocked__1",
      Parameter_Types => (Boolean, Boolean),
      Mechanism       => (Reference, Reference));
   pragma Export_Procedure
     (Clear_Interlocked,
      External        => "system__aux_dec__clear_interlocked__2",
      Parameter_Types => (Boolean, Boolean, Natural, Boolean),
      Mechanism       => (Reference, Reference, Value, Reference));
   pragma Inline_Always (Clear_Interlocked);

   pragma Export_Procedure
     (Set_Interlocked,
      External        => "system__aux_dec__set_interlocked__1",
      Parameter_Types => (Boolean, Boolean),
      Mechanism       => (Reference, Reference));
   pragma Export_Procedure
     (Set_Interlocked,
      External        => "system__aux_dec__set_interlocked__2",
      Parameter_Types => (Boolean, Boolean, Natural, Boolean),
      Mechanism       => (Reference, Reference, Value, Reference));
   pragma Inline_Always (Set_Interlocked);

   pragma Export_Procedure
     (Add_Interlocked,
      External        => "system__aux_dec__add_interlocked__1",
      Mechanism       => (Value, Reference, Reference));
   pragma Inline_Always (Add_Interlocked);

   pragma Export_Procedure
     (Add_Atomic,
      External        => "system__aux_dec__add_atomic__1",
      Parameter_Types => (Aligned_Integer, Integer),
      Mechanism       => (Reference, Value));
   pragma Export_Procedure
     (Add_Atomic,
      External        => "system__aux_dec__add_atomic__2",
      Parameter_Types => (Aligned_Integer, Integer, Natural, Integer, Boolean),
      Mechanism       => (Reference, Value, Value, Reference, Reference));
   pragma Export_Procedure
     (Add_Atomic,
      External        => "system__aux_dec__add_atomic__3",
      Parameter_Types => (Aligned_Long_Integer, Long_Integer),
      Mechanism       => (Reference, Value));
   pragma Export_Procedure
     (Add_Atomic,
      External        => "system__aux_dec__add_atomic__4",
      Parameter_Types => (Aligned_Long_Integer, Long_Integer, Natural,
                          Long_Integer, Boolean),
      Mechanism       => (Reference, Value, Value, Reference, Reference));
   pragma Inline_Always (Add_Atomic);

   pragma Export_Procedure
     (And_Atomic,
      External        => "system__aux_dec__and_atomic__1",
      Parameter_Types => (Aligned_Integer, Integer),
      Mechanism       => (Reference, Value));
   pragma Export_Procedure
     (And_Atomic,
      External        => "system__aux_dec__and_atomic__2",
      Parameter_Types => (Aligned_Integer, Integer, Natural, Integer, Boolean),
      Mechanism       => (Reference, Value, Value, Reference, Reference));
   pragma Export_Procedure
     (And_Atomic,
      External => "system__aux_dec__and_atomic__3",
      Parameter_Types => (Aligned_Long_Integer, Long_Integer),
      Mechanism => (Reference, Value));
   pragma Export_Procedure
     (And_Atomic,
      External        => "system__aux_dec__and_atomic__4",
      Parameter_Types => (Aligned_Long_Integer, Long_Integer, Natural,
                          Long_Integer, Boolean),
      Mechanism       => (Reference, Value, Value, Reference, Reference));
   pragma Inline_Always (And_Atomic);

   pragma Export_Procedure
     (Or_Atomic,
      External        => "system__aux_dec__or_atomic__1",
      Parameter_Types => (Aligned_Integer, Integer),
      Mechanism       => (Reference, Value));
   pragma Export_Procedure
     (Or_Atomic,
      External        => "system__aux_dec__or_atomic__2",
      Parameter_Types => (Aligned_Integer, Integer, Natural, Integer, Boolean),
      Mechanism       => (Reference, Value, Value, Reference, Reference));
   pragma Export_Procedure
     (Or_Atomic,
      External        => "system__aux_dec__or_atomic__3",
      Parameter_Types => (Aligned_Long_Integer, Long_Integer),
      Mechanism       => (Reference, Value));
   pragma Export_Procedure
     (Or_Atomic,
      External        => "system__aux_dec__or_atomic__4",
      Parameter_Types => (Aligned_Long_Integer, Long_Integer, Natural,
                          Long_Integer, Boolean),
      Mechanism       => (Reference, Value, Value, Reference, Reference));
   pragma Inline_Always (Or_Atomic);

   --  Provide proper unchecked conversion definitions for transfer
   --  functions. Note that we need this level of indirection because
   --  the formal parameter name is X and not Source (and this is indeed
   --  detectable by a program)

   function To_Unsigned_Byte_A is new
     Ada.Unchecked_Conversion (Bit_Array_8, Unsigned_Byte);

   function To_Unsigned_Byte (X : Bit_Array_8) return Unsigned_Byte
     renames To_Unsigned_Byte_A;

   function To_Bit_Array_8_A is new
     Ada.Unchecked_Conversion (Unsigned_Byte, Bit_Array_8);

   function To_Bit_Array_8 (X : Unsigned_Byte) return Bit_Array_8
     renames To_Bit_Array_8_A;

   function To_Unsigned_Word_A is new
     Ada.Unchecked_Conversion (Bit_Array_16, Unsigned_Word);

   function To_Unsigned_Word (X : Bit_Array_16) return Unsigned_Word
     renames To_Unsigned_Word_A;

   function To_Bit_Array_16_A is new
     Ada.Unchecked_Conversion (Unsigned_Word, Bit_Array_16);

   function To_Bit_Array_16 (X : Unsigned_Word) return Bit_Array_16
     renames To_Bit_Array_16_A;

   function To_Unsigned_Longword_A is new
     Ada.Unchecked_Conversion (Bit_Array_32, Unsigned_Longword);

   function To_Unsigned_Longword (X : Bit_Array_32) return Unsigned_Longword
     renames To_Unsigned_Longword_A;

   function To_Bit_Array_32_A is new
     Ada.Unchecked_Conversion (Unsigned_Longword, Bit_Array_32);

   function To_Bit_Array_32 (X : Unsigned_Longword) return Bit_Array_32
     renames To_Bit_Array_32_A;

   function To_Unsigned_32_A is new
     Ada.Unchecked_Conversion (Bit_Array_32, Unsigned_32);

   function To_Unsigned_32 (X : Bit_Array_32) return Unsigned_32
     renames To_Unsigned_32_A;

   function To_Bit_Array_32_A is new
     Ada.Unchecked_Conversion (Unsigned_32, Bit_Array_32);

   function To_Bit_Array_32 (X : Unsigned_32) return Bit_Array_32
     renames To_Bit_Array_32_A;

   function To_Unsigned_Quadword_A is new
     Ada.Unchecked_Conversion (Bit_Array_64, Unsigned_Quadword);

   function To_Unsigned_Quadword (X : Bit_Array_64) return Unsigned_Quadword
     renames To_Unsigned_Quadword_A;

   function To_Bit_Array_64_A is new
     Ada.Unchecked_Conversion (Unsigned_Quadword, Bit_Array_64);

   function To_Bit_Array_64 (X : Unsigned_Quadword) return Bit_Array_64
     renames To_Bit_Array_64_A;

   pragma Warnings (Off);
   --  Turn warnings off. This is needed for systems with 64-bit integers,
   --  where some of these operations are of dubious meaning, but we do not
   --  want warnings when we compile on such systems.

   function To_Address_A is new
     Ada.Unchecked_Conversion (Integer, Address);
   pragma Pure_Function (To_Address_A);

   function To_Address (X : Integer) return Address
     renames To_Address_A;
   pragma Pure_Function (To_Address);

   function To_Address_Long_A is new
     Ada.Unchecked_Conversion (Unsigned_Longword, Address);
   pragma Pure_Function (To_Address_Long_A);

   function To_Address_Long (X : Unsigned_Longword) return Address
     renames To_Address_Long_A;
   pragma Pure_Function (To_Address_Long);

   function To_Integer_A is new
     Ada.Unchecked_Conversion (Address, Integer);

   function To_Integer (X : Address) return Integer
     renames To_Integer_A;

   function To_Unsigned_Longword_A is new
     Ada.Unchecked_Conversion (Address, Unsigned_Longword);

   function To_Unsigned_Longword (X : Address) return Unsigned_Longword
     renames To_Unsigned_Longword_A;

   function To_Unsigned_Longword_A is new
     Ada.Unchecked_Conversion (AST_Handler, Unsigned_Longword);

   function To_Unsigned_Longword (X : AST_Handler) return Unsigned_Longword
     renames To_Unsigned_Longword_A;

   pragma Warnings (On);

end System.Aux_DEC;
