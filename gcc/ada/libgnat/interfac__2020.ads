------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           I N T E R F A C E S                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the implementation dependent sections of this file.      --
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

--  This is the runtime version of this unit (not used during GNAT build)

--  ``Interfaces`` is the parent of several library packages that declare types
--  and other entities useful for interfacing to foreign languages as defined
--  by ARM B.2.
--
--  It defines signed and modular integer types of 8, 16, 32, 64 and 128 bits.
--  For each such modular type, shifting and rotating intrinsic subprograms
--  are specified. There is also the definition of IEEE 754 floating point
--  types (``IEEE_Float_32``, ``IEEE_Float_64``, and ``IEEE_Extended_Float``).

package Interfaces with
  Always_Terminates
is
   pragma No_Elaboration_Code_All;
   pragma Pure;

   pragma Implementation_Defined;
   --  All identifiers in this unit are implementation defined

   --  Definitions of 8, 16, 24, 32, 64 and 128 bit signed and unsigned integer
   --  types.

   type Integer_8  is range -2 **  7 .. 2 **  7 - 1;
   for Integer_8'Size use  8;

   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer_16'Size use 16;

   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Integer_32'Size use 32;

   type Integer_64 is new Long_Long_Integer;
   for Integer_64'Size use 64;
   --  We use Long_Long_Integer'First instead of -2 ** 63 to allow this unit to
   --  compile when using custom target configuration files where the maximum
   --  integer is 32 bits. This is useful for static analysis tools such as
   --  SPARK or CodePeer. In the normal case Long_Long_Integer is 64-bits so we
   --  get the desired 64-bit type.

   type Integer_128 is new Long_Long_Long_Integer;
   --  We use Long_Long_Long_Integer instead of literal bounds to allow this
   --  unit to be compiled with compilers not supporting 128-bit integers.
   --  We do not put a confirming size clause of 128 bits for the same reason.

   type Unsigned_8 is mod 2 ** 8;
   for Unsigned_8'Size use  8;

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;

   type Unsigned_24 is mod 2 ** 24;
   for Unsigned_24'Size use 24;
   --  Declare this type for compatibility with legacy Ada compilers.
   --  This is particularly useful in the context of CodePeer analysis.

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;

   type Unsigned_64 is mod 2 ** Long_Long_Integer'Size;
   for Unsigned_64'Size use 64;
   --  See comment on Integer_64 above

   type Unsigned_128 is mod 2 ** Long_Long_Long_Integer'Size;
   --  See comment on Integer_128 above

   --  Compiler intrinsics implemented by the compiler

   function Shift_Left
     (Value  : Unsigned_8;
      Amount : Natural) return Unsigned_8
      with Import, Convention => Intrinsic, Static;

   function Shift_Right
     (Value  : Unsigned_8;
      Amount : Natural) return Unsigned_8
      with Import, Convention => Intrinsic, Static;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_8;
      Amount : Natural) return Unsigned_8
      with Import, Convention => Intrinsic, Static;

   function Rotate_Left
     (Value  : Unsigned_8;
      Amount : Natural) return Unsigned_8
      with Import, Convention => Intrinsic, Static;

   function Rotate_Right
     (Value  : Unsigned_8;
      Amount : Natural) return Unsigned_8
      with Import, Convention => Intrinsic, Static;

   function Shift_Left
     (Value  : Unsigned_16;
      Amount : Natural) return Unsigned_16
      with Import, Convention => Intrinsic, Static;

   function Shift_Right
     (Value  : Unsigned_16;
      Amount : Natural) return Unsigned_16
      with Import, Convention => Intrinsic, Static;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_16;
      Amount : Natural) return Unsigned_16
      with Import, Convention => Intrinsic, Static;

   function Rotate_Left
     (Value  : Unsigned_16;
      Amount : Natural) return Unsigned_16
      with Import, Convention => Intrinsic, Static;

   function Rotate_Right
     (Value  : Unsigned_16;
      Amount : Natural) return Unsigned_16
      with Import, Convention => Intrinsic, Static;

   function Shift_Left
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32
      with Import, Convention => Intrinsic, Static;

   function Shift_Right
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32
      with Import, Convention => Intrinsic, Static;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32
      with Import, Convention => Intrinsic, Static;

   function Rotate_Left
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32
      with Import, Convention => Intrinsic, Static;

   function Rotate_Right
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32
      with Import, Convention => Intrinsic, Static;

   function Shift_Left
     (Value  : Unsigned_64;
      Amount : Natural) return Unsigned_64
      with Import, Convention => Intrinsic, Static;

   function Shift_Right
     (Value  : Unsigned_64;
      Amount : Natural) return Unsigned_64
      with Import, Convention => Intrinsic, Static;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_64;
      Amount : Natural) return Unsigned_64
      with Import, Convention => Intrinsic, Static;

   function Rotate_Left
     (Value  : Unsigned_64;
      Amount : Natural) return Unsigned_64
      with Import, Convention => Intrinsic, Static;

   function Rotate_Right
     (Value  : Unsigned_64;
      Amount : Natural) return Unsigned_64
      with Import, Convention => Intrinsic, Static;

   function Shift_Left
     (Value  : Unsigned_128;
      Amount : Natural) return Unsigned_128
      with Import, Convention => Intrinsic, Static;

   function Shift_Right
     (Value  : Unsigned_128;
      Amount : Natural) return Unsigned_128
      with Import, Convention => Intrinsic, Static;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_128;
      Amount : Natural) return Unsigned_128
      with Import, Convention => Intrinsic, Static;

   function Rotate_Left
     (Value  : Unsigned_128;
      Amount : Natural) return Unsigned_128
      with Import, Convention => Intrinsic, Static;

   function Rotate_Right
     (Value  : Unsigned_128;
      Amount : Natural) return Unsigned_128
      with Import, Convention => Intrinsic, Static;

   --  IEEE Floating point types

   type IEEE_Float_32 is digits 6;
   for IEEE_Float_32'Size use 32;

   type IEEE_Float_64 is digits 15;
   for IEEE_Float_64'Size use 64;

   --  If there is an IEEE extended float available on the machine, we
   --  assume that it is available as Long_Long_Float.
   --
   --  Note: it is harmless, and explicitly permitted, to include additional
   --  types in interfaces, so it is not wrong to have IEEE_Extended_Float
   --  defined even if the extended format is not available.
   --  See RM-B.2(11).

   type IEEE_Extended_Float is new Long_Long_Float;

end Interfaces;
