------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           I N T E R F A C E S                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2011, Free Software Foundation, Inc.         --
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

package Interfaces is
   pragma Pure;

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   type Integer_8  is range -2 **  7 .. 2 **  7 - 1;
   for Integer_8'Size use  8;

   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer_16'Size use 16;

   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Integer_32'Size use 32;

   type Integer_64 is range -2 ** 63 .. 2 ** 63 - 1;
   for Integer_64'Size use 64;

   type Unsigned_8  is mod 2 **  8;
   for Unsigned_8'Size use  8;

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;

   type Unsigned_64 is mod 2 ** 64;
   for Unsigned_64'Size use 64;

   function Shift_Left
     (Value  : Unsigned_8;
      Amount : Natural) return Unsigned_8;

   function Shift_Right
     (Value  : Unsigned_8;
      Amount : Natural) return Unsigned_8;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_8;
      Amount : Natural) return Unsigned_8;

   function Rotate_Left
     (Value  : Unsigned_8;
      Amount : Natural) return Unsigned_8;

   function Rotate_Right
     (Value  : Unsigned_8;
      Amount : Natural) return Unsigned_8;

   function Shift_Left
     (Value  : Unsigned_16;
      Amount : Natural) return Unsigned_16;

   function Shift_Right
     (Value  : Unsigned_16;
      Amount : Natural) return Unsigned_16;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_16;
      Amount : Natural) return Unsigned_16;

   function Rotate_Left
     (Value  : Unsigned_16;
      Amount : Natural) return Unsigned_16;

   function Rotate_Right
     (Value  : Unsigned_16;
      Amount : Natural) return Unsigned_16;

   function Shift_Left
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32;

   function Shift_Right
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32;

   function Rotate_Left
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32;

   function Rotate_Right
     (Value  : Unsigned_32;
      Amount : Natural) return Unsigned_32;

   function Shift_Left
     (Value  : Unsigned_64;
      Amount : Natural) return Unsigned_64;

   function Shift_Right
     (Value  : Unsigned_64;
      Amount : Natural) return Unsigned_64;

   function Shift_Right_Arithmetic
     (Value  : Unsigned_64;
      Amount : Natural) return Unsigned_64;

   function Rotate_Left
     (Value  : Unsigned_64;
      Amount : Natural) return Unsigned_64;

   function Rotate_Right
     (Value  : Unsigned_64;
      Amount : Natural) return Unsigned_64;

   pragma Import (Intrinsic, Shift_Left);
   pragma Import (Intrinsic, Shift_Right);
   pragma Import (Intrinsic, Shift_Right_Arithmetic);
   pragma Import (Intrinsic, Rotate_Left);
   pragma Import (Intrinsic, Rotate_Right);

   --  IEEE Floating point types. Note that the form of these definitions
   --  ensures that the work on VMS, even if the standard library is compiled
   --  using a Float_Representation pragma for Vax_Float.

   pragma Warnings (Off);
   --  Turn off warnings for targets not providing IEEE floating-point types

   type IEEE_Float_32 is digits 6;
   pragma Float_Representation (IEEE_Float, IEEE_Float_32);

   type IEEE_Float_64 is digits 15;
   pragma Float_Representation (IEEE_Float, IEEE_Float_64);

   --  If there is an IEEE extended float available on the machine, we assume
   --  that it is available as Long_Long_Float.

   --  Note: it is harmless, and explicitly permitted, to include additional
   --  types in interfaces, so it is not wrong to have IEEE_Extended_Float
   --  defined even if the extended format is not available.

   type IEEE_Extended_Float is new Long_Long_Float;

end Interfaces;
