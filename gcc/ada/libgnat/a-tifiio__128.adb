------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . T E X T _ I O . F I X E D _ I O                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2023, Free Software Foundation, Inc.       --
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

--  -------------------
--  - Fixed point I/O -
--  -------------------

--  The following text documents implementation details of the fixed point
--  input/output routines in the GNAT runtime. The first part describes the
--  general properties of fixed point types as defined by the Ada standard,
--  including the Information Systems Annex.

--  Subsequently these are reduced to implementation constraints and the impact
--  of these constraints on a few possible approaches to input/output is given.
--  Based on this analysis, a specific implementation is selected for use in
--  the GNAT runtime. Finally the chosen algorithms are analyzed numerically in
--  order to provide user-level documentation on limits for range and precision
--  of fixed point types as well as accuracy of input/output conversions.

--  -------------------------------------------
--  - General Properties of Fixed Point Types -
--  -------------------------------------------

--  Operations on fixed point types, other than input/output, are not important
--  for the purpose of this document. Only the set of values that a fixed point
--  type can represent and the input/output operations are significant.

--  Values
--  ------

--  The set of values of a fixed point type comprise the integral multiples of
--  a number called the small of the type. The small can be either a power of
--  two, a power of ten or (if the implementation allows) an arbitrary strictly
--  positive real value.

--  Implementations need to support ordinary fixed point types with a precision
--  of at least 24 bits, and (in order to comply with the Information Systems
--  Annex) decimal fixed point types with at least 18 digits. For the rest, no
--  requirements exist for the minimal small and range that must be supported.

--  Operations
--  ----------

--  [Wide_[Wide_]]Image attribute (see RM 3.5(27.1/2))

--          These attributes return a decimal real literal best approximating
--          the value (rounded away from zero if halfway between) with a
--          single leading character that is either a minus sign or a space,
--          one or more digits before the decimal point (with no redundant
--          leading zeros), a decimal point, and N digits after the decimal
--          point. For a subtype S, the value of N is S'Aft, the smallest
--          positive integer such that (10**N)*S'Delta is greater or equal to
--          one, see RM 3.5.10(5).

--          For an arbitrary small, this means large number arithmetic needs
--          to be performed.

--  Put (see RM A.10.9(22-26))

--          The requirements for Put add no extra constraints over the image
--          attributes, although it would be nice to be able to output more
--          than S'Aft digits after the decimal point for values of subtype S.

--  [Wide_[Wide_]]Value attribute (RM 3.5(39.1/2))

--          Since the input can be given in any base in the range 2..16,
--          accurate conversion to a fixed point number may require
--          arbitrary precision arithmetic if there is no limit on the
--          magnitude of the small of the fixed point type.

--  Get (see RM A.10.9(12-21))

--          The requirements for Get are identical to those of the Value
--          attribute.

--  ------------------------------
--  - Implementation Constraints -
--  ------------------------------

--  The requirements listed above for the input/output operations lead to
--  significant complexity, if no constraints are put on supported smalls.

--  Implementation Strategies
--  -------------------------

--  * Floating point arithmetic
--  * Arbitrary-precision integer arithmetic
--  * Fixed-precision integer arithmetic

--  Although it seems convenient to convert fixed point numbers to floating
--  point and then print them, this leads to a number of restrictions.
--  The first one is precision. The widest floating-point type generally
--  available has 53 bits of mantissa. This means that Fine_Delta cannot
--  be less than 2.0**(-53).

--  In GNAT, Fine_Delta is 2.0**(-127), and Duration for example is a 64-bit
--  type. This means that a floating-point type with 128 bits of mantissa needs
--  to be used, which currently does not exist in any common architecture. It
--  would still be possible to use multi-precision floating point to perform
--  calculations using longer mantissas, but this is a much harder approach.

--  The base conversions needed for input/output of (non-decimal) fixed point
--  types can be seen as pairs of integer multiplications and divisions.

--  Arbitrary-precision integer arithmetic would be suitable for the job at
--  hand, but has the drawback that it is very heavy implementation-wise.
--  Especially in embedded systems, where fixed point types are often used,
--  it may not be desirable to require large amounts of storage and time
--  for fixed I/O operations.

--  Fixed-precision integer arithmetic has the advantage of simplicity and
--  speed. For the most common fixed point types this would be a perfect
--  solution. The downside however may be a restricted set of acceptable
--  fixed point types.

--  Implementation Choices
--  ----------------------

--  The current implementation in the GNAT runtime uses fixed-precision integer
--  arithmetic for fixed point types whose Small is the ratio of two integers
--  whose magnitude is bounded relatively to the size of the mantissa, with a
--  three-tiered approach for 32-bit, 64-bit and 128-bit fixed point types. For
--  other fixed point types, the implementation uses floating-point arithmetic.

--  The exact requirements of the algorithms are analyzed and documented along
--  with the implementation in their respective units.

with Interfaces;
with Ada.Text_IO.Fixed_Aux;
with Ada.Text_IO.Float_Aux;
with System.Img_Fixed_32;  use System.Img_Fixed_32;
with System.Img_Fixed_64;  use System.Img_Fixed_64;
with System.Img_Fixed_128; use System.Img_Fixed_128;
with System.Img_LFlt;      use System.Img_LFlt;
with System.Val_Fixed_32;  use System.Val_Fixed_32;
with System.Val_Fixed_64;  use System.Val_Fixed_64;
with System.Val_Fixed_128; use System.Val_Fixed_128;
with System.Val_LFlt;      use System.Val_LFlt;

package body Ada.Text_IO.Fixed_IO with SPARK_Mode => Off is

   --  Note: we still use the floating-point I/O routines for types whose small
   --  is not the ratio of two sufficiently small integers. This will result in
   --  inaccuracies for fixed point types that require more precision than is
   --  available in Long_Float.

   subtype Int32  is Interfaces.Integer_32;  use type Int32;
   subtype Int64  is Interfaces.Integer_64;  use type Int64;
   subtype Int128 is Interfaces.Integer_128; use type Int128;

   package Aux32 is new
     Ada.Text_IO.Fixed_Aux (Int32, Scan_Fixed32, Set_Image_Fixed32);

   package Aux64 is new
     Ada.Text_IO.Fixed_Aux (Int64, Scan_Fixed64, Set_Image_Fixed64);

   package Aux128 is new
     Ada.Text_IO.Fixed_Aux (Int128, Scan_Fixed128, Set_Image_Fixed128);

   package Aux_Long_Float is new
     Ada.Text_IO.Float_Aux (Long_Float, Scan_Long_Float, Set_Image_Long_Float);

   --  Throughout this generic body, we distinguish between the case where type
   --  Int32 is OK, where type Int64 is OK and where type Int128 is OK. These
   --  boolean constants are used to test for this, such that only code for the
   --  relevant case is included in the instance; that's why the computation of
   --  their value must be fully static (although it is not a static expression
   --  in the RM sense).

   OK_Get_32 : constant Boolean :=
     Num'Base'Object_Size <= 32
       and then
         ((Num'Small_Numerator = 1 and then Num'Small_Denominator <= 2**31)
           or else
          (Num'Small_Denominator = 1 and then Num'Small_Numerator <= 2**31)
           or else
          (Num'Small_Numerator <= 2**27
            and then Num'Small_Denominator <= 2**27));
   --  These conditions are derived from the prerequisites of System.Value_F

   OK_Put_32 : constant Boolean :=
     Num'Base'Object_Size <= 32
       and then
         ((Num'Small_Numerator = 1 and then Num'Small_Denominator <= 2**31)
           or else
          (Num'Small_Denominator = 1 and then Num'Small_Numerator <= 2**31)
           or else
          (Num'Small_Numerator < Num'Small_Denominator
            and then Num'Small_Denominator <= 2**27)
           or else
          (Num'Small_Denominator < Num'Small_Numerator
            and then Num'Small_Numerator <= 2**25));
   --  These conditions are derived from the prerequisites of System.Image_F

   OK_Get_64 : constant Boolean :=
     Num'Base'Object_Size <= 64
       and then
         ((Num'Small_Numerator = 1 and then Num'Small_Denominator <= 2**63)
           or else
          (Num'Small_Denominator = 1 and then Num'Small_Numerator <= 2**63)
           or else
          (Num'Small_Numerator <= 2**59
            and then Num'Small_Denominator <= 2**59));
   --  These conditions are derived from the prerequisites of System.Value_F

   OK_Put_64 : constant Boolean :=
     Num'Base'Object_Size <= 64
       and then
         ((Num'Small_Numerator = 1 and then Num'Small_Denominator <= 2**63)
           or else
          (Num'Small_Denominator = 1 and then Num'Small_Numerator <= 2**63)
           or else
          (Num'Small_Numerator < Num'Small_Denominator
            and then Num'Small_Denominator <= 2**59)
           or else
          (Num'Small_Denominator < Num'Small_Numerator
            and then Num'Small_Numerator <= 2**53));
   --  These conditions are derived from the prerequisites of System.Image_F

   OK_Get_128 : constant Boolean :=
     Num'Base'Object_Size <= 128
       and then
         ((Num'Small_Numerator = 1 and then Num'Small_Denominator <= 2**127)
           or else
          (Num'Small_Denominator = 1 and then Num'Small_Numerator <= 2**127)
           or else
          (Num'Small_Numerator <= 2**123
            and then Num'Small_Denominator <= 2**123));
   --  These conditions are derived from the prerequisites of System.Value_F

   OK_Put_128 : constant Boolean :=
     Num'Base'Object_Size <= 128
       and then
         ((Num'Small_Numerator = 1 and then Num'Small_Denominator <= 2**127)
           or else
          (Num'Small_Denominator = 1 and then Num'Small_Numerator <= 2**127)
           or else
          (Num'Small_Numerator < Num'Small_Denominator
            and then Num'Small_Denominator <= 2**123)
           or else
          (Num'Small_Denominator < Num'Small_Numerator
            and then Num'Small_Numerator <= 2**122));
   --  These conditions are derived from the prerequisites of System.Image_F

   E : constant Natural :=
         127 - 64 * Boolean'Pos (OK_Put_64) - 32 * Boolean'Pos (OK_Put_32);
   --  T'Size - 1 for the selected Int{32,64,128}

   F0 : constant Natural := 0;
   F1 : constant Natural :=
          F0 + 38 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F0) >= 1.0E+38);
   F2 : constant Natural :=
          F1 + 19 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F1) >= 1.0E+19);
   F3 : constant Natural :=
          F2 +  9 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F2) >= 1.0E+9);
   F4 : constant Natural :=
          F3 +  5 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F3) >= 1.0E+5);
   F5 : constant Natural :=
          F4 +  3 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F4) >= 1.0E+3);
   F6 : constant Natural :=
          F5 +  2 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F5) >= 1.0E+2);
   F7 : constant Natural :=
          F6 +  1 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F6) >= 1.0E+1);
   --  Binary search for the number of digits - 1 before the decimal point of
   --  the product 2.0**E * Num'Small.

   For0 : constant Natural := 2 + F7;
   --  Fore value for the fixed point type whose mantissa is Int{32,64,128} and
   --  whose small is Num'Small.

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
      pragma Unsuppress (Range_Check);

   begin
      if OK_Get_32 then
         Item := Num'Fixed_Value
                   (Aux32.Get (File, Width,
                               -Num'Small_Numerator,
                               -Num'Small_Denominator));
      elsif OK_Get_64 then
         Item := Num'Fixed_Value
                   (Aux64.Get (File, Width,
                               -Num'Small_Numerator,
                               -Num'Small_Denominator));
      elsif OK_Get_128 then
         Item := Num'Fixed_Value
                   (Aux128.Get (File, Width,
                                -Num'Small_Numerator,
                                -Num'Small_Denominator));
      else
         Aux_Long_Float.Get (File, Long_Float (Item), Width);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (Item  : out Num;
      Width : Field := 0)
   is
   begin
      Get (Current_In, Item, Width);
   end Get;

   procedure Get
     (From : String;
      Item : out Num;
      Last : out Positive)
   is
      pragma Unsuppress (Range_Check);

   begin
      if OK_Get_32 then
         Item := Num'Fixed_Value
                   (Aux32.Gets (From, Last,
                                -Num'Small_Numerator,
                                -Num'Small_Denominator));
      elsif OK_Get_64 then
         Item := Num'Fixed_Value
                   (Aux64.Gets (From, Last,
                                -Num'Small_Numerator,
                                -Num'Small_Denominator));
      elsif OK_Get_128 then
         Item := Num'Fixed_Value
                   (Aux128.Gets (From, Last,
                                 -Num'Small_Numerator,
                                 -Num'Small_Denominator));
      else
         Aux_Long_Float.Gets (From, Long_Float (Item), Last);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      if OK_Put_32 then
         Aux32.Put (File, Int32'Integer_Value (Item), Fore, Aft, Exp,
                    -Num'Small_Numerator, -Num'Small_Denominator,
                    For0, Num'Aft);
      elsif OK_Put_64 then
         Aux64.Put (File, Int64'Integer_Value (Item), Fore, Aft, Exp,
                    -Num'Small_Numerator, -Num'Small_Denominator,
                    For0, Num'Aft);
      elsif OK_Put_128 then
         Aux128.Put (File, Int128'Integer_Value (Item), Fore, Aft, Exp,
                     -Num'Small_Numerator, -Num'Small_Denominator,
                     For0, Num'Aft);
      else
         Aux_Long_Float.Put (File, Long_Float (Item), Fore, Aft, Exp);
      end if;
   end Put;

   procedure Put
     (Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      Put (Current_Out, Item, Fore, Aft, Exp);
   end Put;

   procedure Put
     (To   : out String;
      Item : Num;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      if OK_Put_32 then
         Aux32.Puts (To, Int32'Integer_Value (Item), Aft, Exp,
                     -Num'Small_Numerator, -Num'Small_Denominator,
                     For0, Num'Aft);
      elsif OK_Put_64 then
         Aux64.Puts (To, Int64'Integer_Value (Item), Aft, Exp,
                     -Num'Small_Numerator, -Num'Small_Denominator,
                     For0, Num'Aft);
      elsif OK_Put_128 then
         Aux128.Puts (To, Int128'Integer_Value (Item), Aft, Exp,
                      -Num'Small_Numerator, -Num'Small_Denominator,
                      For0, Num'Aft);
      else
         Aux_Long_Float.Puts (To, Long_Float (Item), Aft, Exp);
      end if;
   end Put;

end Ada.Text_IO.Fixed_IO;
