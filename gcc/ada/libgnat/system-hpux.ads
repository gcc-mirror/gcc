------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                               S Y S T E M                                --
--                                                                          --
--                                 S p e c                                  --
--                             (HP-UX Version)                              --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package System is
   pragma Pure;
   --  Note that we take advantage of the implementation permission to make
   --  this unit Pure instead of Preelaborable; see RM 13.7.1(15). In Ada
   --  2005, this is Pure in any case (AI-362).

   pragma No_Elaboration_Code_All;
   --  Allow the use of that restriction in units that WITH this unit

   type Name is (SYSTEM_NAME_GNAT);
   System_Name : constant Name := SYSTEM_NAME_GNAT;

   --  System-Dependent Named Numbers

   Min_Int             : constant := -2 ** (Standard'Max_Integer_Size - 1);
   Max_Int             : constant :=  2 ** (Standard'Max_Integer_Size - 1) - 1;

   Max_Binary_Modulus    : constant := 2 ** Standard'Max_Integer_Size;
   Max_Nonbinary_Modulus : constant := 2 ** Integer'Size - 1;

   Max_Base_Digits       : constant := Long_Long_Float'Digits;
   Max_Digits            : constant := Long_Long_Float'Digits;

   Max_Mantissa          : constant := Standard'Max_Integer_Size - 1;
   Fine_Delta            : constant := 2.0 ** (-Max_Mantissa);

   Tick                  : constant := 0.01;

   --  Storage-related Declarations

   type Address is private;
   pragma Preelaborable_Initialization (Address);
   Null_Address : constant Address;

   Storage_Unit : constant := 8;
   Word_Size    : constant := 32;
   Memory_Size  : constant := 2 ** 32;

   --  Address comparison

   function "<"  (Left, Right : Address) return Boolean;
   function "<=" (Left, Right : Address) return Boolean;
   function ">"  (Left, Right : Address) return Boolean;
   function ">=" (Left, Right : Address) return Boolean;
   function "="  (Left, Right : Address) return Boolean;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "=");

   --  Other System-Dependent Declarations

   type Bit_Order is (High_Order_First, Low_Order_First);
   Default_Bit_Order : constant Bit_Order := High_Order_First;
   pragma Warnings (Off, Default_Bit_Order); -- kill constant condition warning

   --  Priority-related Declarations (RM D.1)

   Max_Priority           : constant Positive := 30;
   Max_Interrupt_Priority : constant Positive := 31;

   subtype Any_Priority       is Integer      range  0 .. 31;
   subtype Priority           is Any_Priority range  0 .. 30;
   subtype Interrupt_Priority is Any_Priority range 31 .. 31;

   Default_Priority : constant Priority := 15;

private

   type Address is mod Memory_Size;
   Null_Address : constant Address := 0;

   --------------------------------------
   -- System Implementation Parameters --
   --------------------------------------

   --  These parameters provide information about the target that is used
   --  by the compiler. They are in the private part of System, where they
   --  can be accessed using the special circuitry in the Targparm unit
   --  whose source should be consulted for more detailed descriptions
   --  of the individual switch values.

   Backend_Divide_Checks     : constant Boolean := False;
   Backend_Overflow_Checks   : constant Boolean := True;
   Command_Line_Args         : constant Boolean := True;
   Configurable_Run_Time     : constant Boolean := False;
   Denorm                    : constant Boolean := True;
   Duration_32_Bits          : constant Boolean := False;
   Exit_Status_Supported     : constant Boolean := True;
   Machine_Overflows         : constant Boolean := False;
   Machine_Rounds            : constant Boolean := True;
   Preallocated_Stacks       : constant Boolean := False;
   Signed_Zeros              : constant Boolean := True;
   Stack_Check_Default       : constant Boolean := False;
   Stack_Check_Probes        : constant Boolean := True;
   Stack_Check_Limits        : constant Boolean := False;
   Support_Aggregates        : constant Boolean := True;
   Support_Composite_Assign  : constant Boolean := True;
   Support_Composite_Compare : constant Boolean := True;
   Support_Long_Shifts       : constant Boolean := True;
   Always_Compatible_Rep     : constant Boolean := False;
   Suppress_Standard_Library : constant Boolean := False;
   Use_Ada_Main_Program_Name : constant Boolean := False;
   ZCX_By_Default            : constant Boolean := True;

   --------------------------
   -- Underlying Priorities --
   ---------------------------

   --  Important note: this section of the file must come AFTER the
   --  definition of the system implementation parameters to ensure
   --  that the value of these parameters is available for analysis
   --  of the declarations here (using Rtsfind at compile time).

   --  The underlying priorities table provides a generalized mechanism
   --  for mapping from Ada priorities to system priorities. In some
   --  cases a 1-1 mapping is not the convenient or optimal choice.

   --  For HP/UX DCE Threads, we use the full range of 31 priorities
   --  in the Ada model, but map them by compression onto the more limited
   --  range of priorities available in HP/UX.
   --  For POSIX Threads, this table is ignored.

   --  To replace the default values of the Underlying_Priorities mapping,
   --  copy this source file into your build directory, edit the file to
   --  reflect your desired behavior, and recompile with the command:

   --     $ gcc -c -O2 -gnatpgn system.ads

   --  then recompile the run-time parts that depend on this package:

   --     $ gnatmake -a -gnatn -O2 <your application>

   --  then force rebuilding your application if you need different options:

   --     $ gnatmake -f <your options> <your application>

   type Priorities_Mapping is array (Any_Priority) of Integer;
   pragma Suppress_Initialization (Priorities_Mapping);
   --  Suppress initialization in case gnat.adc specifies Normalize_Scalars

   Underlying_Priorities : constant Priorities_Mapping :=

     (Priority'First => 16,

      1  => 17,
      2  => 18,
      3  => 18,
      4  => 18,
      5  => 18,
      6  => 19,
      7  => 19,
      8  => 19,
      9  => 20,
      10 => 20,
      11 => 21,
      12 => 21,
      13 => 22,
      14 => 23,

      Default_Priority   => 24,

      16 => 25,
      17 => 25,
      18 => 25,
      19 => 26,
      20 => 26,
      21 => 26,
      22 => 27,
      23 => 27,
      24 => 27,
      25 => 28,
      26 => 28,
      27 => 29,
      28 => 29,
      29 => 30,

      Priority'Last      => 30,

      Interrupt_Priority => 31);

end System;
