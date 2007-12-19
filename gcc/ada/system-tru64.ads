------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                               S Y S T E M                                --
--                                                                          --
--                                 S p e c                                  --
--                           (DEC Unix Version)                             --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
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

package System is
   pragma Pure;
   --  Note that we take advantage of the implementation permission to make
   --  this unit Pure instead of Preelaborable; see RM 13.7.1(15). In Ada
   --  2005, this is Pure in any case (AI-362).

   type Name is (SYSTEM_NAME_GNAT);
   System_Name : constant Name := SYSTEM_NAME_GNAT;

   --  System-Dependent Named Numbers

   Min_Int               : constant := Long_Long_Integer'First;
   Max_Int               : constant := Long_Long_Integer'Last;

   Max_Binary_Modulus    : constant := 2 ** Long_Long_Integer'Size;
   Max_Nonbinary_Modulus : constant := 2 ** Integer'Size - 1;

   Max_Base_Digits       : constant := Long_Long_Float'Digits;
   Max_Digits            : constant := Long_Long_Float'Digits;

   Max_Mantissa          : constant := 63;
   Fine_Delta            : constant := 2.0 ** (-Max_Mantissa);

   Tick                  : constant := 1.0 / 1024.0;

   --  Storage-related Declarations

   type Address is private;
   pragma Preelaborable_Initialization (Address);
   Null_Address : constant Address;

   Storage_Unit : constant := 8;
   Word_Size    : constant := 64;
   Memory_Size  : constant := 2 ** 64;

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
   Default_Bit_Order : constant Bit_Order := Low_Order_First;
   pragma Warnings (Off, Default_Bit_Order); -- kill constant condition warning

   --  Priority-related Declarations (RM D.1)

   Max_Priority           : constant Positive := 60;
   Max_Interrupt_Priority : constant Positive := 63;

   subtype Any_Priority       is Integer      range  0 .. 63;
   subtype Priority           is Any_Priority range  0 .. 60;
   subtype Interrupt_Priority is Any_Priority range 61 .. 63;

   Default_Priority : constant Priority := 30;

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
   Backend_Overflow_Checks   : constant Boolean := False;
   Command_Line_Args         : constant Boolean := True;
   Configurable_Run_Time     : constant Boolean := False;
   Denorm                    : constant Boolean := False;
   Duration_32_Bits          : constant Boolean := False;
   Exit_Status_Supported     : constant Boolean := True;
   Fractional_Fixed_Ops      : constant Boolean := False;
   Frontend_Layout           : constant Boolean := False;
   Machine_Overflows         : constant Boolean := False;
   Machine_Rounds            : constant Boolean := True;
   Preallocated_Stacks       : constant Boolean := False;
   Signed_Zeros              : constant Boolean := True;
   Stack_Check_Default       : constant Boolean := True;
   Stack_Check_Probes        : constant Boolean := True;
   Stack_Check_Limits        : constant Boolean := False;
   Support_64_Bit_Divides    : constant Boolean := True;
   Support_Aggregates        : constant Boolean := True;
   Support_Composite_Assign  : constant Boolean := True;
   Support_Composite_Compare : constant Boolean := True;
   Support_Long_Shifts       : constant Boolean := True;
   Always_Compatible_Rep     : constant Boolean := True;
   Suppress_Standard_Library : constant Boolean := False;
   Use_Ada_Main_Program_Name : constant Boolean := False;
   ZCX_By_Default            : constant Boolean := True;
   GCC_ZCX_Support           : constant Boolean := True;

   --  Note: Denorm is False because denormals are only handled properly
   --  if the -mieee switch is set, and we do not require this usage.

   ---------------------------
   -- Underlying Priorities --
   ---------------------------

   --  Important note: this section of the file must come AFTER the
   --  definition of the system implementation parameters to ensure
   --  that the value of these parameters is available for analysis
   --  of the declarations here (using Rtsfind at compile time).

   --  The underlying priorities table provides a generalized mechanism
   --  for mapping from Ada priorities to system priorities. In some
   --  cases a 1-1 mapping is not the convenient or optimal choice.

   --  For Dec Unix 4.0d, we use a default 1-to-1 mapping that provides
   --  the full range of 64 priorities available from the operating system.

   --  On DU prior to 4.0d, less than 64 priorities are available so there
   --  are two possibilities:

   --    Limit your range of priorities to the range provided by the
   --    OS (e.g 16 .. 32 on 4.0b)

   --    Replace the standard table as described below

   --  To replace the default values of the Underlying_Priorities mapping,
   --  copy this source file into your build directory, edit the file to
   --  reflect your desired behavior, and recompile with the command:

   --     $ gcc -c -O3 -gnatpgn system.ads

   --  then recompile the run-time parts that depend on this package:

   --     $ gnatmake -a -gnatn -O3 <your application>

   --  then force rebuilding your application if you need different options:

   --     $ gnatmake -f <your options> <your application>

   type Priorities_Mapping is array (Any_Priority) of Integer;
   pragma Suppress_Initialization (Priorities_Mapping);
   --  Suppress initialization in case gnat.adc specifies Normalize_Scalars

   Underlying_Priorities : constant Priorities_Mapping :=

     (Priority'First => 0,

       1 =>  1,  2 =>  2,  3 =>  3,  4 =>  4,  5 =>  5,
       6 =>  6,  7 =>  7,  8 =>  8,  9 =>  9, 10 => 10,
      11 => 11, 12 => 12, 13 => 13, 14 => 14, 15 => 15,
      16 => 16, 17 => 17, 18 => 18, 19 => 19, 20 => 20,
      21 => 21, 22 => 22, 23 => 23, 24 => 24, 25 => 25,
      26 => 26, 27 => 27, 28 => 28, 29 => 29,

      Default_Priority => 30,

      31 => 31, 32 => 32, 33 => 33, 34 => 34, 35 => 35,
      36 => 36, 37 => 37, 38 => 38, 39 => 39, 40 => 40,
      41 => 41, 42 => 42, 43 => 43, 44 => 44, 45 => 45,
      46 => 46, 47 => 47, 48 => 48, 49 => 49, 50 => 50,
      51 => 51, 52 => 52, 53 => 53, 54 => 54, 55 => 55,
      56 => 56, 57 => 57, 58 => 58, 59 => 59,

      Priority'Last => 60,

      61 => 61, 62 => 62,

      Interrupt_Priority'Last => 63);

end System;
