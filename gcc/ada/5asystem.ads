------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                               S Y S T E M                                --
--                                                                          --
--                                 S p e c                                  --
--                           (DEC Unix Version)                             --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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
pragma Pure (System);
--  Note that we take advantage of the implementation permission to
--  make this unit Pure instead of Preelaborable, see RM 13.7(36)

   type Name is (SYSTEM_NAME_GNAT);
   System_Name : constant Name := SYSTEM_NAME_GNAT;

   --  System-Dependent Named Numbers

   Min_Int               : constant := Long_Long_Integer'First;
   Max_Int               : constant := Long_Long_Integer'Last;

   Max_Binary_Modulus    : constant := 2 ** Long_Long_Integer'Size;
   Max_Nonbinary_Modulus : constant := Integer'Last;

   Max_Base_Digits       : constant := Long_Long_Float'Digits;
   Max_Digits            : constant := Long_Long_Float'Digits;

   Max_Mantissa          : constant := 63;
   Fine_Delta            : constant := 2.0 ** (-Max_Mantissa);

   Tick                  : constant := Standard'Tick;

   --  Storage-related Declarations

   type Address is private;
   Null_Address : constant Address;

   Storage_Unit : constant := Standard'Storage_Unit;
   Word_Size    : constant := Standard'Word_Size;
   Memory_Size  : constant := 2 ** Standard'Address_Size;

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

   --  Priority-related Declarations (RM D.1)

   Max_Priority : constant Positive := 30;

   Max_Interrupt_Priority : constant Positive := 31;

   subtype Any_Priority is Integer
     range 0 .. Standard'Max_Interrupt_Priority;

   subtype Priority is Any_Priority
     range 0 .. Standard'Max_Priority;

   --  Functional notation is needed in the following to avoid visibility
   --  problems when this package is compiled through rtsfind in the middle
   --  of another compilation.

   subtype Interrupt_Priority is Any_Priority
     range
       Standard."+" (Standard'Max_Priority,  1) ..
         Standard'Max_Interrupt_Priority;

   Default_Priority : constant Priority :=
     Standard."/" (Standard."+" (Priority'First, Priority'Last), 2);

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

   AAMP                      : constant Boolean := False;
   Command_Line_Args         : constant Boolean := True;
   Denorm                    : constant Boolean := False;
   Frontend_Layout           : constant Boolean := False;
   Functions_Return_By_DSP   : constant Boolean := True;
   Long_Shifts_Inlined       : constant Boolean := True;
   High_Integrity_Mode       : constant Boolean := False;
   Machine_Overflows         : constant Boolean := False;
   Machine_Rounds            : constant Boolean := True;
   OpenVMS                   : constant Boolean := False;
   Signed_Zeros              : constant Boolean := True;
   Stack_Check_Default       : constant Boolean := True;
   Stack_Check_Probes        : constant Boolean := True;
   Use_Ada_Main_Program_Name : constant Boolean := False;
   ZCX_By_Default            : constant Boolean := True;
   GCC_ZCX_Support           : constant Boolean := False;
   Front_End_ZCX_Support     : constant Boolean := True;

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
     (Priority'First     => 16,
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
