------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                       A D A . I N T E R R U P T S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with System.Interrupts;
with System.Multiprocessors;
with Ada.Task_Identification;

package Ada.Interrupts is

   type Interrupt_ID is new System.Interrupts.Ada_Interrupt_ID;

   type Parameterless_Handler is access protected procedure;

   function Is_Reserved (Interrupt : Interrupt_ID) return Boolean with
     SPARK_Mode,
     Volatile_Function,
     Global => Ada.Task_Identification.Tasking_State;

   function Is_Attached (Interrupt : Interrupt_ID) return Boolean with
     SPARK_Mode,
     Volatile_Function,
     Global => Ada.Task_Identification.Tasking_State;

   function Current_Handler
     (Interrupt : Interrupt_ID) return Parameterless_Handler
   with
     SPARK_Mode => Off,
     Global     => null;

   procedure Attach_Handler
     (New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID)
   with
     SPARK_Mode => Off,
     Global     => null;

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID)
   with
     SPARK_Mode => Off,
     Global     => null;

   procedure Detach_Handler (Interrupt : Interrupt_ID) with
     SPARK_Mode,
     Global => (In_Out => Ada.Task_Identification.Tasking_State);

   function Reference (Interrupt : Interrupt_ID) return System.Address with
     SPARK_Mode => Off,
     Global     => null;

   function Get_CPU
     (Interrupt : Interrupt_ID) return System.Multiprocessors.CPU_Range
   with
     SPARK_Mode,
     Volatile_Function,
     Global => Ada.Task_Identification.Tasking_State;

private
   pragma Inline (Is_Reserved);
   pragma Inline (Is_Attached);
   pragma Inline (Current_Handler);
   pragma Inline (Attach_Handler);
   pragma Inline (Detach_Handler);
   pragma Inline (Exchange_Handler);
   pragma Inline (Get_CPU);
end Ada.Interrupts;
