------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                   SYSTEM.INTERRUPT_MANAGEMENT.OPERATIONS                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-2017, Florida State University            --
--                     Copyright (C) 1995-2025, AdaCore                     --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a hardware interrupt version of this package. Many operations are
--  null as this package supports the use of Ada interrupt handling facilities
--  for signals, while those facilities are used for hardware interrupts on
--  these targets.

with Ada.Exceptions;

with Interfaces.C;

with System.OS_Interface;

package body System.Interrupt_Management.Operations is

   use Ada.Exceptions;
   use Interfaces.C;
   use System.OS_Interface;

   ----------------------------
   -- Thread_Block_Interrupt --
   ----------------------------

   procedure Thread_Block_Interrupt
     (Interrupt : Interrupt_ID)
   is
      pragma Unreferenced (Interrupt);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Thread_Block_Interrupt unimplemented");
   end Thread_Block_Interrupt;

   ------------------------------
   -- Thread_Unblock_Interrupt --
   ------------------------------

   procedure Thread_Unblock_Interrupt
     (Interrupt : Interrupt_ID)
   is
      pragma Unreferenced (Interrupt);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Thread_Unblock_Interrupt unimplemented");
   end Thread_Unblock_Interrupt;

   ------------------------
   -- Set_Interrupt_Mask --
   ------------------------

   procedure Set_Interrupt_Mask (Mask : access Interrupt_Mask) is
      pragma Unreferenced (Mask);
   begin
      null;
   end Set_Interrupt_Mask;

   procedure Set_Interrupt_Mask
     (Mask  : access Interrupt_Mask;
      OMask : access Interrupt_Mask)
   is
      pragma Unreferenced (Mask, OMask);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Set_Interrupt_Mask unimplemented");
   end Set_Interrupt_Mask;

   ------------------------
   -- Get_Interrupt_Mask --
   ------------------------

   procedure Get_Interrupt_Mask (Mask : access Interrupt_Mask) is
      pragma Unreferenced (Mask);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Get_Interrupt_Mask unimplemented");
   end Get_Interrupt_Mask;

   --------------------
   -- Interrupt_Wait --
   --------------------

   function Interrupt_Wait
     (Mask : access Interrupt_Mask) return Interrupt_ID
   is
      pragma Unreferenced (Mask);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Interrupt_Wait unimplemented");
      return 0;
   end Interrupt_Wait;

   ----------------------------
   -- Install_Default_Action --
   ----------------------------

   procedure Install_Default_Action (Interrupt : Interrupt_ID) is
      pragma Unreferenced (Interrupt);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Install_Default_Action unimplemented");
   end Install_Default_Action;

   ---------------------------
   -- Install_Ignore_Action --
   ---------------------------

   procedure Install_Ignore_Action (Interrupt : Interrupt_ID) is
      pragma Unreferenced (Interrupt);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Install_Ignore_Action unimplemented");
   end Install_Ignore_Action;

   -------------------------
   -- Fill_Interrupt_Mask --
   -------------------------

   procedure Fill_Interrupt_Mask (Mask : access Interrupt_Mask) is
      pragma Unreferenced (Mask);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Fill_Interrupt_Mask unimplemented");
   end Fill_Interrupt_Mask;

   --------------------------
   -- Empty_Interrupt_Mask --
   --------------------------

   procedure Empty_Interrupt_Mask (Mask : access Interrupt_Mask) is
      pragma Unreferenced (Mask);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Empty_Interrupt_Mask unimplemented");
   end Empty_Interrupt_Mask;

   ---------------------------
   -- Add_To_Interrupt_Mask --
   ---------------------------

   procedure Add_To_Interrupt_Mask
     (Mask      : access Interrupt_Mask;
      Interrupt : Interrupt_ID)
   is
      pragma Unreferenced (Mask, Interrupt);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Add_To_Interrupt_Mask unimplemented");
   end Add_To_Interrupt_Mask;

   --------------------------------
   -- Delete_From_Interrupt_Mask --
   --------------------------------

   procedure Delete_From_Interrupt_Mask
     (Mask      : access Interrupt_Mask;
      Interrupt : Interrupt_ID)
   is
      pragma Unreferenced (Mask, Interrupt);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Delete_From_Interrupt_Mask unimplemented");
   end Delete_From_Interrupt_Mask;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (Mask      : access Interrupt_Mask;
      Interrupt : Interrupt_ID) return Boolean
   is
      pragma Unreferenced (Mask, Interrupt);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Is_Member unimplemented");
      return False;
   end Is_Member;

   -------------------------
   -- Copy_Interrupt_Mask --
   -------------------------

   procedure Copy_Interrupt_Mask
     (X : out Interrupt_Mask;
      Y : Interrupt_Mask) is
      pragma Unreferenced (X, Y);
   begin
      Raise_Exception
        (Program_Error'Identity,
         "Copy_Interrupt_Mask unimplemented");
   end Copy_Interrupt_Mask;

   ----------------------------
   -- Interrupt_Self_Process --
   ----------------------------

   procedure Interrupt_Self_Process (Interrupt : Interrupt_ID) is
      Result : Interfaces.C.int;
   begin
      Result := kill (getpid, Signal (Interrupt));
      pragma Assert (Result = 0);
   end Interrupt_Self_Process;

   --------------------------
   -- Setup_Interrupt_Mask --
   --------------------------

   procedure Setup_Interrupt_Mask is
   begin
      --  Nothing to be done. Ada interrupt facilities on VxWorks do not use
      --  signals but hardware interrupts. Therefore, interrupt management does
      --  not need anything related to signal masking. Note that this procedure
      --  cannot raise an exception (as some others in this package) because
      --  the generic implementation of the Timer_Server and timing events make
      --  explicit calls to this routine to make ensure proper signal masking
      --  on targets needed that.

      null;
   end Setup_Interrupt_Mask;

end System.Interrupt_Management.Operations;
