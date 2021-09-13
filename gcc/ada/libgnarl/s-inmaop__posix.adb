------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  SYSTEM.INTERRUPT_MANAGEMENT.OPERATIONS                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-2017, Florida State University            --
--                     Copyright (C) 1995-2021, AdaCore                     --
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

--  This is a POSIX-like version of this package

--  Note: this file can only be used for POSIX compliant systems

with Interfaces.C;

with System.OS_Interface;
with System.Storage_Elements;

package body System.Interrupt_Management.Operations is

   use Interfaces.C;
   use System.OS_Interface;

   ---------------------
   -- Local Variables --
   ---------------------

   Initial_Action : array (Signal) of aliased struct_sigaction;

   Default_Action : aliased struct_sigaction;
   pragma Warnings (Off, Default_Action);

   Ignore_Action : aliased struct_sigaction;

   ----------------------------
   -- Thread_Block_Interrupt --
   ----------------------------

   procedure Thread_Block_Interrupt
     (Interrupt : Interrupt_ID)
   is
      Result : Interfaces.C.int;
      Mask   : aliased sigset_t;
   begin
      Result := sigemptyset (Mask'Access);
      pragma Assert (Result = 0);
      Result := sigaddset (Mask'Access, Signal (Interrupt));
      pragma Assert (Result = 0);
      Result := pthread_sigmask (SIG_BLOCK, Mask'Access, null);
      pragma Assert (Result = 0);
   end Thread_Block_Interrupt;

   ------------------------------
   -- Thread_Unblock_Interrupt --
   ------------------------------

   procedure Thread_Unblock_Interrupt
     (Interrupt : Interrupt_ID)
   is
      Mask   : aliased sigset_t;
      Result : Interfaces.C.int;
   begin
      Result := sigemptyset (Mask'Access);
      pragma Assert (Result = 0);
      Result := sigaddset (Mask'Access, Signal (Interrupt));
      pragma Assert (Result = 0);
      Result := pthread_sigmask (SIG_UNBLOCK, Mask'Access, null);
      pragma Assert (Result = 0);
   end Thread_Unblock_Interrupt;

   ------------------------
   -- Set_Interrupt_Mask --
   ------------------------

   procedure Set_Interrupt_Mask (Mask : access Interrupt_Mask) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_sigmask (SIG_SETMASK, Mask, null);
      pragma Assert (Result = 0);
   end Set_Interrupt_Mask;

   procedure Set_Interrupt_Mask
     (Mask  : access Interrupt_Mask;
      OMask : access Interrupt_Mask)
   is
      Result  : Interfaces.C.int;
   begin
      Result := pthread_sigmask (SIG_SETMASK, Mask, OMask);
      pragma Assert (Result = 0);
   end Set_Interrupt_Mask;

   ------------------------
   -- Get_Interrupt_Mask --
   ------------------------

   procedure Get_Interrupt_Mask (Mask : access Interrupt_Mask) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_sigmask (SIG_SETMASK, null, Mask);
      pragma Assert (Result = 0);
   end Get_Interrupt_Mask;

   --------------------
   -- Interrupt_Wait --
   --------------------

   function Interrupt_Wait
     (Mask : access Interrupt_Mask) return Interrupt_ID
   is
      Result : Interfaces.C.int;
      Sig    : aliased Signal;

   begin
      Result := sigwait (Mask, Sig'Access);

      if Result /= 0 then
         return 0;
      end if;

      return Interrupt_ID (Sig);
   end Interrupt_Wait;

   ----------------------------
   -- Install_Default_Action --
   ----------------------------

   procedure Install_Default_Action (Interrupt : Interrupt_ID) is
      Result : Interfaces.C.int;
   begin
      Result := sigaction
        (Signal (Interrupt),
         Initial_Action (Signal (Interrupt))'Access, null);
      pragma Assert (Result = 0);
   end Install_Default_Action;

   ---------------------------
   -- Install_Ignore_Action --
   ---------------------------

   procedure Install_Ignore_Action (Interrupt : Interrupt_ID) is
      Result : Interfaces.C.int;
   begin
      Result := sigaction (Signal (Interrupt), Ignore_Action'Access, null);
      pragma Assert (Result = 0);
   end Install_Ignore_Action;

   -------------------------
   -- Fill_Interrupt_Mask --
   -------------------------

   procedure Fill_Interrupt_Mask (Mask : access Interrupt_Mask) is
      Result : Interfaces.C.int;
   begin
      Result := sigfillset (Mask);
      pragma Assert (Result = 0);
   end Fill_Interrupt_Mask;

   --------------------------
   -- Empty_Interrupt_Mask --
   --------------------------

   procedure Empty_Interrupt_Mask (Mask : access Interrupt_Mask) is
      Result : Interfaces.C.int;
   begin
      Result := sigemptyset (Mask);
      pragma Assert (Result = 0);
   end Empty_Interrupt_Mask;

   ---------------------------
   -- Add_To_Interrupt_Mask --
   ---------------------------

   procedure Add_To_Interrupt_Mask
     (Mask      : access Interrupt_Mask;
      Interrupt : Interrupt_ID)
   is
      Result : Interfaces.C.int;
   begin
      Result := sigaddset (Mask, Signal (Interrupt));
      pragma Assert (Result = 0);
   end Add_To_Interrupt_Mask;

   --------------------------------
   -- Delete_From_Interrupt_Mask --
   --------------------------------

   procedure Delete_From_Interrupt_Mask
     (Mask      : access Interrupt_Mask;
      Interrupt : Interrupt_ID)
   is
      Result : Interfaces.C.int;
   begin
      Result := sigdelset (Mask, Signal (Interrupt));
      pragma Assert (Result = 0);
   end Delete_From_Interrupt_Mask;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (Mask      : access Interrupt_Mask;
      Interrupt : Interrupt_ID) return Boolean
   is
      Result : Interfaces.C.int;
   begin
      Result := sigismember (Mask, Signal (Interrupt));
      pragma Assert (Result = 0 or else Result = 1);
      return Result = 1;
   end Is_Member;

   -------------------------
   -- Copy_Interrupt_Mask --
   -------------------------

   procedure Copy_Interrupt_Mask
     (X : out Interrupt_Mask;
      Y : Interrupt_Mask) is
   begin
      X := Y;
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
      --  Mask task for all signals. The original mask of the Environment task
      --  will be recovered by Interrupt_Manager task during the elaboration
      --  of s-interr.adb.

      Set_Interrupt_Mask (All_Tasks_Mask'Access);
   end Setup_Interrupt_Mask;

begin
   declare
      mask    : aliased sigset_t;
      allmask : aliased sigset_t;
      Result  : Interfaces.C.int;

   begin
      Interrupt_Management.Initialize;

      for Sig in 1 .. Signal'Last loop
         Result := sigaction
           (Sig, null, Initial_Action (Sig)'Access);

         --  ??? [assert 1]
         --  we can't check Result here since sigaction will fail on
         --  SIGKILL, SIGSTOP, and possibly other signals
         --  pragma Assert (Result = 0);

      end loop;

      --  Setup the masks to be exported

      Result := sigemptyset (mask'Access);
      pragma Assert (Result = 0);

      Result := sigfillset (allmask'Access);
      pragma Assert (Result = 0);

      Default_Action.sa_flags   := 0;
      Default_Action.sa_mask    := mask;
      Default_Action.sa_handler :=
        Storage_Elements.To_Address
          (Storage_Elements.Integer_Address (SIG_DFL));

      Ignore_Action.sa_flags   := 0;
      Ignore_Action.sa_mask    := mask;
      Ignore_Action.sa_handler :=
        Storage_Elements.To_Address
          (Storage_Elements.Integer_Address (SIG_IGN));

      for J in Interrupt_ID loop
         if Keep_Unmasked (J) then
            Result := sigaddset (mask'Access, Signal (J));
            pragma Assert (Result = 0);
            Result := sigdelset (allmask'Access, Signal (J));
            pragma Assert (Result = 0);
         end if;
      end loop;

      --  The Keep_Unmasked signals should be unmasked for Environment task

      Result := pthread_sigmask (SIG_UNBLOCK, mask'Access, null);
      pragma Assert (Result = 0);

      --  Get the signal mask of the Environment Task

      Result := pthread_sigmask (SIG_SETMASK, null, mask'Access);
      pragma Assert (Result = 0);

      --  Setup the constants exported

      Environment_Mask := Interrupt_Mask (mask);

      All_Tasks_Mask := Interrupt_Mask (allmask);
   end;

end System.Interrupt_Management.Operations;
