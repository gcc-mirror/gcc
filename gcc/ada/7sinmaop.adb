------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                   SYSTEM.INTERRUPT_MANAGEMENT.OPERATIONS                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1997-1998, Florida State University            --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is a POSIX-like version of this package.
--  Note: this file can only be used for POSIX compliant systems.

with Interfaces.C;
--  used for int
--           size_t
--           unsigned

with System.OS_Interface;
--  used for various type, constant, and operations

with System.Storage_Elements;
--  used for To_Address
--           Integer_Address

with Unchecked_Conversion;

package body System.Interrupt_Management.Operations is

   use Interfaces.C;
   use System.OS_Interface;

   type Interrupt_Mask_Ptr is access all Interrupt_Mask;

   function "+" is new
     Unchecked_Conversion (Interrupt_Mask_Ptr, sigset_t_ptr);

   ---------------------
   -- Local Variables --
   ---------------------

   Initial_Action : array (Signal) of aliased struct_sigaction;

   Default_Action : aliased struct_sigaction;

   Ignore_Action  : aliased struct_sigaction;

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
      Result := pthread_sigmask (SIG_BLOCK, Mask'Unchecked_Access, null);
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
      Result := pthread_sigmask (SIG_UNBLOCK, Mask'Unchecked_Access, null);
      pragma Assert (Result = 0);
   end Thread_Unblock_Interrupt;

   ------------------------
   -- Set_Interrupt_Mask --
   ------------------------

   procedure Set_Interrupt_Mask (Mask : access Interrupt_Mask) is
      Result   : Interfaces.C.int;

   begin
      Result := pthread_sigmask
        (SIG_SETMASK, +Interrupt_Mask_Ptr (Mask), null);
      pragma Assert (Result = 0);
   end Set_Interrupt_Mask;

   procedure Set_Interrupt_Mask
     (Mask  : access Interrupt_Mask;
      OMask : access Interrupt_Mask)
   is
      Result  : Interfaces.C.int;

   begin
      Result := pthread_sigmask
        (SIG_SETMASK, +Interrupt_Mask_Ptr (Mask), +Interrupt_Mask_Ptr (OMask));
      pragma Assert (Result = 0);
   end Set_Interrupt_Mask;

   ------------------------
   -- Get_Interrupt_Mask --
   ------------------------

   procedure Get_Interrupt_Mask (Mask : access Interrupt_Mask) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_sigmask
        (SIG_SETMASK, null, +Interrupt_Mask_Ptr (Mask));
      pragma Assert (Result = 0);
   end Get_Interrupt_Mask;

   --------------------
   -- Interrupt_Wait --
   --------------------

   function Interrupt_Wait
     (Mask : access Interrupt_Mask)
      return Interrupt_ID
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
      Y : Interrupt_Mask)
   is
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

begin

   declare
      mask    : aliased sigset_t;
      allmask : aliased sigset_t;
      Result  : Interfaces.C.int;

   begin
      for Sig in 1 .. Signal'Last loop
         Result := sigaction
           (Sig, null, Initial_Action (Sig)'Unchecked_Access);

         --  ??? [assert 1]
         --  we can't check Result here since sigaction will fail on
         --  SIGKILL, SIGSTOP, and possibly other signals
         --  pragma Assert (Result = 0);

      end loop;

      --  Setup the masks to be exported.

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

      for I in Interrupt_ID loop
         if Keep_Unmasked (I) then
            Result := sigaddset (mask'Access, Signal (I));
            pragma Assert (Result = 0);
            Result := sigdelset (allmask'Access, Signal (I));
            pragma Assert (Result = 0);
         end if;
      end loop;

      --  The Keep_Unmasked signals should be unmasked for Environment task

      Result := pthread_sigmask (SIG_UNBLOCK, mask'Unchecked_Access, null);
      pragma Assert (Result = 0);

      --  Get the signal mask of the Environment Task

      Result := pthread_sigmask (SIG_SETMASK, null, mask'Unchecked_Access);
      pragma Assert (Result = 0);

      --  Setup the constants exported

      Environment_Mask := Interrupt_Mask (mask);

      All_Tasks_Mask := Interrupt_Mask (allmask);
   end;

end System.Interrupt_Management.Operations;
