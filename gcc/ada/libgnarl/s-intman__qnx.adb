------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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

--  This is the QNX/Neutrino threads version of this package

--  Make a careful study of all signals available under the OS, to see which
--  need to be reserved, kept always unmasked, or kept always unmasked. Be on
--  the lookout for special signals that may be used by the thread library.

--  Since this is a multi target file, the signal <-> exception mapping
--  is simple minded. If you need a more precise and target specific
--  signal handling, create a new s-intman.adb that will fit your needs.

--  This file assumes that:

--    SIGFPE, SIGILL, SIGSEGV and SIGBUS exist. They are mapped as follows:
--      SIGPFE  => Constraint_Error
--      SIGILL  => Program_Error
--      SIGSEGV => Storage_Error
--      SIGBUS  => Storage_Error

--    SIGINT exists and will be kept unmasked unless the pragma
--     Unreserve_All_Interrupts is specified anywhere in the application.

--    System.OS_Interface contains the following:
--      SIGADAABORT: the signal that will be used to abort tasks.
--      Unmasked: the OS specific set of signals that should be unmasked in
--                all the threads. SIGADAABORT is unmasked by
--                default
--      Reserved: the OS specific set of signals that are reserved.

with System.Task_Primitives;

package body System.Interrupt_Management is

   use Interfaces.C;
   use System.OS_Interface;

   type Interrupt_List is array (Interrupt_ID range <>) of Interrupt_ID;
   Exception_Interrupts : constant Interrupt_List :=
     (SIGFPE, SIGILL, SIGSEGV, SIGBUS);

   Unreserve_All_Interrupts : Interfaces.C.int;
   pragma Import
     (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Signal_Trampoline
     (signo    : Signal;
      siginfo  : System.Address;
      ucontext : System.Address;
      handler  : System.Address);
   pragma Import (C, Signal_Trampoline, "__gnat_sigtramp");
   --  Pass the real handler to a speical function that handles unwinding by
   --  skipping over the kernel signal frame (which doesn't contain any unwind
   --  information).

   procedure Map_Signal
     (signo    : Signal;
      siginfo  : System.Address;
      ucontext : System.Address);
   pragma Import (C, Map_Signal, "__gnat_map_signal");

   function State (Int : Interrupt_ID) return Character;
   pragma Import (C, State, "__gnat_get_interrupt_state");
   --  Get interrupt state. Defined in init.c The input argument is the
   --  interrupt number, and the result is one of the following:

   User    : constant Character := 'u';
   Runtime : constant Character := 'r';
   Default : constant Character := 's';
   --    'n'   this interrupt not set by any Interrupt_State pragma
   --    'u'   Interrupt_State pragma set state to User
   --    'r'   Interrupt_State pragma set state to Runtime
   --    's'   Interrupt_State pragma set state to System (use "default"
   --           system handler)

   procedure Notify_Exception
     (signo    : Signal;
      siginfo  : System.Address;
      ucontext : System.Address);
   --  This function identifies the Ada exception to be raised using the
   --  information when the system received a synchronous signal. Since this
   --  function is machine and OS dependent, different code has to be provided
   --  for different target.

   ----------------------
   -- Notify_Exception --
   ----------------------

   Signal_Mask : aliased sigset_t;
   --  The set of signals handled by Notify_Exception

   procedure Notify_Exception
     (signo    : Signal;
      siginfo  : System.Address;
      ucontext : System.Address)
   is
      Result : Interfaces.C.int;

   begin
      --  With the __builtin_longjmp, the signal mask is not restored, so we
      --  need to restore it explicitly.

      Result := pthread_sigmask (SIG_UNBLOCK, Signal_Mask'Access, null);
      pragma Assert (Result = 0);

      --  Perform the necessary context adjustments prior to a raise
      --  from a signal handler.

      Adjust_Context_For_Raise (signo, ucontext);

      --  Check that treatment of exception propagation here is consistent with
      --  treatment of the abort signal in System.Task_Primitives.Operations.

      Signal_Trampoline (signo, siginfo, ucontext, Map_Signal'Address);
   end Notify_Exception;

   ----------------
   -- Initialize --
   ----------------

   Initialized : Boolean := False;

   procedure Initialize is
      act     : aliased struct_sigaction;
      old_act : aliased struct_sigaction;
      Result  : System.OS_Interface.int;

      Use_Alternate_Stack : constant Boolean :=
                              System.Task_Primitives.Alternate_Stack_Size /= 0;
      --  Whether to use an alternate signal stack for stack overflows

   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      --  Need to call pthread_init very early because it is doing signal
      --  initializations.

      pthread_init;

      Abort_Task_Interrupt := SIGADAABORT;

      act.sa_handler := Notify_Exception'Address;

      --  Setting SA_SIGINFO asks the kernel to pass more than just the signal
      --  number argument to the handler when it is called. The set of extra
      --  parameters includes a pointer to the interrupted context, which the
      --  ZCX propagation scheme needs.

      --  Most man pages for sigaction mention that sa_sigaction should be set
      --  instead of sa_handler when SA_SIGINFO is on.  In practice, the two
      --  fields are actually union'ed and located at the same offset.

      --  On some targets, we set sa_flags to SA_NODEFER so that during the
      --  handler execution we do not change the Signal_Mask to be masked for
      --  the Signal.

      --  This is a temporary fix to the problem that the Signal_Mask is not
      --  restored after the exception (longjmp) from the handler. The right
      --  fix should be made in sigsetjmp so that we save the Signal_Set and
      --  restore it after a longjmp.

      --  Since SA_NODEFER is obsolete, instead we reset explicitly the mask
      --  in the exception handler.

      Result := sigemptyset (Signal_Mask'Access);
      pragma Assert (Result = 0);

      --  Add signals that map to Ada exceptions to the mask

      for J in Exception_Interrupts'Range loop
         if State (Exception_Interrupts (J)) /= Default then
            Result :=
            sigaddset (Signal_Mask'Access, Signal (Exception_Interrupts (J)));
            pragma Assert (Result = 0);
         end if;
      end loop;

      act.sa_mask := Signal_Mask;

      pragma Assert (Keep_Unmasked = (Interrupt_ID'Range => False));
      pragma Assert (Reserve = (Interrupt_ID'Range => False));

      --  Process state of exception signals

      for J in Exception_Interrupts'Range loop
         if State (Exception_Interrupts (J)) /= User then
            Keep_Unmasked (Exception_Interrupts (J)) := True;
            Reserve (Exception_Interrupts (J)) := True;

            if State (Exception_Interrupts (J)) /= Default then
               act.sa_flags := SA_SIGINFO;

               if Use_Alternate_Stack
                 and then Exception_Interrupts (J) = SIGSEGV
               then
                  act.sa_flags := act.sa_flags + SA_ONSTACK;
               end if;

               Result :=
                 sigaction
                   (Signal (Exception_Interrupts (J)), act'Unchecked_Access,
                    old_act'Unchecked_Access);
               pragma Assert (Result = 0);
            end if;
         end if;
      end loop;

      if State (Abort_Task_Interrupt) /= User then
         Keep_Unmasked (Abort_Task_Interrupt) := True;
         Reserve (Abort_Task_Interrupt) := True;
      end if;

      --  Set SIGINT to unmasked state as long as it is not in "User" state.
      --  Check for Unreserve_All_Interrupts last.

      if State (SIGINT) /= User then
         Keep_Unmasked (SIGINT) := True;
         Reserve (SIGINT) := True;
      end if;

      --  Check all signals for state that requires keeping them unmasked and
      --  reserved.

      for J in Interrupt_ID'Range loop
         if State (J) = Default or else State (J) = Runtime then
            Keep_Unmasked (J) := True;
            Reserve (J) := True;
         end if;
      end loop;

      --  Add the set of signals that must always be unmasked for this target

      for J in Unmasked'Range loop
         Keep_Unmasked (Interrupt_ID (Unmasked (J))) := True;
         Reserve (Interrupt_ID (Unmasked (J))) := True;
      end loop;

      --  Add target-specific reserved signals

      if Reserved'Length > 0 then
         for J in Reserved'Range loop
            Reserve (Interrupt_ID (Reserved (J))) := True;
         end loop;
      end if;

      --  Process pragma Unreserve_All_Interrupts. This overrides any settings
      --  due to pragma Interrupt_State:

      if Unreserve_All_Interrupts /= 0 then
         Keep_Unmasked (SIGINT) := False;
         Reserve (SIGINT) := False;
      end if;

      --  We do not really have Signal 0. We just use this value to identify
      --  non-existent signals (see s-intnam.ads). Therefore, Signal should not
      --  be used in all signal related operations hence mark it as reserved.

      Reserve (0) := True;
   end Initialize;

end System.Interrupt_Management;
