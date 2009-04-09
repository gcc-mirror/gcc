------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  This is the VxWorks version of this package

--  Make a careful study of all signals available under the OS, to see which
--  need to be reserved, kept always unmasked, or kept always unmasked. Be on
--  the lookout for special signals that may be used by the thread library.

package body System.Interrupt_Management is

   use System.OS_Interface;
   use type Interfaces.C.int;

   type Signal_List is array (Signal_ID range <>) of Signal_ID;
   Exception_Signals : constant Signal_List (1 .. 4) :=
                         (SIGFPE, SIGILL, SIGSEGV, SIGBUS);

   Exception_Action : aliased struct_sigaction;
   --  Keep this variable global so that it is initialized only once

   procedure Notify_Exception
     (signo      : Signal;
      siginfo    : System.Address;
      sigcontext : System.Address);
   pragma Import (C, Notify_Exception, "__gnat_error_handler");
   --  Map signal to Ada exception and raise it.  Different versions
   --  of VxWorks need different mappings.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function State (Int : Interrupt_ID) return Character;
   pragma Import (C, State, "__gnat_get_interrupt_state");
   --  Get interrupt state. Defined in init.c The input argument is the
   --  interrupt number, and the result is one of the following:

   Runtime : constant Character := 'r';
   Default : constant Character := 's';
   --    'n'   this interrupt not set by any Interrupt_State pragma
   --    'u'   Interrupt_State pragma set state to User
   --    'r'   Interrupt_State pragma set state to Runtime
   --    's'   Interrupt_State pragma set state to System (use "default"
   --           system handler)

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   --  Since there is no signal inheritance between VxWorks tasks, we need
   --  to initialize signal handling in each task.

   procedure Initialize_Interrupts is
      Result  : int;
      old_act : aliased struct_sigaction;
   begin
      for J in Exception_Signals'Range loop
         Result :=
           sigaction
             (Signal (Exception_Signals (J)), Exception_Action'Access,
              old_act'Unchecked_Access);
         pragma Assert (Result = 0);
      end loop;
   end Initialize_Interrupts;

   ----------------
   -- Initialize --
   ----------------

   Initialized : Boolean := False;
   --  Set to True once Initialize is called, further calls have no effect

   procedure Initialize is
      mask   : aliased sigset_t;
      Result : int;

   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      --  Change this if you want to use another signal for task abort.
      --  SIGTERM might be a good one.

      Abort_Task_Interrupt := SIGABRT;

      Exception_Action.sa_handler := Notify_Exception'Address;
      Exception_Action.sa_flags := SA_ONSTACK + SA_SIGINFO;
      Result := sigemptyset (mask'Access);
      pragma Assert (Result = 0);

      for J in Exception_Signals'Range loop
         Result := sigaddset (mask'Access, Signal (Exception_Signals (J)));
         pragma Assert (Result = 0);
      end loop;

      Exception_Action.sa_mask := mask;

      --  Initialize hardware interrupt handling

      pragma Assert (Reserve = (Interrupt_ID'Range => False));

      --  Check all interrupts for state that requires keeping them reserved

      for J in Interrupt_ID'Range loop
         if State (J) = Default or else State (J) = Runtime then
            Reserve (J) := True;
         end if;
      end loop;

      --  Add exception signals to the set of unmasked signals

      for J in Exception_Signals'Range loop
         Keep_Unmasked (Exception_Signals (J)) := True;
      end loop;

      --  The abort signal must also be unmasked

      Keep_Unmasked (Abort_Task_Interrupt) := True;
   end Initialize;

end System.Interrupt_Management;
