------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a Solaris version of this package.

--  PLEASE DO NOT add any dependences on other packages.
--  This package is designed to work with or without tasking support.

--  Make a careful study of all signals available under the OS,
--  to see which need to be reserved, kept always unmasked,
--  or kept always unmasked.

--  Be on the lookout for special signals that
--  may be used by the thread library.

with Interfaces.C;
--  used for int

with System.OS_Interface;
--  used for various Constants, Signal and types

package body System.Interrupt_Management is

   use Interfaces.C;
   use System.OS_Interface;

   type Interrupt_List is array (Interrupt_ID range <>) of Interrupt_ID;

   Exception_Interrupts : constant Interrupt_List :=
     (SIGFPE, SIGILL, SIGSEGV, SIGBUS);

   Unreserve_All_Interrupts : Interfaces.C.int;
   pragma Import
     (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");

   ----------------------
   -- Notify_Exception --
   ----------------------

   --  This function identifies the Ada exception to be raised using
   --  the information when the system received a synchronous signal.
   --  Since this function is machine and OS dependent, different code
   --  has to be provided for different target.

   procedure Notify_Exception
     (signo   : Signal;
      info    : access siginfo_t;
      context : access ucontext_t);

   procedure Notify_Exception
     (signo   : Signal;
      info    : access siginfo_t;
      context : access ucontext_t) is
   begin
      --  Check that treatment of exception propagation here
      --  is consistent with treatment of the abort signal in
      --  System.Task_Primitives.Operations.

      case signo is
         when SIGFPE =>
            case info.si_code is
               when  FPE_INTDIV |
                     FPE_INTOVF |
                     FPE_FLTDIV |
                     FPE_FLTOVF |
                     FPE_FLTUND |
                     FPE_FLTRES |
                     FPE_FLTINV |
                     FPE_FLTSUB =>

                  raise Constraint_Error;

               when others =>
                  pragma Assert (False);
                  null;
            end case;

         when SIGILL | SIGSEGV | SIGBUS  =>
            raise Storage_Error;

         when others =>
            pragma Assert (False);
            null;
      end case;
   end Notify_Exception;

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   --  Nothing needs to be done on this platform.

   procedure Initialize_Interrupts is
   begin
      null;
   end Initialize_Interrupts;

----------------------------
-- Package Initialization --
----------------------------

begin
   declare
      act     : aliased struct_sigaction;
      old_act : aliased struct_sigaction;
      mask    : aliased sigset_t;
      Result  : Interfaces.C.int;

   begin
      --  Need to call pthread_init very early because it is doing signal
      --  initializations.

      pthread_init;

      --  Change this if you want to use another signal for task abort.
      --  SIGTERM might be a good one.

      Abort_Task_Interrupt := SIGABRT;

      act.sa_handler := Notify_Exception'Address;

      --  Set sa_flags to SA_NODEFER so that during the handler execution
      --  we do not change the Signal_Mask to be masked for the Signal.
      --  This is a temporary fix to the problem that the Signal_Mask is
      --  not restored after the exception (longjmp) from the handler.
      --  The right fix should be made in sigsetjmp so that we save
      --  the Signal_Set and restore it after a longjmp.

      --  In that case, this field should be changed back to 0. ??? (Dong-Ik)

      act.sa_flags := 16;

      Result := sigemptyset (mask'Access);
      pragma Assert (Result = 0);

      --  ??? For the same reason explained above, we can't mask these
      --  signals because otherwise we won't be able to catch more than
      --  one signal.

      act.sa_mask := mask;

      Keep_Unmasked (Abort_Task_Interrupt) := True;

      --  By keeping SIGINT unmasked, allow the user to do a Ctrl-C, but in the
      --  same time, disable the ability of handling this signal
      --  via Ada.Interrupts.
      --  The pragma Unreserve_All_Interrupts let the user the ability to
      --  change this behavior.

      if Unreserve_All_Interrupts = 0 then
         Keep_Unmasked (SIGINT) := True;
      end if;

      for J in Exception_Interrupts'Range loop
         Keep_Unmasked (Exception_Interrupts (J)) := True;
         Result :=
           sigaction
           (Signal (Exception_Interrupts (J)), act'Unchecked_Access,
            old_act'Unchecked_Access);
         pragma Assert (Result = 0);
      end loop;

      for J in Unmasked'Range loop
         Keep_Unmasked (Interrupt_ID (Unmasked (J))) := True;
      end loop;

      Reserve := Keep_Unmasked or Keep_Masked;

      for J in Reserved'Range loop
         Reserve (Interrupt_ID (Reserved (J))) := True;
      end loop;

      --  We do not have Signal 0 in reality. We just use this value
      --  to identify not existing signals (see s-intnam.ads). Therefore,
      --  Signal 0 should not be used in all signal related operations hence
      --  mark it as reserved.

      Reserve (0) := True;
   end;
end System.Interrupt_Management;
