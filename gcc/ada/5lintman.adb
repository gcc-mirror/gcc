------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.2 $
--                                                                          --
--             Copyright (C) 1991-2001 Florida State University             --
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

--  This is the GNU/Linux version of this package

--  This file performs the system-dependent translation between machine
--  exceptions and the Ada exceptions, if any, that should be raised when they
--  occur. This version works for the x86 running linux.

--  This is a Sun OS (FSU THREADS) version of this package

--  PLEASE DO NOT add any dependences on other packages. ??? why not ???
--  This package is designed to work with or without tasking support.

--  Make a careful study of all signals available under the OS, to see which
--  need to be reserved, kept always unmasked, or kept always unmasked. Be on
--  the lookout for special signals that may be used by the thread library.

--  The definitions of "reserved" differ slightly between the ARM and POSIX.
--  Here is the ARM definition of reserved interrupt:

--  The set of reserved interrupts is implementation defined. A reserved
--  interrupt is either an interrupt for which user-defined handlers are not
--  supported, or one which already has an attached handler by some other
--  implementation-defined means. Program units can be connected to
--  non-reserved interrupts.

--  POSIX.5b/.5c specifies further:

--  Signals which the application cannot accept, and for which the application
--  cannot modify the signal action or masking, because the signals are
--  reserved for use by the Ada language implementation. The reserved signals
--  defined by this standard are Signal_Abort, Signal_Alarm,
--  Signal_Floating_Point_Error, Signal_Illegal_Instruction,
--  Signal_Segmentation_Violation, Signal_Bus_Error. If the implementation
--  supports any signals besides those defined by this standard, the
--  implementation may also reserve some of those.

--  The signals defined by POSIX.5b/.5c that are not specified as being
--  reserved are SIGHUP, SIGINT, SIGPIPE, SIGQUIT, SIGTERM, SIGUSR1, SIGUSR2,
--  SIGCHLD, SIGCONT, SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGIO SIGURG, and all
--  the real-time signals.

--  Beware of reserving signals that POSIX.5b/.5c require to be available for
--  users. POSIX.5b/.5c say:

--  An implementation shall not impose restrictions on the ability of an
--  application to send, accept, block, or ignore the signals defined by this
--  standard, except as specified in this standard.

--  Here are some other relevant requirements from POSIX.5b/.5c:

--  For the environment task, the initial signal mask is that specified for
--  the process...

--  It is anticipated that the paragraph above may be modified by a future
--  revision of this standard, to require that the realtime signals always be
--  initially masked for a process that is an Ada active partition.

--  For all other tasks, the initial signal mask shall include all the signals
--  that are not reserved signals and are not bound to entries of the task.

with Interfaces.C;
--  used for int and other types

with System.Error_Reporting;
--  used for Shutdown

with System.OS_Interface;
--  used for various Constants, Signal and types

with Ada.Exceptions;
--  used for Exception_Id
--           Raise_From_Signal_Handler

with System.Soft_Links;
--  used for Get_Machine_State_Addr

with Unchecked_Conversion;

package body System.Interrupt_Management is

   use Interfaces.C;
   use System.Error_Reporting;
   use System.OS_Interface;

   package TSL renames System.Soft_Links;

   type Interrupt_List is array (Interrupt_ID range <>) of Interrupt_ID;
   Exception_Interrupts : constant Interrupt_List :=
     (SIGFPE, SIGILL, SIGSEGV);

   Unreserve_All_Interrupts : Interfaces.C.int;
   pragma Import
     (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");

   subtype int is Interfaces.C.int;
   subtype unsigned_short is Interfaces.C.unsigned_short;
   subtype unsigned_long is Interfaces.C.unsigned_long;

   ----------------------
   -- Notify_Exception --
   ----------------------

   Signal_Mask : aliased sigset_t;
   --  The set of signals handled by Notify_Exception

   --  This function identifies the Ada exception to be raised using
   --  the information when the system received a synchronous signal.
   --  Since this function is machine and OS dependent, different code
   --  has to be provided for different target.

   procedure Notify_Exception
     (signo         : Signal;
      gs            : unsigned_short;
      fs            : unsigned_short;
      es            : unsigned_short;
      ds            : unsigned_short;
      edi           : unsigned_long;
      esi           : unsigned_long;
      ebp           : unsigned_long;
      esp           : unsigned_long;
      ebx           : unsigned_long;
      edx           : unsigned_long;
      ecx           : unsigned_long;
      eax           : unsigned_long;
      trapno        : unsigned_long;
      err           : unsigned_long;
      eip           : unsigned_long;
      cs            : unsigned_short;
      eflags        : unsigned_long;
      esp_at_signal : unsigned_long;
      ss            : unsigned_short;
      fpstate       : System.Address;
      oldmask       : unsigned_long;
      cr2           : unsigned_long);

   procedure Notify_Exception
     (signo         : Signal;
      gs            : unsigned_short;
      fs            : unsigned_short;
      es            : unsigned_short;
      ds            : unsigned_short;
      edi           : unsigned_long;
      esi           : unsigned_long;
      ebp           : unsigned_long;
      esp           : unsigned_long;
      ebx           : unsigned_long;
      edx           : unsigned_long;
      ecx           : unsigned_long;
      eax           : unsigned_long;
      trapno        : unsigned_long;
      err           : unsigned_long;
      eip           : unsigned_long;
      cs            : unsigned_short;
      eflags        : unsigned_long;
      esp_at_signal : unsigned_long;
      ss            : unsigned_short;
      fpstate       : System.Address;
      oldmask       : unsigned_long;
      cr2           : unsigned_long)
   is

      function To_Machine_State_Ptr is new
        Unchecked_Conversion (Address, Machine_State_Ptr);

      --  These are not directly visible

      procedure Raise_From_Signal_Handler
        (E : Ada.Exceptions.Exception_Id;
         M : System.Address);
      pragma Import
        (Ada, Raise_From_Signal_Handler,
         "ada__exceptions__raise_from_signal_handler");
      pragma No_Return (Raise_From_Signal_Handler);

      mstate  : Machine_State_Ptr;
      message : aliased constant String := "" & ASCII.Nul;
      --  a null terminated String.

      Result  : int;

   begin

      --  Raise_From_Signal_Handler makes sure that the exception is raised
      --  safely from this signal handler.

      --  ??? The original signal mask (the one we had before coming into this
      --  signal catching function) should be restored by
      --  Raise_From_Signal_Handler. For now, restore it explicitly

      Result := pthread_sigmask (SIG_UNBLOCK, Signal_Mask'Access, null);
      pragma Assert (Result = 0);

      --  Check that treatment of exception propagation here
      --  is consistent with treatment of the abort signal in
      --  System.Task_Primitives.Operations.

      mstate := To_Machine_State_Ptr (TSL.Get_Machine_State_Addr.all);
      mstate.eip := eip;
      mstate.ebx := ebx;
      mstate.esp := esp_at_signal;
      mstate.ebp := ebp;
      mstate.esi := esi;
      mstate.edi := edi;

      case signo is
         when SIGFPE =>
            Raise_From_Signal_Handler
              (Constraint_Error'Identity, message'Address);
         when SIGILL =>
            Raise_From_Signal_Handler
              (Constraint_Error'Identity, message'Address);
         when SIGSEGV =>
            Raise_From_Signal_Handler
              (Storage_Error'Identity, message'Address);
         when others =>
            if Shutdown ("Unexpected signal") then
               null;
            end if;
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

begin
   declare
      act     : aliased struct_sigaction;
      old_act : aliased struct_sigaction;
      Result  : int;

   begin

      --  Need to call pthread_init very early because it is doing signal
      --  initializations.

      pthread_init;

      Abort_Task_Interrupt := SIGADAABORT;

      act.sa_handler := Notify_Exception'Address;

      act.sa_flags := 0;
      --  On some targets, we set sa_flags to SA_NODEFER so that during the
      --  handler execution we do not change the Signal_Mask to be masked for
      --  the Signal.
      --  This is a temporary fix to the problem that the Signal_Mask is
      --  not restored after the exception (longjmp) from the handler.
      --  The right fix should be made in sigsetjmp so that we save
      --  the Signal_Set and restore it after a longjmp.
      --  Since SA_NODEFER is obsolete, instead we reset explicitly
      --  the mask in the exception handler.

      Result := sigemptyset (Signal_Mask'Access);
      pragma Assert (Result = 0);

      for J in Exception_Interrupts'Range loop
         Result :=
           sigaddset (Signal_Mask'Access, Signal (Exception_Interrupts (J)));
         pragma Assert (Result = 0);
      end loop;

      act.sa_mask := Signal_Mask;

      Result :=
        sigaction
        (Signal (SIGFPE), act'Unchecked_Access,
         old_act'Unchecked_Access);
      pragma Assert (Result = 0);

      for J in Exception_Interrupts'First + 1 .. Exception_Interrupts'Last loop
         Keep_Unmasked (Exception_Interrupts (J)) := True;
         if Unreserve_All_Interrupts = 0 then
            Result :=
              sigaction
              (Signal (Exception_Interrupts (J)),
               act'Unchecked_Access,
               old_act'Unchecked_Access);
            pragma Assert (Result = 0);
         end if;
      end loop;

      Keep_Unmasked (Abort_Task_Interrupt) := True;
      Keep_Unmasked (SIGXCPU) := True;
      Keep_Unmasked (SIGBUS) := True;
      Keep_Unmasked (SIGFPE) := True;

      --  By keeping SIGINT unmasked, allow the user to do a Ctrl-C, but in the
      --  same time, disable the ability of handling this signal
      --  via Ada.Interrupts.
      --  The pragma Unreserve_All_Interrupts let the user the ability to
      --  change this behavior.

      if Unreserve_All_Interrupts = 0 then
         Keep_Unmasked (SIGINT) := True;
      end if;

      for J in Unmasked'Range loop
         Keep_Unmasked (Interrupt_ID (Unmasked (J))) := True;
      end loop;

      Reserve := Keep_Unmasked or Keep_Masked;

      for J in Reserved'Range loop
         Reserve (Interrupt_ID (Reserved (J))) := True;
      end loop;

      Reserve (0) := True;
      --  We do not have Signal 0 in reality. We just use this value
      --  to identify non-existent signals (see s-intnam.ads). Therefore,
      --  Signal 0 should not be used in all signal related operations hence
      --  mark it as reserved.

   end;
end System.Interrupt_Management;
