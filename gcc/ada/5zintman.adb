------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.11 $
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

--  This is the VxWorks version of this package.

--  It is likely to need tailoring to fit each operating system
--  and machine architecture.

--  PLEASE DO NOT add any dependences on other packages.
--  This package is designed to work with or without tasking support.

--  See the other warnings in the package specification before making
--  any modifications to this file.

--  Make a careful study of all signals available under the OS,
--  to see which need to be reserved, kept always unmasked,
--  or kept always unmasked.
--  Be on the lookout for special signals that
--  may be used by the thread library.

with Interfaces.C;
--  used for int and other types

with System.Error_Reporting;
pragma Warnings (Off, System.Error_Reporting);
--  used for Shutdown

with System.OS_Interface;
--  used for various Constants, Signal and types

with Unchecked_Conversion;

package body System.Interrupt_Management is

   use Interfaces.C;
   use System.Error_Reporting;
   use System.OS_Interface;

   function To_Isr is new Unchecked_Conversion (Long_Integer, isr_address);

   type Interrupt_List is array (Interrupt_ID range <>) of Interrupt_ID;
   Exception_Interrupts : constant Interrupt_List :=
     (SIGFPE, SIGILL, SIGSEGV, SIGBUS);

   --  Keep these variables global so that they are initialized only once.

   Exception_Action : aliased struct_sigaction;
   Default_Action : aliased struct_sigaction;

   --  ????? Use these horrible imports here to solve elaboration order
   --  problems.

   type Task_Id is access all Integer;

   Interrupt_ID_Map : array (Interrupt_ID) of Task_Id;
   pragma Import (Ada, Interrupt_ID_Map,
     "system__task_primitives__interrupt_operations__interrupt_id_map");

   ----------------------
   -- Notify_Exception --
   ----------------------

   procedure Notify_Exception (signo : Signal);
   --  Identify the Ada exception to be raised using
   --  the information when the system received a synchronous signal.

   procedure Notify_Exception (signo : Signal) is
      Mask   : aliased sigset_t;
      Result : Interfaces.C.int;
      My_Id  : pthread_t;
   begin
      --  VxWorks will always mask out the signal during the signal
      --  handler and will reenable it on a longjmp.  GNAT does
      --  not generate a longjmp to return from a signal handler
      --  so the signal will still be masked unless we unmask it.
      Result := pthread_sigmask (SIG_SETMASK, null, Mask'Unchecked_Access);
      Result := sigdelset (Mask'Access, signo);
      Result := pthread_sigmask (SIG_SETMASK, Mask'Unchecked_Access, null);

      --  VxWorks will suspend the task when it gets a hardware
      --  exception.  We take the liberty of resuming the task
      --  for the application.
      My_Id := taskIdSelf;
      if taskIsSuspended (My_Id) /= 0 then
         Result := taskResume (My_Id);
      end if;

      --  As long as we are using a longjmp to return control to the
      --  exception handler on the runtime stack, we are safe. The original
      --  signal mask (the one we had before coming into this signal catching
      --  function) will be restored by the longjmp. Therefore, raising
      --  an exception in this handler should be a safe operation.

      --  Check that treatment of exception propagation here
      --  is consistent with treatment of the abort signal in
      --  System.Task_Primitives.Operations.

      --  How can SIGSEGV be split into constraint and storage errors?
      --  What should SIGILL really raise ? Some implementations have
      --  codes for different types of SIGILL and some raise Storage_Error.
      --  What causes SIGBUS and should it be caught?
      --  Peter Burwood

      case signo is
         when SIGFPE =>
            raise Constraint_Error;
         when SIGILL =>
            raise Constraint_Error;
         when SIGSEGV =>
            raise Program_Error;
         when SIGBUS =>
            raise Program_Error;
         when others =>
            pragma Assert (Shutdown ("Unexpected signal"));
            null;
      end case;
   end Notify_Exception;

   -------------------
   -- Notify_Signal --
   -------------------

   --  VxWorks needs a special casing here. Each VxWorks task has a completely
   --  separate signal handling, so the usual signal masking can't work.
   --  This idea is to handle all the signals in all the tasks, and when
   --  such a signal occurs, redirect it to the dedicated task (if any) or
   --  reraise it.

   procedure Notify_Signal (signo : Signal);

   procedure Notify_Signal (signo : Signal) is
      Mask    : aliased sigset_t;
      Result  : Interfaces.C.int;
      My_Id   : pthread_t;
      old_isr : isr_address;

      function Get_Thread_Id (T : Task_Id) return pthread_t;
      pragma Import (Ada, Get_Thread_Id,
        "system__task_primitives__operations__get_thread_id");

   begin
      --  VxWorks will always mask out the signal during the signal
      --  handler and will reenable it on a longjmp.  GNAT does
      --  not generate a longjmp to return from a signal handler
      --  so the signal will still be masked unless we unmask it.
      Result := pthread_sigmask (SIG_SETMASK, null, Mask'Unchecked_Access);
      Result := sigdelset (Mask'Access, signo);
      Result := pthread_sigmask (SIG_SETMASK, Mask'Unchecked_Access, null);

      --  VxWorks will suspend the task when it gets a hardware
      --  exception.  We take the liberty of resuming the task
      --  for the application.
      My_Id := taskIdSelf;
      if taskIsSuspended (My_Id) /= 0 then
         Result := taskResume (My_Id);
      end if;

      --  ??? Need a lock around this, in case the handler is detached
      --  between the two following statements.

      if Interrupt_ID_Map (Interrupt_ID (signo)) /= null then
         Result :=
           kill (Get_Thread_Id (Interrupt_ID_Map (Interrupt_ID (signo))),
             Signal (signo));
      else
         old_isr := c_signal (signo, To_Isr (SIG_DFL));
         Result := kill (My_Id, Signal (signo));
      end if;
   end Notify_Signal;

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   --  Since there is no signal inheritance between VxWorks tasks, we need
   --  to initialize signal handling in each task.

   procedure Initialize_Interrupts is
      old_act : aliased struct_sigaction;
      Result  : Interfaces.C.int;

   begin
      for J in Interrupt_ID'First + 1 .. Interrupt_ID'Last loop
         if J /= Abort_Task_Interrupt then
            Result := sigaction (Signal (J), Default_Action'Access,
              old_act'Unchecked_Access);
            pragma Assert (Result = 0);
         end if;
      end loop;

      for J in Exception_Interrupts'Range loop
         Keep_Unmasked (Exception_Interrupts (J)) := True;
         Result :=
           sigaction
             (Signal (Exception_Interrupts (J)), Exception_Action'Access,
              old_act'Unchecked_Access);
         pragma Assert (Result = 0);
      end loop;
   end Initialize_Interrupts;

begin
   declare
      mask         : aliased sigset_t;
      default_mask : aliased sigset_t;
      Result       : Interfaces.C.int;

   begin
      --  The VxWorks POSIX threads library currently needs initialization.
      --  We wish it could be in System.OS_Interface, but that would
      --  cause an elaboration problem.

      pthread_init;

      Abort_Task_Interrupt := SIGABRT;
      --  Change this if you want to use another signal for task abort.
      --  SIGTERM might be a good one.

      Exception_Action.sa_handler := Notify_Exception'Address;
      Default_Action.sa_handler   := Notify_Signal'Address;

      Exception_Action.sa_flags := SA_SIGINFO + SA_ONSTACK;
      Default_Action.sa_flags := SA_SIGINFO + SA_ONSTACK;
      --  Send us extra signal information (SA_SIGINFO) on the
      --  stack (SA_ONSTACK).
      --  There is no SA_NODEFER in VxWorks.  The signal mask is
      --  restored after a longjmp so the SA_NODEFER option is
      --  not needed.  - Dan Eischen

      Result := sigemptyset (mask'Access);
      pragma Assert (Result = 0);
      Result := sigemptyset (default_mask'Access);
      pragma Assert (Result = 0);

      for J in Interrupt_ID'First + 1 .. Interrupt_ID'Last loop
         Result := sigaddset (default_mask'Access, Signal (J));
         pragma Assert (Result = 0);
      end loop;

      for J in Exception_Interrupts'Range loop
         Result := sigaddset (mask'Access, Signal (Exception_Interrupts (J)));
         pragma Assert (Result = 0);
         Result :=
           sigdelset (default_mask'Access, Signal (Exception_Interrupts (J)));
         pragma Assert (Result = 0);
      end loop;

      Exception_Action.sa_mask := mask;
      Default_Action.sa_mask := default_mask;

      --  Initialize_Interrupts is called for each task in Enter_Task

      Keep_Unmasked (Abort_Task_Interrupt) := True;

      Reserve := Reserve or Keep_Unmasked or Keep_Masked;

      Reserve (0) := True;
      --  We do not have Signal 0 in reality. We just use this value
      --  to identify non-existent signals (see s-intnam.ads). Therefore,
      --  Signal 0 should not be used in all signal related operations hence
      --  mark it as reserved.
   end;
end System.Interrupt_Management;
