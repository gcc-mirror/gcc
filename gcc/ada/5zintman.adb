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
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
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

with System.OS_Interface;
--  used for various Constants, Signal and types

package body System.Interrupt_Management is

   use System.OS_Interface;
   use type Interfaces.C.int;

   type Interrupt_List is array (Interrupt_ID range <>) of Interrupt_ID;
   Exception_Interrupts : constant Interrupt_List (1 .. 4) :=
     (SIGFPE, SIGILL, SIGSEGV, SIGBUS);

   --  Keep these variables global so that they are initialized only once.

   Exception_Action : aliased struct_sigaction;

   ----------------------
   -- Notify_Exception --
   ----------------------

   procedure Notify_Exception (signo : Signal);
   --  Identify the Ada exception to be raised using
   --  the information when the system received a synchronous signal.

   procedure Notify_Exception (signo : Signal) is
      Mask   : aliased sigset_t;
      Result : int;
      My_Id  : t_id;

   begin
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
            --  Unexpected signal
            raise Program_Error;
      end case;
   end Notify_Exception;

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   --  Since there is no signal inheritance between VxWorks tasks, we need
   --  to initialize signal handling in each task.

   procedure Initialize_Interrupts is
      Result  : int;
      old_act : aliased struct_sigaction;

   begin
      for J in Exception_Interrupts'Range loop
         Result :=
           sigaction
             (Signal (Exception_Interrupts (J)), Exception_Action'Access,
              old_act'Unchecked_Access);
         pragma Assert (Result = 0);
      end loop;
   end Initialize_Interrupts;

begin
   declare
      mask   : aliased sigset_t;
      Result : int;
   begin
      Abort_Task_Interrupt := SIGABRT;
      --  Change this if you want to use another signal for task abort.
      --  SIGTERM might be a good one.

      Exception_Action.sa_handler := Notify_Exception'Address;
      Exception_Action.sa_flags := SA_ONSTACK;
      Result := sigemptyset (mask'Access);
      pragma Assert (Result = 0);

      for J in Exception_Interrupts'Range loop
         Result := sigaddset (mask'Access, Signal (Exception_Interrupts (J)));
         pragma Assert (Result = 0);
      end loop;

      Exception_Action.sa_mask := mask;
   end;
end System.Interrupt_Management;
