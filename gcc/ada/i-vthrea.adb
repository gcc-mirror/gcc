------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   I N T E R F A C E S . V T H R E A D S                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 2002-2003, Free Software Foundation, Inc.           --
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

--  Implement APEX process registration for AE653

with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Unchecked_Conversion;

with Interfaces.C;

with System.Secondary_Stack;
with System.Soft_Links;
with System.Task_Primitives.Ae_653;
with System.Task_Primitives.Operations; use System.Task_Primitives.Operations;
with System.Tasking;                    use System.Tasking;
with System.Task_Info;
with System.Tasking.Initialization;

package body Interfaces.Vthreads is

   use System.OS_Interface;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Enter_Task (T : Task_ID; Thread : Thread_Id);
   --  Duplicate and generalize
   --  System.Task_Primitives.Operations.Enter_Task

   procedure GNAT_Error_Handler (Sig : Signal);
   --  Signal handler for ARINC processes

   procedure Init_Float;
   pragma Import (C, Init_Float, "__gnat_init_float");
   --  Properly initializes the FPU for PPC systems.

   procedure Install_Handler;
   --  Install signal handlers for the calling ARINC process

   function Register_Foreign_Thread (Thread : Thread_Id) return Task_ID;
   --  Duplicate and generalize
   --  System.Task_Primitives.Operations.Register_Foreign_Thread

   -----------------------------
   -- Install_Signal_Handlers --
   -----------------------------

   function Install_Signal_Handlers return Interfaces.C.int is
   begin
      Install_Handler;
      Init_Float;
      return 0;
   end Install_Signal_Handlers;

   ----------------------
   -- Register_Foreign --
   ----------------------

   --  Create Ada task data structures for an ARINC process. All dynamic
   --  allocation of related data structures must be done via this routine.

   function Register_Foreign (T : OSI.Thread_Id) return OSI.STATUS is
      use Interfaces.C;
      use System.Task_Primitives.Ae_653;

      pragma Assert (taskVarGet (T, ATCB_Key_Addr) = ERROR);
      --  "T" is not yet registered

      Result  : OSI.STATUS := taskIdVerify (T);
      Status  : OSI.STATUS := OK;
      Temp_Id : Task_ID;

   begin
      if Result = OK then
         Status := taskVarGet (T, ATCB_Key_Addr);

         --  Error of already registered

         if Status /= ERROR then
            Result := ERROR;

         else
            --  Create a TCB

            declare
               --  Make sure the caller has a TCB, since it's possible to have
               --  pure C APEX processes that create ones calling Ada code

               Caller : Task_ID;

            begin
               Status := taskVarGet (taskIdSelf, ATCB_Key_Addr);

               if Status = ERROR then
                  Caller := Register_Foreign_Thread (taskIdSelf);
               end if;
            end;

            if taskIdSelf /= T then
               Temp_Id := Register_Foreign_Thread (T);
            end if;

            Result := OK;
         end if;
      end if;

      return Result;
   end Register_Foreign;

   -------------------
   -- Reset_Foreign --
   -------------------

   --  Reinitialize Ada task data structures. No dynamic allocation
   --  may occur via this routine.

   function Reset_Foreign (T : Thread_Id) return STATUS is
      use Interfaces.C;
      use System.Secondary_Stack;
      use System.Task_Primitives.Ae_653;
      use type System.Address;

      pragma Assert (taskVarGet (T, ATCB_Key_Addr) /= ERROR);
      --  "T" has already been registered

      Result : STATUS := taskVarGet (T, ATCB_Key_Addr);
      function To_Address is new Ada.Unchecked_Conversion
        (Interfaces.C.int, System.Address);

      pragma Assert (
                     To_Task_Id
                     (To_Address (Result)).Common.Compiler_Data.Sec_Stack_Addr
                       /= System.Null_Address);
      --  "T" already has a secondary stack

   begin
      if  Result /= ERROR then

         --  Just reset the secondary stack pointer.  The implementation here
         --  assumes that the fixed secondary stack implementation is used.
         --  If not, there will be a memory leak (along with allocation, which
         --  is prohibited for ARINC processes once the system enters "normal"
         --  mode).

         SS_Init
           (To_Task_Id
              (To_Address (Result)).Common.Compiler_Data.Sec_Stack_Addr);
         Result := OK;
      end if;

      return Result;
   end Reset_Foreign;

   ------------------
   -- Setup_Thread --
   ------------------

   function Setup_Thread return System.Address is
      Result : System.Address := System.Null_Address;
      Status : OSI.STATUS;

   begin
      if Is_Valid_Task then
         Status := Reset_Foreign (taskIdSelf);
         Result :=
           To_Address (System.Task_Primitives.Operations.Self);
      else
         Status := Register_Foreign (taskIdSelf);
         Install_Handler;
         Init_Float;
         Result :=
           To_Address (System.Task_Primitives.Operations.Self);
      end if;

      return Result;
   end Setup_Thread;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (T : Task_ID; Thread : Thread_Id) is
      use System.Task_Primitives.Ae_653;

   begin
      Set_Task_Thread (T, Thread);
   end Enter_Task;

   ------------------------
   -- GNAT_Error_Handler --
   ------------------------

   procedure GNAT_Error_Handler (Sig : Signal) is
      Mask   : aliased sigset_t;
      Result : int;

   begin
      --  This code is the Ada replacement for init.c in the
      --  AE653 level B runtime.

      --  VxWorks will always mask out the signal during the signal
      --  handler and will reenable it on a longjmp.  GNAT does not
      --  generate a longjmp to return from a signal handler so the
      --  signal will still be masked unless we unmask it.

      Result := pthread_sigmask (SIG_SETMASK, null, Mask'Unchecked_Access);
      Result := sigdelset (Mask'Access, Sig);
      Result := pthread_sigmask (SIG_SETMASK, Mask'Unchecked_Access, null);

      case Sig is
         when SIGFPE =>
            Raise_Exception (Constraint_Error'Identity, "SIGFPE");
         when SIGILL =>
            Raise_Exception (Constraint_Error'Identity, "SIGILL");
         when SIGSEGV =>
            Raise_Exception
              (Program_Error'Identity,
               "erroneous memory access");
         when SIGBUS =>
            --  SIGBUS indicates stack overflow when it occurs
            --  in an application domain (but not in the Core
            --  OS under AE653, or in the kernel domain under
            --  AE 1.1).
            Raise_Exception
              (Storage_Error'Identity,
               "stack overflow or SIGBUS");
         when others =>
            Raise_Exception (Program_Error'Identity, "unhandled signal");
      end case;
   end GNAT_Error_Handler;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler is
      Mask          : aliased sigset_t;
      Signal_Action : aliased struct_sigaction;
      Result        : Interfaces.C.int;

   begin
      --  Set up signal handler to map synchronous signals to appropriate
      --  exceptions. Make sure that the handler isn't interrupted by
      --  another signal that might cause a scheduling event!

      --  This code is the Ada replacement for init.c in the
      --  AE653 level B runtime.
      Signal_Action.sa_handler := GNAT_Error_Handler'Address;
      Signal_Action.sa_flags := SA_ONSTACK;
      Result := sigemptyset (Mask'Access);
      Signal_Action.sa_mask := Mask;

      Result := sigaction
        (Signal (SIGFPE), Signal_Action'Unchecked_Access, null);

      Result := sigaction
        (Signal (SIGILL), Signal_Action'Unchecked_Access, null);

      Result := sigaction
        (Signal (SIGSEGV), Signal_Action'Unchecked_Access, null);

      Result := sigaction
        (Signal (SIGBUS), Signal_Action'Unchecked_Access, null);

   end Install_Handler;

   -----------------------------
   -- Register_Foreign_Thread --
   -----------------------------

   Foreign_Task_Elaborated : aliased Boolean := True;

   function Register_Foreign_Thread (Thread : Thread_Id) return Task_ID is
      pragma Assert (Thread = taskIdSelf or else Is_Valid_Task);
      --  Ensure that allocation will work

      Local_ATCB : aliased Ada_Task_Control_Block (0);
      New_Id    : Task_ID;
      Succeeded  : Boolean;

      use type Interfaces.C.unsigned;
      use type System.Address;
      use System.Task_Info;
      use System.Task_Primitives.Ae_653;

   begin
      if taskIdSelf = Thread then
         declare
            Self : Task_ID := Local_ATCB'Unchecked_Access;
            --  Temporarily record this as the Task_ID for the thread

         begin
            Set_Current_Priority (Self, System.Priority'First);
            Set_Task_Thread (Self, Thread);
         end;
      end if;

      pragma Assert (Is_Valid_Task);
      --  It is now safe to use an allocator for the real TCB

      New_Id := new Ada_Task_Control_Block (0);

      --  Finish initialization

      System.Tasking.Initialize_ATCB
        (New_Id, null, System.Null_Address, Null_Task,
         Foreign_Task_Elaborated'Access,
         System.Priority'First,
         System.Task_Info.Unspecified_Task_Info, 0, New_Id,
         Succeeded);
      pragma Assert (Succeeded);

      New_Id.Master_of_Task := 0;
      New_Id.Master_Within := New_Id.Master_of_Task + 1;

      for L in New_Id.Entry_Calls'Range loop
         New_Id.Entry_Calls (L).Self := New_Id;
         New_Id.Entry_Calls (L).Level := L;
      end loop;

      New_Id.Common.State := Runnable;
      New_Id.Awake_Count := 1;

      --  Since this is not an ordinary Ada task, we will start out undeferred

      New_Id.Deferral_Level := 0;

      System.Soft_Links.Create_TSD (New_Id.Common.Compiler_Data);

      --  Allocate a fixed secondary stack

      pragma Assert
        (New_Id.Common.Compiler_Data.Sec_Stack_Addr = System.Null_Address);
      System.Secondary_Stack.SS_Init
        (New_Id.Common.Compiler_Data.Sec_Stack_Addr);

      Enter_Task (New_Id, Thread);

      return New_Id;
   end Register_Foreign_Thread;

   --  Force use of tasking versions of secondary stack routines:

   procedure Force_Closure renames
     System.Tasking.Initialization.Defer_Abortion;
   pragma Unreferenced (Force_Closure);

--  Package elaboration code

begin
   --  Register the exported routines with the vThreads ARINC API

   procCreateHookAdd (Register_Foreign'Access);
   procStartHookAdd (Reset_Foreign'Access);
end Interfaces.Vthreads;
