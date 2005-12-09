------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S O F T _ L I N K S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains a set of subprogram access variables that access
--  some low-level primitives that are called different depending whether
--  tasking is involved or not (e.g. the Get/Set_Jmpbuf_Address that needs
--  to provide a different value for each task). To avoid dragging in the
--  tasking all the time, we use a system of soft links where the links are
--  initialized to non-tasking versions, and then if the tasking is
--  initialized, they are reset to the real tasking versions.

with Ada.Exceptions;
with System.Stack_Checking;

package System.Soft_Links is
   pragma Warnings (Off);
   pragma Preelaborate_05;
   pragma Warnings (On);

   subtype EOA is Ada.Exceptions.Exception_Occurrence_Access;
   subtype EO is Ada.Exceptions.Exception_Occurrence;

   function Current_Target_Exception return EO;
   pragma Import
     (Ada, Current_Target_Exception,
      "__gnat_current_target_exception");
   --  Import this subprogram from the private part of Ada.Exceptions

   --  First we have the access subprogram types used to establish the links.
   --  The approach is to establish variables containing access subprogram
   --  values which by default point to dummy no tasking versions of routines.

   type No_Param_Proc     is access procedure;
   type Addr_Param_Proc   is access procedure (Addr : Address);
   type EO_Param_Proc     is access procedure (Excep : EO);

   type Get_Address_Call  is access function return Address;
   type Set_Address_Call  is access procedure (Addr : Address);
   type Set_Address_Call2 is access procedure
     (Self_ID : Address; Addr : Address);

   type Get_Integer_Call  is access function return Integer;
   type Set_Integer_Call  is access procedure (Len : Integer);

   type Get_EOA_Call      is access function return EOA;
   type Set_EOA_Call      is access procedure (Excep : EOA);
   type Set_EO_Call       is access procedure (Excep : EO);

   type Special_EO_Call   is access
     procedure (Excep : EO := Current_Target_Exception);

   type Timed_Delay_Call  is access
     procedure (Time : Duration; Mode : Integer);

   type Get_Stack_Access_Call is access
     function return Stack_Checking.Stack_Access;

   type Task_Name_Call is access
     function return String;

   --  Suppress checks on all these types, since we know corrresponding
   --  values can never be null (the soft links are always initialized).

   pragma Suppress (Access_Check, No_Param_Proc);
   pragma Suppress (Access_Check, Addr_Param_Proc);
   pragma Suppress (Access_Check, EO_Param_Proc);
   pragma Suppress (Access_Check, Get_Address_Call);
   pragma Suppress (Access_Check, Set_Address_Call);
   pragma Suppress (Access_Check, Set_Address_Call2);
   pragma Suppress (Access_Check, Get_Integer_Call);
   pragma Suppress (Access_Check, Set_Integer_Call);
   pragma Suppress (Access_Check, Get_EOA_Call);
   pragma Suppress (Access_Check, Set_EOA_Call);
   pragma Suppress (Access_Check, Timed_Delay_Call);
   pragma Suppress (Access_Check, Get_Stack_Access_Call);
   pragma Suppress (Access_Check, Task_Name_Call);

   --  The following one is not related to tasking/no-tasking but to the
   --  traceback decorators for exceptions.

   type Traceback_Decorator_Wrapper_Call is access
     function (Traceback : System.Address;
               Len       : Natural)
               return      String;

   --  Declarations for the no tasking versions of the required routines

   procedure Abort_Defer_NT;
   --  Defer task abort (non-tasking case, does nothing)

   procedure Abort_Undefer_NT;
   --  Undefer task abort (non-tasking case, does nothing)

   procedure Abort_Handler_NT;
   --  Handle task abort (non-tasking case, does nothing). Currently, only VMS
   --  uses this.

   procedure Update_Exception_NT (X : EO := Current_Target_Exception);
   --  Handle exception setting. This routine is provided for targets which
   --  have built-in exception handling such as the Java Virtual Machine.
   --  Currently, only JGNAT uses this. See 4jexcept.ads for an explanation on
   --  how this routine is used.

   function Check_Abort_Status_NT return Integer;
   --  Returns Boolean'Pos (True) iff abort signal should raise
   --  Standard.Abort_Signal.

   procedure Task_Lock_NT;
   --  Lock out other tasks (non-tasking case, does nothing)

   procedure Task_Unlock_NT;
   --  Release lock set by Task_Lock (non-tasking case, does nothing)

   procedure Task_Termination_NT (Excep : EO);
   --  Handle task termination routines for the environment task (non-tasking
   --  case, does nothing).

   procedure Null_Finalize_Global_List;
   --  Finalize global list for controlled objects (does nothing)

   procedure Adafinal_NT;
   --  Shuts down the runtime system (non-tasking case)

   Abort_Defer : No_Param_Proc := Abort_Defer_NT'Access;
   pragma Suppress (Access_Check, Abort_Defer);
   --  Defer task abort (task/non-task case as appropriate)

   Abort_Undefer : No_Param_Proc := Abort_Undefer_NT'Access;
   pragma Suppress (Access_Check, Abort_Undefer);
   --  Undefer task abort (task/non-task case as appropriate)

   Abort_Handler : No_Param_Proc := Abort_Handler_NT'Access;
   --  Handle task abort (task/non-task case as appropriate)

   Update_Exception : Special_EO_Call := Update_Exception_NT'Access;
   --  Handle exception setting and tasking polling when appropriate

   Check_Abort_Status : Get_Integer_Call := Check_Abort_Status_NT'Access;
   --  Called when Abort_Signal is delivered to the process.  Checks to
   --  see if signal should result in raising Standard.Abort_Signal.

   Lock_Task : No_Param_Proc := Task_Lock_NT'Access;
   --  Locks out other tasks. Preceding a section of code by Task_Lock and
   --  following it by Task_Unlock creates a critical region. This is used
   --  for ensuring that a region of non-tasking code (such as code used to
   --  allocate memory) is tasking safe. Note that it is valid for calls to
   --  Task_Lock/Task_Unlock to be nested, and this must work properly, i.e.
   --  only the corresponding outer level Task_Unlock will actually unlock.
   --  This routine also prevents against asynchronous aborts (abort is
   --  deferred).

   Unlock_Task : No_Param_Proc := Task_Unlock_NT'Access;
   --  Releases lock previously set by call to Lock_Task. In the nested case,
   --  all nested locks must be released before other tasks competing for the
   --  tasking lock are released.
   --
   --  In the non nested case, this routine terminates the protection against
   --  asynchronous aborts introduced by Lock_Task (unless abort was already
   --  deferred before the call to Lock_Task (e.g in a protected procedures).
   --
   --  Note: the recommended protocol for using Lock_Task and Unlock_Task
   --  is as follows:
   --
   --    Locked_Processing : begin
   --       System.Soft_Links.Lock_Task.all;
   --       ...
   --       System.Soft_Links..Unlock_Task.all;
   --
   --    exception
   --       when others =>
   --          System.Soft_Links..Unlock_Task.all;
   --          raise;
   --    end Locked_Processing;
   --
   --  This ensures that the lock is not left set if an exception is raised
   --  explicitly or implicitly during the critical locked region.

   Task_Termination_Handler : EO_Param_Proc := Task_Termination_NT'Access;
   --  Handle task termination routines (task/non-task case as appropriate)

   Finalize_Global_List : No_Param_Proc := Null_Finalize_Global_List'Access;
   --  Performs finalization of global list for controlled objects

   Adafinal : No_Param_Proc := Adafinal_NT'Access;
   --  Performs the finalization of the Ada Runtime

   function  Get_Jmpbuf_Address_NT return  Address;
   procedure Set_Jmpbuf_Address_NT (Addr : Address);

   Get_Jmpbuf_Address : Get_Address_Call := Get_Jmpbuf_Address_NT'Access;
   Set_Jmpbuf_Address : Set_Address_Call := Set_Jmpbuf_Address_NT'Access;

   function  Get_Sec_Stack_Addr_NT return  Address;
   procedure Set_Sec_Stack_Addr_NT (Addr : Address);

   Get_Sec_Stack_Addr : Get_Address_Call := Get_Sec_Stack_Addr_NT'Access;
   Set_Sec_Stack_Addr : Set_Address_Call := Set_Sec_Stack_Addr_NT'Access;

   function Get_Exc_Stack_Addr_NT return Address;
   Get_Exc_Stack_Addr : Get_Address_Call := Get_Exc_Stack_Addr_NT'Access;

   function  Get_Current_Excep_NT return EOA;

   Get_Current_Excep : Get_EOA_Call := Get_Current_Excep_NT'Access;

   function Get_Stack_Info_NT return Stack_Checking.Stack_Access;

   Get_Stack_Info : Get_Stack_Access_Call := Get_Stack_Info_NT'Access;

   --------------------------
   -- Master_Id Soft-Links --
   --------------------------

   --  Soft-Links are used for procedures that manipulate  Master_Ids because
   --  a Master_Id must be generated for access to limited class-wide types,
   --  whose root may be extended with task components.

   function Current_Master_NT return Integer;
   procedure Enter_Master_NT;
   procedure Complete_Master_NT;

   Current_Master  : Get_Integer_Call :=  Current_Master_NT'Access;
   Enter_Master    : No_Param_Proc    :=  Enter_Master_NT'Access;
   Complete_Master : No_Param_Proc    :=  Complete_Master_NT'Access;

   ----------------------
   -- Delay Soft-Links --
   ----------------------

   --  Soft-Links are used for procedures that manipulate time to avoid
   --  dragging the tasking run time when using delay statements.

   Timed_Delay : Timed_Delay_Call;

   --------------------------
   -- Task Name Soft-Links --
   --------------------------

   function Task_Name_NT return String;

   Task_Name : Task_Name_Call := Task_Name_NT'Access;

   -------------------------------------
   -- Exception Tracebacks Soft-Links --
   -------------------------------------

   Traceback_Decorator_Wrapper : Traceback_Decorator_Wrapper_Call;
   --  Wrapper to the possible user specified traceback decorator to be
   --  called during automatic output of exception data.

   --  The nullity of this wrapper shall correspond to the nullity of the
   --  current actual decorator. This is ensured first by the null initial
   --  value of the corresponding variables, and then by Set_Trace_Decorator
   --  in g-exctra.adb.

   pragma Atomic (Traceback_Decorator_Wrapper);
   --  Since concurrent read/write operations may occur on this variable.
   --  See the body of Tailored_Exception_Traceback in Ada.Exceptions for
   --  a more detailed description of the potential problems.

   ------------------------
   -- Task Specific Data --
   ------------------------

   --  Here we define a single type that encapsulates the various task
   --  specific data. This type is used to store the necessary data into
   --  the Task_Control_Block or into a global variable in the non tasking
   --  case.

   type TSD is record
      Pri_Stack_Info : aliased Stack_Checking.Stack_Info;
      --  Information on stack (Base/Limit/Size) that is used
      --  by System.Stack_Checking. If this TSD does not belong to
      --  the environment task, the Size field must be initialized
      --  to the tasks requested stack size before the task can do
      --  its first stack check.

      pragma Warnings (Off);
      Jmpbuf_Address : System.Address := System.Null_Address;
      --  Address of jump buffer used to store the address of the
      --  current longjmp/setjmp buffer for exception management.
      --  These buffers are threaded into a stack, and the address
      --  here is the top of the stack. A null address means that
      --  no exception handler is currently active.

      Sec_Stack_Addr : System.Address := System.Null_Address;
      pragma Warnings (On);
      --  Address of currently allocated secondary stack

      Current_Excep : aliased EO;
      --  Exception occurrence that contains the information for the
      --  current exception. Note that any exception in the same task
      --  destroys this information, so the data in this variable must
      --  be copied out before another exception can occur.
      --
      --  Also act as a list of the active exceptions in the case of the GCC
      --  exception mechanism, organized as a stack with the most recent first.
   end record;

   procedure Create_TSD (New_TSD : in out TSD);
   pragma Inline (Create_TSD);
   --  Called from s-tassta when a new thread is created to perform
   --  any required initialization of the TSD.

   procedure Destroy_TSD (Old_TSD : in out TSD);
   pragma Inline (Destroy_TSD);
   --  Called from s-tassta just before a thread is destroyed to perform
   --  any required finalization.

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id;
   pragma Inline (Get_GNAT_Exception);
   --  This function obtains the Exception_Id from the Exception_Occurrence
   --  referenced by the Current_Excep field of the task specific data, i.e.
   --  the call is equivalent to
   --  Exception_Identity (Get_Current_Exception.all)

   --  Export the Get/Set routines for the various Task Specific Data (TSD)
   --  elements as callable subprograms instead of objects of access to
   --  subprogram types.

   function  Get_Jmpbuf_Address_Soft return  Address;
   procedure Set_Jmpbuf_Address_Soft (Addr : Address);
   pragma Inline (Get_Jmpbuf_Address_Soft);
   pragma Inline (Set_Jmpbuf_Address_Soft);

   function  Get_Sec_Stack_Addr_Soft return  Address;
   procedure Set_Sec_Stack_Addr_Soft (Addr : Address);
   pragma Inline (Get_Sec_Stack_Addr_Soft);
   pragma Inline (Set_Sec_Stack_Addr_Soft);

   function Get_Exc_Stack_Addr_Soft return Address;

end System.Soft_Links;
