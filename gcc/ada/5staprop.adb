------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--            Copyright (C) 1991-2001, Florida State University             --
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

--  This is a Solaris (native) version of this package

--  This package contains all the GNULL primitives that interface directly
--  with the underlying OS.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Tasking.Debug;
--  used for Known_Tasks

with Ada.Exceptions;
--  used for Raise_Exception

with GNAT.OS_Lib;
--  used for String_Access, Getenv

with Interfaces.C;
--  used for int
--           size_t

with System.Interrupt_Management;
--  used for Keep_Unmasked
--           Abort_Task_Interrupt
--           Interrupt_ID

with System.Interrupt_Management.Operations;
--  used for Set_Interrupt_Mask
--           All_Tasks_Mask
pragma Elaborate_All (System.Interrupt_Management.Operations);

with System.Parameters;
--  used for Size_Type

with System.Tasking;
--  used for Ada_Task_Control_Block
--           Task_ID
--           ATCB components and types

with System.Task_Info;
--  to initialize Task_Info for a C thread, in function Self

with System.Soft_Links;
--  used for Defer/Undefer_Abort
--       to initialize TSD for a C thread, in function Self

--  Note that we do not use System.Tasking.Initialization directly since
--  this is a higher level package that we shouldn't depend on. For example
--  when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Initialization

with System.OS_Primitives;
--  used for Delay_Modes

with Unchecked_Conversion;
with Unchecked_Deallocation;

package body System.Task_Primitives.Operations is

   use System.Tasking.Debug;
   use System.Tasking;
   use Interfaces.C;
   use System.OS_Interface;
   use System.Parameters;
   use Ada.Exceptions;
   use System.OS_Primitives;

   package SSL renames System.Soft_Links;

   ------------------
   --  Local Data  --
   ------------------

   ATCB_Magic_Code : constant := 16#ADAADAAD#;
   --  This is used to allow us to catch attempts to call Self
   --  from outside an Ada task, with high probability.
   --  For an Ada task, Task_Wrapper.Magic_Number = ATCB_Magic_Code.

   --  The following are logically constants, but need to be initialized
   --  at run time.

   Environment_Task_ID : Task_ID;
   --  A variable to hold Task_ID for the environment task.
   --  If we use this variable to get the Task_ID, we need the following
   --  ATCB_Key only for non-Ada threads.

   Unblocked_Signal_Mask : aliased sigset_t;
   --  The set of signals that should unblocked in all tasks

   ATCB_Key : aliased thread_key_t;
   --  Key used to find the Ada Task_ID associated with a thread,
   --  at least for C threads unknown to the Ada run-time system.

   All_Tasks_L : aliased System.Task_Primitives.RTS_Lock;
   --  See comments on locking rules in System.Tasking (spec).

   Next_Serial_Number : Task_Serial_Number := 100;
   --  We start at 100, to reserve some special values for
   --  using in error checking.
   --  The following are internal configuration constants needed.

   ------------------------
   --  Priority Support  --
   ------------------------

   Dynamic_Priority_Support : constant Boolean := True;
   --  controls whether we poll for pending priority changes during sleeps

   Priority_Ceiling_Emulation : constant Boolean := True;
   --  controls whether we emulate priority ceiling locking

   --  To get a scheduling close to annex D requirements, we use the real-time
   --  class provided for LWP's and map each task/thread to a specific and
   --  unique LWP (there is 1 thread per LWP, and 1 LWP per thread).

   --  The real time class can only be set when the process has root
   --  priviledges, so in the other cases, we use the normal thread scheduling
   --  and priority handling.

   Using_Real_Time_Class : Boolean := False;
   --  indicates wether the real time class is being used (i.e the process
   --  has root priviledges).

   Prio_Param : aliased struct_pcparms;
   --  Hold priority info (Real_Time) initialized during the package
   --  elaboration.

   -------------------------------------
   --  External Configuration Values  --
   -------------------------------------

   Time_Slice_Val : Interfaces.C.long;
   pragma Import (C, Time_Slice_Val, "__gl_time_slice_val");

   Locking_Policy : Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");

   Dispatching_Policy : Character;
   pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");

   --------------------------------
   --  Foreign Threads Detection --
   --------------------------------

   --  The following are used to allow the Self function to
   --  automatically generate ATCB's for C threads that happen to call
   --  Ada procedure, which in turn happen to call the Ada run-time system.

   type Fake_ATCB;
   type Fake_ATCB_Ptr is access Fake_ATCB;
   type Fake_ATCB is record
      Stack_Base : Interfaces.C.unsigned := 0;
      --  A value of zero indicates the node is not in use.
      Next       : Fake_ATCB_Ptr;
      Real_ATCB  : aliased Ada_Task_Control_Block (0);
   end record;

   Fake_ATCB_List : Fake_ATCB_Ptr;
   --  A linear linked list.
   --  The list is protected by All_Tasks_L;
   --  Nodes are added to this list from the front.
   --  Once a node is added to this list, it is never removed.

   Fake_Task_Elaborated : aliased Boolean := True;
   --  Used to identified fake tasks (i.e., non-Ada Threads).

   Next_Fake_ATCB : Fake_ATCB_Ptr;
   --  Used to allocate one Fake_ATCB in advance. See comment in New_Fake_ATCB

   ------------
   -- Checks --
   ------------

   Check_Count  : Integer := 0;
   Old_Owner    : Task_ID;
   Lock_Count   : Integer := 0;
   Unlock_Count : Integer := 0;

   function To_Lock_Ptr is
     new Unchecked_Conversion (RTS_Lock_Ptr, Lock_Ptr);
   function To_Task_ID is
     new Unchecked_Conversion (Owner_ID, Task_ID);
   function To_Owner_ID is
     new Unchecked_Conversion (Task_ID, Owner_ID);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function sysconf (name : System.OS_Interface.int)
     return processorid_t;
   pragma Import (C, sysconf, "sysconf");

   SC_NPROCESSORS_CONF : constant System.OS_Interface.int := 14;

   function Num_Procs (name : System.OS_Interface.int := SC_NPROCESSORS_CONF)
     return processorid_t renames sysconf;

   procedure Abort_Handler
     (Sig     : Signal;
      Code    : access siginfo_t;
      Context : access ucontext_t);

   function To_thread_t is new Unchecked_Conversion
     (Integer, System.OS_Interface.thread_t);

   function To_Task_ID is new Unchecked_Conversion (System.Address, Task_ID);

   function To_Address is new Unchecked_Conversion (Task_ID, System.Address);

   type Ptr is access Task_ID;
   function To_Ptr is new Unchecked_Conversion (Interfaces.C.unsigned, Ptr);
   function To_Ptr is new Unchecked_Conversion (System.Address, Ptr);

   type Iptr is access Interfaces.C.unsigned;
   function To_Iptr is new Unchecked_Conversion (Interfaces.C.unsigned, Iptr);

   function Thread_Body_Access is
     new Unchecked_Conversion (System.Address, Thread_Body);

   function New_Fake_ATCB (Stack_Base : Interfaces.C.unsigned) return Task_ID;
   --  Allocate and Initialize a new ATCB. This code can safely be called from
   --  a foreign thread, as it doesn't access implicitly or explicitly
   --  "self" before having initialized the new ATCB.

   ------------
   -- Checks --
   ------------

   function Check_Initialize_Lock (L : Lock_Ptr; Level : Lock_Level)
     return Boolean;
   pragma Inline (Check_Initialize_Lock);

   function Check_Lock (L : Lock_Ptr) return Boolean;
   pragma Inline (Check_Lock);

   function Record_Lock (L : Lock_Ptr) return Boolean;
   pragma Inline (Record_Lock);

   function Check_Sleep (Reason : Task_States) return Boolean;
   pragma Inline (Check_Sleep);

   function Record_Wakeup
     (L : Lock_Ptr;
      Reason : Task_States) return Boolean;
   pragma Inline (Record_Wakeup);

   function Check_Wakeup
     (T : Task_ID;
      Reason : Task_States) return Boolean;
   pragma Inline (Check_Wakeup);

   function Check_Unlock (L : Lock_Ptr) return Boolean;
   pragma Inline (Check_Lock);

   function Check_Finalize_Lock (L : Lock_Ptr) return Boolean;
   pragma Inline (Check_Finalize_Lock);

   -------------------
   -- New_Fake_ATCB --
   -------------------

   function New_Fake_ATCB (Stack_Base : Interfaces.C.unsigned)
     return Task_ID
   is
      Self_ID   : Task_ID;
      P, Q      : Fake_ATCB_Ptr;
      Succeeded : Boolean;
      Result    : Interfaces.C.int;

   begin
      --  This section is ticklish.
      --  We dare not call anything that might require an ATCB, until
      --  we have the new ATCB in place.
      --  Note: we don't use "Write_Lock (All_Tasks_L'Access);" because
      --  we don't yet have an ATCB, and so can't pass the safety check.

      Result := mutex_lock (All_Tasks_L.L'Access);
      Q := null;
      P := Fake_ATCB_List;

      while P /= null loop
         if P.Stack_Base = 0 then
            Q := P;
         elsif thr_kill (P.Real_ATCB.Common.LL.Thread, 0) /= 0 then
            --  ????
            --  If a C thread that has dependent Ada tasks terminates
            --  abruptly, e.g. as a result of cancellation, any dependent
            --  tasks are likely to hang up in termination.
            P.Stack_Base := 0;
            Q := P;
         end if;

         P := P.Next;
      end loop;

      if Q = null then

         --  Create a new ATCB with zero entries.

         Self_ID := Next_Fake_ATCB.Real_ATCB'Access;
         Next_Fake_ATCB.Stack_Base := Stack_Base;
         Next_Fake_ATCB.Next := Fake_ATCB_List;
         Fake_ATCB_List := Next_Fake_ATCB;
         Next_Fake_ATCB := null;

      else

         --  Reuse an existing fake ATCB.

         Self_ID := Q.Real_ATCB'Access;
         Q.Stack_Base := Stack_Base;
      end if;

      --  Do the standard initializations

      System.Tasking.Initialize_ATCB
        (Self_ID, null, Null_Address, Null_Task, Fake_Task_Elaborated'Access,
         System.Priority'First, Task_Info.Unspecified_Task_Info, 0, Self_ID,
         Succeeded);
      pragma Assert (Succeeded);

      --  Record this as the Task_ID for the current thread.

      Self_ID.Common.LL.Thread := thr_self;
      Result := thr_setspecific (ATCB_Key, To_Address (Self_ID));
      pragma Assert (Result = 0);

      --  Finally, it is safe to use an allocator in this thread.

      if Next_Fake_ATCB = null then
         Next_Fake_ATCB := new Fake_ATCB;
      end if;

      Self_ID.Master_of_Task := 0;
      Self_ID.Master_Within := Self_ID.Master_of_Task + 1;

      for L in Self_ID.Entry_Calls'Range loop
         Self_ID.Entry_Calls (L).Self := Self_ID;
         Self_ID.Entry_Calls (L).Level := L;
      end loop;

      Self_ID.Common.State := Runnable;
      Self_ID.Awake_Count := 1;

      --  Since this is not an ordinary Ada task, we will start out undeferred

      Self_ID.Deferral_Level := 0;

      --  Give the task a unique serial number.

      Self_ID.Serial_Number := Next_Serial_Number;
      Next_Serial_Number := Next_Serial_Number + 1;
      pragma Assert (Next_Serial_Number /= 0);

      System.Soft_Links.Create_TSD (Self_ID.Common.Compiler_Data);

      --  ????
      --  The following call is commented out to avoid dependence on
      --  the System.Tasking.Initialization package.

      --  It seems that if we want Ada.Task_Attributes to work correctly
      --  for C threads we will need to raise the visibility of this soft
      --  link to System.Soft_Links.

      --  We are putting that off until this new functionality is otherwise
      --  stable.

      --  System.Tasking.Initialization.Initialize_Attributes_Link.all (T);

      --  Must not unlock until Next_ATCB is again allocated.

      for J in Known_Tasks'Range loop
         if Known_Tasks (J) = null then
            Known_Tasks (J) := Self_ID;
            Self_ID.Known_Tasks_Index := J;
            exit;
         end if;
      end loop;

      Result := mutex_unlock (All_Tasks_L.L'Access);

      --  We cannot use "Unlock (All_Tasks_L'Access);" because
      --  we did not use Write_Lock, and so would not pass the checks.

      return Self_ID;
   end New_Fake_ATCB;

   -------------------
   -- Abort_Handler --
   -------------------

   --  Target-dependent binding of inter-thread Abort signal to
   --  the raising of the Abort_Signal exception.

   --  The technical issues and alternatives here are essentially
   --  the same as for raising exceptions in response to other
   --  signals (e.g. Storage_Error).  See code and comments in
   --  the package body System.Interrupt_Management.

   --  Some implementations may not allow an exception to be propagated
   --  out of a handler, and others might leave the signal or
   --  interrupt that invoked this handler masked after the exceptional
   --  return to the application code.

   --  GNAT exceptions are originally implemented using setjmp()/longjmp().
   --  On most UNIX systems, this will allow transfer out of a signal handler,
   --  which is usually the only mechanism available for implementing
   --  asynchronous handlers of this kind.  However, some
   --  systems do not restore the signal mask on longjmp(), leaving the
   --  abort signal masked.

   --  Alternative solutions include:

   --       1. Change the PC saved in the system-dependent Context
   --          parameter to point to code that raises the exception.
   --          Normal return from this handler will then raise
   --          the exception after the mask and other system state has
   --          been restored (see example below).
   --       2. Use siglongjmp()/sigsetjmp() to implement exceptions.
   --       3. Unmask the signal in the Abortion_Signal exception handler
   --          (in the RTS).

   --  The following procedure would be needed if we can't longjmp out of
   --  a signal handler.  (See below.)

   --  procedure Raise_Abort_Signal is
   --  begin
   --     raise Standard'Abort_Signal;
   --  end if;

   --  ???
   --  The comments above need revising.  They are partly obsolete.

   procedure Abort_Handler
     (Sig     : Signal;
      Code    : access siginfo_t;
      Context : access ucontext_t)
   is
      Self_ID : Task_ID := Self;
      Result  : Interfaces.C.int;
      Old_Set : aliased sigset_t;

   begin
      --  Assuming it is safe to longjmp out of a signal handler, the
      --  following code can be used:

      if Self_ID.Deferral_Level = 0
        and then Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
        and then not Self_ID.Aborting
      then
         --  You can comment the following out,
         --  to make all aborts synchronous, for debugging.

         Self_ID.Aborting := True;

         --  Make sure signals used for RTS internal purpose are unmasked

         Result := thr_sigsetmask (SIG_UNBLOCK,
           Unblocked_Signal_Mask'Unchecked_Access, Old_Set'Unchecked_Access);
         pragma Assert (Result = 0);

         raise Standard'Abort_Signal;

         --  ?????
         --  Must be certain that the implementation of "raise"
         --  does not make any OS/thread calls, or at least that
         --  if it makes any, they are safe for interruption by
         --  async. signals.
      end if;

      --  Otherwise, something like this is required:
      --  if not Abort_Is_Deferred.all then
      --    --  Overwrite the return PC address with the address of the
      --    --  special raise routine, and "return" to that routine's
      --    --  starting address.
      --    Context.PC := Raise_Abort_Signal'Address;
      --    return;
      --  end if;

   end Abort_Handler;

   -------------------
   --  Stack_Guard  --
   -------------------

   --  The underlying thread system sets a guard page at the
   --  bottom of a thread stack, so nothing is needed.

   procedure Stack_Guard (T : ST.Task_ID; On : Boolean) is
   begin
      null;
   end Stack_Guard;

   --------------------
   -- Get_Thread_Id  --
   --------------------

   function Get_Thread_Id (T : ST.Task_ID) return OSI.Thread_Id is
   begin
      return T.Common.LL.Thread;
   end Get_Thread_Id;

   -----------
   -- Self  --
   -----------

   function Self return Task_ID is separate;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   --  Note: mutexes and cond_variables needed per-task basis are
   --        initialized in Initialize_TCB and the Storage_Error is
   --        handled. Other mutexes (such as All_Tasks_L, Memory_Lock...)
   --        used in RTS is initialized before any status change of RTS.
   --        Therefore rasing Storage_Error in the following routines
   --        should be able to be handled safely.

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : access Lock)
   is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Initialize_Lock (Lock_Ptr (L), PO_Level));

      if Priority_Ceiling_Emulation then
         L.Ceiling := Prio;
      end if;

      Result := mutex_init (L.L'Access, USYNC_THREAD, System.Null_Address);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         Raise_Exception (Storage_Error'Identity, "Failed to allocate a lock");
      end if;
   end Initialize_Lock;

   procedure Initialize_Lock
     (L : access RTS_Lock;
      Level : Lock_Level)
   is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Initialize_Lock
        (To_Lock_Ptr (RTS_Lock_Ptr (L)), Level));
      Result := mutex_init (L.L'Access, USYNC_THREAD, System.Null_Address);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         Raise_Exception (Storage_Error'Identity, "Failed to allocate a lock");
      end if;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : access Lock) is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Finalize_Lock (Lock_Ptr (L)));
      Result := mutex_destroy (L.L'Access);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   procedure Finalize_Lock (L : access RTS_Lock) is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Finalize_Lock (To_Lock_Ptr (RTS_Lock_Ptr (L))));
      Result := mutex_destroy (L.L'Access);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Lock (Lock_Ptr (L)));

      if Priority_Ceiling_Emulation and then Locking_Policy = 'C' then
         declare
            Self_Id        : constant Task_ID := Self;
            Saved_Priority : System.Any_Priority;

         begin
            if Self_Id.Common.LL.Active_Priority > L.Ceiling then
               Ceiling_Violation := True;
               return;
            end if;

            Saved_Priority := Self_Id.Common.LL.Active_Priority;

            if Self_Id.Common.LL.Active_Priority < L.Ceiling then
               Set_Priority (Self_Id, L.Ceiling);
            end if;

            Result := mutex_lock (L.L'Access);
            pragma Assert (Result = 0);
            Ceiling_Violation := False;

            L.Saved_Priority := Saved_Priority;
         end;

      else
         Result := mutex_lock (L.L'Access);
         pragma Assert (Result = 0);
         Ceiling_Violation := False;
      end if;

      pragma Assert (Record_Lock (Lock_Ptr (L)));
   end Write_Lock;

   procedure Write_Lock (L : access RTS_Lock) is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Lock (To_Lock_Ptr (RTS_Lock_Ptr (L))));
      Result := mutex_lock (L.L'Access);
      pragma Assert (Result = 0);
      pragma Assert (Record_Lock (To_Lock_Ptr (RTS_Lock_Ptr (L))));
   end Write_Lock;

   procedure Write_Lock (T : Task_ID) is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Lock (To_Lock_Ptr (T.Common.LL.L'Access)));
      Result := mutex_lock (T.Common.LL.L.L'Access);
      pragma Assert (Result = 0);
      pragma Assert (Record_Lock (To_Lock_Ptr (T.Common.LL.L'Access)));
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
   begin
      Write_Lock (L, Ceiling_Violation);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : access Lock) is
      Result  : Interfaces.C.int;

   begin
      pragma Assert (Check_Unlock (Lock_Ptr (L)));

      if Priority_Ceiling_Emulation and then Locking_Policy = 'C' then
         declare
            Self_Id : constant Task_ID := Self;

         begin
            Result := mutex_unlock (L.L'Access);
            pragma Assert (Result = 0);

            if Self_Id.Common.LL.Active_Priority > L.Saved_Priority then
               Set_Priority (Self_Id, L.Saved_Priority);
            end if;
         end;
      else
         Result := mutex_unlock (L.L'Access);
         pragma Assert (Result = 0);
      end if;
   end Unlock;

   procedure Unlock (L : access RTS_Lock) is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Unlock (To_Lock_Ptr (RTS_Lock_Ptr (L))));
      Result := mutex_unlock (L.L'Access);
      pragma Assert (Result = 0);
   end Unlock;

   procedure Unlock (T : Task_ID) is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Unlock (To_Lock_Ptr (T.Common.LL.L'Access)));
      Result := mutex_unlock (T.Common.LL.L.L'Access);
      pragma Assert (Result = 0);
   end Unlock;

   --  For the time delay implementation, we need to make sure we
   --  achieve following criteria:

   --  1) We have to delay at least for the amount requested.
   --  2) We have to give up CPU even though the actual delay does not
   --     result in blocking.
   --  3) Except for restricted run-time systems that do not support
   --     ATC or task abort, the delay must be interrupted by the
   --     abort_task operation.
   --  4) The implementation has to be efficient so that the delay overhead
   --     is relatively cheap.
   --  (1)-(3) are Ada requirements. Even though (2) is an Annex-D
   --     requirement we still want to provide the effect in all cases.
   --     The reason is that users may want to use short delays to implement
   --     their own scheduling effect in the absence of language provided
   --     scheduling policies.

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration is
      TS     : aliased timespec;
      Result : Interfaces.C.int;

   begin
      Result := clock_gettime (CLOCK_REALTIME, TS'Unchecked_Access);
      pragma Assert (Result = 0);
      return To_Duration (TS);
   end Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
   begin
      return 10#1.0#E-6;
   end RT_Resolution;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
   begin
      if Do_Yield then
         System.OS_Interface.thr_yield;
      end if;
   end Yield;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T : Task_ID;
      Prio : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
      Result  : Interfaces.C.int;
      Param   : aliased struct_pcparms;

      use Task_Info;

   begin
      T.Common.Current_Priority := Prio;

      if Priority_Ceiling_Emulation then
         T.Common.LL.Active_Priority := Prio;
      end if;

      if Using_Real_Time_Class then
         Param.pc_cid := Prio_Param.pc_cid;
         Param.rt_pri := pri_t (Prio);
         Param.rt_tqsecs := Prio_Param.rt_tqsecs;
         Param.rt_tqnsecs := Prio_Param.rt_tqnsecs;

         Result := Interfaces.C.int (
           priocntl (PC_VERSION, P_LWPID, T.Common.LL.LWP, PC_SETPARMS,
             Param'Address));

      else
         if T.Common.Task_Info /= null
           and then not T.Common.Task_Info.Bound_To_LWP
         then
            --  The task is not bound to a LWP, so use thr_setprio

            Result :=
              thr_setprio (T.Common.LL.Thread, Interfaces.C.int (Prio));

         else

            --  The task is bound to a LWP, use priocntl
            --  ??? TBD

            null;
         end if;
      end if;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_ID) return System.Any_Priority is
   begin
      return T.Common.Current_Priority;
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_ID) is
      Result    : Interfaces.C.int;
      Proc      : processorid_t;  --  User processor #
      Last_Proc : processorid_t;  --  Last processor #

      use System.Task_Info;
   begin
      Self_ID.Common.LL.Thread := thr_self;

      Self_ID.Common.LL.LWP := lwp_self;

      if Self_ID.Common.Task_Info /= null then
         if Self_ID.Common.Task_Info.New_LWP
           and then Self_ID.Common.Task_Info.CPU /= CPU_UNCHANGED
         then
            Last_Proc := Num_Procs - 1;

            if Self_ID.Common.Task_Info.CPU = ANY_CPU then
               Result := 0;
               Proc := 0;

               while Proc < Last_Proc loop
                  Result := p_online (Proc, PR_STATUS);
                  exit when Result = PR_ONLINE;
                  Proc := Proc + 1;
               end loop;

               Result := processor_bind (P_LWPID, P_MYID, Proc, null);
               pragma Assert (Result = 0);

            else
               --  Use specified processor

               if Self_ID.Common.Task_Info.CPU < 0
                 or else Self_ID.Common.Task_Info.CPU > Last_Proc
               then
                  raise Invalid_CPU_Number;
               end if;

               Result := processor_bind
                 (P_LWPID, P_MYID, Self_ID.Common.Task_Info.CPU, null);
               pragma Assert (Result = 0);
            end if;
         end if;
      end if;

      Result := thr_setspecific (ATCB_Key, To_Address (Self_ID));
      pragma Assert (Result = 0);

      --  We need the above code even if we do direct fetch of Task_ID in Self
      --  for the main task on Sun, x86 Solaris and for gcc 2.7.2.

      Lock_All_Tasks_List;

      for I in Known_Tasks'Range loop
         if Known_Tasks (I) = null then
            Known_Tasks (I) := Self_ID;
            Self_ID.Known_Tasks_Index := I;
            exit;
         end if;
      end loop;
      Unlock_All_Tasks_List;
   end Enter_Task;

   --------------
   -- New_ATCB --
   --------------

   function New_ATCB (Entry_Num : Task_Entry_Index) return Task_ID is
   begin
      return new Ada_Task_Control_Block (Entry_Num);
   end New_ATCB;

   ----------------------
   --  Initialize_TCB  --
   ----------------------

   procedure Initialize_TCB (Self_ID : Task_ID; Succeeded : out Boolean) is
      Result : Interfaces.C.int;

   begin
      --  Give the task a unique serial number.

      Self_ID.Serial_Number := Next_Serial_Number;
      Next_Serial_Number := Next_Serial_Number + 1;
      pragma Assert (Next_Serial_Number /= 0);

      Self_ID.Common.LL.Thread := To_thread_t (-1);
      Result := mutex_init
        (Self_ID.Common.LL.L.L'Access, USYNC_THREAD, System.Null_Address);
      Self_ID.Common.LL.L.Level :=
        Private_Task_Serial_Number (Self_ID.Serial_Number);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = 0 then
         Result := cond_init (Self_ID.Common.LL.CV'Access, USYNC_THREAD, 0);
         pragma Assert (Result = 0 or else Result = ENOMEM);

         if Result /= 0 then
            Result := mutex_destroy (Self_ID.Common.LL.L.L'Access);
            pragma Assert (Result = 0);
            Succeeded := False;
         else
            Succeeded := True;
         end if;

      else
         Succeeded := False;
      end if;
   end Initialize_TCB;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : Task_ID;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean)
   is
      Result     : Interfaces.C.int;
      Adjusted_Stack_Size : Interfaces.C.size_t;
      Opts       : Interfaces.C.int := THR_DETACHED;

      Page_Size  : constant System.Parameters.Size_Type := 4096;
      --  This constant is for reserving extra space at the
      --  end of the stack, which can be used by the stack
      --  checking as guard page. The idea is that we need
      --  to have at least Stack_Size bytes available for
      --  actual use.

      use System.Task_Info;
   begin
      if Stack_Size = System.Parameters.Unspecified_Size then
         Adjusted_Stack_Size :=
           Interfaces.C.size_t (Default_Stack_Size + Page_Size);

      elsif Stack_Size < Minimum_Stack_Size then
         Adjusted_Stack_Size :=
           Interfaces.C.size_t (Minimum_Stack_Size + Page_Size);

      else
         Adjusted_Stack_Size :=
           Interfaces.C.size_t (Stack_Size + Page_Size);
      end if;

      --  Since the initial signal mask of a thread is inherited from the
      --  creator, and the Environment task has all its signals masked, we
      --  do not need to manipulate caller's signal mask at this point.
      --  All tasks in RTS will have All_Tasks_Mask initially.

      if T.Common.Task_Info /= null then

         if T.Common.Task_Info.New_LWP then
            Opts := Opts + THR_NEW_LWP;
         end if;

         if T.Common.Task_Info.Bound_To_LWP then
            Opts := Opts + THR_BOUND;
         end if;

      else
         Opts := THR_DETACHED + THR_BOUND;
      end if;

      Result := thr_create
        (System.Null_Address,
         Adjusted_Stack_Size,
         Thread_Body_Access (Wrapper),
         To_Address (T),
         Opts,
         T.Common.LL.Thread'Access);

      Succeeded := Result = 0;
      pragma Assert
        (Result = 0
          or else Result = ENOMEM
          or else Result = EAGAIN);
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_ID) is
      Result : Interfaces.C.int;
      Tmp    : Task_ID := T;

      procedure Free is new
        Unchecked_Deallocation (Ada_Task_Control_Block, Task_ID);

   begin
      T.Common.LL.Thread := To_thread_t (0);
      Result := mutex_destroy (T.Common.LL.L.L'Access);
      pragma Assert (Result = 0);
      Result := cond_destroy (T.Common.LL.CV'Access);
      pragma Assert (Result = 0);

      if T.Known_Tasks_Index /= -1 then
         Known_Tasks (T.Known_Tasks_Index) := null;
      end if;

      Free (Tmp);
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   --  This procedure must be called with abort deferred.
   --  It can no longer call Self or access
   --  the current task's ATCB, since the ATCB has been deallocated.

   procedure Exit_Task is
   begin
      thr_exit (System.Null_Address);
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_ID) is
      Result : Interfaces.C.int;
   begin
      pragma Assert (T /= Self);

      Result := thr_kill (T.Common.LL.Thread,
        Signal (System.Interrupt_Management.Abort_Task_Interrupt));
      null;

      pragma Assert (Result = 0);
   end Abort_Task;

   -------------
   --  Sleep  --
   -------------

   procedure Sleep
     (Self_ID : Task_ID;
      Reason  : Task_States)
   is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Sleep (Reason));

      if Dynamic_Priority_Support
        and then Self_ID.Pending_Priority_Change
      then
         Self_ID.Pending_Priority_Change := False;
         Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
         Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
      end if;

      Result := cond_wait
        (Self_ID.Common.LL.CV'Access, Self_ID.Common.LL.L.L'Access);
      pragma Assert (Result = 0 or else Result = EINTR);
      pragma Assert (Record_Wakeup
        (To_Lock_Ptr (Self_ID.Common.LL.L'Access), Reason));
   end Sleep;

   --  Note that we are relying heaviliy here on the GNAT feature
   --  that Calendar.Time, System.Real_Time.Time, Duration, and
   --  System.Real_Time.Time_Span are all represented in the same
   --  way, i.e., as a 64-bit count of nanoseconds.

   --  This allows us to always pass the timeout value as a Duration.

   --  ???
   --  We are taking liberties here with the semantics of the delays.
   --  That is, we make no distinction between delays on the Calendar clock
   --  and delays on the Real_Time clock.  That is technically incorrect, if
   --  the Calendar clock happens to be reset or adjusted.
   --  To solve this defect will require modification to the compiler
   --  interface, so that it can pass through more information, to tell
   --  us here which clock to use!

   --  cond_timedwait will return if any of the following happens:
   --  1) some other task did cond_signal on this condition variable
   --     In this case, the return value is 0
   --  2) the call just returned, for no good reason
   --     This is called a "spurious wakeup".
   --     In this case, the return value may also be 0.
   --  3) the time delay expires
   --     In this case, the return value is ETIME
   --  4) this task received a signal, which was handled by some
   --     handler procedure, and now the thread is resuming execution
   --     UNIX calls this an "interrupted" system call.
   --     In this case, the return value is EINTR

   --  If the cond_timedwait returns 0 or EINTR, it is still
   --  possible that the time has actually expired, and by chance
   --  a signal or cond_signal occurred at around the same time.

   --  We have also observed that on some OS's the value ETIME
   --  will be returned, but the clock will show that the full delay
   --  has not yet expired.

   --  For these reasons, we need to check the clock after return
   --  from cond_timedwait.  If the time has expired, we will set
   --  Timedout = True.

   --  This check might be omitted for systems on which the
   --  cond_timedwait() never returns early or wakes up spuriously.

   --  Annex D requires that completion of a delay cause the task
   --  to go to the end of its priority queue, regardless of whether
   --  the task actually was suspended by the delay.  Since
   --  cond_timedwait does not do this on Solaris, we add a call
   --  to thr_yield at the end.  We might do this at the beginning,
   --  instead, but then the round-robin effect would not be the
   --  same; the delayed task would be ahead of other tasks of the
   --  same priority that awoke while it was sleeping.

   --  For Timed_Sleep, we are expecting possible cond_signals
   --  to indicate other events (e.g., completion of a RV or
   --  completion of the abortable part of an async. select),
   --  we want to always return if interrupted. The caller will
   --  be responsible for checking the task state to see whether
   --  the wakeup was spurious, and to go back to sleep again
   --  in that case.  We don't need to check for pending abort
   --  or priority change on the way in our out; that is the
   --  caller's responsibility.

   --  For Timed_Delay, we are not expecting any cond_signals or
   --  other interruptions, except for priority changes and aborts.
   --  Therefore, we don't want to return unless the delay has
   --  actually expired, or the call has been aborted.  In this
   --  case, since we want to implement the entire delay statement
   --  semantics, we do need to check for pending abort and priority
   --  changes.  We can quietly handle priority changes inside the
   --  procedure, since there is no entry-queue reordering involved.

   -----------------
   -- Timed_Sleep --
   -----------------

   --  This is for use within the run-time system, so abort is
   --  assumed to be already deferred, and the caller should be
   --  holding its own ATCB lock.

   --  Yielded should be False unles we know for certain that the
   --  operation resulted in the calling task going to the end of
   --  the dispatching queue for its priority.

   --  ???
   --  This version presumes the worst, so Yielded is always False.
   --  On some targets, if cond_timedwait always yields, we could
   --  set Yielded to True just before the cond_timedwait call.

   procedure Timed_Sleep
     (Self_ID  : Task_ID;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
      Check_Time : constant Duration := Monotonic_Clock;
      Abs_Time   : Duration;
      Request    : aliased timespec;
      Result     : Interfaces.C.int;

   begin
      pragma Assert (Check_Sleep (Reason));
      Timedout := True;
      Yielded := False;

      if Mode = Relative then
         Abs_Time := Duration'Min (Time, Max_Sensible_Delay) + Check_Time;
      else
         Abs_Time := Duration'Min (Check_Time + Max_Sensible_Delay, Time);
      end if;

      if Abs_Time > Check_Time then
         Request := To_Timespec (Abs_Time);

         loop
            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
              or else (Dynamic_Priority_Support and then
                Self_ID.Pending_Priority_Change);

            Result := cond_timedwait (Self_ID.Common.LL.CV'Access,
              Self_ID.Common.LL.L.L'Access, Request'Access);

            exit when Abs_Time <= Monotonic_Clock;

            if Result = 0 or Result = EINTR then
               --  somebody may have called Wakeup for us
               Timedout := False;
               exit;
            end if;

            pragma Assert (Result = ETIME);
         end loop;
      end if;

      pragma Assert (Record_Wakeup
        (To_Lock_Ptr (Self_ID.Common.LL.L'Access), Reason));
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   --  This is for use in implementing delay statements, so
   --  we assume the caller is abort-deferred but is holding
   --  no locks.

   procedure Timed_Delay
     (Self_ID  : Task_ID;
      Time     : Duration;
      Mode     : ST.Delay_Modes)
   is
      Check_Time : constant Duration := Monotonic_Clock;
      Abs_Time   : Duration;
      Request    : aliased timespec;
      Result     : Interfaces.C.int;

   begin
      --  Only the little window between deferring abort and
      --  locking Self_ID is the reason we need to
      --  check for pending abort and priority change below!

      SSL.Abort_Defer.all;
      Write_Lock (Self_ID);

      if Mode = Relative then
         Abs_Time := Time + Check_Time;
      else
         Abs_Time := Duration'Min (Check_Time + Max_Sensible_Delay, Time);
      end if;

      if Abs_Time > Check_Time then
         Request := To_Timespec (Abs_Time);
         Self_ID.Common.State := Delay_Sleep;

         pragma Assert (Check_Sleep (Delay_Sleep));

         loop
            if Dynamic_Priority_Support and then
              Self_ID.Pending_Priority_Change then
               Self_ID.Pending_Priority_Change := False;
               Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
               Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
            end if;

            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            Result := cond_timedwait (Self_ID.Common.LL.CV'Access,
              Self_ID.Common.LL.L.L'Access, Request'Access);

            exit when Abs_Time <= Monotonic_Clock;

            pragma Assert (Result = 0 or else
              Result = ETIME or else
              Result = EINTR);
         end loop;

         pragma Assert (Record_Wakeup
           (To_Lock_Ptr (Self_ID.Common.LL.L'Access), Delay_Sleep));

         Self_ID.Common.State := Runnable;
      end if;

      Unlock (Self_ID);
      thr_yield;
      SSL.Abort_Undefer.all;
   end Timed_Delay;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup
     (T : Task_ID;
      Reason : Task_States)
   is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Check_Wakeup (T, Reason));
      Result := cond_signal (T.Common.LL.CV'Access);
      pragma Assert (Result = 0);
   end Wakeup;

   ---------------------------
   -- Check_Initialize_Lock --
   ---------------------------

   --  The following code is intended to check some of the invariant
   --  assertions related to lock usage, on which we depend.

   function Check_Initialize_Lock
     (L     : Lock_Ptr;
      Level : Lock_Level)
      return  Boolean
   is
      Self_ID : constant Task_ID := Self;

   begin
      --  Check that caller is abort-deferred

      if Self_ID.Deferral_Level <= 0 then
         return False;
      end if;

      --  Check that the lock is not yet initialized

      if L.Level /= 0 then
         return False;
      end if;

      L.Level := Lock_Level'Pos (Level) + 1;
      return True;
   end Check_Initialize_Lock;

   ----------------
   -- Check_Lock --
   ----------------

   function Check_Lock (L : Lock_Ptr) return Boolean is
      Self_ID : Task_ID := Self;
      P       : Lock_Ptr;

   begin
      --  Check that the argument is not null

      if L = null then
         return False;
      end if;

      --  Check that L is not frozen

      if L.Frozen then
         return False;
      end if;

      --  Check that caller is abort-deferred

      if Self_ID.Deferral_Level <= 0 then
         return False;
      end if;

      --  Check that caller is not holding this lock already

      if L.Owner = To_Owner_ID (Self_ID) then
         return False;
      end if;

      --  Check that TCB lock order rules are satisfied

      P := Self_ID.Common.LL.Locks;
      if P /= null then
         if P.Level >= L.Level
           and then (P.Level > 2 or else L.Level > 2)
         then
            return False;
         end if;
      end if;

      return True;
   end Check_Lock;

   -----------------
   -- Record_Lock --
   -----------------

   function Record_Lock (L : Lock_Ptr) return Boolean is
      Self_ID : Task_ID := Self;
      P       : Lock_Ptr;

   begin
      Lock_Count := Lock_Count + 1;

      --  There should be no owner for this lock at this point

      if L.Owner /= null then
         return False;
      end if;

      --  Record new owner

      L.Owner := To_Owner_ID (Self_ID);

      --  Check that TCB lock order rules are satisfied

      P := Self_ID.Common.LL.Locks;

      if P /= null then
         L.Next := P;
      end if;

      Self_ID.Common.LL.Locking := null;
      Self_ID.Common.LL.Locks := L;
      return True;
   end Record_Lock;

   -----------------
   -- Check_Sleep --
   -----------------

   function Check_Sleep (Reason : Task_States) return Boolean is
      Self_ID : Task_ID := Self;
      P       : Lock_Ptr;

   begin
      --  Check that caller is abort-deferred

      if Self_ID.Deferral_Level <= 0 then
         return False;
      end if;

      --  Check that caller is holding own lock, on top of list

      if Self_ID.Common.LL.Locks /=
        To_Lock_Ptr (Self_ID.Common.LL.L'Access)
      then
         return False;
      end if;

      --  Check that TCB lock order rules are satisfied

      if Self_ID.Common.LL.Locks.Next /= null then
         return False;
      end if;

      Self_ID.Common.LL.L.Owner := null;
      P := Self_ID.Common.LL.Locks;
      Self_ID.Common.LL.Locks := Self_ID.Common.LL.Locks.Next;
      P.Next := null;
      return True;
   end Check_Sleep;

   -------------------
   -- Record_Wakeup --
   -------------------

   function Record_Wakeup
     (L      : Lock_Ptr;
      Reason : Task_States)
      return   Boolean
   is
      Self_ID : Task_ID := Self;
      P       : Lock_Ptr;

   begin
      --  Record new owner

      L.Owner := To_Owner_ID (Self_ID);

      --  Check that TCB lock order rules are satisfied

      P := Self_ID.Common.LL.Locks;

      if P /= null then
         L.Next := P;
      end if;

      Self_ID.Common.LL.Locking := null;
      Self_ID.Common.LL.Locks := L;
      return True;
   end Record_Wakeup;

   ------------------
   -- Check_Wakeup --
   ------------------

   function Check_Wakeup
     (T      : Task_ID;
      Reason : Task_States)
      return   Boolean
   is
      Self_ID : Task_ID := Self;

   begin
      --  Is caller holding T's lock?

      if T.Common.LL.L.Owner /= To_Owner_ID (Self_ID) then
         return False;
      end if;

      --  Are reasons for wakeup and sleep consistent?

      if T.Common.State /= Reason then
         return False;
      end if;

      return True;
   end Check_Wakeup;

   ------------------
   -- Check_Unlock --
   ------------------

   function Check_Unlock (L : Lock_Ptr) return Boolean is
      Self_ID : Task_ID := Self;
      P       : Lock_Ptr;

   begin
      Unlock_Count := Unlock_Count + 1;

      if L = null then
         return False;
      end if;

      if L.Buddy /= null then
         return False;
      end if;

      if L.Level = 4 then
         Check_Count := Unlock_Count;
      end if;

      if Unlock_Count - Check_Count > 1000 then
         Check_Count := Unlock_Count;
         Old_Owner := To_Task_ID (All_Tasks_L.Owner);
      end if;

      --  Check that caller is abort-deferred

      if Self_ID.Deferral_Level <= 0 then
         return False;
      end if;

      --  Check that caller is holding this lock, on top of list

      if Self_ID.Common.LL.Locks /= L then
         return False;
      end if;

      --  Record there is no owner now

      L.Owner := null;
      P := Self_ID.Common.LL.Locks;
      Self_ID.Common.LL.Locks := Self_ID.Common.LL.Locks.Next;
      P.Next := null;
      return True;
   end Check_Unlock;

   --------------------
   -- Check_Finalize --
   --------------------

   function Check_Finalize_Lock (L : Lock_Ptr) return Boolean is
      Self_ID : Task_ID := Self;

   begin
      --  Check that caller is abort-deferred

      if Self_ID.Deferral_Level <= 0 then
         return False;
      end if;

      --  Check that no one is holding this lock

      if L.Owner /= null then
         return False;
      end if;

      L.Frozen := True;
      return True;
   end Check_Finalize_Lock;

   ----------------
   -- Check_Exit --
   ----------------

   function Check_Exit (Self_ID : Task_ID) return Boolean is
   begin
      --  Check that caller is just holding Global_Task_Lock
      --  and no other locks

      if Self_ID.Common.LL.Locks = null then
         return False;
      end if;

      --  2 = Global_Task_Level

      if Self_ID.Common.LL.Locks.Level /= 2 then
         return False;
      end if;

      if Self_ID.Common.LL.Locks.Next /= null then
         return False;
      end if;

      --  Check that caller is abort-deferred

      if Self_ID.Deferral_Level <= 0 then
         return False;
      end if;

      return True;
   end Check_Exit;

   --------------------
   -- Check_No_Locks --
   --------------------

   function Check_No_Locks (Self_ID : Task_ID) return Boolean is
   begin
      return Self_ID.Common.LL.Locks = null;
   end Check_No_Locks;

   ----------------------
   -- Environment_Task --
   ----------------------

   function Environment_Task return Task_ID is
   begin
      return Environment_Task_ID;
   end Environment_Task;

   -------------------------
   -- Lock_All_Tasks_List --
   -------------------------

   procedure Lock_All_Tasks_List is
   begin
      Write_Lock (All_Tasks_L'Access);
   end Lock_All_Tasks_List;

   ---------------------------
   -- Unlock_All_Tasks_List --
   ---------------------------

   procedure Unlock_All_Tasks_List is
   begin
      Unlock (All_Tasks_L'Access);
   end Unlock_All_Tasks_List;

   ------------------
   -- Suspend_Task --
   ------------------

   function Suspend_Task
     (T           : ST.Task_ID;
      Thread_Self : Thread_Id) return Boolean is
   begin
      if T.Common.LL.Thread /= Thread_Self then
         return thr_suspend (T.Common.LL.Thread) = 0;
      else
         return True;
      end if;
   end Suspend_Task;

   -----------------
   -- Resume_Task --
   -----------------

   function Resume_Task
     (T           : ST.Task_ID;
      Thread_Self : Thread_Id) return Boolean is
   begin
      if T.Common.LL.Thread /= Thread_Self then
         return thr_continue (T.Common.LL.Thread) = 0;
      else
         return True;
      end if;
   end Resume_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : ST.Task_ID) is
      act       : aliased struct_sigaction;
      old_act   : aliased struct_sigaction;
      Tmp_Set   : aliased sigset_t;
      Result    : Interfaces.C.int;

      procedure Configure_Processors;
      --  Processors configuration
      --  The user can specify a processor which the program should run
      --  on to emulate a single-processor system. This can be easily
      --  done by setting environment variable GNAT_PROCESSOR to one of
      --  the following :
      --
      --    -2 : use the default configuration (run the program on all
      --         available processors) - this is the same as having
      --         GNAT_PROCESSOR unset
      --    -1 : let the RTS choose one processor and run the program on
      --         that processor
      --    0 .. Last_Proc : run the program on the specified processor
      --
      --  Last_Proc is equal to the value of the system variable
      --  _SC_NPROCESSORS_CONF, minus one.

      procedure Configure_Processors is

         Proc_Acc : constant GNAT.OS_Lib.String_Access :=
                        GNAT.OS_Lib.Getenv ("GNAT_PROCESSOR");
      begin
         if Proc_Acc.all'Length /= 0 then

            --  Environment variable is defined

            declare
               Proc : aliased processorid_t;  --  User processor #
               Last_Proc : processorid_t;     --  Last processor #

            begin
               Last_Proc := Num_Procs - 1;

               if Last_Proc = -1 then

                  --  Unable to read system variable _SC_NPROCESSORS_CONF
                  --  Ignore environment variable GNAT_PROCESSOR

                  null;

               else
                  Proc := processorid_t'Value (Proc_Acc.all);

                  if Proc < -2  or Proc > Last_Proc then
                     raise Constraint_Error;

                  elsif Proc = -2 then

                     --  Use the default configuration

                     null;

                  elsif Proc = -1 then

                     --  Choose a processor

                     Result := 0;
                     while Proc < Last_Proc loop
                        Proc := Proc + 1;
                        Result := p_online (Proc, PR_STATUS);
                        exit when Result = PR_ONLINE;
                     end loop;

                     pragma Assert (Result = PR_ONLINE);
                     Result := processor_bind (P_PID, P_MYID, Proc, null);
                     pragma Assert (Result = 0);

                  else
                     --  Use user processor

                     Result := processor_bind (P_PID, P_MYID, Proc, null);
                     pragma Assert (Result = 0);
                  end if;
               end if;

            exception
               when Constraint_Error =>

                  --  Illegal environment variable GNAT_PROCESSOR - ignored

                  null;
            end;
         end if;
      end Configure_Processors;

   --  Start of processing for Initialize

   begin
      Environment_Task_ID := Environment_Task;

      --  This is done in Enter_Task, but this is too late for the
      --  Environment Task, since we need to call Self in Check_Locks when
      --  the run time is compiled with assertions on.

      Result := thr_setspecific (ATCB_Key, To_Address (Environment_Task));
      pragma Assert (Result = 0);

      --  Initialize the lock used to synchronize chain of all ATCBs.

      Initialize_Lock (All_Tasks_L'Access, All_Tasks_Level);

      Enter_Task (Environment_Task);

      --  Install the abort-signal handler

      --  Set sa_flags to SA_NODEFER so that during the handler execution
      --  we do not change the Signal_Mask to be masked for the Abort_Signal.
      --  This is a temporary fix to the problem that the Signal_Mask is
      --  not restored after the exception (longjmp) from the handler.
      --  The right fix should be made in sigsetjmp so that we save
      --  the Signal_Set and restore it after a longjmp.
      --  In that case, this field should be changed back to 0. ???

      act.sa_flags := 16;

      act.sa_handler := Abort_Handler'Address;
      Result := sigemptyset (Tmp_Set'Access);
      pragma Assert (Result = 0);
      act.sa_mask := Tmp_Set;

      Result :=
        sigaction (
          Signal (System.Interrupt_Management.Abort_Task_Interrupt),
          act'Unchecked_Access,
          old_act'Unchecked_Access);
      pragma Assert (Result = 0);

      Configure_Processors;

      --  Create a free ATCB for use on the Fake_ATCB_List.

      Next_Fake_ATCB := new Fake_ATCB;
   end Initialize;

--  Package elaboration

begin
   declare
      Result : Interfaces.C.int;

   begin
      --  Mask Environment task for all signals. The original mask of the
      --  Environment task will be recovered by Interrupt_Server task
      --  during the elaboration of s-interr.adb.

      System.Interrupt_Management.Operations.Set_Interrupt_Mask
        (System.Interrupt_Management.Operations.All_Tasks_Mask'Access);

      --  Prepare the set of signals that should unblocked in all tasks

      Result := sigemptyset (Unblocked_Signal_Mask'Access);
      pragma Assert (Result = 0);

      for J in Interrupt_Management.Interrupt_ID loop
         if System.Interrupt_Management.Keep_Unmasked (J) then
            Result := sigaddset (Unblocked_Signal_Mask'Access, Signal (J));
            pragma Assert (Result = 0);
         end if;
      end loop;

      --  We need the following code to support automatic creation of fake
      --  ATCB's for C threads that call the Ada run-time system, even if
      --  we use a faster way of getting Self for real Ada tasks.

      Result := thr_keycreate (ATCB_Key'Access, System.Null_Address);
      pragma Assert (Result = 0);
   end;

   if Dispatching_Policy = 'F' then
      declare
         Result : Interfaces.C.long;
         Class_Info  : aliased struct_pcinfo;
         Secs, Nsecs : Interfaces.C.long;

      begin

         --  If a pragma Time_Slice is specified, takes the value in account.

         if Time_Slice_Val > 0 then
            --  Convert Time_Slice_Val (microseconds) into seconds and
            --  nanoseconds

            Secs := Time_Slice_Val / 1_000_000;
            Nsecs := (Time_Slice_Val rem 1_000_000) * 1_000;

         --  Otherwise, default to no time slicing (i.e run until blocked)

         else
            Secs := RT_TQINF;
            Nsecs := RT_TQINF;
         end if;

         --  Get the real time class id.

         Class_Info.pc_clname (1) := 'R';
         Class_Info.pc_clname (2) := 'T';
         Class_Info.pc_clname (3) := ASCII.Nul;

         Result := priocntl (PC_VERSION, P_LWPID, P_MYID, PC_GETCID,
           Class_Info'Address);

         --  Request the real time class

         Prio_Param.pc_cid := Class_Info.pc_cid;
         Prio_Param.rt_pri := pri_t (Class_Info.rt_maxpri);
         Prio_Param.rt_tqsecs := Secs;
         Prio_Param.rt_tqnsecs := Nsecs;

         Result := priocntl (PC_VERSION, P_LWPID, P_MYID, PC_SETPARMS,
           Prio_Param'Address);

         Using_Real_Time_Class := Result /= -1;
      end;
   end if;
end System.Task_Primitives.Operations;
