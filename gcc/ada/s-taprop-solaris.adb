------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2004, Free Software Foundation, Inc.          --
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
--           Task_Id
--           ATCB components and types

with System.Task_Info;
--  to initialize Task_Info for a C thread, in function Self

with System.Soft_Links;
--  used for Defer/Undefer_Abort
--       to initialize TSD for a C thread, in function Self

--  Note that we do not use System.Tasking.Initialization directly since
--  this is a higher level package that we shouldn't depend on. For example
--  when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Stages.

with System.OS_Primitives;
--  used for Delay_Modes

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

   ----------------
   -- Local Data --
   ----------------

   --  The following are logically constants, but need to be initialized
   --  at run time.

   Environment_Task_Id : Task_Id;
   --  A variable to hold Task_Id for the environment task.
   --  If we use this variable to get the Task_Id, we need the following
   --  ATCB_Key only for non-Ada threads.

   Unblocked_Signal_Mask : aliased sigset_t;
   --  The set of signals that should unblocked in all tasks

   ATCB_Key : aliased thread_key_t;
   --  Key used to find the Ada Task_Id associated with a thread,
   --  at least for C threads unknown to the Ada run-time system.

   Single_RTS_Lock : aliased RTS_Lock;
   --  This is a lock to allow only one thread of control in the RTS at
   --  a time; it is used to execute in mutual exclusion from all other tasks.
   --  Used mainly in Single_Lock mode, but also to protect All_Tasks_List

   Next_Serial_Number : Task_Serial_Number := 100;
   --  We start at 100, to reserve some special values for
   --  using in error checking.
   --  The following are internal configuration constants needed.

   ----------------------
   -- Priority Support --
   ----------------------

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

   -----------------------------------
   -- External Configuration Values --
   -----------------------------------

   Time_Slice_Val : Interfaces.C.long;
   pragma Import (C, Time_Slice_Val, "__gl_time_slice_val");

   Locking_Policy : Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");

   Dispatching_Policy : Character;
   pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");

   Foreign_Task_Elaborated : aliased Boolean := True;
   --  Used to identified fake tasks (i.e., non-Ada Threads).

   -----------------------
   -- Local Subprograms --
   -----------------------

   function sysconf (name : System.OS_Interface.int) return processorid_t;
   pragma Import (C, sysconf, "sysconf");

   SC_NPROCESSORS_CONF : constant System.OS_Interface.int := 14;

   function Num_Procs
     (name : System.OS_Interface.int := SC_NPROCESSORS_CONF)
      return processorid_t renames sysconf;

   procedure Abort_Handler
     (Sig     : Signal;
      Code    : access siginfo_t;
      Context : access ucontext_t);
   --  Target-dependent binding of inter-thread Abort signal to
   --  the raising of the Abort_Signal exception.
   --  See also comments in 7staprop.adb

   ------------
   -- Checks --
   ------------

   function Check_Initialize_Lock
     (L     : Lock_Ptr;
      Level : Lock_Level) return Boolean;
   pragma Inline (Check_Initialize_Lock);

   function Check_Lock (L : Lock_Ptr) return Boolean;
   pragma Inline (Check_Lock);

   function Record_Lock (L : Lock_Ptr) return Boolean;
   pragma Inline (Record_Lock);

   function Check_Sleep (Reason : Task_States) return Boolean;
   pragma Inline (Check_Sleep);

   function Record_Wakeup
     (L      : Lock_Ptr;
      Reason : Task_States) return Boolean;
   pragma Inline (Record_Wakeup);

   function Check_Wakeup
     (T      : Task_Id;
      Reason : Task_States) return Boolean;
   pragma Inline (Check_Wakeup);

   function Check_Unlock (L : Lock_Ptr) return Boolean;
   pragma Inline (Check_Unlock);

   function Check_Finalize_Lock (L : Lock_Ptr) return Boolean;
   pragma Inline (Check_Finalize_Lock);

   --------------------
   -- Local Packages --
   --------------------

   package Specific is

      procedure Initialize (Environment_Task : Task_Id);
      pragma Inline (Initialize);
      --  Initialize various data needed by this package.

      function Is_Valid_Task return Boolean;
      pragma Inline (Is_Valid_Task);
      --  Does executing thread have a TCB?

      procedure Set (Self_Id : Task_Id);
      pragma Inline (Set);
      --  Set the self id for the current task.

      function Self return Task_Id;
      pragma Inline (Self);
      --  Return a pointer to the Ada Task Control Block of the calling task.

   end Specific;

   package body Specific is separate;
   --  The body of this package is target specific.

   ---------------------------------
   -- Support for foreign threads --
   ---------------------------------

   function Register_Foreign_Thread (Thread : Thread_Id) return Task_Id;
   --  Allocate and Initialize a new ATCB for the current Thread.

   function Register_Foreign_Thread
     (Thread : Thread_Id) return Task_Id is separate;

   ------------
   -- Checks --
   ------------

   Check_Count  : Integer := 0;
   Lock_Count   : Integer := 0;
   Unlock_Count : Integer := 0;

   -------------------
   -- Abort_Handler --
   -------------------

   procedure Abort_Handler
     (Sig     : Signal;
      Code    : access siginfo_t;
      Context : access ucontext_t)
   is
      pragma Unreferenced (Sig);
      pragma Unreferenced (Code);
      pragma Unreferenced (Context);

      Self_ID : constant Task_Id := Self;
      Old_Set : aliased sigset_t;

      Result : Interfaces.C.int;
      pragma Unreferenced (Result);

   begin
      --  It is not safe to raise an exception when using ZCX and the GCC
      --  exception handling mechanism.

      if ZCX_By_Default and then GCC_ZCX_Support then
         return;
      end if;

      if Self_ID.Deferral_Level = 0
        and then Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
        and then not Self_ID.Aborting
      then
         Self_ID.Aborting := True;

         --  Make sure signals used for RTS internal purpose are unmasked

         Result := thr_sigsetmask (SIG_UNBLOCK,
           Unblocked_Signal_Mask'Unchecked_Access, Old_Set'Unchecked_Access);
         pragma Assert (Result = 0);

         raise Standard'Abort_Signal;
      end if;
   end Abort_Handler;

   -----------------
   -- Stack_Guard --
   -----------------

   --  The underlying thread system sets a guard page at the
   --  bottom of a thread stack, so nothing is needed.

   procedure Stack_Guard (T : ST.Task_Id; On : Boolean) is
      pragma Unreferenced (T);
      pragma Unreferenced (On);
   begin
      null;
   end Stack_Guard;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id (T : ST.Task_Id) return OSI.Thread_Id is
   begin
      return T.Common.LL.Thread;
   end Get_Thread_Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : ST.Task_Id) is
      act     : aliased struct_sigaction;
      old_act : aliased struct_sigaction;
      Tmp_Set : aliased sigset_t;
      Result  : Interfaces.C.int;

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
         Proc_Acc  : constant GNAT.OS_Lib.String_Access :=
                       GNAT.OS_Lib.Getenv ("GNAT_PROCESSOR");
         Proc      : aliased processorid_t;  --  User processor #
         Last_Proc : processorid_t;          --  Last processor #

      begin
         if Proc_Acc.all'Length /= 0 then
            --  Environment variable is defined

            Last_Proc := Num_Procs - 1;

            if Last_Proc /= -1 then
               Proc := processorid_t'Value (Proc_Acc.all);

               if Proc <= -2  or else Proc > Last_Proc then
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
         end if;

      exception
         when Constraint_Error =>

            --  Illegal environment variable GNAT_PROCESSOR - ignored

            null;
      end Configure_Processors;

      function State
        (Int : System.Interrupt_Management.Interrupt_ID) return Character;
      pragma Import (C, State, "__gnat_get_interrupt_state");
      --  Get interrupt state.  Defined in a-init.c
      --  The input argument is the interrupt number,
      --  and the result is one of the following:

      Default : constant Character := 's';
      --    'n'   this interrupt not set by any Interrupt_State pragma
      --    'u'   Interrupt_State pragma set state to User
      --    'r'   Interrupt_State pragma set state to Runtime
      --    's'   Interrupt_State pragma set state to System (use "default"
      --           system handler)

   --  Start of processing for Initialize

   begin
      Environment_Task_Id := Environment_Task;

      --  This is done in Enter_Task, but this is too late for the
      --  Environment Task, since we need to call Self in Check_Locks when
      --  the run time is compiled with assertions on.

      Specific.Initialize (Environment_Task);

      --  Initialize the lock used to synchronize chain of all ATCBs.

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);

      Enter_Task (Environment_Task);

      --  Install the abort-signal handler

      if State (System.Interrupt_Management.Abort_Task_Interrupt)
        /= Default
      then
         --  Set sa_flags to SA_NODEFER so that during the handler execution
         --  we do not change the Signal_Mask to be masked for the Abort_Signal
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
      end if;

      Configure_Processors;
   end Initialize;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   --  Note: mutexes and cond_variables needed per-task basis are
   --        initialized in Initialize_TCB and the Storage_Error is
   --        handled. Other mutexes (such as RTS_Lock, Memory_Lock...)
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
     (L     : access RTS_Lock;
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
            Self_Id        : constant Task_Id := Self;
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

   procedure Write_Lock
     (L          : access RTS_Lock;
     Global_Lock : Boolean := False)
   is
      Result : Interfaces.C.int;

   begin
      if not Single_Lock or else Global_Lock then
         pragma Assert (Check_Lock (To_Lock_Ptr (RTS_Lock_Ptr (L))));
         Result := mutex_lock (L.L'Access);
         pragma Assert (Result = 0);
         pragma Assert (Record_Lock (To_Lock_Ptr (RTS_Lock_Ptr (L))));
      end if;
   end Write_Lock;

   procedure Write_Lock (T : Task_Id) is
      Result : Interfaces.C.int;

   begin
      if not Single_Lock then
         pragma Assert (Check_Lock (To_Lock_Ptr (T.Common.LL.L'Access)));
         Result := mutex_lock (T.Common.LL.L.L'Access);
         pragma Assert (Result = 0);
         pragma Assert (Record_Lock (To_Lock_Ptr (T.Common.LL.L'Access)));
      end if;
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
            Self_Id : constant Task_Id := Self;

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

   procedure Unlock (L : access RTS_Lock; Global_Lock : Boolean := False) is
      Result : Interfaces.C.int;

   begin
      if not Single_Lock or else Global_Lock then
         pragma Assert (Check_Unlock (To_Lock_Ptr (RTS_Lock_Ptr (L))));
         Result := mutex_unlock (L.L'Access);
         pragma Assert (Result = 0);
      end if;
   end Unlock;

   procedure Unlock (T : Task_Id) is
      Result : Interfaces.C.int;

   begin
      if not Single_Lock then
         pragma Assert (Check_Unlock (To_Lock_Ptr (T.Common.LL.L'Access)));
         Result := mutex_unlock (T.Common.LL.L.L'Access);
         pragma Assert (Result = 0);
      end if;
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

   -----------
   -- Self ---
   -----------

   function Self return Task_Id renames Specific.Self;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T                   : Task_Id;
      Prio                : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
      pragma Unreferenced (Loss_Of_Inheritance);

      Result : Interfaces.C.int;
      pragma Unreferenced (Result);

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

   function Get_Priority (T : Task_Id) return System.Any_Priority is
   begin
      return T.Common.Current_Priority;
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_Id) is
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

      Specific.Set (Self_ID);

      --  We need the above code even if we do direct fetch of Task_Id in Self
      --  for the main task on Sun, x86 Solaris and for gcc 2.7.2.

      Lock_RTS;

      for J in Known_Tasks'Range loop
         if Known_Tasks (J) = null then
            Known_Tasks (J) := Self_ID;
            Self_ID.Known_Tasks_Index := J;
            exit;
         end if;
      end loop;

      Unlock_RTS;
   end Enter_Task;

   --------------
   -- New_ATCB --
   --------------

   function New_ATCB (Entry_Num : Task_Entry_Index) return Task_Id is
   begin
      return new Ada_Task_Control_Block (Entry_Num);
   end New_ATCB;

   -------------------
   -- Is_Valid_Task --
   -------------------

   function Is_Valid_Task return Boolean renames Specific.Is_Valid_Task;

   -----------------------------
   -- Register_Foreign_Thread --
   -----------------------------

   function Register_Foreign_Thread return Task_Id is
   begin
      if Is_Valid_Task then
         return Self;
      else
         return Register_Foreign_Thread (thr_self);
      end if;
   end Register_Foreign_Thread;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_Id; Succeeded : out Boolean) is
      Result : Interfaces.C.int := 0;

   begin
      --  Give the task a unique serial number.

      Self_ID.Serial_Number := Next_Serial_Number;
      Next_Serial_Number := Next_Serial_Number + 1;
      pragma Assert (Next_Serial_Number /= 0);

      Self_ID.Common.LL.Thread := To_thread_t (-1);

      if not Single_Lock then
         Result := mutex_init
           (Self_ID.Common.LL.L.L'Access, USYNC_THREAD, System.Null_Address);
         Self_ID.Common.LL.L.Level :=
           Private_Task_Serial_Number (Self_ID.Serial_Number);
         pragma Assert (Result = 0 or else Result = ENOMEM);
      end if;

      if Result = 0 then
         Result := cond_init (Self_ID.Common.LL.CV'Access, USYNC_THREAD, 0);
         pragma Assert (Result = 0 or else Result = ENOMEM);
      end if;

      if Result = 0 then
         Succeeded := True;
      else
         if not Single_Lock then
            Result := mutex_destroy (Self_ID.Common.LL.L.L'Access);
            pragma Assert (Result = 0);
         end if;

         Succeeded := False;
      end if;
   end Initialize_TCB;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : Task_Id;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean)
   is
      pragma Unreferenced (Priority);

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

   procedure Finalize_TCB (T : Task_Id) is
      Result : Interfaces.C.int;
      Tmp    : Task_Id := T;
      Is_Self : constant Boolean := T = Self;

      procedure Free is new
        Unchecked_Deallocation (Ada_Task_Control_Block, Task_Id);

   begin
      T.Common.LL.Thread := To_thread_t (0);

      if not Single_Lock then
         Result := mutex_destroy (T.Common.LL.L.L'Access);
         pragma Assert (Result = 0);
      end if;

      Result := cond_destroy (T.Common.LL.CV'Access);
      pragma Assert (Result = 0);

      if T.Known_Tasks_Index /= -1 then
         Known_Tasks (T.Known_Tasks_Index) := null;
      end if;

      Free (Tmp);

      if Is_Self then
         Specific.Set (null);
      end if;
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   --  This procedure must be called with abort deferred.
   --  It can no longer call Self or access
   --  the current task's ATCB, since the ATCB has been deallocated.

   procedure Exit_Task is
   begin
      Specific.Set (null);
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_Id) is
      Result : Interfaces.C.int;
   begin
      pragma Assert (T /= Self);

      Result := thr_kill (T.Common.LL.Thread,
        Signal (System.Interrupt_Management.Abort_Task_Interrupt));
      null;

      pragma Assert (Result = 0);
   end Abort_Task;

   -----------
   -- Sleep --
   -----------

   procedure Sleep
     (Self_ID : Task_Id;
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

      if Single_Lock then
         Result := cond_wait
           (Self_ID.Common.LL.CV'Access, Single_RTS_Lock.L'Access);
      else
         Result := cond_wait
           (Self_ID.Common.LL.CV'Access, Self_ID.Common.LL.L.L'Access);
      end if;

      pragma Assert (Record_Wakeup
        (To_Lock_Ptr (Self_ID.Common.LL.L'Access), Reason));
      pragma Assert (Result = 0 or else Result = EINTR);
   end Sleep;

   --  Note that we are relying heaviliy here on the GNAT feature
   --  that Calendar.Time, System.Real_Time.Time, Duration, and
   --  System.Real_Time.Time_Span are all represented in the same
   --  way, i.e., as a 64-bit count of nanoseconds.

   --  This allows us to always pass the timeout value as a Duration.

   --  ???
   --  We are taking liberties here with the semantics of the delays.
   --  That is, we make no distinction between delays on the Calendar clock
   --  and delays on the Real_Time clock. That is technically incorrect, if
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
   --  the task actually was suspended by the delay. Since
   --  cond_timedwait does not do this on Solaris, we add a call
   --  to thr_yield at the end. We might do this at the beginning,
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
   --  actually expired, or the call has been aborted. In this
   --  case, since we want to implement the entire delay statement
   --  semantics, we do need to check for pending abort and priority
   --  changes. We can quietly handle priority changes inside the
   --  procedure, since there is no entry-queue reordering involved.

   -----------------
   -- Timed_Sleep --
   -----------------

   procedure Timed_Sleep
     (Self_ID  : Task_Id;
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

            if Single_Lock then
               Result := cond_timedwait (Self_ID.Common.LL.CV'Access,
                 Single_RTS_Lock.L'Access, Request'Access);
            else
               Result := cond_timedwait (Self_ID.Common.LL.CV'Access,
                 Self_ID.Common.LL.L.L'Access, Request'Access);
            end if;

            Yielded := True;

            exit when Abs_Time <= Monotonic_Clock;

            if Result = 0 or Result = EINTR then

               --  Somebody may have called Wakeup for us

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

   procedure Timed_Delay
     (Self_ID  : Task_Id;
      Time     : Duration;
      Mode     : ST.Delay_Modes)
   is
      Check_Time : constant Duration := Monotonic_Clock;
      Abs_Time   : Duration;
      Request    : aliased timespec;
      Result     : Interfaces.C.int;
      Yielded    : Boolean := False;

   begin
      --  Only the little window between deferring abort and
      --  locking Self_ID is the reason we need to
      --  check for pending abort and priority change below!

      SSL.Abort_Defer.all;

      if Single_Lock then
         Lock_RTS;
      end if;

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

            if Single_Lock then
               Result := cond_timedwait (Self_ID.Common.LL.CV'Access,
                 Single_RTS_Lock.L'Access, Request'Access);
            else
               Result := cond_timedwait (Self_ID.Common.LL.CV'Access,
                 Self_ID.Common.LL.L.L'Access, Request'Access);
            end if;

            Yielded := True;

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

      if Single_Lock then
         Unlock_RTS;
      end if;

      if not Yielded then
         thr_yield;
      end if;

      SSL.Abort_Undefer.all;
   end Timed_Delay;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup
     (T : Task_Id;
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
      Level : Lock_Level) return Boolean
   is
      Self_ID : constant Task_Id := Self;

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
      Self_ID : constant Task_Id := Self;
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

      if L.Owner = To_Owner_ID (To_Address (Self_ID)) then
         return False;
      end if;

      if Single_Lock then
         return True;
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
      Self_ID : constant Task_Id := Self;
      P       : Lock_Ptr;

   begin
      Lock_Count := Lock_Count + 1;

      --  There should be no owner for this lock at this point

      if L.Owner /= null then
         return False;
      end if;

      --  Record new owner

      L.Owner := To_Owner_ID (To_Address (Self_ID));

      if Single_Lock then
         return True;
      end if;

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
      pragma Unreferenced (Reason);

      Self_ID : constant Task_Id := Self;
      P       : Lock_Ptr;

   begin
      --  Check that caller is abort-deferred

      if Self_ID.Deferral_Level <= 0 then
         return False;
      end if;

      if Single_Lock then
         return True;
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
      Reason : Task_States) return Boolean
   is
      pragma Unreferenced (Reason);

      Self_ID : constant Task_Id := Self;
      P       : Lock_Ptr;

   begin
      --  Record new owner

      L.Owner := To_Owner_ID (To_Address (Self_ID));

      if Single_Lock then
         return True;
      end if;

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
     (T      : Task_Id;
      Reason : Task_States) return Boolean
   is
      Self_ID : constant Task_Id := Self;

   begin
      --  Is caller holding T's lock?

      if T.Common.LL.L.Owner /= To_Owner_ID (To_Address (Self_ID)) then
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
      Self_ID : constant Task_Id := Self;
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
      Self_ID : constant Task_Id := Self;

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

   function Check_Exit (Self_ID : Task_Id) return Boolean is
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

   function Check_No_Locks (Self_ID : Task_Id) return Boolean is
   begin
      return Self_ID.Common.LL.Locks = null;
   end Check_No_Locks;

   ----------------------
   -- Environment_Task --
   ----------------------

   function Environment_Task return Task_Id is
   begin
      return Environment_Task_Id;
   end Environment_Task;

   --------------
   -- Lock_RTS --
   --------------

   procedure Lock_RTS is
   begin
      Write_Lock (Single_RTS_Lock'Access, Global_Lock => True);
   end Lock_RTS;

   ----------------
   -- Unlock_RTS --
   ----------------

   procedure Unlock_RTS is
   begin
      Unlock (Single_RTS_Lock'Access, Global_Lock => True);
   end Unlock_RTS;

   ------------------
   -- Suspend_Task --
   ------------------

   function Suspend_Task
     (T           : ST.Task_Id;
      Thread_Self : Thread_Id) return Boolean
   is
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
     (T           : ST.Task_Id;
      Thread_Self : Thread_Id) return Boolean
   is
   begin
      if T.Common.LL.Thread /= Thread_Self then
         return thr_continue (T.Common.LL.Thread) = 0;
      else
         return True;
      end if;
   end Resume_Task;

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
         Result      : Interfaces.C.long;
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
         Class_Info.pc_clname (3) := ASCII.NUL;

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
