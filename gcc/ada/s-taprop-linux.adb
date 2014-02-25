------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2012, Free Software Foundation, Inc.          --
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

--  This is a GNU/Linux (GNU/LinuxThreads) version of this package

--  This package contains all the GNULL primitives that interface directly with
--  the underlying OS.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during tasking
--  operations. It causes infinite loops and other problems.

with Interfaces.C;

with System.Task_Info;
with System.Tasking.Debug;
with System.Interrupt_Management;
with System.OS_Primitives;
with System.Stack_Checking.Operations;
with System.Multiprocessors;

with System.Soft_Links;
--  We use System.Soft_Links instead of System.Tasking.Initialization
--  because the later is a higher level package that we shouldn't depend on.
--  For example when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Stages.

package body System.Task_Primitives.Operations is

   package SSL renames System.Soft_Links;
   package SC renames System.Stack_Checking.Operations;

   use System.Tasking.Debug;
   use System.Tasking;
   use Interfaces.C;
   use System.OS_Interface;
   use System.Parameters;
   use System.OS_Primitives;
   use System.Task_Info;

   ----------------
   -- Local Data --
   ----------------

   --  The followings are logically constants, but need to be initialized
   --  at run time.

   Single_RTS_Lock : aliased RTS_Lock;
   --  This is a lock to allow only one thread of control in the RTS at
   --  a time; it is used to execute in mutual exclusion from all other tasks.
   --  Used mainly in Single_Lock mode, but also to protect All_Tasks_List

   Environment_Task_Id : Task_Id;
   --  A variable to hold Task_Id for the environment task

   Unblocked_Signal_Mask : aliased sigset_t;
   --  The set of signals that should be unblocked in all tasks

   --  The followings are internal configuration constants needed

   Next_Serial_Number : Task_Serial_Number := 100;
   --  We start at 100 (reserve some special values for using in error checks)

   Time_Slice_Val : Integer;
   pragma Import (C, Time_Slice_Val, "__gl_time_slice_val");

   Dispatching_Policy : Character;
   pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");

   Locking_Policy : Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");

   Foreign_Task_Elaborated : aliased Boolean := True;
   --  Used to identified fake tasks (i.e., non-Ada Threads)

   Use_Alternate_Stack : constant Boolean := Alternate_Stack_Size /= 0;
   --  Whether to use an alternate signal stack for stack overflows

   Abort_Handler_Installed : Boolean := False;
   --  True if a handler for the abort signal is installed

   Null_Thread_Id : constant pthread_t := pthread_t'Last;
   --  Constant to indicate that the thread identifier has not yet been
   --  initialized.

   --------------------
   -- Local Packages --
   --------------------

   package Specific is

      procedure Initialize (Environment_Task : Task_Id);
      pragma Inline (Initialize);
      --  Initialize various data needed by this package

      function Is_Valid_Task return Boolean;
      pragma Inline (Is_Valid_Task);
      --  Does executing thread have a TCB?

      procedure Set (Self_Id : Task_Id);
      pragma Inline (Set);
      --  Set the self id for the current task

      function Self return Task_Id;
      pragma Inline (Self);
      --  Return a pointer to the Ada Task Control Block of the calling task

   end Specific;

   package body Specific is separate;
   --  The body of this package is target specific

   ----------------------------------
   -- ATCB allocation/deallocation --
   ----------------------------------

   package body ATCB_Allocation is separate;
   --  The body of this package is shared across several targets

   ---------------------------------
   -- Support for foreign threads --
   ---------------------------------

   function Register_Foreign_Thread (Thread : Thread_Id) return Task_Id;
   --  Allocate and Initialize a new ATCB for the current Thread

   function Register_Foreign_Thread
     (Thread : Thread_Id) return Task_Id is separate;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Abort_Handler (signo : Signal);

   -------------------
   -- Abort_Handler --
   -------------------

   procedure Abort_Handler (signo : Signal) is
      pragma Unreferenced (signo);

      Self_Id : constant Task_Id := Self;
      Result  : Interfaces.C.int;
      Old_Set : aliased sigset_t;

   begin
      --  It's not safe to raise an exception when using GCC ZCX mechanism.
      --  Note that we still need to install a signal handler, since in some
      --  cases (e.g. shutdown of the Server_Task in System.Interrupts) we
      --  need to send the Abort signal to a task.

      if ZCX_By_Default then
         return;
      end if;

      if Self_Id.Deferral_Level = 0
        and then Self_Id.Pending_ATC_Level < Self_Id.ATC_Nesting_Level
        and then not Self_Id.Aborting
      then
         Self_Id.Aborting := True;

         --  Make sure signals used for RTS internal purpose are unmasked

         Result :=
           pthread_sigmask
             (SIG_UNBLOCK,
              Unblocked_Signal_Mask'Access,
              Old_Set'Access);
         pragma Assert (Result = 0);

         raise Standard'Abort_Signal;
      end if;
   end Abort_Handler;

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

   -----------------
   -- Stack_Guard --
   -----------------

   --  The underlying thread system extends the memory (up to 2MB) when needed

   procedure Stack_Guard (T : ST.Task_Id; On : Boolean) is
      pragma Unreferenced (T);
      pragma Unreferenced (On);
   begin
      null;
   end Stack_Guard;

   --------------------
   -- Get_Thread_Id  --
   --------------------

   function Get_Thread_Id (T : ST.Task_Id) return OSI.Thread_Id is
   begin
      return T.Common.LL.Thread;
   end Get_Thread_Id;

   ----------
   -- Self --
   ----------

   function Self return Task_Id renames Specific.Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   --  Note: mutexes and cond_variables needed per-task basis are initialized
   --  in Initialize_TCB and the Storage_Error is handled. Other mutexes (such
   --  as RTS_Lock, Memory_Lock...) used in RTS is initialized before any
   --  status change of RTS. Therefore raising Storage_Error in the following
   --  routines should be able to be handled safely.

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : not null access Lock)
   is
      pragma Unreferenced (Prio);

   begin
      if Locking_Policy = 'R' then
         declare
            RWlock_Attr : aliased pthread_rwlockattr_t;
            Result      : Interfaces.C.int;

         begin
            --  Set the rwlock to prefer writer to avoid writers starvation

            Result := pthread_rwlockattr_init (RWlock_Attr'Access);
            pragma Assert (Result = 0);

            Result := pthread_rwlockattr_setkind_np
              (RWlock_Attr'Access,
               PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP);
            pragma Assert (Result = 0);

            Result := pthread_rwlock_init (L.RW'Access, RWlock_Attr'Access);

            pragma Assert (Result = 0 or else Result = ENOMEM);

            if Result = ENOMEM then
               raise Storage_Error with "Failed to allocate a lock";
            end if;
         end;

      else
         declare
            Result : Interfaces.C.int;

         begin
            Result := pthread_mutex_init (L.WO'Access, null);

            pragma Assert (Result = 0 or else Result = ENOMEM);

            if Result = ENOMEM then
               raise Storage_Error with "Failed to allocate a lock";
            end if;
         end;
      end if;
   end Initialize_Lock;

   procedure Initialize_Lock
     (L     : not null access RTS_Lock;
      Level : Lock_Level)
   is
      pragma Unreferenced (Level);

      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_init (L, null);

      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         raise Storage_Error;
      end if;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : not null access Lock) is
      Result : Interfaces.C.int;
   begin
      if Locking_Policy = 'R' then
         Result := pthread_rwlock_destroy (L.RW'Access);
      else
         Result := pthread_mutex_destroy (L.WO'Access);
      end if;
      pragma Assert (Result = 0);
   end Finalize_Lock;

   procedure Finalize_Lock (L : not null access RTS_Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_destroy (L);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock
     (L                 : not null access Lock;
      Ceiling_Violation : out Boolean)
   is
      Result : Interfaces.C.int;
   begin
      if Locking_Policy = 'R' then
         Result := pthread_rwlock_wrlock (L.RW'Access);
      else
         Result := pthread_mutex_lock (L.WO'Access);
      end if;

      Ceiling_Violation := Result = EINVAL;

      --  Assume the cause of EINVAL is a priority ceiling violation

      pragma Assert (Result = 0 or else Result = EINVAL);
   end Write_Lock;

   procedure Write_Lock
     (L           : not null access RTS_Lock;
      Global_Lock : Boolean := False)
   is
      Result : Interfaces.C.int;
   begin
      if not Single_Lock or else Global_Lock then
         Result := pthread_mutex_lock (L);
         pragma Assert (Result = 0);
      end if;
   end Write_Lock;

   procedure Write_Lock (T : Task_Id) is
      Result : Interfaces.C.int;
   begin
      if not Single_Lock then
         Result := pthread_mutex_lock (T.Common.LL.L'Access);
         pragma Assert (Result = 0);
      end if;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock
     (L                 : not null access Lock;
      Ceiling_Violation : out Boolean)
   is
      Result : Interfaces.C.int;
   begin
      if Locking_Policy = 'R' then
         Result := pthread_rwlock_rdlock (L.RW'Access);
      else
         Result := pthread_mutex_lock (L.WO'Access);
      end if;

      Ceiling_Violation := Result = EINVAL;

      --  Assume the cause of EINVAL is a priority ceiling violation

      pragma Assert (Result = 0 or else Result = EINVAL);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : not null access Lock) is
      Result : Interfaces.C.int;
   begin
      if Locking_Policy = 'R' then
         Result := pthread_rwlock_unlock (L.RW'Access);
      else
         Result := pthread_mutex_unlock (L.WO'Access);
      end if;
      pragma Assert (Result = 0);
   end Unlock;

   procedure Unlock
     (L           : not null access RTS_Lock;
      Global_Lock : Boolean := False)
   is
      Result : Interfaces.C.int;
   begin
      if not Single_Lock or else Global_Lock then
         Result := pthread_mutex_unlock (L);
         pragma Assert (Result = 0);
      end if;
   end Unlock;

   procedure Unlock (T : Task_Id) is
      Result : Interfaces.C.int;
   begin
      if not Single_Lock then
         Result := pthread_mutex_unlock (T.Common.LL.L'Access);
         pragma Assert (Result = 0);
      end if;
   end Unlock;

   -----------------
   -- Set_Ceiling --
   -----------------

   --  Dynamic priority ceilings are not supported by the underlying system

   procedure Set_Ceiling
     (L    : not null access Lock;
      Prio : System.Any_Priority)
   is
      pragma Unreferenced (L, Prio);
   begin
      null;
   end Set_Ceiling;

   -----------
   -- Sleep --
   -----------

   procedure Sleep
     (Self_ID  : Task_Id;
      Reason   : System.Tasking.Task_States)
   is
      pragma Unreferenced (Reason);

      Result : Interfaces.C.int;

   begin
      pragma Assert (Self_ID = Self);

      Result :=
        pthread_cond_wait
          (cond  => Self_ID.Common.LL.CV'Access,
           mutex => (if Single_Lock
                     then Single_RTS_Lock'Access
                     else Self_ID.Common.LL.L'Access));

      --  EINTR is not considered a failure

      pragma Assert (Result = 0 or else Result = EINTR);
   end Sleep;

   -----------------
   -- Timed_Sleep --
   -----------------

   --  This is for use within the run-time system, so abort is
   --  assumed to be already deferred, and the caller should be
   --  holding its own ATCB lock.

   procedure Timed_Sleep
     (Self_ID  : Task_Id;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
      pragma Unreferenced (Reason);

      Base_Time  : constant Duration := Monotonic_Clock;
      Check_Time : Duration := Base_Time;
      Abs_Time   : Duration;
      Request    : aliased timespec;
      Result     : Interfaces.C.int;

   begin
      Timedout := True;
      Yielded := False;

      Abs_Time :=
        (if Mode = Relative
         then Duration'Min (Time, Max_Sensible_Delay) + Check_Time
         else Duration'Min (Check_Time + Max_Sensible_Delay, Time));

      if Abs_Time > Check_Time then
         Request := To_Timespec (Abs_Time);

         loop
            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            Result :=
              pthread_cond_timedwait
                (cond    => Self_ID.Common.LL.CV'Access,
                 mutex   => (if Single_Lock
                             then Single_RTS_Lock'Access
                             else Self_ID.Common.LL.L'Access),
                 abstime => Request'Access);

            Check_Time := Monotonic_Clock;
            exit when Abs_Time <= Check_Time or else Check_Time < Base_Time;

            if Result = 0 or else Result = EINTR then

               --  Somebody may have called Wakeup for us

               Timedout := False;
               exit;
            end if;

            pragma Assert (Result = ETIMEDOUT);
         end loop;
      end if;
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   --  This is for use in implementing delay statements, so we assume the
   --  caller is abort-deferred but is holding no locks.

   procedure Timed_Delay
     (Self_ID : Task_Id;
      Time    : Duration;
      Mode    : ST.Delay_Modes)
   is
      Base_Time  : constant Duration := Monotonic_Clock;
      Check_Time : Duration := Base_Time;
      Abs_Time   : Duration;
      Request    : aliased timespec;

      Result : Interfaces.C.int;
      pragma Warnings (Off, Result);

   begin
      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (Self_ID);

      Abs_Time :=
        (if Mode = Relative
         then Time + Check_Time
         else Duration'Min (Check_Time + Max_Sensible_Delay, Time));

      if Abs_Time > Check_Time then
         Request := To_Timespec (Abs_Time);
         Self_ID.Common.State := Delay_Sleep;

         loop
            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            Result :=
              pthread_cond_timedwait
                (cond    => Self_ID.Common.LL.CV'Access,
                 mutex   => (if Single_Lock
                             then Single_RTS_Lock'Access
                             else Self_ID.Common.LL.L'Access),
                 abstime => Request'Access);

            Check_Time := Monotonic_Clock;
            exit when Abs_Time <= Check_Time or else Check_Time < Base_Time;

            pragma Assert (Result = 0 or else
              Result = ETIMEDOUT or else
              Result = EINTR);
         end loop;

         Self_ID.Common.State := Runnable;
      end if;

      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Result := sched_yield;
   end Timed_Delay;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration is
      use Interfaces;

      type timeval is array (1 .. 2) of System.OS_Interface.time_t;

      procedure timeval_to_duration
        (T    : not null access timeval;
         sec  : not null access C.long;
         usec : not null access C.long);
      pragma Import (C, timeval_to_duration, "__gnat_timeval_to_duration");

      Micro  : constant := 10**6;
      sec    : aliased C.long;
      usec   : aliased C.long;
      TV     : aliased timeval;
      Result : int;

      function gettimeofday
        (Tv : access timeval;
         Tz : System.Address := System.Null_Address) return int;
      pragma Import (C, gettimeofday, "gettimeofday");

   begin
      Result := gettimeofday (TV'Access, System.Null_Address);
      pragma Assert (Result = 0);
      timeval_to_duration (TV'Access, sec'Access, usec'Access);
      return Duration (sec) + Duration (usec) / Micro;
   end Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
   begin
      return 10#1.0#E-6;
   end RT_Resolution;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_Id; Reason : System.Tasking.Task_States) is
      pragma Unreferenced (Reason);
      Result : Interfaces.C.int;
   begin
      Result := pthread_cond_signal (T.Common.LL.CV'Access);
      pragma Assert (Result = 0);
   end Wakeup;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
      Result : Interfaces.C.int;
      pragma Unreferenced (Result);
   begin
      if Do_Yield then
         Result := sched_yield;
      end if;
   end Yield;

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
      Param  : aliased struct_sched_param;

      function Get_Policy (Prio : System.Any_Priority) return Character;
      pragma Import (C, Get_Policy, "__gnat_get_specific_dispatching");
      --  Get priority specific dispatching policy

      Priority_Specific_Policy : constant Character := Get_Policy (Prio);
      --  Upper case first character of the policy name corresponding to the
      --  task as set by a Priority_Specific_Dispatching pragma.

   begin
      T.Common.Current_Priority := Prio;

      --  Priorities are 1 .. 99 on GNU/Linux, so we map 0 .. 98 to 1 .. 99

      Param.sched_priority := Interfaces.C.int (Prio) + 1;

      if Dispatching_Policy = 'R'
        or else Priority_Specific_Policy = 'R'
        or else Time_Slice_Val > 0
      then
         Result :=
           pthread_setschedparam
             (T.Common.LL.Thread, SCHED_RR, Param'Access);

      elsif Dispatching_Policy = 'F'
        or else Priority_Specific_Policy = 'F'
        or else Time_Slice_Val = 0
      then
         Result :=
           pthread_setschedparam
             (T.Common.LL.Thread, SCHED_FIFO, Param'Access);

      else
         Param.sched_priority := 0;
         Result :=
           pthread_setschedparam
             (T.Common.LL.Thread,
              SCHED_OTHER, Param'Access);
      end if;

      pragma Assert (Result = 0 or else Result = EPERM);
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
   begin
      if Self_ID.Common.Task_Info /= null
        and then Self_ID.Common.Task_Info.CPU_Affinity = No_CPU
      then
         raise Invalid_CPU_Number;
      end if;

      Self_ID.Common.LL.Thread := pthread_self;
      Self_ID.Common.LL.LWP := lwp_self;

      if Self_ID.Common.Task_Image_Len > 0 then
         declare
            Task_Name : String (1 .. Parameters.Max_Task_Image_Length + 1);
            Result    : int;

         begin
            --  Set thread name to ease debugging

            Task_Name (1 .. Self_ID.Common.Task_Image_Len) :=
              Self_ID.Common.Task_Image (1 .. Self_ID.Common.Task_Image_Len);
            Task_Name (Self_ID.Common.Task_Image_Len + 1) := ASCII.NUL;

            Result := prctl (PR_SET_NAME, unsigned_long (Task_Name'Address));
            pragma Assert (Result = 0);
         end;
      end if;

      Specific.Set (Self_ID);

      if Use_Alternate_Stack
        and then Self_ID.Common.Task_Alternate_Stack /= Null_Address
      then
         declare
            Stack  : aliased stack_t;
            Result : Interfaces.C.int;
         begin
            Stack.ss_sp    := Self_ID.Common.Task_Alternate_Stack;
            Stack.ss_size  := Alternate_Stack_Size;
            Stack.ss_flags := 0;
            Result := sigaltstack (Stack'Access, null);
            pragma Assert (Result = 0);
         end;
      end if;
   end Enter_Task;

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
         return Register_Foreign_Thread (pthread_self);
      end if;
   end Register_Foreign_Thread;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_Id; Succeeded : out Boolean) is
      Cond_Attr : aliased pthread_condattr_t;
      Result    : Interfaces.C.int;

   begin
      --  Give the task a unique serial number

      Self_ID.Serial_Number := Next_Serial_Number;
      Next_Serial_Number := Next_Serial_Number + 1;
      pragma Assert (Next_Serial_Number /= 0);

      Self_ID.Common.LL.Thread := Null_Thread_Id;

      if not Single_Lock then
         Result :=
           pthread_mutex_init (Self_ID.Common.LL.L'Access, null);
         pragma Assert (Result = 0 or else Result = ENOMEM);

         if Result /= 0 then
            Succeeded := False;
            return;
         end if;
      end if;

      Result := pthread_condattr_init (Cond_Attr'Access);
      pragma Assert (Result = 0);

      Result :=
        pthread_cond_init (Self_ID.Common.LL.CV'Access, Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = 0 then
         Succeeded := True;
      else
         if not Single_Lock then
            Result := pthread_mutex_destroy (Self_ID.Common.LL.L'Access);
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
      Attributes          : aliased pthread_attr_t;
      Adjusted_Stack_Size : Interfaces.C.size_t;
      Result              : Interfaces.C.int;

      use type System.Multiprocessors.CPU_Range;

   begin
      --  Check whether both Dispatching_Domain and CPU are specified for
      --  the task, and the CPU value is not contained within the range of
      --  processors for the domain.

      if T.Common.Domain /= null
        and then T.Common.Base_CPU /= System.Multiprocessors.Not_A_Specific_CPU
        and then
          (T.Common.Base_CPU not in T.Common.Domain'Range
            or else not T.Common.Domain (T.Common.Base_CPU))
      then
         Succeeded := False;
         return;
      end if;

      Adjusted_Stack_Size :=
         Interfaces.C.size_t (Stack_Size + Alternate_Stack_Size);

      Result := pthread_attr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Succeeded := False;
         return;
      end if;

      Result :=
        pthread_attr_setstacksize (Attributes'Access, Adjusted_Stack_Size);
      pragma Assert (Result = 0);

      Result :=
        pthread_attr_setdetachstate
          (Attributes'Access, PTHREAD_CREATE_DETACHED);
      pragma Assert (Result = 0);

      --  Set the required attributes for the creation of the thread

      --  Note: Previously, we called pthread_setaffinity_np (after thread
      --  creation but before thread activation) to set the affinity but it was
      --  not behaving as expected. Setting the required attributes for the
      --  creation of the thread works correctly and it is more appropriate.

      --  Do nothing if required support not provided by the operating system

      if pthread_attr_setaffinity_np'Address = System.Null_Address then
         null;

      --  Support is available

      elsif T.Common.Base_CPU /= System.Multiprocessors.Not_A_Specific_CPU then
         declare
            CPUs    : constant size_t :=
                        Interfaces.C.size_t
                          (System.Multiprocessors.Number_Of_CPUs);
            CPU_Set : constant cpu_set_t_ptr := CPU_ALLOC (CPUs);
            Size    : constant size_t := CPU_ALLOC_SIZE (CPUs);

         begin
            CPU_ZERO (Size, CPU_Set);
            System.OS_Interface.CPU_SET
              (int (T.Common.Base_CPU), Size, CPU_Set);
            Result :=
              pthread_attr_setaffinity_np (Attributes'Access, Size, CPU_Set);
            pragma Assert (Result = 0);

            CPU_FREE (CPU_Set);
         end;

      --  Handle Task_Info

      elsif T.Common.Task_Info /= null then
         Result :=
           pthread_attr_setaffinity_np
             (Attributes'Access,
              CPU_SETSIZE / 8,
              T.Common.Task_Info.CPU_Affinity'Access);
         pragma Assert (Result = 0);

      --  Handle dispatching domains

      --  To avoid changing CPU affinities when not needed, we set the
      --  affinity only when assigning to a domain other than the default
      --  one, or when the default one has been modified.

      elsif T.Common.Domain /= null and then
        (T.Common.Domain /= ST.System_Domain
          or else T.Common.Domain.all /=
                    (Multiprocessors.CPU'First ..
                     Multiprocessors.Number_Of_CPUs => True))
      then
         declare
            CPUs    : constant size_t :=
                        Interfaces.C.size_t
                          (System.Multiprocessors.Number_Of_CPUs);
            CPU_Set : constant cpu_set_t_ptr := CPU_ALLOC (CPUs);
            Size    : constant size_t := CPU_ALLOC_SIZE (CPUs);

         begin
            CPU_ZERO (Size, CPU_Set);

            --  Set the affinity to all the processors belonging to the
            --  dispatching domain.

            for Proc in T.Common.Domain'Range loop
               if T.Common.Domain (Proc) then
                  System.OS_Interface.CPU_SET (int (Proc), Size, CPU_Set);
               end if;
            end loop;

            Result :=
              pthread_attr_setaffinity_np (Attributes'Access, Size, CPU_Set);
            pragma Assert (Result = 0);

            CPU_FREE (CPU_Set);
         end;
      end if;

      --  Since the initial signal mask of a thread is inherited from the
      --  creator, and the Environment task has all its signals masked, we
      --  do not need to manipulate caller's signal mask at this point.
      --  All tasks in RTS will have All_Tasks_Mask initially.

      --  Note: the use of Unrestricted_Access in the following call is needed
      --  because otherwise we have an error of getting a access-to-volatile
      --  value which points to a non-volatile object. But in this case it is
      --  safe to do this, since we know we have no problems with aliasing and
      --  Unrestricted_Access bypasses this check.

      Result :=
        pthread_create
          (T.Common.LL.Thread'Unrestricted_Access,
           Attributes'Access,
           Thread_Body_Access (Wrapper),
           To_Address (T));

      pragma Assert
        (Result = 0 or else Result = EAGAIN or else Result = ENOMEM);

      if Result /= 0 then
         Succeeded := False;
         Result := pthread_attr_destroy (Attributes'Access);
         pragma Assert (Result = 0);
         return;
      end if;

      Succeeded := True;

      Result := pthread_attr_destroy (Attributes'Access);
      pragma Assert (Result = 0);

      Set_Priority (T, Priority);
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_Id) is
      Result : Interfaces.C.int;

   begin
      if not Single_Lock then
         Result := pthread_mutex_destroy (T.Common.LL.L'Access);
         pragma Assert (Result = 0);
      end if;

      Result := pthread_cond_destroy (T.Common.LL.CV'Access);
      pragma Assert (Result = 0);

      if T.Known_Tasks_Index /= -1 then
         Known_Tasks (T.Known_Tasks_Index) := null;
      end if;

      SC.Invalidate_Stack_Cache (T.Common.Compiler_Data.Pri_Stack_Info'Access);

      ATCB_Allocation.Free_ATCB (T);
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   procedure Exit_Task is
   begin
      Specific.Set (null);
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_Id) is
      Result : Interfaces.C.int;
      ESRCH  : constant := 3; -- No such process
      --  It can happen that T has already vanished, in which case pthread_kill
      --  returns ESRCH, so we don't consider that to be an error.
   begin
      if Abort_Handler_Installed then
         Result :=
           pthread_kill
             (T.Common.LL.Thread,
              Signal (System.Interrupt_Management.Abort_Task_Interrupt));
         pragma Assert (Result = 0 or else Result = ESRCH);
      end if;
   end Abort_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : in out Suspension_Object) is
      Result : Interfaces.C.int;

   begin
      --  Initialize internal state (always to False (RM D.10(6)))

      S.State := False;
      S.Waiting := False;

      --  Initialize internal mutex

      Result := pthread_mutex_init (S.L'Access, null);

      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         raise Storage_Error;
      end if;

      --  Initialize internal condition variable

      Result := pthread_cond_init (S.CV'Access, null);

      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Result := pthread_mutex_destroy (S.L'Access);
         pragma Assert (Result = 0);

         if Result = ENOMEM then
            raise Storage_Error;
         end if;
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (S : in out Suspension_Object) is
      Result : Interfaces.C.int;

   begin
      --  Destroy internal mutex

      Result := pthread_mutex_destroy (S.L'Access);
      pragma Assert (Result = 0);

      --  Destroy internal condition variable

      Result := pthread_cond_destroy (S.CV'Access);
      pragma Assert (Result = 0);
   end Finalize;

   -------------------
   -- Current_State --
   -------------------

   function Current_State (S : Suspension_Object) return Boolean is
   begin
      --  We do not want to use lock on this read operation. State is marked
      --  as Atomic so that we ensure that the value retrieved is correct.

      return S.State;
   end Current_State;

   ---------------
   -- Set_False --
   ---------------

   procedure Set_False (S : in out Suspension_Object) is
      Result : Interfaces.C.int;

   begin
      SSL.Abort_Defer.all;

      Result := pthread_mutex_lock (S.L'Access);
      pragma Assert (Result = 0);

      S.State := False;

      Result := pthread_mutex_unlock (S.L'Access);
      pragma Assert (Result = 0);

      SSL.Abort_Undefer.all;
   end Set_False;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
      Result : Interfaces.C.int;

   begin
      SSL.Abort_Defer.all;

      Result := pthread_mutex_lock (S.L'Access);
      pragma Assert (Result = 0);

      --  If there is already a task waiting on this suspension object then
      --  we resume it, leaving the state of the suspension object to False,
      --  as it is specified in ARM D.10 par. 9. Otherwise, it just leaves
      --  the state to True.

      if S.Waiting then
         S.Waiting := False;
         S.State := False;

         Result := pthread_cond_signal (S.CV'Access);
         pragma Assert (Result = 0);

      else
         S.State := True;
      end if;

      Result := pthread_mutex_unlock (S.L'Access);
      pragma Assert (Result = 0);

      SSL.Abort_Undefer.all;
   end Set_True;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
      Result : Interfaces.C.int;

   begin
      SSL.Abort_Defer.all;

      Result := pthread_mutex_lock (S.L'Access);
      pragma Assert (Result = 0);

      if S.Waiting then

         --  Program_Error must be raised upon calling Suspend_Until_True
         --  if another task is already waiting on that suspension object
         --  (RM D.10(10)).

         Result := pthread_mutex_unlock (S.L'Access);
         pragma Assert (Result = 0);

         SSL.Abort_Undefer.all;

         raise Program_Error;

      else
         --  Suspend the task if the state is False. Otherwise, the task
         --  continues its execution, and the state of the suspension object
         --  is set to False (ARM D.10 par. 9).

         if S.State then
            S.State := False;
         else
            S.Waiting := True;

            loop
               --  Loop in case pthread_cond_wait returns earlier than expected
               --  (e.g. in case of EINTR caused by a signal). This should not
               --  happen with the current Linux implementation of pthread, but
               --  POSIX does not guarantee it so this may change in future.

               Result := pthread_cond_wait (S.CV'Access, S.L'Access);
               pragma Assert (Result = 0 or else Result = EINTR);

               exit when not S.Waiting;
            end loop;
         end if;

         Result := pthread_mutex_unlock (S.L'Access);
         pragma Assert (Result = 0);

         SSL.Abort_Undefer.all;
      end if;
   end Suspend_Until_True;

   ----------------
   -- Check_Exit --
   ----------------

   --  Dummy version

   function Check_Exit (Self_ID : ST.Task_Id) return Boolean is
      pragma Unreferenced (Self_ID);
   begin
      return True;
   end Check_Exit;

   --------------------
   -- Check_No_Locks --
   --------------------

   function Check_No_Locks (Self_ID : ST.Task_Id) return Boolean is
      pragma Unreferenced (Self_ID);
   begin
      return True;
   end Check_No_Locks;

   ----------------------
   -- Environment_Task --
   ----------------------

   function Environment_Task return Task_Id is
   begin
      return Environment_Task_Id;
   end Environment_Task;

   ------------------
   -- Suspend_Task --
   ------------------

   function Suspend_Task
     (T           : ST.Task_Id;
      Thread_Self : Thread_Id) return Boolean
   is
   begin
      if T.Common.LL.Thread /= Thread_Self then
         return pthread_kill (T.Common.LL.Thread, SIGSTOP) = 0;
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
         return pthread_kill (T.Common.LL.Thread, SIGCONT) = 0;
      else
         return True;
      end if;
   end Resume_Task;

   --------------------
   -- Stop_All_Tasks --
   --------------------

   procedure Stop_All_Tasks is
   begin
      null;
   end Stop_All_Tasks;

   ---------------
   -- Stop_Task --
   ---------------

   function Stop_Task (T : ST.Task_Id) return Boolean is
      pragma Unreferenced (T);
   begin
      return False;
   end Stop_Task;

   -------------------
   -- Continue_Task --
   -------------------

   function Continue_Task (T : ST.Task_Id) return Boolean is
      pragma Unreferenced (T);
   begin
      return False;
   end Continue_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_Id) is
      act     : aliased struct_sigaction;
      old_act : aliased struct_sigaction;
      Tmp_Set : aliased sigset_t;
      Result  : Interfaces.C.int;
      --  Whether to use an alternate signal stack for stack overflows

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

      use type System.Multiprocessors.CPU_Range;

   begin
      Environment_Task_Id := Environment_Task;

      Interrupt_Management.Initialize;

      --  Prepare the set of signals that should be unblocked in all tasks

      Result := sigemptyset (Unblocked_Signal_Mask'Access);
      pragma Assert (Result = 0);

      for J in Interrupt_Management.Interrupt_ID loop
         if System.Interrupt_Management.Keep_Unmasked (J) then
            Result := sigaddset (Unblocked_Signal_Mask'Access, Signal (J));
            pragma Assert (Result = 0);
         end if;
      end loop;

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);

      --  Initialize the global RTS lock

      Specific.Initialize (Environment_Task);

      if Use_Alternate_Stack then
         Environment_Task.Common.Task_Alternate_Stack :=
           Alternate_Stack'Address;
      end if;

      --  Make environment task known here because it doesn't go through
      --  Activate_Tasks, which does it for all other tasks.

      Known_Tasks (Known_Tasks'First) := Environment_Task;
      Environment_Task.Known_Tasks_Index := Known_Tasks'First;

      Enter_Task (Environment_Task);

      if State
          (System.Interrupt_Management.Abort_Task_Interrupt) /= Default
      then
         act.sa_flags := 0;
         act.sa_handler := Abort_Handler'Address;

         Result := sigemptyset (Tmp_Set'Access);
         pragma Assert (Result = 0);
         act.sa_mask := Tmp_Set;

         Result :=
           sigaction
           (Signal (Interrupt_Management.Abort_Task_Interrupt),
            act'Unchecked_Access,
            old_act'Unchecked_Access);
         pragma Assert (Result = 0);
         Abort_Handler_Installed := True;
      end if;

      --  pragma CPU and dispatching domains for the environment task

      Set_Task_Affinity (Environment_Task);
   end Initialize;

   -----------------------
   -- Set_Task_Affinity --
   -----------------------

   procedure Set_Task_Affinity (T : ST.Task_Id) is
      use type System.Multiprocessors.CPU_Range;

   begin
      --  Do nothing if there is no support for setting affinities or the
      --  underlying thread has not yet been created. If the thread has not
      --  yet been created then the proper affinity will be set during its
      --  creation.

      if pthread_setaffinity_np'Address /= System.Null_Address
        and then T.Common.LL.Thread /= Null_Thread_Id
      then
         declare
            CPUs    : constant size_t :=
                        Interfaces.C.size_t
                          (System.Multiprocessors.Number_Of_CPUs);
            CPU_Set : cpu_set_t_ptr := null;
            Size    : constant size_t := CPU_ALLOC_SIZE (CPUs);

            Result  : Interfaces.C.int;

         begin
            --  We look at the specific CPU (Base_CPU) first, then at the
            --  Task_Info field, and finally at the assigned dispatching
            --  domain, if any.

            if T.Common.Base_CPU /= Multiprocessors.Not_A_Specific_CPU then

               --  Set the affinity to an unique CPU

               CPU_Set := CPU_ALLOC (CPUs);
               System.OS_Interface.CPU_ZERO (Size, CPU_Set);
               System.OS_Interface.CPU_SET
                 (int (T.Common.Base_CPU), Size, CPU_Set);

            --  Handle Task_Info

            elsif T.Common.Task_Info /= null then
               CPU_Set := T.Common.Task_Info.CPU_Affinity'Access;

            --  Handle dispatching domains

            elsif T.Common.Domain /= null and then
              (T.Common.Domain /= ST.System_Domain
                or else T.Common.Domain.all /=
                          (Multiprocessors.CPU'First ..
                           Multiprocessors.Number_Of_CPUs => True))
            then
               --  Set the affinity to all the processors belonging to the
               --  dispatching domain. To avoid changing CPU affinities when
               --  not needed, we set the affinity only when assigning to a
               --  domain other than the default one, or when the default one
               --  has been modified.

               CPU_Set := CPU_ALLOC (CPUs);
               System.OS_Interface.CPU_ZERO (Size, CPU_Set);

               for Proc in T.Common.Domain'Range loop
                  System.OS_Interface.CPU_SET (int (Proc), Size, CPU_Set);
               end loop;
            end if;

            --  We set the new affinity if needed. Otherwise, the new task
            --  will inherit its creator's CPU affinity mask (according to
            --  the documentation of pthread_setaffinity_np), which is
            --  consistent with Ada's required semantics.

            if CPU_Set /= null then
               Result :=
                 pthread_setaffinity_np (T.Common.LL.Thread, Size, CPU_Set);
               pragma Assert (Result = 0);

               CPU_FREE (CPU_Set);
            end if;
         end;
      end if;
   end Set_Task_Affinity;

end System.Task_Primitives.Operations;
