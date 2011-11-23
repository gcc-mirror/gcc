------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2011, Free Software Foundation, Inc.          --
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

--  This is a IRIX (pthread library) version of this package

--  This package contains all the GNULL primitives that interface directly with
--  the underlying OS.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during tasking
--  operations. It causes infinite loops and other problems.

with Ada.Unchecked_Conversion;

with Interfaces.C;

with System.Task_Info;
with System.Tasking.Debug;
with System.Interrupt_Management;
with System.OS_Constants;
with System.OS_Primitives;
with System.IO;

with System.Soft_Links;
--  We use System.Soft_Links instead of System.Tasking.Initialization
--  because the later is a higher level package that we shouldn't depend on.
--  For example when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Stages.

package body System.Task_Primitives.Operations is

   package OSC renames System.OS_Constants;
   package SSL renames System.Soft_Links;

   use System.Tasking;
   use System.Tasking.Debug;
   use Interfaces.C;
   use System.OS_Interface;
   use System.OS_Primitives;
   use System.Parameters;

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

   Locking_Policy : Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");

   Time_Slice_Val : Integer;
   pragma Import (C, Time_Slice_Val, "__gl_time_slice_val");

   Dispatching_Policy : Character;
   pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");

   Unblocked_Signal_Mask : aliased sigset_t;

   Foreign_Task_Elaborated : aliased Boolean := True;
   --  Used to identified fake tasks (i.e., non-Ada Threads)

   Abort_Handler_Installed : Boolean := False;
   --  True if a handler for the abort signal is installed

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

   function To_Address is
     new Ada.Unchecked_Conversion (Task_Id, System.Address);

   procedure Abort_Handler (Sig : Signal);
   --  Signal handler used to implement asynchronous abort

   -------------------
   -- Abort_Handler --
   -------------------

   procedure Abort_Handler (Sig : Signal) is
      pragma Unreferenced (Sig);

      T       : constant Task_Id := Self;
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

      if T.Deferral_Level = 0
        and then T.Pending_ATC_Level < T.ATC_Nesting_Level
      then
         --  Make sure signals used for RTS internal purpose are unmasked

         Result := pthread_sigmask
           (SIG_UNBLOCK,
            Unblocked_Signal_Mask'Access,
            Old_Set'Access);
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
      pragma Unreferenced (On);
      pragma Unreferenced (T);
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
      Attributes : aliased pthread_mutexattr_t;
      Result     : Interfaces.C.int;

   begin
      Result := pthread_mutexattr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         raise Storage_Error;
      end if;

      if Locking_Policy = 'C' then
         Result :=
           pthread_mutexattr_setprotocol
             (Attributes'Access, PTHREAD_PRIO_PROTECT);
         pragma Assert (Result = 0);

         Result :=
           pthread_mutexattr_setprioceiling
             (Attributes'Access, Interfaces.C.int (Prio));
         pragma Assert (Result = 0);
      end if;

      Result := pthread_mutex_init (L.WO'Access, Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         Result := pthread_mutexattr_destroy (Attributes'Access);
         raise Storage_Error;
      end if;

      Result := pthread_mutexattr_destroy (Attributes'Access);
      pragma Assert (Result = 0);
   end Initialize_Lock;

   procedure Initialize_Lock
     (L     : not null access RTS_Lock;
      Level : Lock_Level)
   is
      pragma Unreferenced (Level);

      Attributes : aliased pthread_mutexattr_t;
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutexattr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         raise Storage_Error;
      end if;

      if Locking_Policy = 'C' then
         Result := pthread_mutexattr_setprotocol
           (Attributes'Access, PTHREAD_PRIO_PROTECT);
         pragma Assert (Result = 0);

         Result := pthread_mutexattr_setprioceiling
            (Attributes'Access, Interfaces.C.int (System.Any_Priority'Last));
         pragma Assert (Result = 0);
      end if;

      Result := pthread_mutex_init (L, Attributes'Access);

      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         Result := pthread_mutexattr_destroy (Attributes'Access);
         raise Storage_Error;
      end if;

      Result := pthread_mutexattr_destroy (Attributes'Access);
      pragma Assert (Result = 0);
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : not null access Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_destroy (L.WO'Access);
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
     (L : not null access Lock; Ceiling_Violation : out Boolean)
   is
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_lock (L.WO'Access);
      Ceiling_Violation := Result = EINVAL;

      --  Assumes the cause of EINVAL is a priority ceiling violation

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
     (L : not null access Lock; Ceiling_Violation : out Boolean) is
   begin
      Write_Lock (L, Ceiling_Violation);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : not null access Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_unlock (L.WO'Access);
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
     (Self_ID : ST.Task_Id;
      Reason  : System.Tasking.Task_States)
   is
      pragma Unreferenced (Reason);
      Result : Interfaces.C.int;

   begin
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

   procedure Timed_Sleep
     (Self_ID  : Task_Id;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : Task_States;
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
      Yielded  := False;

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

            if Result = 0 or else errno = EINTR then
               Timedout := False;
               exit;
            end if;
         end loop;
      end if;
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   --  This is for use in implementing delay statements, so we assume
   --  the caller is abort-deferred but is holding no locks.

   procedure Timed_Delay
     (Self_ID : Task_Id;
      Time    : Duration;
      Mode    : ST.Delay_Modes)
   is
      Base_Time  : constant Duration := Monotonic_Clock;
      Check_Time : Duration := Base_Time;
      Abs_Time   : Duration;
      Request    : aliased timespec;
      Result     : Interfaces.C.int;

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

            pragma Assert (Result = 0
              or else Result = ETIMEDOUT
              or else Result = EINTR);
         end loop;

         Self_ID.Common.State := Runnable;
      end if;

      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Yield;
   end Timed_Delay;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration is
      TS     : aliased timespec;
      Result : Interfaces.C.int;
   begin
      Result := clock_gettime (OSC.CLOCK_RT_Ada, TS'Unchecked_Access);
      pragma Assert (Result = 0);
      return To_Duration (TS);
   end Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
   begin
      --  The clock_getres (OSC.CLOCK_RT_Ada) function appears to return
      --  the interrupt resolution of the realtime clock and not the actual
      --  resolution of reading the clock. Even though this last value is
      --  only guaranteed to be 100 Hz, at least the Origin 200 appears to
      --  have a microsecond resolution or better.

      --  ??? We should figure out a method to return the right value on
      --  all SGI hardware.

      return 0.000_001;
   end RT_Resolution;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : ST.Task_Id; Reason : System.Tasking.Task_States) is
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

      Result       : Interfaces.C.int;
      Param        : aliased struct_sched_param;
      Sched_Policy : Interfaces.C.int;

      use type System.Task_Info.Task_Info_Type;

      function To_Int is new Ada.Unchecked_Conversion
        (System.Task_Info.Thread_Scheduling_Policy, Interfaces.C.int);

      function Get_Policy (Prio : System.Any_Priority) return Character;
      pragma Import (C, Get_Policy, "__gnat_get_specific_dispatching");
      --  Get priority specific dispatching policy

      Priority_Specific_Policy : constant Character := Get_Policy (Prio);
      --  Upper case first character of the policy name corresponding to the
      --  task as set by a Priority_Specific_Dispatching pragma.

   begin
      T.Common.Current_Priority := Prio;
      Param.sched_priority := Interfaces.C.int (Prio);

      if T.Common.Task_Info /= null then
         Sched_Policy := To_Int (T.Common.Task_Info.Policy);

      elsif Dispatching_Policy = 'R'
        or else Priority_Specific_Policy = 'R'
        or else Time_Slice_Val > 0
      then
         Sched_Policy := SCHED_RR;

      else
         Sched_Policy := SCHED_FIFO;
      end if;

      Result := pthread_setschedparam (T.Common.LL.Thread, Sched_Policy,
        Param'Access);
      pragma Assert (Result = 0);
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
      Result : Interfaces.C.int;

      function To_Int is new Ada.Unchecked_Conversion
        (System.Task_Info.CPU_Number, Interfaces.C.int);

      use System.Task_Info;

   begin
      Self_ID.Common.LL.Thread := pthread_self;
      Specific.Set (Self_ID);

      if Self_ID.Common.Task_Info /= null
        and then Self_ID.Common.Task_Info.Scope = PTHREAD_SCOPE_SYSTEM
        and then Self_ID.Common.Task_Info.Runon_CPU /= ANY_CPU
      then
         Result := pthread_setrunon_np
           (To_Int (Self_ID.Common.Task_Info.Runon_CPU));
         pragma Assert (Result = 0);
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
      Result    : Interfaces.C.int;
      Cond_Attr : aliased pthread_condattr_t;

   begin
      if not Single_Lock then
         Initialize_Lock (Self_ID.Common.LL.L'Access, ATCB_Level);
      end if;

      Result := pthread_condattr_init (Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = 0 then
         Result :=
           pthread_cond_init (Self_ID.Common.LL.CV'Access, Cond_Attr'Access);
         pragma Assert (Result = 0 or else Result = ENOMEM);
      end if;

      if Result = 0 then
         Succeeded := True;
      else
         if not Single_Lock then
            Result := pthread_mutex_destroy (Self_ID.Common.LL.L'Access);
            pragma Assert (Result = 0);
         end if;

         Succeeded := False;
      end if;

      Result := pthread_condattr_destroy (Cond_Attr'Access);
      pragma Assert (Result = 0);
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
      use System.Task_Info;

      Attributes  : aliased pthread_attr_t;
      Sched_Param : aliased struct_sched_param;
      Result      : Interfaces.C.int;

      function Thread_Body_Access is new
        Ada.Unchecked_Conversion (System.Address, Thread_Body);
      function To_Int is new Ada.Unchecked_Conversion
        (System.Task_Info.Thread_Scheduling_Scope, Interfaces.C.int);
      function To_Int is new Ada.Unchecked_Conversion
        (System.Task_Info.Thread_Scheduling_Inheritance, Interfaces.C.int);
      function To_Int is new Ada.Unchecked_Conversion
        (System.Task_Info.Thread_Scheduling_Policy, Interfaces.C.int);

   begin
      Result := pthread_attr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Succeeded := False;
         return;
      end if;

      Result :=
        pthread_attr_setdetachstate
          (Attributes'Access, PTHREAD_CREATE_DETACHED);
      pragma Assert (Result = 0);

      Result :=
        pthread_attr_setstacksize
          (Attributes'Access, Interfaces.C.size_t (Stack_Size));
      pragma Assert (Result = 0);

      if T.Common.Task_Info /= null then
         Result :=
           pthread_attr_setscope
             (Attributes'Access, To_Int (T.Common.Task_Info.Scope));
         pragma Assert (Result = 0);

         Result :=
           pthread_attr_setinheritsched
             (Attributes'Access, To_Int (T.Common.Task_Info.Inheritance));
         pragma Assert (Result = 0);

         Result :=
           pthread_attr_setschedpolicy
             (Attributes'Access, To_Int (T.Common.Task_Info.Policy));
         pragma Assert (Result = 0);

         Sched_Param.sched_priority :=
           Interfaces.C.int (T.Common.Task_Info.Priority);

         Result :=
           pthread_attr_setschedparam
             (Attributes'Access, Sched_Param'Access);
         pragma Assert (Result = 0);
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

      if Result /= 0
        and then T.Common.Task_Info /= null
        and then T.Common.Task_Info.Scope = PTHREAD_SCOPE_SYSTEM
      then
         --  The pthread_create call may have failed because we asked for a
         --  system scope pthread and none were available (probably because
         --  the program was not executed by the superuser). Let's try for
         --  a process scope pthread instead of raising Tasking_Error.

         System.IO.Put_Line
           ("Request for PTHREAD_SCOPE_SYSTEM in Task_Info pragma for task");
         System.IO.Put ("""");
         System.IO.Put (T.Common.Task_Image (1 .. T.Common.Task_Image_Len));
         System.IO.Put_Line (""" could not be honored. ");
         System.IO.Put_Line ("Scope changed to PTHREAD_SCOPE_PROCESS");

         T.Common.Task_Info.Scope := PTHREAD_SCOPE_PROCESS;
         Result :=
           pthread_attr_setscope
             (Attributes'Access, To_Int (T.Common.Task_Info.Scope));
         pragma Assert (Result = 0);

         --  Note: the use of Unrestricted_Access in the following call
         --  is needed because otherwise we have an error of getting a
         --  access-to-volatile value which points to a non-volatile object.
         --  But in this case it is safe to do this, since we know we have no
         --  aliasing problems and Unrestricted_Access bypasses this check.

         Result :=
           pthread_create
             (T.Common.LL.Thread'Unrestricted_Access,
              Attributes'Access,
              Thread_Body_Access (Wrapper),
              To_Address (T));
      end if;

      pragma Assert (Result = 0 or else Result = EAGAIN);

      Succeeded := Result = 0;

      if Succeeded then

         --  The following needs significant commenting ???

         if T.Common.Task_Info /= null then
            T.Common.Base_Priority := T.Common.Task_Info.Priority;
            Set_Priority (T, T.Common.Task_Info.Priority);
         else
            Set_Priority (T, Priority);
         end if;
      end if;

      Result := pthread_attr_destroy (Attributes'Access);
      pragma Assert (Result = 0);
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
   begin
      if Abort_Handler_Installed then
         Result :=
           pthread_kill
             (T.Common.LL.Thread,
              Signal (System.Interrupt_Management.Abort_Task_Interrupt));
         pragma Assert (Result = 0);
      end if;
   end Abort_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : in out Suspension_Object) is
      Mutex_Attr : aliased pthread_mutexattr_t;
      Cond_Attr  : aliased pthread_condattr_t;
      Result     : Interfaces.C.int;

   begin
      --  Initialize internal state (always to False (RM D.10(6))

      S.State := False;
      S.Waiting := False;

      --  Initialize internal mutex

      Result := pthread_mutexattr_init (Mutex_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         raise Storage_Error;
      end if;

      Result := pthread_mutex_init (S.L'Access, Mutex_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         Result := pthread_mutexattr_destroy (Mutex_Attr'Access);
         pragma Assert (Result = 0);

         raise Storage_Error;
      end if;

      Result := pthread_mutexattr_destroy (Mutex_Attr'Access);
      pragma Assert (Result = 0);

      --  Initialize internal condition variable

      Result := pthread_condattr_init (Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Result := pthread_mutex_destroy (S.L'Access);
         pragma Assert (Result = 0);

         if Result = ENOMEM then
            raise Storage_Error;
         end if;
      end if;

      Result := pthread_cond_init (S.CV'Access, Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Result := pthread_mutex_destroy (S.L'Access);
         pragma Assert (Result = 0);

         if Result = ENOMEM then
            Result := pthread_condattr_destroy (Cond_Attr'Access);
            pragma Assert (Result = 0);
            raise Storage_Error;
         end if;
      end if;

      Result := pthread_condattr_destroy (Cond_Attr'Access);
      pragma Assert (Result = 0);
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
               --  (e.g. in case of EINTR caused by a signal).

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
      pragma Unreferenced (T);
      pragma Unreferenced (Thread_Self);
   begin
      return False;
   end Suspend_Task;

   -----------------
   -- Resume_Task --
   -----------------

   function Resume_Task
     (T           : ST.Task_Id;
      Thread_Self : Thread_Id) return Boolean
   is
      pragma Unreferenced (T);
      pragma Unreferenced (Thread_Self);
   begin
      return False;
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

      function State
        (Int : System.Interrupt_Management.Interrupt_ID) return Character;
      pragma Import (C, State, "__gnat_get_interrupt_state");
      --  Get interrupt state. Defined in a-init.c. The input argument is
      --  the interrupt number, and the result is one of the following:

      Default : constant Character := 's';
      --    'n'   this interrupt not set by any Interrupt_State pragma
      --    'u'   Interrupt_State pragma set state to User
      --    'r'   Interrupt_State pragma set state to Runtime
      --    's'   Interrupt_State pragma set state to System (use "default"
      --           system handler)

   begin
      Environment_Task_Id := Environment_Task;

      Interrupt_Management.Initialize;

      --  Initialize the lock used to synchronize chain of all ATCBs

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);

      Specific.Initialize (Environment_Task);

      --  Make environment task known here because it doesn't go through
      --  Activate_Tasks, which does it for all other tasks.

      Known_Tasks (Known_Tasks'First) := Environment_Task;
      Environment_Task.Known_Tasks_Index := Known_Tasks'First;

      Enter_Task (Environment_Task);

      --  Prepare the set of signals that should unblocked in all tasks

      Result := sigemptyset (Unblocked_Signal_Mask'Access);
      pragma Assert (Result = 0);

      for J in Interrupt_Management.Interrupt_ID loop
         if System.Interrupt_Management.Keep_Unmasked (J) then
            Result := sigaddset (Unblocked_Signal_Mask'Access, Signal (J));
            pragma Assert (Result = 0);
         end if;
      end loop;

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
             (Signal (System.Interrupt_Management.Abort_Task_Interrupt),
              act'Unchecked_Access,
              old_act'Unchecked_Access);
         pragma Assert (Result = 0);
         Abort_Handler_Installed := True;
      end if;
   end Initialize;

   -----------------------
   -- Set_Task_Affinity --
   -----------------------

   procedure Set_Task_Affinity (T : ST.Task_Id) is
      pragma Unreferenced (T);

   begin
      --  Setting task affinity is not supported by the underlying system

      null;
   end Set_Task_Affinity;

end System.Task_Primitives.Operations;
