------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--         Copyright (C) 1992-2001, Free Software Foundation, Inc.          --
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

--  This is an Irix (old athread library) version of this package

--  This package contains all the GNULL primitives that interface directly
--  with the underlying OS.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Interfaces.C;
--  used for int
--           size_t

with System.Tasking.Debug;
--  used for Known_Tasks

with System.Task_Info;

with System.Interrupt_Management;
--  used for Keep_Unmasked
--           Abort_Task_Interrupt
--           Interrupt_ID

with System.Parameters;
--  used for Size_Type

with System.Tasking;
--  used for Ada_Task_Control_Block
--           Task_ID

with System.Program_Info;
--  used for Default_Task_Stack
--           Default_Time_Slice
--           Stack_Guard_Pages
--           Pthread_Sched_Signal
--           Pthread_Arena_Size

with System.Soft_Links;
--  used for Defer/Undefer_Abort

--  Note that we do not use System.Tasking.Initialization directly since
--  this is a higher level package that we shouldn't depend on. For example
--  when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Initialization

with System.OS_Primitives;
--  used for Delay_Modes

with System.Storage_Elements;
--  used for To_Address

with Unchecked_Conversion;
with Unchecked_Deallocation;

package body System.Task_Primitives.Operations is

   use System.Tasking.Debug;
   use System.Tasking;
   use Interfaces.C;
   use System.OS_Interface;
   use System.Parameters;
   use System.OS_Primitives;

   package SSL renames System.Soft_Links;

   ------------------
   --  Local Data  --
   ------------------

   --  The followings are logically constants, but need to be initialized
   --  at run time.

   Single_RTS_Lock : aliased RTS_Lock;
   --  This is a lock to allow only one thread of control in the RTS at
   --  a time; it is used to execute in mutual exclusion from all other tasks.
   --  Used mainly in Single_Lock mode, but also to protect All_Tasks_List

   Environment_Task_ID : Task_ID;
   --  A variable to hold Task_ID for the environment task.

   Locking_Policy : Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");

   Clock_Address : constant System.Address :=
     System.Storage_Elements.To_Address (16#200F90#);

   RT_Clock_Id : clockid_t;
   for RT_Clock_Id'Address use Clock_Address;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Initialize_Athread_Library;

   function To_Task_ID is new Unchecked_Conversion (System.Address, Task_ID);

   function To_Address is new Unchecked_Conversion (Task_ID, System.Address);

   -------------------
   --  Stack_Guard  --
   -------------------

   --  The underlying thread system sets a guard page at the
   --  bottom of a thread stack, so nothing is needed.
   --  ??? Check the comment above

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

   ----------
   -- Self --
   ----------

   function Self return Task_ID is
   begin
      return To_Task_ID (pthread_get_current_ada_tcb);
   end Self;

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
      Attributes : aliased pthread_mutexattr_t;
      Result     : Interfaces.C.int;

   begin
      Result := pthread_mutexattr_init (Attributes'Access);

      if Result = FUNC_ERR then
         raise Storage_Error;
      end if;

      if Locking_Policy = 'C' then

         Result := pthread_mutexattr_setqueueorder
           (Attributes'Access, MUTEX_PRIORITY_CEILING);

         pragma Assert (Result /= FUNC_ERR);

         Result := pthread_mutexattr_setceilingprio
            (Attributes'Access, Interfaces.C.int (Prio));

         pragma Assert (Result /= FUNC_ERR);
      end if;

      Result := pthread_mutex_init (L, Attributes'Access);

      if Result = FUNC_ERR then
         Result := pthread_mutexattr_destroy (Attributes'Access);
         raise Storage_Error;
      end if;

      Result := pthread_mutexattr_destroy (Attributes'Access);
   end Initialize_Lock;

   procedure Initialize_Lock (L : access RTS_Lock; Level : Lock_Level) is
      Attributes : aliased pthread_mutexattr_t;
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutexattr_init (Attributes'Access);

      if Result = FUNC_ERR then
         raise Storage_Error;
      end if;

      if Locking_Policy = 'C' then
         Result := pthread_mutexattr_setqueueorder
           (Attributes'Access, MUTEX_PRIORITY_CEILING);
         pragma Assert (Result /= FUNC_ERR);

         Result := pthread_mutexattr_setceilingprio
            (Attributes'Access, Interfaces.C.int (System.Any_Priority'Last));
         pragma Assert (Result /= FUNC_ERR);
      end if;

      Result := pthread_mutex_init (L, Attributes'Access);

      if Result = FUNC_ERR then
         Result := pthread_mutexattr_destroy (Attributes'Access);
         raise Storage_Error;
      end if;

      Result := pthread_mutexattr_destroy (Attributes'Access);
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : access Lock) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_destroy (L);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   procedure Finalize_Lock (L : access RTS_Lock) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_destroy (L);
      pragma Assert (Result = 0);
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_lock (L);

      Ceiling_Violation := Result = FUNC_ERR and then errno = EINVAL;
      pragma Assert (Result /= FUNC_ERR);
   end Write_Lock;

   procedure Write_Lock
     (L : access RTS_Lock; Global_Lock : Boolean := False)
   is
      Result : Interfaces.C.int;
   begin
      if not Single_Lock or else Global_Lock then
         Result := pthread_mutex_lock (L);
         pragma Assert (Result = 0);
      end if;
   end Write_Lock;

   procedure Write_Lock (T : Task_ID) is
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

   procedure Read_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
   begin
      Write_Lock (L, Ceiling_Violation);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : access Lock) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_mutex_unlock (L);
      pragma Assert (Result = 0);
   end Unlock;

   procedure Unlock (L : access RTS_Lock; Global_Lock : Boolean := False) is
      Result : Interfaces.C.int;
   begin
      if not Single_Lock or else Global_Lock then
         Result := pthread_mutex_unlock (L);
         pragma Assert (Result = 0);
      end if;
   end Unlock;

   procedure Unlock (T : Task_ID) is
      Result : Interfaces.C.int;
   begin
      if not Single_Lock then
         Result := pthread_mutex_unlock (T.Common.LL.L'Access);
         pragma Assert (Result = 0);
      end if;
   end Unlock;

   -----------
   -- Sleep --
   -----------

   procedure Sleep
     (Self_ID  : ST.Task_ID;
      Reason   : System.Tasking.Task_States)
   is
      Result : Interfaces.C.int;
   begin
      if Single_Lock then
         Result := pthread_cond_wait
           (Self_ID.Common.LL.CV'Access, Single_RTS_Lock'Access);
      else
         Result := pthread_cond_wait
           (Self_ID.Common.LL.CV'Access, Self_ID.Common.LL.L'Access);
      end if;

      --  EINTR is not considered a failure.
      pragma Assert (Result = 0 or else Result = EINTR);
   end Sleep;

   -----------------
   -- Timed_Sleep --
   -----------------

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
      Request    : aliased struct_timeval;
      Result     : Interfaces.C.int;
   begin
      Timedout := True;
      Yielded := False;

      if Mode = Relative then
         Abs_Time := Duration'Min (Time, Max_Sensible_Delay) + Check_Time;
      else
         Abs_Time := Duration'Min (Check_Time + Max_Sensible_Delay, Time);
      end if;

      if Abs_Time > Check_Time then
         Request := To_Timeval (Abs_Time);

         loop
            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
              or else Self_ID.Pending_Priority_Change;

            if Single_Lock then
               Result := pthread_cond_timedwait
                 (Self_ID.Common.LL.CV'Access, Single_RTS_Lock'Access,
                  Request'Access);

            else
               Result := pthread_cond_timedwait
                 (Self_ID.Common.LL.CV'Access, Self_ID.Common.LL.L'Access,
                  Request'Access);
            end if;

            exit when Abs_Time <= Monotonic_Clock;

            if Result = 0 or Result = EINTR then
               --  somebody may have called Wakeup for us
               Timedout := False;
               exit;
            end if;

            pragma Assert (Result = ETIMEDOUT
              or else (Result = -1 and then errno = EAGAIN));
         end loop;
      end if;
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Self_ID  : Task_ID;
      Time     : Duration;
      Mode     : ST.Delay_Modes)
   is
      Check_Time : constant Duration := Monotonic_Clock;
      Abs_Time   : Duration;
      Request    : aliased struct_timeval;
      Result     : Interfaces.C.int;

   begin
      --  Only the little window between deferring abort and
      --  locking Self_ID is the reason we need to
      --  check for pending abort and priority change below! :(

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
         Request := To_Timeval (Abs_Time);
         Self_ID.Common.State := Delay_Sleep;

         loop
            if Self_ID.Pending_Priority_Change then
               Self_ID.Pending_Priority_Change := False;
               Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
               Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
            end if;

            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            if Single_Lock then
               Result := pthread_cond_timedwait (Self_ID.Common.LL.CV'Access,
                 Single_RTS_Lock'Access, Request'Access);
            else
               Result := pthread_cond_timedwait (Self_ID.Common.LL.CV'Access,
                 Self_ID.Common.LL.L'Access, Request'Access);
            end if;

            exit when Abs_Time <= Monotonic_Clock;

            pragma Assert (Result = 0 or else
              Result = ETIMEDOUT or else
              (Result = -1 and then errno = EAGAIN) or else
              Result = EINTR);
         end loop;

         Self_ID.Common.State := Runnable;
      end if;

      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      pthread_yield;
      SSL.Abort_Undefer.all;
   end Timed_Delay;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration is
      type timeval is record
         tv_sec  : Integer;
         tv_usec : Integer;
      end record;
      pragma Convention (C, timeval);

      tv : aliased timeval;

      procedure gettimeofday (tp : access timeval);
      pragma Import (C, gettimeofday, "gettimeofday", "gettimeofday");

   begin
      gettimeofday (tv'Access);
      return Duration (tv.tv_sec) + Duration (tv.tv_usec) / 1_000_000.0;
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

   procedure Wakeup
     (T : ST.Task_ID;
      Reason : System.Tasking.Task_States)
   is
      Result : Interfaces.C.int;
   begin
      Result := pthread_cond_signal (T.Common.LL.CV'Access);
      pragma Assert (Result = 0);
   end Wakeup;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
   begin
      if Do_Yield then
         pthread_yield;
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
      Result : Interfaces.C.int;
   begin
      T.Common.Current_Priority := Prio;
      Result := pthread_setprio (T.Common.LL.Thread, Interfaces.C.int (Prio));
      pragma Assert (Result /= FUNC_ERR);

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
      Result : Interfaces.C.int;
   begin
      Self_ID.Common.LL.Thread := pthread_self;
      Self_ID.Common.LL.LWP := sproc_self;

      Result :=
        pthread_set_ada_tcb (Self_ID.Common.LL.Thread, To_Address (Self_ID));

      pragma Assert (Result = 0);

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

   function New_ATCB (Entry_Num : Task_Entry_Index) return Task_ID is
   begin
      return new Ada_Task_Control_Block (Entry_Num);
   end New_ATCB;

   ----------------------
   --  Initialize_TCB  --
   ----------------------

   procedure Initialize_TCB (Self_ID : Task_ID; Succeeded : out Boolean) is
      Result    : Interfaces.C.int;
      Cond_Attr : aliased pthread_condattr_t;

   begin
      if not Single_Lock then
         Initialize_Lock (Self_ID.Common.LL.L'Access, ATCB_Level);
      end if;

      Result := pthread_condattr_init (Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = 0 then
         Result := pthread_cond_init (Self_ID.Common.LL.CV'Access,
           Cond_Attr'Access);
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
     (T          : Task_ID;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean)
   is
      Attributes          : aliased pthread_attr_t;
      Adjusted_Stack_Size : Interfaces.C.size_t;
      Result              : Interfaces.C.int;

      function Thread_Body_Access is new
        Unchecked_Conversion (System.Address, start_addr);

      function To_Resource_T is new Unchecked_Conversion
        (System.Task_Info.Resource_Vector_T, System.OS_Interface.resource_t);

      use System.Task_Info;

   begin
      if Stack_Size = Unspecified_Size then
         Adjusted_Stack_Size :=
           Interfaces.C.size_t (System.Program_Info.Default_Task_Stack);

      elsif Stack_Size < Minimum_Stack_Size then
         Adjusted_Stack_Size := Interfaces.C.size_t (Minimum_Stack_Size);

      else
         Adjusted_Stack_Size := Interfaces.C.size_t (Stack_Size);
      end if;

      Result := pthread_attr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Succeeded := False;
         return;
      end if;

      Result := pthread_attr_setdetachstate (Attributes'Access, 1);
      pragma Assert (Result = 0);

      Result := pthread_attr_setstacksize
        (Attributes'Access, Adjusted_Stack_Size);
      pragma Assert (Result = 0);

      if T.Common.Task_Info /= null then
         Result := pthread_attr_setresources
           (Attributes'Access,
            To_Resource_T (T.Common.Task_Info.Thread_Resources));
         pragma Assert (Result /= FUNC_ERR);

         if T.Common.Task_Info.Thread_Timeslice /= 0.0 then
            declare
               use System.OS_Interface;

               Tv : aliased struct_timeval := To_Timeval
                 (T.Common.Task_Info.Thread_Timeslice);
            begin
               Result := pthread_attr_set_tslice
                 (Attributes'Access, Tv'Access);
            end;
         end if;

         if T.Common.Task_Info.Bound_To_Sproc then
            Result := pthread_attr_set_boundtosproc
              (Attributes'Access, PTHREAD_BOUND);
            Result := pthread_attr_set_bsproc
              (Attributes'Access, T.Common.Task_Info.Sproc);
         end if;

      end if;

      --  Since the initial signal mask of a thread is inherited from the
      --  creator, and the Environment task has all its signals masked, we
      --  do not need to manipulate caller's signal mask at this point.
      --  All tasks in RTS will have All_Tasks_Mask initially.

      Result := pthread_create
        (T.Common.LL.Thread'Access,
         Attributes'Access,
         Thread_Body_Access (Wrapper),
         To_Address (T));
      pragma Assert (Result = 0 or else Result = EAGAIN);

      Succeeded := Result = 0;

      Set_Priority (T, Priority);

      Result := pthread_attr_destroy (Attributes'Access);
      pragma Assert (Result /= FUNC_ERR);
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_ID) is
      procedure Free is new
        Unchecked_Deallocation (Ada_Task_Control_Block, Task_ID);

      Result : Interfaces.C.int;
      Tmp    : Task_ID := T;

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

      Free (Tmp);
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   procedure Exit_Task is
   begin
      pthread_exit (System.Null_Address);
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_ID) is
      Result : Interfaces.C.int;
   begin
      Result := pthread_kill (T.Common.LL.Thread,
        Interfaces.C.int (System.Interrupt_Management.Abort_Task_Interrupt));
      pragma Assert (Result = 0);
   end Abort_Task;

   ----------------
   -- Check_Exit --
   ----------------

   --  Dummy versions.  The only currently working versions is for solaris
   --  (native).

   function Check_Exit (Self_ID : ST.Task_ID) return Boolean is
   begin
      return True;
   end Check_Exit;

   --------------------
   -- Check_No_Locks --
   --------------------

   function Check_No_Locks (Self_ID : ST.Task_ID) return Boolean is
   begin
      return True;
   end Check_No_Locks;

   ----------------------
   -- Environment_Task --
   ----------------------

   function Environment_Task return Task_ID is
   begin
      return Environment_Task_ID;
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
     (T           : ST.Task_ID;
      Thread_Self : Thread_Id) return Boolean is
   begin
      if T.Common.LL.Thread /= Thread_Self then
         return pthread_suspend (T.Common.LL.Thread) = 0;
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
         return pthread_resume (T.Common.LL.Thread) = 0;
      else
         return True;
      end if;
   end Resume_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_ID) is
   begin
      Environment_Task_ID := Environment_Task;

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);
      --  Initialize the lock used to synchronize chain of all ATCBs.

      Enter_Task (Environment_Task);

      Set_Priority (Environment_Task,
        Environment_Task.Common.Current_Priority);
   end Initialize;

   procedure Initialize_Athread_Library is
      Result : Interfaces.C.int;
      Init   : aliased pthread_init_struct;

      package PINF renames System.Program_Info;
      package C    renames Interfaces.C;

   begin
      Init.conf_initsize       := C.int (PINF.Pthread_Arena_Size);
      Init.max_sproc_count     := C.int (PINF.Max_Sproc_Count);
      Init.sproc_stack_size    := C.size_t (PINF.Sproc_Stack_Size);
      Init.os_default_priority := C.int (PINF.Os_Default_Priority);
      Init.os_sched_signal     := C.int (PINF.Pthread_Sched_Signal);
      Init.guard_pages         := C.int (PINF.Stack_Guard_Pages);
      Init.init_sproc_count    := C.int (PINF.Initial_Sproc_Count);

      Result := pthread_exec_begin (Init'Access);
      pragma Assert (Result /= FUNC_ERR);

      if Result = FUNC_ERR then
         raise Storage_Error;               --  Insufficient resources.
      end if;

   end Initialize_Athread_Library;

begin
   Initialize_Athread_Library;
end System.Task_Primitives.Operations;
