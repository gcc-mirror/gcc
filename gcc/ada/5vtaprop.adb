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
--             Copyright (C) 1991-2001, Florida State University            --
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

--  This is a OpenVMS/Alpha version of this package

--  This package contains all the GNULL primitives that interface directly
--  with the underlying OS.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Tasking.Debug;
--  used for Known_Tasks

with Interfaces.C;
--  used for int
--           size_t

with System.Parameters;
--  used for Size_Type

with System.Tasking;
--  used for Ada_Task_Control_Block
--           Task_ID

with System.Soft_Links;
--  used for Defer/Undefer_Abort
--           Set_Exc_Stack_Addr

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
   use System.OS_Primitives;
   use type System.OS_Primitives.OS_Time;

   package SSL renames System.Soft_Links;

   ------------------
   --  Local Data  --
   ------------------

   --  The followings are logically constants, but need to be initialized
   --  at run time.

   ATCB_Key : aliased pthread_key_t;
   --  Key used to find the Ada Task_ID associated with a thread

   All_Tasks_L : aliased System.Task_Primitives.RTS_Lock;
   --  See comments on locking rules in System.Tasking (spec).

   Environment_Task_ID : Task_ID;
   --  A variable to hold Task_ID for the environment task.

   Time_Slice_Val : Integer;
   pragma Import (C, Time_Slice_Val, "__gl_time_slice_val");

   Dispatching_Policy : Character;
   pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");

   FIFO_Within_Priorities : constant Boolean := Dispatching_Policy = 'F';
   --  Indicates whether FIFO_Within_Priorities is set.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_Task_ID is new Unchecked_Conversion (System.Address, Task_ID);

   function To_Address is new Unchecked_Conversion (Task_ID, System.Address);

   procedure Timer_Sleep_AST (ID : Address);
   --  Signal the condition variable when AST fires.

   procedure Timer_Sleep_AST (ID : Address) is
      Result     : Interfaces.C.int;
      Self_ID    : Task_ID := To_Task_ID (ID);

   begin
      Self_ID.Common.LL.AST_Pending := False;
      Result := pthread_cond_signal_int_np (Self_ID.Common.LL.CV'Access);
   end Timer_Sleep_AST;

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
      Result : System.Address;

   begin
      Result := pthread_getspecific (ATCB_Key);
      pragma Assert (Result /= System.Null_Address);
      return To_Task_ID (Result);
   end Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   --  Note: mutexes and cond_variables needed per-task basis are
   --        initialized in Initialize_TCB and the Storage_Error is
   --        handled. Other mutexes (such as All_Tasks_Lock, Memory_Lock...)
   --        used in RTS is initialized before any status change of RTS.
   --        Therefore rasing Storage_Error in the following routines
   --        should be able to be handled safely.

   procedure Initialize_Lock (Prio : System.Any_Priority; L : access Lock) is
      Attributes : aliased pthread_mutexattr_t;
      Result     : Interfaces.C.int;

   begin
      Result := pthread_mutexattr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         raise Storage_Error;
      end if;

      L.Prio_Save := 0;
      L.Prio := Interfaces.C.int (Prio);

      Result := pthread_mutex_init (L.L'Access, Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         raise Storage_Error;
      end if;

      Result := pthread_mutexattr_destroy (Attributes'Access);
      pragma Assert (Result = 0);
   end Initialize_Lock;

   procedure Initialize_Lock (L : access RTS_Lock; Level : Lock_Level) is
      Attributes : aliased pthread_mutexattr_t;
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutexattr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         raise Storage_Error;
      end if;

--      Don't use, see comment in s-osinte.ads about ERRORCHECK mutexes.
--      Result := pthread_mutexattr_settype_np
--        (Attributes'Access, PTHREAD_MUTEX_ERRORCHECK_NP);
--      pragma Assert (Result = 0);

--      Result := pthread_mutexattr_setprotocol
--        (Attributes'Access, PTHREAD_PRIO_PROTECT);
--      pragma Assert (Result = 0);

--      Result := pthread_mutexattr_setprioceiling
--         (Attributes'Access, Interfaces.C.int (System.Any_Priority'Last));
--      pragma Assert (Result = 0);

      Result := pthread_mutex_init (L, Attributes'Access);

      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = ENOMEM then
         raise Storage_Error;
      end if;

      Result := pthread_mutexattr_destroy (Attributes'Access);
      pragma Assert (Result = 0);
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : access Lock) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_destroy (L.L'Access);
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
      Self_ID        : constant Task_ID := Self;
      All_Tasks_Link : constant Task_ID := Self.Common.All_Tasks_Link;
      Current_Prio   : System.Any_Priority;
      Result         : Interfaces.C.int;

   begin
      Current_Prio := Get_Priority (Self_ID);

      --  If there is no other tasks, no need to check priorities.

      if All_Tasks_Link /= Null_Task
        and then L.Prio < Interfaces.C.int (Current_Prio)
      then
         Ceiling_Violation := True;
         return;
      end if;

      Result := pthread_mutex_lock (L.L'Access);
      pragma Assert (Result = 0);

      Ceiling_Violation := False;
--  Why is this commented out ???
--      L.Prio_Save := Interfaces.C.int (Current_Prio);
--      Set_Priority (Self_ID, System.Any_Priority (L.Prio));
   end Write_Lock;

   procedure Write_Lock (L : access RTS_Lock) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_lock (L);
      pragma Assert (Result = 0);
   end Write_Lock;

   procedure Write_Lock (T : Task_ID) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_lock (T.Common.LL.L'Access);
      pragma Assert (Result = 0);
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
      Result := pthread_mutex_unlock (L.L'Access);
      pragma Assert (Result = 0);
   end Unlock;

   procedure Unlock (L : access RTS_Lock) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_unlock (L);
      pragma Assert (Result = 0);
   end Unlock;

   procedure Unlock (T : Task_ID) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_mutex_unlock (T.Common.LL.L'Access);
      pragma Assert (Result = 0);
   end Unlock;

   -------------
   --  Sleep  --
   -------------

   procedure Sleep (Self_ID : Task_ID;
                    Reason   : System.Tasking.Task_States) is
      Result : Interfaces.C.int;

   begin
      pragma Assert (Self_ID = Self);
      Result := pthread_cond_wait (Self_ID.Common.LL.CV'Access,
        Self_ID.Common.LL.L'Access);
      --  EINTR is not considered a failure.
      pragma Assert (Result = 0 or else Result = EINTR);

      if Self_ID.Deferral_Level = 0
        and then Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
      then
         Unlock (Self_ID);
         raise Standard'Abort_Signal;
      end if;
   end Sleep;

   -----------------
   -- Timed_Sleep --
   -----------------

   --  This is for use within the run-time system, so abort is
   --  assumed to be already deferred, and the caller should be
   --  holding its own ATCB lock.

   procedure Timed_Sleep
     (Self_ID  : Task_ID;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
      Sleep_Time : OS_Time;
      Result     : Interfaces.C.int;
      Status     : Cond_Value_Type;

   begin
      Timedout := False;
      Yielded := False;

      Sleep_Time := To_OS_Time (Time, Mode);

      if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
           or else Self_ID.Pending_Priority_Change
      then
         return;
      end if;

      Self_ID.Common.LL.AST_Pending := True;

      Sys_Setimr
       (Status, 0, Sleep_Time,
        Timer_Sleep_AST'Access, To_Address (Self_ID), 0);

      if (Status and 1) /= 1 then
         raise Storage_Error;
      end if;

      Result := pthread_cond_wait (Self_ID.Common.LL.CV'Access,
        Self_ID.Common.LL.L'Access);

      if not Self_ID.Common.LL.AST_Pending then
         Timedout := True;
      else
         Sys_Cantim (Status, To_Address (Self_ID), 0);
         pragma Assert ((Status and 1) = 1);
      end if;

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
      Sleep_Time : OS_Time;
      Result     : Interfaces.C.int;
      Status     : Cond_Value_Type;

   begin

      --  Only the little window between deferring abort and
      --  locking Self_ID is the reason we need to
      --  check for pending abort and priority change below! :(

      SSL.Abort_Defer.all;
      Write_Lock (Self_ID);

      if not (Time = 0.0 and then Mode = Relative) then

         Sleep_Time := To_OS_Time (Time, Mode);

         if Mode = Relative or else OS_Clock < Sleep_Time then

            Self_ID.Common.State := Delay_Sleep;
            Self_ID.Common.LL.AST_Pending := True;

            Sys_Setimr
             (Status, 0, Sleep_Time,
              Timer_Sleep_AST'Access, To_Address (Self_ID), 0);

            if (Status and 1) /= 1 then
               raise Storage_Error;
            end if;

            loop
               if Self_ID.Pending_Priority_Change then
                  Self_ID.Pending_Priority_Change := False;
                  Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
                  Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
               end if;

               if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level then
                  Sys_Cantim (Status, To_Address (Self_ID), 0);
                  pragma Assert ((Status and 1) = 1);
                  exit;
               end if;

               Result := pthread_cond_wait (Self_ID.Common.LL.CV'Access,
                 Self_ID.Common.LL.L'Access);

               exit when not Self_ID.Common.LL.AST_Pending;

            end loop;

            Self_ID.Common.State := Runnable;

         end if;
      end if;

      Unlock (Self_ID);
      Result := sched_yield;
      SSL.Abort_Undefer.all;
   end Timed_Delay;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration
     renames System.OS_Primitives.Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
   begin
      return 10#1.0#E-3;
   end RT_Resolution;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_ID; Reason : System.Tasking.Task_States) is
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

   begin
      if Do_Yield then
         Result := sched_yield;
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
      Result     : Interfaces.C.int;
      Param      : aliased struct_sched_param;
   begin
      T.Common.Current_Priority := Prio;
      Param.sched_priority  := Interfaces.C.int (Underlying_Priorities (Prio));

      if Time_Slice_Val > 0 then
         Result := pthread_setschedparam
           (T.Common.LL.Thread, SCHED_RR, Param'Access);

      elsif FIFO_Within_Priorities or else Time_Slice_Val = 0 then
         Result := pthread_setschedparam
           (T.Common.LL.Thread, SCHED_FIFO, Param'Access);

      else
         Result := pthread_setschedparam
           (T.Common.LL.Thread, SCHED_OTHER, Param'Access);
      end if;

      pragma Assert (Result = 0);
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
      Result  : Interfaces.C.int;

   begin
      Self_ID.Common.LL.Thread := pthread_self;

      --  It is not safe for the new task accept signals until it
      --  has bound its TCB pointer to the thread with pthread_setspecific (),
      --  since the handler wrappers use the TCB pointer
      --  to restore the stack limit.

      Result := pthread_setspecific (ATCB_Key, To_Address (Self_ID));
      pragma Assert (Result = 0);

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
      Mutex_Attr   : aliased pthread_mutexattr_t;
      Result       : Interfaces.C.int;
      Cond_Attr    : aliased pthread_condattr_t;

   begin
      Result := pthread_mutexattr_init (Mutex_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Succeeded := False;
         return;
      end if;

--      Don't use, see comment in s-osinte.ads about ERRORCHECK mutexes.
--      Result := pthread_mutexattr_settype_np
--        (Mutex_Attr'Access, PTHREAD_MUTEX_ERRORCHECK_NP);
--      pragma Assert (Result = 0);

--      Result := pthread_mutexattr_setprotocol
--        (Mutex_Attr'Access, PTHREAD_PRIO_PROTECT);
--      pragma Assert (Result = 0);

--      Result := pthread_mutexattr_setprioceiling
--        (Mutex_Attr'Access, Interfaces.C.int (System.Any_Priority'Last));
--      pragma Assert (Result = 0);

      Result := pthread_mutex_init (Self_ID.Common.LL.L'Access,
        Mutex_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Succeeded := False;
         return;
      end if;

      Result := pthread_mutexattr_destroy (Mutex_Attr'Access);
      pragma Assert (Result = 0);

      Result := pthread_condattr_init (Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Result := pthread_mutex_destroy (Self_ID.Common.LL.L'Access);
         pragma Assert (Result = 0);
         Succeeded := False;
         return;
      end if;

      Result := pthread_cond_init (Self_ID.Common.LL.CV'Access,
        Cond_Attr'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result = 0 then
         Succeeded := True;
         Self_ID.Common.LL.Exc_Stack_Ptr := new Exc_Stack_T;
         SSL.Set_Exc_Stack_Addr
           (To_Address (Self_ID),
            Self_ID.Common.LL.Exc_Stack_Ptr (Exc_Stack_T'Last)'Address);

      else
         Result := pthread_mutex_destroy (Self_ID.Common.LL.L'Access);
         pragma Assert (Result = 0);
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
        Unchecked_Conversion (System.Address, Thread_Body);

   begin
      if Stack_Size = Unspecified_Size then
         Adjusted_Stack_Size := Interfaces.C.size_t (Default_Stack_Size);

      elsif Stack_Size < Minimum_Stack_Size then
         Adjusted_Stack_Size := Interfaces.C.size_t (Minimum_Stack_Size);

      else
         Adjusted_Stack_Size := Interfaces.C.size_t (Stack_Size);
      end if;

      --  Since the initial signal mask of a thread is inherited from the
      --  creator, we need to set our local signal mask mask all signals
      --  during the creation operation, to make sure the new thread is
      --  not disturbed by signals before it has set its own Task_ID.

      Result := pthread_attr_init (Attributes'Access);
      pragma Assert (Result = 0 or else Result = ENOMEM);

      if Result /= 0 then
         Succeeded := False;
         return;
      end if;

      Result := pthread_attr_setdetachstate
        (Attributes'Access, PTHREAD_CREATE_DETACHED);
      pragma Assert (Result = 0);

      Result := pthread_attr_setstacksize
        (Attributes'Access, Adjusted_Stack_Size);
      pragma Assert (Result = 0);

      --  This call may be unnecessary, not sure. ???

      Result := pthread_attr_setinheritsched
        (Attributes'Access, PTHREAD_EXPLICIT_SCHED);
      pragma Assert (Result = 0);

      Result := pthread_create
        (T.Common.LL.Thread'Access,
         Attributes'Access,
         Thread_Body_Access (Wrapper),
         To_Address (T));

      --  ENOMEM is a valid run-time error.  Don't shut down.

      pragma Assert (Result = 0
        or else Result = EAGAIN or else Result = ENOMEM);

      Succeeded := Result = 0;

      Result := pthread_attr_destroy (Attributes'Access);
      pragma Assert (Result = 0);

      if Succeeded then
         Set_Priority (T, Priority);
      end if;
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_ID) is
      Result : Interfaces.C.int;
      Tmp    : Task_ID := T;

      procedure Free is new
        Unchecked_Deallocation (Ada_Task_Control_Block, Task_ID);

      procedure Free is new Unchecked_Deallocation
       (Exc_Stack_T, Exc_Stack_Ptr_T);

   begin
      Result := pthread_mutex_destroy (T.Common.LL.L'Access);
      pragma Assert (Result = 0);
      Result := pthread_cond_destroy (T.Common.LL.CV'Access);
      pragma Assert (Result = 0);
      if T.Known_Tasks_Index /= -1 then
         Known_Tasks (T.Known_Tasks_Index) := null;
      end if;
      Free (T.Common.LL.Exc_Stack_Ptr);
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

   begin

      --  Why is this commented out ???
--      if T = Self and then T.Deferral_Level = 0
--           and then T.Pending_ATC_Level < T.ATC_Nesting_Level
--      then
--         raise Standard'Abort_Signal;
--      end if;

      --
      --  Interrupt Server_Tasks may be waiting on an event flag
      --
      if T.Common.State = Interrupt_Server_Blocked_On_Event_Flag then
         Wakeup (T, Interrupt_Server_Blocked_On_Event_Flag);
      end if;

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
      return False;
   end Suspend_Task;

   -----------------
   -- Resume_Task --
   -----------------

   function Resume_Task
     (T           : ST.Task_ID;
      Thread_Self : Thread_Id) return Boolean is
   begin
      return False;
   end Resume_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_ID) is
   begin
      Environment_Task_ID := Environment_Task;

      Initialize_Lock (All_Tasks_L'Access, All_Tasks_Level);
      --  Initialize the lock used to synchronize chain of all ATCBs.

      Enter_Task (Environment_Task);
   end Initialize;

begin
   declare
      Result   : Interfaces.C.int;
   begin
      Result := pthread_key_create (ATCB_Key'Access, null);
      pragma Assert (Result = 0);
   end;
end System.Task_Primitives.Operations;
