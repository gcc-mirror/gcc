------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--         Copyright (C) 1992-2002, Free Software Foundation, Inc.          --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a NT (native) version of this package.

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

with Interfaces.C.Strings;
--  used for Null_Ptr

with System.OS_Interface;
--  used for various type, constant, and operations

with System.Parameters;
--  used for Size_Type

with System.Tasking;
--  used for Ada_Task_Control_Block
--           Task_ID

with System.Soft_Links;
--  used for Defer/Undefer_Abort
--       to initialize TSD for a C thread, in function Self

--  Note that we do not use System.Tasking.Initialization directly since
--  this is a higher level package that we shouldn't depend on. For example
--  when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Initialization

with System.OS_Primitives;
--  used for Delay_Modes

with System.Task_Info;
--  used for Unspecified_Task_Info

with Unchecked_Conversion;
with Unchecked_Deallocation;

package body System.Task_Primitives.Operations is

   use System.Tasking.Debug;
   use System.Tasking;
   use Interfaces.C;
   use Interfaces.C.Strings;
   use System.OS_Interface;
   use System.Parameters;
   use System.OS_Primitives;

   pragma Link_With ("-Xlinker --stack=0x800000,0x1000");
   --  Change the stack size (8 MB) for tasking programs on Windows. This
   --  permit to have more than 30 tasks running at the same time. Note that
   --  we set the stack size for non tasking programs on System unit.

   package SSL renames System.Soft_Links;

   ------------------
   --  Local Data  --
   ------------------

   Environment_Task_ID : Task_ID;
   --  A variable to hold Task_ID for the environment task.

   Single_RTS_Lock : aliased RTS_Lock;
   --  This is a lock to allow only one thread of control in the RTS at
   --  a time; it is used to execute in mutual exclusion from all other tasks.
   --  Used mainly in Single_Lock mode, but also to protect All_Tasks_List

   Time_Slice_Val : Integer;
   pragma Import (C, Time_Slice_Val, "__gl_time_slice_val");

   Dispatching_Policy : Character;
   pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");

   FIFO_Within_Priorities : constant Boolean := Dispatching_Policy = 'F';
   --  Indicates whether FIFO_Within_Priorities is set.

   ---------------------------------
   --  Foreign Threads Detection  --
   ---------------------------------

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
   --  The list is protected by Single_RTS_Lock;
   --  Nodes are added to this list from the front.
   --  Once a node is added to this list, it is never removed.

   Fake_Task_Elaborated : aliased Boolean := True;
   --  Used to identified fake tasks (i.e., non-Ada Threads).

   Next_Fake_ATCB : Fake_ATCB_Ptr;
   --  Used to allocate one Fake_ATCB in advance. See comment in New_Fake_ATCB

   ---------------------------------
   --  Support for New_Fake_ATCB  --
   ---------------------------------

   function New_Fake_ATCB return Task_ID;
   --  Allocate and Initialize a new ATCB. This code can safely be called from
   --  a foreign thread, as it doesn't access implicitly or explicitly
   --  "self" before having initialized the new ATCB.

   ------------------------------------
   -- The thread local storage index --
   ------------------------------------

   TlsIndex : DWORD;
   pragma Export (Ada, TlsIndex);
   --  To ensure that this variable won't be local to this package, since
   --  in some cases, inlining forces this variable to be global anyway.

   ----------------------------------
   -- Utility Conversion Functions --
   ----------------------------------

   function To_Task_Id is new Unchecked_Conversion (System.Address, Task_ID);

   function To_Address is new Unchecked_Conversion (Task_ID, System.Address);

   -------------------
   -- New_Fake_ATCB --
   -------------------

   function New_Fake_ATCB return Task_ID is
      Self_ID   : Task_ID;
      P, Q      : Fake_ATCB_Ptr;
      Succeeded : Boolean;
      Res       : BOOL;

   begin
      --  This section is ticklish.
      --  We dare not call anything that might require an ATCB, until
      --  we have the new ATCB in place.

      Lock_RTS;
      Q := null;
      P := Fake_ATCB_List;

      while P /= null loop
         if P.Stack_Base = 0 then
            Q := P;
         end if;

         P := P.Next;
      end loop;

      if Q = null then

         --  Create a new ATCB with zero entries.

         Self_ID := Next_Fake_ATCB.Real_ATCB'Access;
         Next_Fake_ATCB.Stack_Base := 1;
         Next_Fake_ATCB.Next := Fake_ATCB_List;
         Fake_ATCB_List := Next_Fake_ATCB;
         Next_Fake_ATCB := null;

      else
         --  Reuse an existing fake ATCB.

         Self_ID := Q.Real_ATCB'Access;
         Q.Stack_Base := 1;
      end if;

      --  Record this as the Task_ID for the current thread.

      Self_ID.Common.LL.Thread := GetCurrentThread;

      Res := TlsSetValue (TlsIndex, To_Address (Self_ID));
      pragma Assert (Res = True);

      --  Do the standard initializations

      System.Tasking.Initialize_ATCB
        (Self_ID, null, Null_Address, Null_Task, Fake_Task_Elaborated'Access,
         System.Priority'First, Task_Info.Unspecified_Task_Info, 0, Self_ID,
         Succeeded);
      pragma Assert (Succeeded);

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

      Unlock_RTS;
      return Self_ID;
   end New_Fake_ATCB;

   ----------------------------------
   -- Condition Variable Functions --
   ----------------------------------

   procedure Initialize_Cond (Cond : access Condition_Variable);
   --  Initialize given condition variable Cond

   procedure Finalize_Cond (Cond : access Condition_Variable);
   --  Finalize given condition variable Cond.

   procedure Cond_Signal (Cond : access Condition_Variable);
   --  Signal condition variable Cond

   procedure Cond_Wait
     (Cond : access Condition_Variable;
      L    : access RTS_Lock);
   --  Wait on conditional variable Cond, using lock L

   procedure Cond_Timed_Wait
     (Cond      : access Condition_Variable;
      L         : access RTS_Lock;
      Rel_Time  : Duration;
      Timed_Out : out Boolean;
      Status    : out Integer);
   --  Do timed wait on condition variable Cond using lock L. The duration
   --  of the timed wait is given by Rel_Time. When the condition is
   --  signalled, Timed_Out shows whether or not a time out occurred.
   --  Status shows whether Cond_Timed_Wait completed successfully.

   ---------------------
   -- Initialize_Cond --
   ---------------------

   procedure Initialize_Cond (Cond : access Condition_Variable) is
      hEvent : HANDLE;

   begin
      hEvent := CreateEvent (null, True, False, Null_Ptr);
      pragma Assert (hEvent /= 0);
      Cond.all := Condition_Variable (hEvent);
   end Initialize_Cond;

   -------------------
   -- Finalize_Cond --
   -------------------

   --  No such problem here, DosCloseEventSem has been derived.
   --  What does such refer to in above comment???

   procedure Finalize_Cond (Cond : access Condition_Variable) is
      Result : BOOL;

   begin
      Result := CloseHandle (HANDLE (Cond.all));
      pragma Assert (Result = True);
   end Finalize_Cond;

   -----------------
   -- Cond_Signal --
   -----------------

   procedure Cond_Signal (Cond : access Condition_Variable) is
      Result : BOOL;

   begin
      Result := SetEvent (HANDLE (Cond.all));
      pragma Assert (Result = True);
   end Cond_Signal;

   ---------------
   -- Cond_Wait --
   ---------------

   --  Pre-assertion: Cond is posted
   --                 L is locked.

   --  Post-assertion: Cond is posted
   --                  L is locked.

   procedure Cond_Wait
     (Cond : access Condition_Variable;
      L    : access RTS_Lock)
   is
      Result      : DWORD;
      Result_Bool : BOOL;

   begin
      --  Must reset Cond BEFORE L is unlocked.

      Result_Bool := ResetEvent (HANDLE (Cond.all));
      pragma Assert (Result_Bool = True);
      Unlock (L);

      --  No problem if we are interrupted here: if the condition is signaled,
      --  WaitForSingleObject will simply not block

      Result := WaitForSingleObject (HANDLE (Cond.all), Wait_Infinite);
      pragma Assert (Result = 0);

      Write_Lock (L);
   end Cond_Wait;

   ---------------------
   -- Cond_Timed_Wait --
   ---------------------

   --  Pre-assertion: Cond is posted
   --                 L is locked.

   --  Post-assertion: Cond is posted
   --                  L is locked.

   procedure Cond_Timed_Wait
     (Cond      : access Condition_Variable;
      L         : access RTS_Lock;
      Rel_Time  : Duration;
      Timed_Out : out Boolean;
      Status    : out Integer)
   is
      Time_Out : DWORD;
      Result   : BOOL;

      Int_Rel_Time : DWORD;
      Wait_Result  : DWORD;

   begin
      --  Must reset Cond BEFORE L is unlocked.

      Result := ResetEvent (HANDLE (Cond.all));
      pragma Assert (Result = True);
      Unlock (L);

      --  No problem if we are interrupted here: if the condition is signaled,
      --  WaitForSingleObject will simply not block

      if Rel_Time <= 0.0 then
         Timed_Out := True;
      else
         Int_Rel_Time := DWORD (Rel_Time);
         Time_Out := Int_Rel_Time * 1000 +
                     DWORD ((Rel_Time - Duration (Int_Rel_Time)) * 1000.0);
         Wait_Result := WaitForSingleObject (HANDLE (Cond.all), Time_Out);

         if Wait_Result = WAIT_TIMEOUT then
            Timed_Out := True;
            Wait_Result := 0;
         else
            Timed_Out := False;
         end if;
      end if;

      Write_Lock (L);

      --  Ensure post-condition

      if Timed_Out then
         Result := SetEvent (HANDLE (Cond.all));
         pragma Assert (Result = True);
      end if;

      Status := Integer (Wait_Result);
   end Cond_Timed_Wait;

   ------------------
   -- Stack_Guard  --
   ------------------

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
      Self_Id : Task_ID;

   begin
      Self_Id := To_Task_Id (TlsGetValue (TlsIndex));

      if Self_Id = null then
         return New_Fake_ATCB;
      end if;

      return Self_Id;
   end Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   --  Note: mutexes and cond_variables needed per-task basis are
   --  initialized in Initialize_TCB and the Storage_Error is handled.
   --  Other mutexes (such as RTS_Lock, Memory_Lock...) used in
   --  the RTS is initialized before any status change of RTS.
   --  Therefore raising Storage_Error in the following routines
   --  should be able to be handled safely.

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : access Lock) is
   begin
      InitializeCriticalSection (L.Mutex'Access);
      L.Owner_Priority := 0;
      L.Priority := Prio;
   end Initialize_Lock;

   procedure Initialize_Lock (L : access RTS_Lock; Level : Lock_Level) is
   begin
      InitializeCriticalSection (CRITICAL_SECTION (L.all)'Unrestricted_Access);
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : access Lock) is
   begin
      DeleteCriticalSection (L.Mutex'Access);
   end Finalize_Lock;

   procedure Finalize_Lock (L : access RTS_Lock) is
   begin
      DeleteCriticalSection (CRITICAL_SECTION (L.all)'Unrestricted_Access);
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
   begin
      L.Owner_Priority := Get_Priority (Self);

      if L.Priority < L.Owner_Priority then
         Ceiling_Violation := True;
         return;
      end if;

      EnterCriticalSection (L.Mutex'Access);

      Ceiling_Violation := False;
   end Write_Lock;

   procedure Write_Lock
     (L : access RTS_Lock; Global_Lock : Boolean := False) is
   begin
      if not Single_Lock or else Global_Lock then
         EnterCriticalSection (CRITICAL_SECTION (L.all)'Unrestricted_Access);
      end if;
   end Write_Lock;

   procedure Write_Lock (T : Task_ID) is
   begin
      if not Single_Lock then
         EnterCriticalSection
           (CRITICAL_SECTION (T.Common.LL.L)'Unrestricted_Access);
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
   begin
      LeaveCriticalSection (L.Mutex'Access);
   end Unlock;

   procedure Unlock (L : access RTS_Lock; Global_Lock : Boolean := False) is
   begin
      if not Single_Lock or else Global_Lock then
         LeaveCriticalSection (CRITICAL_SECTION (L.all)'Unrestricted_Access);
      end if;
   end Unlock;

   procedure Unlock (T : Task_ID) is
   begin
      if not Single_Lock then
         LeaveCriticalSection
           (CRITICAL_SECTION (T.Common.LL.L)'Unrestricted_Access);
      end if;
   end Unlock;

   -----------
   -- Sleep --
   -----------

   procedure Sleep
     (Self_ID : Task_ID;
      Reason  : System.Tasking.Task_States) is
   begin
      pragma Assert (Self_ID = Self);

      if Single_Lock then
         Cond_Wait (Self_ID.Common.LL.CV'Access, Single_RTS_Lock'Access);
      else
         Cond_Wait (Self_ID.Common.LL.CV'Access, Self_ID.Common.LL.L'Access);
      end if;

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
      Check_Time : constant Duration := Monotonic_Clock;
      Rel_Time   : Duration;
      Abs_Time   : Duration;
      Result     : Integer;

      Local_Timedout : Boolean;

   begin
      Timedout := True;
      Yielded  := False;

      if Mode = Relative then
         Rel_Time := Time;
         Abs_Time := Duration'Min (Time, Max_Sensible_Delay) + Check_Time;
      else
         Rel_Time := Time - Check_Time;
         Abs_Time := Time;
      end if;

      if Rel_Time > 0.0 then
         loop
            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
              or else Self_ID.Pending_Priority_Change;

            if Single_Lock then
               Cond_Timed_Wait (Self_ID.Common.LL.CV'Access,
                 Single_RTS_Lock'Access, Rel_Time, Local_Timedout, Result);
            else
               Cond_Timed_Wait (Self_ID.Common.LL.CV'Access,
                 Self_ID.Common.LL.L'Access, Rel_Time, Local_Timedout, Result);
            end if;

            exit when Abs_Time <= Monotonic_Clock;

            if not Local_Timedout then
               --  somebody may have called Wakeup for us
               Timedout := False;
               exit;
            end if;

            Rel_Time := Abs_Time - Monotonic_Clock;
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
      Rel_Time   : Duration;
      Abs_Time   : Duration;
      Result     : Integer;
      Timedout   : Boolean;

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
         Rel_Time := Time;
         Abs_Time := Time + Check_Time;
      else
         Rel_Time := Time - Check_Time;
         Abs_Time := Time;
      end if;

      if Rel_Time > 0.0 then
         Self_ID.Common.State := Delay_Sleep;

         loop
            if Self_ID.Pending_Priority_Change then
               Self_ID.Pending_Priority_Change := False;
               Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
               Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
            end if;

            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            if Single_Lock then
               Cond_Timed_Wait (Self_ID.Common.LL.CV'Access,
                 Single_RTS_Lock'Access, Rel_Time, Timedout, Result);
            else
               Cond_Timed_Wait (Self_ID.Common.LL.CV'Access,
                 Self_ID.Common.LL.L'Access, Rel_Time, Timedout, Result);
            end if;

            exit when Abs_Time <= Monotonic_Clock;

            Rel_Time := Abs_Time - Monotonic_Clock;
         end loop;

         Self_ID.Common.State := Runnable;
      end if;

      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Yield;
      SSL.Abort_Undefer.all;
   end Timed_Delay;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_ID; Reason : System.Tasking.Task_States) is
   begin
      Cond_Signal (T.Common.LL.CV'Access);
   end Wakeup;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
   begin
      if Do_Yield then
         Sleep (0);
      end if;
   end Yield;

   ------------------
   -- Set_Priority --
   ------------------

   type Prio_Array_Type is array (System.Any_Priority) of Integer;
   pragma Atomic_Components (Prio_Array_Type);

   Prio_Array : Prio_Array_Type;
   --  Global array containing the id of the currently running task for
   --  each priority.
   --
   --  Note: we assume that we are on a single processor with run-til-blocked
   --  scheduling.

   procedure Set_Priority
     (T : Task_ID;
      Prio : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
      Res        : BOOL;
      Array_Item : Integer;

   begin
      Res := SetThreadPriority
        (T.Common.LL.Thread, Interfaces.C.int (Underlying_Priorities (Prio)));
      pragma Assert (Res = True);

      --  ??? Work around a bug in NT 4.0 SP3 scheduler
      --  It looks like when a task with Thread_Priority_Idle (using RT class)
      --  never reaches its time slice (e.g by doing multiple and simple RV,
      --  see CXD8002), the scheduler never gives higher priority task a
      --  chance to run.
      --  Note that this works fine on NT 4.0 SP1

      if Time_Slice_Val = 0
        and then Underlying_Priorities (Prio) = Thread_Priority_Idle
        and then Loss_Of_Inheritance
      then
         Sleep (20);
      end if;

      if FIFO_Within_Priorities then

         --  Annex D requirement [RM D.2.2 par. 9]:
         --    If the task drops its priority due to the loss of inherited
         --    priority, it is added at the head of the ready queue for its
         --    new active priority.

         if Loss_Of_Inheritance
           and then Prio < T.Common.Current_Priority
         then
            Array_Item := Prio_Array (T.Common.Base_Priority) + 1;
            Prio_Array (T.Common.Base_Priority) := Array_Item;

            loop
               --  Let some processes a chance to arrive

               Yield;

               --  Then wait for our turn to proceed

               exit when Array_Item = Prio_Array (T.Common.Base_Priority)
                 or else Prio_Array (T.Common.Base_Priority) = 1;
            end loop;

            Prio_Array (T.Common.Base_Priority) :=
              Prio_Array (T.Common.Base_Priority) - 1;
         end if;
      end if;

      T.Common.Current_Priority := Prio;
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

   --  There were two paths were we needed to call Enter_Task :
   --  1) from System.Task_Primitives.Operations.Initialize
   --  2) from System.Tasking.Stages.Task_Wrapper
   --
   --  The thread initialisation has to be done only for the first case.
   --
   --  This is because the GetCurrentThread NT call does not return the
   --  real thread handler but only a "pseudo" one. It is not possible to
   --  release the thread handle and free the system ressources from this
   --  "pseudo" handle. So we really want to keep the real thread handle
   --  set in System.Task_Primitives.Operations.Create_Task during the
   --  thread creation.

   procedure Enter_Task (Self_ID : Task_ID) is
      procedure Init_Float;
      pragma Import (C, Init_Float, "__gnat_init_float");
      --  Properly initializes the FPU for x86 systems.

      Succeeded : BOOL;

   begin
      Succeeded := TlsSetValue (TlsIndex, To_Address (Self_ID));
      pragma Assert (Succeeded = True);
      Init_Float;

      Self_ID.Common.LL.Thread_Id := GetCurrentThreadId;

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

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_ID; Succeeded : out Boolean) is
   begin
      Initialize_Cond (Self_ID.Common.LL.CV'Access);

      if not Single_Lock then
         Initialize_Lock (Self_ID.Common.LL.L'Access, ATCB_Level);
      end if;

      Succeeded := True;
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
      hTask          : HANDLE;
      TaskId         : aliased DWORD;
      pTaskParameter : System.OS_Interface.PVOID;
      dwStackSize    : DWORD;
      Result         : DWORD;
      Entry_Point    : PTHREAD_START_ROUTINE;

      function To_PTHREAD_START_ROUTINE is new
        Unchecked_Conversion (System.Address, PTHREAD_START_ROUTINE);

   begin
      pTaskParameter := To_Address (T);

      if Stack_Size = Unspecified_Size then
         dwStackSize := DWORD (Default_Stack_Size);

      elsif Stack_Size < Minimum_Stack_Size then
         dwStackSize := DWORD (Minimum_Stack_Size);

      else
         dwStackSize := DWORD (Stack_Size);
      end if;

      Entry_Point := To_PTHREAD_START_ROUTINE (Wrapper);

      hTask := CreateThread
         (null,
          dwStackSize,
          Entry_Point,
          pTaskParameter,
          DWORD (Create_Suspended),
          TaskId'Unchecked_Access);

      --  Step 1: Create the thread in blocked mode

      if hTask = 0 then
         raise Storage_Error;
      end if;

      --  Step 2: set its TCB

      T.Common.LL.Thread := hTask;

      --  Step 3: set its priority (child has inherited priority from parent)

      Set_Priority (T, Priority);

      --  Step 4: Now, start it for good:

      Result := ResumeThread (hTask);
      pragma Assert (Result = 1);

      Succeeded := Result = 1;
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_ID) is
      Self_ID   : Task_ID := T;
      Result    : DWORD;
      Succeeded : BOOL;

      procedure Free is new
        Unchecked_Deallocation (Ada_Task_Control_Block, Task_ID);

   begin
      if not Single_Lock then
         Finalize_Lock (T.Common.LL.L'Access);
      end if;

      Finalize_Cond (T.Common.LL.CV'Access);

      if T.Known_Tasks_Index /= -1 then
         Known_Tasks (T.Known_Tasks_Index) := null;
      end if;

      --  Wait for the thread to terminate then close it. this is needed
      --  to release system ressources.

      Result := WaitForSingleObject (T.Common.LL.Thread, Wait_Infinite);
      pragma Assert (Result /= WAIT_FAILED);
      Succeeded := CloseHandle (T.Common.LL.Thread);
      pragma Assert (Succeeded = True);

      Free (Self_ID);
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   procedure Exit_Task is
   begin
      ExitThread (0);
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_ID) is
   begin
      null;
   end Abort_Task;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_ID) is
      Res : BOOL;
   begin
      Environment_Task_ID := Environment_Task;

      if Time_Slice_Val = 0 or else FIFO_Within_Priorities then
         Res := OS_Interface.SetPriorityClass
           (GetCurrentProcess, Realtime_Priority_Class);
      end if;

      TlsIndex := TlsAlloc;

      --  Initialize the lock used to synchronize chain of all ATCBs.

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);

      Environment_Task.Common.LL.Thread := GetCurrentThread;
      Enter_Task (Environment_Task);

      --  Create a free ATCB for use on the Fake_ATCB_List

      Next_Fake_ATCB := new Fake_ATCB;
   end Initialize;

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
      return 0.000_001; --  1 micro-second
   end RT_Resolution;

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

   ------------------
   -- Suspend_Task --
   ------------------

   function Suspend_Task
     (T           : ST.Task_ID;
      Thread_Self : Thread_Id) return Boolean is
   begin
      if T.Common.LL.Thread /= Thread_Self then
         return SuspendThread (T.Common.LL.Thread) = NO_ERROR;
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
         return ResumeThread (T.Common.LL.Thread) = NO_ERROR;
      else
         return True;
      end if;
   end Resume_Task;

end System.Task_Primitives.Operations;
