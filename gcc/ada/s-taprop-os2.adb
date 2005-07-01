------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--    S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S     --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2005, Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is an OS/2 version of this package

--  This package contains all the GNULL primitives that interface directly
--  with the underlying OS.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Tasking.Debug;
--  used for Known_Tasks

with Interfaces.C;
--  used for size_t

with Interfaces.C.Strings;
--  used for Null_Ptr

with Interfaces.OS2Lib.Errors;
with Interfaces.OS2Lib.Threads;
with Interfaces.OS2Lib.Synchronization;

with System.Parameters;
--  used for Size_Type

with System.Tasking;
--  used for Task_Id

with System.Parameters;
--  used for Size_Type

with System.Soft_Links;
--  used for Defer/Undefer_Abort

--  Note that we do not use System.Tasking.Initialization directly since
--  this is a higher level package that we shouldn't depend on. For example
--  when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Stages.

with System.OS_Primitives;
--  used for Delay_Modes
--           Clock

with Unchecked_Conversion;
with Unchecked_Deallocation;

package body System.Task_Primitives.Operations is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;
   package OSP renames System.OS_Primitives;
   package SSL renames System.Soft_Links;

   use Interfaces.OS2Lib;
   use Interfaces.OS2Lib.Errors;
   use Interfaces.OS2Lib.Threads;
   use Interfaces.OS2Lib.Synchronization;
   use System.Parameters;
   use System.Tasking.Debug;
   use System.Tasking;
   use System.OS_Interface;
   use Interfaces.C;
   use System.OS_Primitives;

   ---------------------
   -- Local Constants --
   ---------------------

   Max_Locks_Per_Task   : constant := 100;
   Suppress_Owner_Check : constant Boolean := False;

   -----------------
   -- Local Types --
   -----------------

   subtype Lock_Range is Integer range 0 .. Max_Locks_Per_Task;

   -----------------
   -- Local Data  --
   -----------------

   --  The OS/2 DosAllocThreadLocalMemory API is used to allocate our TCB_Ptr

   --  This API reserves a small range of virtual addresses that is backed
   --  by different physical memory for each running thread. In this case we
   --  create a pointer at a fixed address that points to the TCB_Ptr for the
   --  running thread. So all threads will be able to query and update their
   --  own TCB_Ptr without destroying the TCB_Ptr of other threads.

   type Thread_Local_Data is record
      Self_ID           : Task_Id;    --  ID of the current thread
      Lock_Prio_Level   : Lock_Range; --  Nr of priority changes due to locks

      --  ... room for expansion here, if we decide to make access to
      --  jump-buffer and exception stack more efficient in future
   end record;

   type Access_Thread_Local_Data is access all Thread_Local_Data;

   --  Pointer to Thread Local Data
   Thread_Local_Data_Ptr : aliased Access_Thread_Local_Data;

   type PPTLD is access all Access_Thread_Local_Data;

   Single_RTS_Lock : aliased RTS_Lock;
   --  This is a lock to allow only one thread of control in the RTS at
   --  a time; it is used to execute in mutual exclusion from all other tasks.
   --  Used mainly in Single_Lock mode, but also to protect All_Tasks_List

   Environment_Task_Id : Task_Id;
   --  A variable to hold Task_Id for the environment task

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_PPVOID is new Unchecked_Conversion (PPTLD, PPVOID);
   function To_Address is new Unchecked_Conversion (Task_Id, System.Address);
   function To_PFNTHREAD is
     new Unchecked_Conversion (System.Address, PFNTHREAD);

   function To_MS (D : Duration) return ULONG;

   procedure Set_Temporary_Priority
     (T            : in Task_Id;
      New_Priority : in System.Any_Priority);

   -----------
   -- To_MS --
   -----------

   function To_MS (D : Duration) return ULONG is
   begin
      return ULONG (D * 1_000);
   end To_MS;

   -----------
   -- Clock --
   -----------

   function Monotonic_Clock return Duration renames OSP.Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
   begin
      return 10#1.0#E-6;
   end RT_Resolution;

   -------------------
   -- Abort_Handler --
   -------------------

   --  OS/2 only has limited support for asynchronous signals.
   --  It seems not to be possible to jump out of an exception
   --  handler or to change the execution context of the thread.
   --  So asynchonous transfer of control is not supported.

   -----------------
   -- Stack_Guard --
   -----------------

   --  The underlying thread system sets a guard page at the
   --  bottom of a thread stack, so nothing is needed.
   --  ??? Check the comment above

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
      return OSI.Thread_Id (T.Common.LL.Thread);
   end Get_Thread_Id;

   ----------
   -- Self --
   ----------

   function Self return Task_Id is
      Self_ID : Task_Id renames Thread_Local_Data_Ptr.Self_ID;

   begin
      --  Check that the thread local data has been initialized

      pragma Assert
        ((Thread_Local_Data_Ptr /= null
          and then Thread_Local_Data_Ptr.Self_ID /= null));

      return Self_ID;
   end Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : access Lock)
   is
   begin
      if DosCreateMutexSem
        (ICS.Null_Ptr, L.Mutex'Unchecked_Access, 0, False32) /= NO_ERROR
      then
         raise Storage_Error;
      end if;

      pragma Assert (L.Mutex /= 0, "Error creating Mutex");
      L.Priority := Prio;
      L.Owner_ID := Null_Address;
   end Initialize_Lock;

   procedure Initialize_Lock (L : access RTS_Lock; Level : Lock_Level) is
      pragma Unreferenced (Level);

   begin
      if DosCreateMutexSem
        (ICS.Null_Ptr, L.Mutex'Unchecked_Access, 0, False32) /= NO_ERROR
      then
         raise Storage_Error;
      end if;

      pragma Assert (L.Mutex /= 0, "Error creating Mutex");

      L.Priority := System.Any_Priority'Last;
      L.Owner_ID := Null_Address;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : access Lock) is
   begin
      Must_Not_Fail (DosCloseMutexSem (L.Mutex));
   end Finalize_Lock;

   procedure Finalize_Lock (L : access RTS_Lock) is
   begin
      Must_Not_Fail (DosCloseMutexSem (L.Mutex));
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
      Self_ID      : constant Task_Id := Thread_Local_Data_Ptr.Self_ID;
      Old_Priority : constant Any_Priority :=
                       Self_ID.Common.LL.Current_Priority;

   begin
      if L.Priority < Old_Priority then
         Ceiling_Violation := True;
         return;
      end if;

      Ceiling_Violation := False;

      --  Increase priority before getting the lock
      --  to prevent priority inversion

      Thread_Local_Data_Ptr.Lock_Prio_Level :=
        Thread_Local_Data_Ptr.Lock_Prio_Level + 1;
      if L.Priority > Old_Priority then
         Set_Temporary_Priority (Self_ID, L.Priority);
      end if;

      --  Request the lock and then update the lock owner data

      Must_Not_Fail (DosRequestMutexSem (L.Mutex, SEM_INDEFINITE_WAIT));
      L.Owner_Priority := Old_Priority;
      L.Owner_ID := Self_ID.all'Address;
   end Write_Lock;

   procedure Write_Lock
     (L           : access RTS_Lock;
      Global_Lock : Boolean := False)
   is
      Self_ID      : Task_Id;
      Old_Priority : Any_Priority;

   begin
      if not Single_Lock or else Global_Lock then
         Self_ID := Thread_Local_Data_Ptr.Self_ID;
         Old_Priority := Self_ID.Common.LL.Current_Priority;

         --  Increase priority before getting the lock
         --  to prevent priority inversion

         Thread_Local_Data_Ptr.Lock_Prio_Level :=
           Thread_Local_Data_Ptr.Lock_Prio_Level + 1;

         if L.Priority > Old_Priority then
            Set_Temporary_Priority (Self_ID, L.Priority);
         end if;

         --  Request the lock and then update the lock owner data

         Must_Not_Fail (DosRequestMutexSem (L.Mutex, SEM_INDEFINITE_WAIT));
         L.Owner_Priority := Old_Priority;
         L.Owner_ID := Self_ID.all'Address;
      end if;
   end Write_Lock;

   procedure Write_Lock (T : Task_Id) is
   begin
      if not Single_Lock then

         --  Request the lock and then update the lock owner data

         Must_Not_Fail
           (DosRequestMutexSem (T.Common.LL.L.Mutex, SEM_INDEFINITE_WAIT));
         T.Common.LL.L.Owner_ID := Null_Address;
      end if;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock
     (L : access Lock; Ceiling_Violation : out Boolean) renames Write_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : access Lock) is
      Self_ID      : constant Task_Id := Thread_Local_Data_Ptr.Self_ID;
      Old_Priority : constant Any_Priority := L.Owner_Priority;

   begin
      --  Check that this task holds the lock

      pragma Assert (Suppress_Owner_Check
        or else L.Owner_ID = Self_ID.all'Address);

      --  Upate the owner data

      L.Owner_ID := Null_Address;

      --  Do the actual unlocking. No more references
      --  to owner data of L after this point.

      Must_Not_Fail (DosReleaseMutexSem (L.Mutex));

      --  Reset priority after unlocking to avoid priority inversion

      Thread_Local_Data_Ptr.Lock_Prio_Level :=
        Thread_Local_Data_Ptr.Lock_Prio_Level - 1;
      if L.Priority /= Old_Priority then
         Set_Temporary_Priority (Self_ID, Old_Priority);
      end if;
   end Unlock;

   procedure Unlock (L : access RTS_Lock; Global_Lock : Boolean := False) is
      Self_ID      : Task_Id;
      Old_Priority : Any_Priority;

   begin
      if not Single_Lock or else Global_Lock then
         Self_ID := Thread_Local_Data_Ptr.Self_ID;
         Old_Priority := L.Owner_Priority;
         --  Check that this task holds the lock

         pragma Assert (Suppress_Owner_Check
           or else L.Owner_ID = Self_ID.all'Address);

         --  Upate the owner data

         L.Owner_ID := Null_Address;

         --  Do the actual unlocking. No more references
         --  to owner data of L after this point.

         Must_Not_Fail (DosReleaseMutexSem (L.Mutex));

         --  Reset priority after unlocking to avoid priority inversion

         Thread_Local_Data_Ptr.Lock_Prio_Level :=
           Thread_Local_Data_Ptr.Lock_Prio_Level - 1;

         if L.Priority /= Old_Priority then
            Set_Temporary_Priority (Self_ID, Old_Priority);
         end if;
      end if;
   end Unlock;

   procedure Unlock (T : Task_Id) is
   begin
      if not Single_Lock then

         --  Check the owner data

         pragma Assert (Suppress_Owner_Check
           or else T.Common.LL.L.Owner_ID = Null_Address);

         --  Do the actual unlocking. No more references
         --  to owner data of T.Common.LL.L after this point.

         Must_Not_Fail (DosReleaseMutexSem (T.Common.LL.L.Mutex));
      end if;
   end Unlock;

   -----------
   -- Sleep --
   -----------

   procedure Sleep
     (Self_ID : Task_Id;
      Reason  : System.Tasking.Task_States)
   is
      pragma Unreferenced (Reason);

      Count : aliased ULONG; -- Used to store dummy result

   begin
      --  Must reset Cond BEFORE L is unlocked

      Sem_Must_Not_Fail
        (DosResetEventSem (Self_ID.Common.LL.CV, Count'Unchecked_Access));

      if Single_Lock then
         Unlock_RTS;
      else
         Unlock (Self_ID);
      end if;

      --  No problem if we are interrupted here.
      --  If the condition is signaled, DosWaitEventSem will simply not block.

      Sem_Must_Not_Fail
        (DosWaitEventSem (Self_ID.Common.LL.CV, SEM_INDEFINITE_WAIT));

      --  Since L was previously accquired, lock operation should not fail

      if Single_Lock then
         Lock_RTS;
      else
         Write_Lock (Self_ID);
      end if;
   end Sleep;

   -----------------
   -- Timed_Sleep --
   -----------------

   --  This is for use within the run-time system, so abort is
   --  assumed to be already deferred, and the caller should be
   --  holding its own ATCB lock.

   --  Pre-assertion: Cond is posted
   --                 Self is locked.

   --  Post-assertion: Cond is posted
   --                  Self is locked.

   procedure Timed_Sleep
     (Self_ID  : Task_Id;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
      pragma Unreferenced (Reason);

      Check_Time : constant Duration := OSP.Monotonic_Clock;
      Rel_Time   : Duration;
      Abs_Time   : Duration;
      Time_Out   : ULONG;
      Result    : APIRET;
      Count      : aliased ULONG;  --  Used to store dummy result

   begin
      --  Must reset Cond BEFORE Self_ID is unlocked

      Sem_Must_Not_Fail
        (DosResetEventSem (Self_ID.Common.LL.CV,
         Count'Unchecked_Access));

      if Single_Lock then
         Unlock_RTS;
      else
         Unlock (Self_ID);
      end if;

      Timedout := True;
      Yielded := False;

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

            Time_Out := To_MS (Rel_Time);
            Result := DosWaitEventSem (Self_ID.Common.LL.CV, Time_Out);
            pragma Assert
             ((Result = NO_ERROR or Result = ERROR_TIMEOUT
                or Result = ERROR_INTERRUPT));

            --  ???
            --  What to do with error condition ERROR_NOT_ENOUGH_MEMORY? Can
            --  we raise an exception here?  And what about ERROR_INTERRUPT?
            --  Should that be treated as a simple timeout?
            --  For now, consider only ERROR_TIMEOUT to be a timeout.

            exit when Abs_Time <= OSP.Monotonic_Clock;

            if Result /= ERROR_TIMEOUT then
               --  somebody may have called Wakeup for us
               Timedout := False;
               exit;
            end if;

            Rel_Time := Abs_Time - OSP.Monotonic_Clock;
         end loop;
      end if;

      --  Ensure post-condition

      if Single_Lock then
         Lock_RTS;
      else
         Write_Lock (Self_ID);
      end if;

      if Timedout then
         Sem_Must_Not_Fail (DosPostEventSem (Self_ID.Common.LL.CV));
      end if;
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Self_ID  : Task_Id;
      Time     : Duration;
      Mode     : ST.Delay_Modes)
   is
      Check_Time : constant Duration := OSP.Monotonic_Clock;
      Rel_Time   : Duration;
      Abs_Time   : Duration;
      Timedout   : Boolean := True;
      Time_Out   : ULONG;
      Result     : APIRET;
      Count      : aliased ULONG;  --  Used to store dummy result

   begin
      --  Only the little window between deferring abort and
      --  locking Self_ID is the reason we need to
      --  check for pending abort and priority change below! :(

      SSL.Abort_Defer.all;

      if Single_Lock then
         Lock_RTS;
      else
         Write_Lock (Self_ID);
      end if;

      --  Must reset Cond BEFORE Self_ID is unlocked

      Sem_Must_Not_Fail
        (DosResetEventSem (Self_ID.Common.LL.CV,
         Count'Unchecked_Access));

      if Single_Lock then
         Unlock_RTS;
      else
         Unlock (Self_ID);
      end if;

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

            Time_Out := To_MS (Rel_Time);
            Result := DosWaitEventSem (Self_ID.Common.LL.CV, Time_Out);

            exit when Abs_Time <= OSP.Monotonic_Clock;

            Rel_Time := Abs_Time - OSP.Monotonic_Clock;
         end loop;

         Self_ID.Common.State := Runnable;
         Timedout := Result = ERROR_TIMEOUT;
      end if;

      if Single_Lock then
         Lock_RTS;
      else
         Write_Lock (Self_ID);
      end if;

      if Timedout then
         Sem_Must_Not_Fail (DosPostEventSem (Self_ID.Common.LL.CV));
      end if;

      if Single_Lock then
         Unlock_RTS;
      else
         Unlock (Self_ID);
      end if;

      System.OS_Interface.Yield;
      SSL.Abort_Undefer.all;
   end Timed_Delay;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_Id; Reason : System.Tasking.Task_States) is
      pragma Unreferenced (Reason);
   begin
      Sem_Must_Not_Fail (DosPostEventSem (T.Common.LL.CV));
   end Wakeup;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
   begin
      if Do_Yield then
         System.OS_Interface.Yield;
      end if;
   end Yield;

   ----------------------------
   -- Set_Temporary_Priority --
   ----------------------------

   procedure Set_Temporary_Priority
     (T            : Task_Id;
      New_Priority : System.Any_Priority)
   is
      use Interfaces.C;
      Delta_Priority : Integer;

   begin
      --  When Lock_Prio_Level = 0, we always need to set the
      --  Active_Priority. In this way we can make priority changes
      --  due to locking independent of those caused by calling
      --  Set_Priority.

      if Thread_Local_Data_Ptr.Lock_Prio_Level = 0
        or else New_Priority < T.Common.Current_Priority
      then
         Delta_Priority := T.Common.Current_Priority -
           T.Common.LL.Current_Priority;
      else
         Delta_Priority := New_Priority - T.Common.LL.Current_Priority;
      end if;

      if Delta_Priority /= 0 then
         --  ??? There is a race-condition here
         --  The TCB is updated before the system call to make
         --  pre-emption in the critical section less likely.

         T.Common.LL.Current_Priority :=
           T.Common.LL.Current_Priority + Delta_Priority;
         Must_Not_Fail
           (DosSetPriority (Scope   => PRTYS_THREAD,
                            Class   => PRTYC_NOCHANGE,
                            Delta_P => IC.long (Delta_Priority),
                            PorTid  => T.Common.LL.Thread));
      end if;
   end Set_Temporary_Priority;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T                   : Task_Id;
      Prio                : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
      pragma Unreferenced (Loss_Of_Inheritance);
   begin
      T.Common.Current_Priority := Prio;
      Set_Temporary_Priority (T, Prio);
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
      --  Initialize thread local data. Must be done first

      Thread_Local_Data_Ptr.Self_ID := Self_ID;
      Thread_Local_Data_Ptr.Lock_Prio_Level := 0;

      Lock_RTS;

      for J in Known_Tasks'Range loop
         if Known_Tasks (J) = null then
            Known_Tasks (J) := Self_ID;
            Self_ID.Known_Tasks_Index := J;
            exit;
         end if;
      end loop;

      Unlock_RTS;

      --  For OS/2, we can set Self_ID.Common.LL.Thread in
      --  Create_Task, since the thread is created suspended.
      --  That is, there is no danger of the thread racing ahead
      --  and trying to reference Self_ID.Common.LL.Thread before it
      --  has been initialized.

      --  .... Do we need to do anything with signals for OS/2 ???
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

   function Is_Valid_Task return Boolean is
   begin
      return False;
   end Is_Valid_Task;

   -----------------------------
   -- Register_Foreign_Thread --
   -----------------------------

   function Register_Foreign_Thread return Task_Id is
   begin
      return null;
   end Register_Foreign_Thread;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_Id; Succeeded : out Boolean) is
   begin
      if DosCreateEventSem (ICS.Null_Ptr,
        Self_ID.Common.LL.CV'Unchecked_Access, 0, True32) = NO_ERROR
      then
         if not Single_Lock
           and then DosCreateMutexSem
             (ICS.Null_Ptr,
              Self_ID.Common.LL.L.Mutex'Unchecked_Access,
              0,
              False32) /= NO_ERROR
         then
            Succeeded := False;
            Must_Not_Fail (DosCloseEventSem (Self_ID.Common.LL.CV));
         else
            Succeeded := True;
         end if;

         --  We now want to do the equivalent of:

         --  Initialize_Lock
         --    (Self_ID.Common.LL.L'Unchecked_Access, ATCB_Level);

         --  But we avoid that because the Initialize_TCB routine has an
         --  exception handler, and it is too early for us to deal with
         --  installing handlers (see comment below), so we do our own
         --  Initialize_Lock operation manually.

         Self_ID.Common.LL.L.Priority := System.Any_Priority'Last;
         Self_ID.Common.LL.L.Owner_ID := Null_Address;

      else
         Succeeded := False;
      end if;

      --  Note: at one time we had an exception handler here, whose code
      --  was as follows:

      --  exception

      --     Assumes any failure must be due to insufficient resources

      --     when Storage_Error =>
      --        Must_Not_Fail (DosCloseEventSem (Self_ID.Common.LL.CV));
      --        Succeeded := False;

      --  but that won't work with the old exception scheme, since it would
      --  result in messing with Jmpbuf values too early. If and when we get
      --  switched entirely to the new zero-cost exception scheme, we could
      --  put this handler back in!
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
      Result              : aliased APIRET;
      Adjusted_Stack_Size : System.Parameters.Size_Type;
      use System.Parameters;

   begin
      --  In OS/2 the allocated stack size should be based on the
      --  amount of address space that should be reserved for the stack.
      --  Actual memory will only be used when the stack is touched anyway.

      --  The new minimum size is 12 kB, although the EMX docs
      --  recommend a minimum size of 32 kB.  (The original was 4 kB)
      --  Systems that use many tasks (say > 30) and require much
      --  memory may run out of virtual address space, since OS/2
      --  has a per-proces limit of 512 MB, of which max. 300 MB is
      --  usable in practise.

      if Stack_Size = Unspecified_Size then
         Adjusted_Stack_Size := Default_Stack_Size;

      elsif Stack_Size < Minimum_Stack_Size then
         Adjusted_Stack_Size := Minimum_Stack_Size;

      else
         Adjusted_Stack_Size := Stack_Size;
      end if;

      --  GB970222:
      --    Because DosCreateThread is called directly here, the
      --    C RTL doesn't get initialized for the new thead. EMX by
      --    default uses per-thread local heaps in addition to the
      --    global heap. There might be other effects of by-passing the
      --    C library here.

      --    When using _beginthread the newly created thread is not
      --    blocked initially. Does this matter or can I create the
      --    thread running anyway? The LL.Thread variable will be set
      --    anyway because the variable is passed by reference to OS/2.

      T.Common.LL.Wrapper := To_PFNTHREAD (Wrapper);

      --  The OS implicitly gives the new task the priority of this task

      T.Common.LL.Current_Priority := Self.Common.LL.Current_Priority;

      --  If task was locked before activator task was
      --  initialized, assume it has OS standard priority

      if T.Common.LL.L.Owner_Priority not in Any_Priority'Range then
         T.Common.LL.L.Owner_Priority := 1;
      end if;

      --  Create the thread, in blocked mode

      Result := DosCreateThread
        (F_ptid   => T.Common.LL.Thread'Unchecked_Access,
         pfn      => T.Common.LL.Wrapper,
         param    => To_Address (T),
         flag     => Block_Child + Commit_Stack,
         cbStack  => ULONG (Adjusted_Stack_Size));

      Succeeded := (Result = NO_ERROR);

      if not Succeeded then
         return;
      end if;

      --  Set the new thread's priority
      --  (child has inherited priority from parent)

      Set_Priority (T, Priority);

      --  Start the thread executing

      Must_Not_Fail (DosResumeThread (T.Common.LL.Thread));

   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_Id) is
      Tmp    : Task_Id := T;

      procedure Free is new
        Unchecked_Deallocation (Ada_Task_Control_Block, Task_Id);

   begin
      Must_Not_Fail (DosCloseEventSem (T.Common.LL.CV));

      if not Single_Lock then
         Finalize_Lock (T.Common.LL.L'Unchecked_Access);
      end if;

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
      Thread_Local_Data_Ptr := null;
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_Id) is
      pragma Unreferenced (T);

   begin
      null;

      --  Task abort not implemented yet.
      --  Should perform other action ???

   end Abort_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : in out Suspension_Object) is
      Result : Interfaces.C.int;
   begin
      --  Initialize internal state. It is always initialized to False (ARM
      --  D.10 par. 6).

      S.State := False;
      S.Waiting := False;

      --  Initialize internal mutex
      if DosCreateMutexSem
        (ICS.Null_Ptr, S.L'Unchecked_Access, 0, False32) /= NO_ERROR
      then
         raise Storage_Error;
      end if;

      pragma Assert (S.L /= 0, "Error creating Mutex");

      --  Initialize internal condition variable

      if DosCreateEventSem
        (ICS.Null_Ptr, S.CV'Unchecked_Access, 0, True32) /= NO_ERROR
      then
         Must_Not_Fail (DosCloseMutexSem (S.L));

         raise Storage_Error;
      end if;

      pragma Assert (S.CV /= 0, "Error creating Condition Variable");
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (S : in out Suspension_Object) is
   begin
      --  Destroy internal mutex

      Must_Not_Fail (DosCloseMutexSem (S.L'Access));

      --  Destroy internal condition variable

      Must_Not_Fail (DosCloseEventSem (S.CV'Access));
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
   begin
      Must_Not_Fail (DosRequestMutexSem (S.L, SEM_INDEFINITE_WAIT));

      S.State := False;

      Must_Not_Fail (DosReleaseMutexSem (S.L));
   end Set_False;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
   begin
      Must_Not_Fail (DosRequestMutexSem (S.L, SEM_INDEFINITE_WAIT));

      --  If there is already a task waiting on this suspension object then
      --  we resume it, leaving the state of the suspension object to False,
      --  as it is specified in ARM D.10 par. 9. Otherwise, it just leaves
      --  the state to True.

      if S.Waiting then
         S.Waiting := False;
         S.State := False;

         Sem_Must_Not_Fail (DosPostEventSem (S.CV));
      else
         S.State := True;
      end if;

      Must_Not_Fail (DosReleaseMutexSem (S.L));
   end Set_True;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
      Count : aliased ULONG; -- Used to store dummy result
   begin
      Must_Not_Fail (DosRequestMutexSem (S.L, SEM_INDEFINITE_WAIT));

      if S.Waiting then
         --  Program_Error must be raised upon calling Suspend_Until_True
         --  if another task is already waiting on that suspension object
         --  (ARM D.10 par. 10).

         Must_Not_Fail (DosReleaseMutexSem (S.L));

         raise Program_Error;
      else
         --  Suspend the task if the state is False. Otherwise, the task
         --  continues its execution, and the state of the suspension object
         --  is set to False (ARM D.10 par. 9).

         if S.State then
            S.State := False;

            Must_Not_Fail (DosReleaseMutexSem (S.L));
         else
            S.Waiting := True;

            --  Must reset Cond BEFORE L is unlocked

            Sem_Must_Not_Fail
              (DosResetEventSem (S.CV, Count'Unchecked_Access));

            Must_Not_Fail (DosReleaseMutexSem (S.L));

            Sem_Must_Not_Fail
              (DosWaitEventSem (S.CV, SEM_INDEFINITE_WAIT));
         end if;
      end if;
   end Suspend_Until_True;

   ----------------
   -- Check_Exit --
   ----------------

   --  Dummy version

   function Check_Exit (Self_ID : ST.Task_Id) return Boolean is
   begin
      return Check_No_Locks (Self_ID);
   end Check_Exit;

   --------------------
   -- Check_No_Locks --
   --------------------

   function Check_No_Locks (Self_ID : ST.Task_Id) return Boolean is
      TLD : constant Access_Thread_Local_Data := Thread_Local_Data_Ptr;
   begin
      return Self_ID = TLD.Self_ID
        and then TLD.Lock_Prio_Level = 0;
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
      if Thread_Id (T.Common.LL.Thread) /= Thread_Self then
         return DosSuspendThread (T.Common.LL.Thread) = NO_ERROR;
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
      if Thread_Id (T.Common.LL.Thread) /= Thread_Self then
         return DosResumeThread (T.Common.LL.Thread) = NO_ERROR;
      else
         return True;
      end if;
   end Resume_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_Id) is
      Succeeded : Boolean;
   begin
      Environment_Task_Id := Environment_Task;

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);
      --  Initialize the lock used to synchronize chain of all ATCBs

      --  Set ID of environment task

      Thread_Local_Data_Ptr.Self_ID := Environment_Task;
      Environment_Task.Common.LL.Thread := 1; --  By definition

      --  This priority is unknown in fact.
      --  If actual current priority is different,
      --  it will get synchronized later on anyway.

      Environment_Task.Common.LL.Current_Priority :=
        Environment_Task.Common.Current_Priority;

      --  Initialize TCB for this task.
      --  This includes all the normal task-external initialization.
      --  This is also done by Initialize_ATCB, why ???

      Initialize_TCB (Environment_Task, Succeeded);

      --  Consider raising Storage_Error,
      --  if propagation can be tolerated ???

      pragma Assert (Succeeded);

      --  Do normal task-internal initialization,
      --  which depends on an initialized TCB.

      Enter_Task (Environment_Task);

      --  Insert here any other special
      --  initialization needed for the environment task.
   end Initialize;

begin
   --  Initialize pointer to task local data.
   --  This is done once, for all tasks.

   Must_Not_Fail (DosAllocThreadLocalMemory
      ((Thread_Local_Data'Size + 31) / 32,  --  nr of 32-bit words
       To_PPVOID (Thread_Local_Data_Ptr'Access)));

   --  Initialize thread local data for main thread

   Thread_Local_Data_Ptr.Self_ID := null;
   Thread_Local_Data_Ptr.Lock_Prio_Level := 0;
end System.Task_Primitives.Operations;
