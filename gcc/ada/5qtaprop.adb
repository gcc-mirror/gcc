------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies, Inc. (http://www.gnat.com).     --
--                                                                          --
------------------------------------------------------------------------------

--  RT GNU/Linux version

--  ???? Later, look at what we might want to provide for interrupt
--  management.

pragma Suppress (All_Checks);

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Machine_Code;
--  used for Asm

with System.OS_Interface;
--  used for various types, constants, and operations

with System.OS_Primitives;
--  used for Delay_Modes

with System.Parameters;
--  used for Size_Type

with System.Storage_Elements;

with System.Tasking;
--  used for Ada_Task_Control_Block
--           Task_ID

with Ada.Unchecked_Conversion;

package body System.Task_Primitives.Operations is

   use System.Machine_Code,
       System.OS_Interface,
       System.OS_Primitives,
       System.Parameters,
       System.Tasking,
       System.Storage_Elements;

   --------------------------------
   -- RT GNU/Linux specific Data --
   --------------------------------

   --  Define two important parameters necessary for a GNU/Linux kernel module.
   --  Any module that is going to be loaded into the kernel space needs these
   --  parameters.

   Mod_Use_Count : Integer;
   pragma Export (C, Mod_Use_Count, "mod_use_count_");
   --  for module usage tracking by the kernel

   type Aliased_String is array (Positive range <>) of aliased Character;
   pragma Convention (C, Aliased_String);

   Kernel_Version : constant Aliased_String := "2.0.33" & ASCII.Nul;
   pragma Export (C, Kernel_Version, "kernel_version");
   --  So that insmod can find the version number.

   --  The following procedures have their name specified by the GNU/Linux
   --  module loader. Note that they simply correspond to adainit/adafinal.

   function Init_Module return Integer;
   pragma Export (C, Init_Module, "init_module");

   procedure Cleanup_Module;
   pragma Export (C, Cleanup_Module, "cleanup_module");

   ----------------
   -- Local Data --
   ----------------

   LF   : constant String := ASCII.LF & ASCII.Nul;

   LFHT : constant String := ASCII.LF & ASCII.HT;
   --  used in inserted assembly code

   Max_Tasks : constant := 10;
   --  ??? Eventually, this should probably be in System.Parameters.

   Known_Tasks : array (0 .. Max_Tasks) of Task_ID;
   --  Global array of tasks read by gdb, and updated by Create_Task and
   --  Finalize_TCB. It's from System.Tasking.Debug. We moved it here to
   --  cut the dependence on that package. Consider moving it here or to
   --  this package specification, permanently????

   Max_Sensible_Delay : constant RTIME :=
     365 * 24 * 60 * 60 * RT_TICKS_PER_SEC;
   --  Max of one year delay, needed to prevent exceptions for large
   --  delay values. It seems unlikely that any test will notice this
   --  restriction.
   --  ??? This is really declared in System.OS_Primitives,
   --  and the type is Duration, here its type is RTIME.

   Tick_Count : constant := RT_TICKS_PER_SEC / 20;
   Nano_Count : constant := 50_000_000;
   --  two constants used in conversions between RTIME and Duration.

   Addr_Bytes : constant Storage_Offset :=
     System.Address'Max_Size_In_Storage_Elements;
   --  number of bytes needed for storing an address.

   Guess : constant RTIME := 10;
   --  an approximate amount of RTIME used in scheduler to awake a task having
   --  its resume time within 'current time + Guess'
   --  The value of 10 is estimated here and may need further refinement

   TCB_Array : array (0 .. Max_Tasks)
     of aliased Restricted_Ada_Task_Control_Block (Entry_Num => 0);
   pragma Volatile_Components (TCB_Array);

   Available_TCBs : Task_ID;
   pragma Atomic (Available_TCBs);
   --  Head of linear linked list of available TCB's, linked using TCB's
   --  LL.Next. This list is Initialized to contain a fixed number of tasks,
   --  when the runtime system starts up.

   Current_Task : Task_ID;
   pragma Export (C, Current_Task, "current_task");
   pragma Atomic (Current_Task);
   --  This is the task currently running. We need the pragma here to specify
   --  the link-name for Current_Task is "current_task", rather than the long
   --  name (including the package name) that the Ada compiler would normally
   --  generate. "current_task" is referenced in procedure Rt_Switch_To below

   Idle_Task : aliased Restricted_Ada_Task_Control_Block (Entry_Num => 0);
   --  Tail of the circular queue of ready to run tasks.

   Scheduler_Idle : Boolean := False;
   --  True when the scheduler is idle (no task other than the idle task
   --  is on the ready queue).

   In_Elab_Code : Boolean := True;
   --  True when we are elaborating our application.
   --  Init_Module will set this flag to false and never revert it.

   Timer_Queue : aliased Restricted_Ada_Task_Control_Block (Entry_Num => 0);
   --  Header of the queue of delayed real-time tasks.
   --  Timer_Queue.LL has to be initialized properly before being used

   Timer_Expired : Boolean := False;
   --  flag to show whether the Timer_Queue needs to be checked
   --  when it becomes true, it means there is a task in the
   --  Timer_Queue having to be awakened and be moved to ready queue

   Environment_Task_ID : Task_ID;
   --  A variable to hold Task_ID for the environment task.
   --  Once initialized, this behaves as a constant.
   --  In the current implementation, this is the task assigned permanently
   --  as the regular GNU/Linux kernel.

   Single_RTS_Lock : aliased RTS_Lock;
   --  This is a lock to allow only one thread of control in the RTS at
   --  a time; it is used to execute in mutual exclusion from all other tasks.
   --  Used mainly in Single_Lock mode, but also to protect All_Tasks_List

   --  The followings are internal configuration constants needed.
   Next_Serial_Number : Task_Serial_Number := 100;
   pragma Volatile (Next_Serial_Number);
   --  We start at 100, to reserve some special values for
   --  using in error checking.

   GNU_Linux_Irq_State : Integer := 0;
   --  This needs comments ???

   type Duration_As_Integer is delta 1.0
      range -2.0**(Duration'Size - 1) .. 2.0**(Duration'Size - 1) - 1.0;
   --  used for output RTIME value during debugging

   type Address_Ptr is access all System.Address;
   pragma Convention (C, Address_Ptr);

   --------------------------------
   -- Local conversion functions --
   --------------------------------

   function To_Task_ID is new
     Ada.Unchecked_Conversion (System.Address, Task_ID);

   function To_Address is new
     Ada.Unchecked_Conversion (Task_ID, System.Address);

   function RTIME_To_D_Int is new
     Ada.Unchecked_Conversion (RTIME, Duration_As_Integer);

   function Raw_RTIME is new
     Ada.Unchecked_Conversion (Duration, RTIME);

   function Raw_Duration is new
     Ada.Unchecked_Conversion (RTIME, Duration);

   function To_Duration (T : RTIME) return Duration;
   pragma Inline (To_Duration);

   function To_RTIME (D : Duration) return RTIME;
   pragma Inline (To_RTIME);

   function To_Integer is new
     Ada.Unchecked_Conversion (System.Parameters.Size_Type, Integer);

   function To_Address_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Address_Ptr);

   function To_RTS_Lock_Ptr is new
     Ada.Unchecked_Conversion (Lock_Ptr, RTS_Lock_Ptr);

   -----------------------------------
   -- Local Subprogram Declarations --
   -----------------------------------

   procedure Rt_Switch_To (Tsk : Task_ID);
   pragma Inline (Rt_Switch_To);
   --  switch from the 'current_task' to 'Tsk'
   --  and 'Tsk' then becomes 'current_task'

   procedure R_Save_Flags (F : out Integer);
   pragma Inline (R_Save_Flags);
   --  save EFLAGS register to 'F'

   procedure R_Restore_Flags (F : Integer);
   pragma Inline (R_Restore_Flags);
   --  restore EFLAGS register from 'F'

   procedure R_Cli;
   pragma Inline (R_Cli);
   --  disable interrupts

   procedure R_Sti;
   pragma Inline (R_Sti);
   --  enable interrupts

   procedure Timer_Wrapper;
   --  the timer handler. It sets Timer_Expired flag to True and
   --  then calls Rt_Schedule

   procedure Rt_Schedule;
   --  the scheduler

   procedure Insert_R (T : Task_ID);
   pragma Inline (Insert_R);
   --  insert 'T' into the tail of the ready queue for its active
   --  priority
   --  if original queue is 6 5 4 4 3 2 and T has priority of 4
   --  then after T is inserted the queue becomes 6 5 4 4 T 3 2

   procedure Insert_RF (T : Task_ID);
   pragma Inline (Insert_RF);
   --  insert 'T' into the front of the ready queue for its active
   --  priority
   --  if original queue is 6 5 4 4 3 2 and T has priority of 4
   --  then after T is inserted the queue becomes 6 5 T 4 4 3 2

   procedure Delete_R (T : Task_ID);
   pragma Inline (Delete_R);
   --  delete 'T' from the ready queue. If 'T' is not in any queue
   --  the operation has no effect

   procedure Insert_T (T : Task_ID);
   pragma Inline (Insert_T);
   --  insert 'T' into the waiting queue according to its Resume_Time.
   --  If there are tasks in the waiting queue that have the same
   --  Resume_Time as 'T', 'T' is then inserted into the queue for
   --  its active priority

   procedure Delete_T (T : Task_ID);
   pragma Inline (Delete_T);
   --  delete 'T' from the waiting queue.

   procedure Move_Top_Task_From_Timer_Queue_To_Ready_Queue;
   pragma Inline (Move_Top_Task_From_Timer_Queue_To_Ready_Queue);
   --  remove the task in the front of the waiting queue and insert it
   --  into the tail of the ready queue for its active priority

   -------------------------
   --  Local Subprograms  --
   -------------------------

   procedure Rt_Switch_To (Tsk : Task_ID) is
   begin
      pragma Debug (Printk ("procedure Rt_Switch_To called" & LF));

      Asm (
        "pushl %%eax" & LFHT &
        "pushl %%ebp" & LFHT &
        "pushl %%edi" & LFHT &
        "pushl %%esi" & LFHT &
        "pushl %%edx" & LFHT &
        "pushl %%ecx" & LFHT &
        "pushl %%ebx" & LFHT &

        "movl current_task, %%edx" & LFHT &
        "cmpl $0, 36(%%edx)" & LFHT &
         --  36 is hard-coded, 36(%%edx) is actually
         --  Current_Task.Common.LL.Uses_Fp

        "jz 25f" & LFHT &
        "sub $108,%%esp" & LFHT &
        "fsave (%%esp)" & LFHT &
        "25:      pushl $1f" & LFHT &
        "movl %%esp, 32(%%edx)" & LFHT &
         --  32 is hard-coded, 32(%%edx) is actually
         --  Current_Task.Common.LL.Stack

        "movl 32(%%ecx), %%esp" & LFHT &
         --  32 is hard-coded, 32(%%ecx) is actually Tsk.Common.LL.Stack.
         --  Tsk is the task to be switched to

        "movl %%ecx, current_task" & LFHT &
        "ret" & LFHT &
        "1:       cmpl $0, 36(%%ecx)" & LFHT &
         --  36(%%exc) is Tsk.Common.LL.Stack (hard coded)
        "jz 26f" & LFHT &
        "frstor (%%esp)" & LFHT &
        "add $108,%%esp" & LFHT &
        "26:      popl %%ebx" & LFHT &
        "popl %%ecx" & LFHT &
        "popl %%edx" & LFHT &
        "popl %%esi" & LFHT &
        "popl %%edi" & LFHT &
        "popl %%ebp" & LFHT &
        "popl %%eax",
        Outputs  => No_Output_Operands,
        Inputs   => Task_ID'Asm_Input ("c", Tsk),
        Clobber  => "cx",
        Volatile => True);
   end Rt_Switch_To;

   procedure R_Save_Flags (F : out Integer) is
   begin
      Asm (
        "pushfl" & LFHT &
        "popl %0",
        Outputs  => Integer'Asm_Output ("=g", F),
        Inputs   => No_Input_Operands,
        Clobber  => "memory",
        Volatile => True);
   end R_Save_Flags;

   procedure R_Restore_Flags (F : Integer) is
   begin
      Asm (
        "pushl %0" & LFHT &
        "popfl",
        Outputs  => No_Output_Operands,
        Inputs   => Integer'Asm_Input ("g", F),
        Clobber  => "memory",
        Volatile => True);
   end R_Restore_Flags;

   procedure R_Sti is
   begin
      Asm (
         "sti",
         Outputs  => No_Output_Operands,
         Inputs   => No_Input_Operands,
         Clobber  => "memory",
         Volatile => True);
   end R_Sti;

   procedure R_Cli is
   begin
      Asm (
        "cli",
        Outputs  => No_Output_Operands,
        Inputs   => No_Input_Operands,
        Clobber  => "memory",
        Volatile => True);
   end R_Cli;

   --  A wrapper for Rt_Schedule, works as the timer handler

   procedure Timer_Wrapper is
   begin
      pragma Debug (Printk ("procedure Timer_Wrapper called" & LF));

      Timer_Expired := True;
      Rt_Schedule;
   end Timer_Wrapper;

   procedure Rt_Schedule is
      Now      : RTIME;
      Top_Task : Task_ID;
      Flags    : Integer;

      procedure Debug_Timer_Queue;
      --  Check the state of the Timer Queue.

      procedure Debug_Timer_Queue is
      begin
         if Timer_Queue.Common.LL.Succ /= Timer_Queue'Address then
            Printk ("Timer_Queue not empty" & LF);
         end if;

         if To_Task_ID (Timer_Queue.Common.LL.Succ).Common.LL.Resume_Time <
           Now + Guess
         then
            Printk ("and need to move top task to ready queue" & LF);
         end if;
      end Debug_Timer_Queue;

   begin
      pragma Debug (Printk ("procedure Rt_Schedule called" & LF));

      --  Scheduler_Idle means that this call comes from an interrupt
      --  handler (e.g timer) that interrupted the idle loop below.

      if Scheduler_Idle then
         return;
      end if;

      <<Idle>>
      R_Save_Flags (Flags);
      R_Cli;

      Scheduler_Idle := False;

      if Timer_Expired then
         pragma Debug (Printk ("Timer expired" & LF));
         Timer_Expired := False;

         --  Check for expired time delays.
         Now := Rt_Get_Time;

         --  Need another (circular) queue for delayed tasks, this one ordered
         --  by wakeup time, so the one at the front has the earliest resume
         --  time. Wake up all the tasks sleeping on time delays that should
         --  be awakened at this time.

         --  ??? This is not very good, since we may waste time here waking
         --  up a bunch of lower priority tasks, adding to the blocking time
         --  of higher priority ready tasks, but we don't see how to get
         --  around this without adding more wasted time elsewhere.

         pragma Debug (Debug_Timer_Queue);

         while Timer_Queue.Common.LL.Succ /= Timer_Queue'Address and then
           To_Task_ID
             (Timer_Queue.Common.LL.Succ).Common.LL.Resume_Time < Now + Guess
         loop
            To_Task_ID (Timer_Queue.Common.LL.Succ).Common.LL.State :=
              RT_TASK_READY;
            Move_Top_Task_From_Timer_Queue_To_Ready_Queue;
         end loop;

         --  Arm the timer if necessary.
         --  ??? This may be wasteful, if the tasks on the timer queue are
         --  of lower priority than the current task's priority. The problem
         --  is that we can't tell this without scanning the whole timer
         --  queue. This scanning takes extra time.

         if Timer_Queue.Common.LL.Succ /= Timer_Queue'Address then
            --  Timer_Queue is not empty, so set the timer to interrupt at
            --  the next resume time. The Wakeup procedure must also do this,
            --  and must do it while interrupts are disabled so that there is
            --  no danger of interleaving with this code.
            Rt_Set_Timer
              (To_Task_ID (Timer_Queue.Common.LL.Succ).Common.LL.Resume_Time);
         else
            Rt_No_Timer;
         end if;
      end if;

      Top_Task := To_Task_ID (Idle_Task.Common.LL.Succ);

      --  If the ready queue is empty, the kernel has to wait until the timer
      --  or another interrupt makes a task ready.

      if Top_Task = To_Task_ID (Idle_Task'Address) then
         Scheduler_Idle := True;
         R_Restore_Flags (Flags);
         pragma Debug (Printk ("!!!kernel idle!!!" & LF));
         goto Idle;
      end if;

      if Top_Task = Current_Task then
         pragma Debug (Printk ("Rt_Schedule: Top_Task = Current_Task" & LF));
         --  if current task continues, just return.

         R_Restore_Flags (Flags);
         return;
      end if;

      if Top_Task = Environment_Task_ID then
         pragma Debug (Printk
           ("Rt_Schedule: Top_Task = Environment_Task" & LF));
         --  If there are no RT tasks ready, we execute the regular
         --  GNU/Linux kernel, and allow the regular GNU/Linux interrupt
         --  handlers to preempt the current task again.

         if not In_Elab_Code then
            SFIF := GNU_Linux_Irq_State;
         end if;

      elsif Current_Task = Environment_Task_ID then
         pragma Debug (Printk
           ("Rt_Schedule: Current_Task = Environment_Task" & LF));
         --  We are going to preempt the regular GNU/Linux kernel to
         --  execute an RT task, so don't allow the regular GNU/Linux
         --  interrupt handlers to preempt the current task any more.

         GNU_Linux_Irq_State := SFIF;
         SFIF := 0;
      end if;

      Top_Task.Common.LL.State := RT_TASK_READY;
      Rt_Switch_To (Top_Task);
      R_Restore_Flags (Flags);
   end Rt_Schedule;

   procedure Insert_R (T : Task_ID) is
      Q : Task_ID := To_Task_ID (Idle_Task.Common.LL.Succ);
   begin
      pragma Debug (Printk ("procedure Insert_R called" & LF));

      pragma Assert (T.Common.LL.Succ = To_Address (T));
      pragma Assert (T.Common.LL.Pred = To_Address (T));

      --  T is inserted in the queue between a task that has higher
      --  or the same Active_Priority as T and a task that has lower
      --  Active_Priority than T

      while Q /= To_Task_ID (Idle_Task'Address)
        and then T.Common.LL.Active_Priority <= Q.Common.LL.Active_Priority
      loop
         Q := To_Task_ID (Q.Common.LL.Succ);
      end loop;

      --  Q is successor of T

      T.Common.LL.Succ := To_Address (Q);
      T.Common.LL.Pred := Q.Common.LL.Pred;
      To_Task_ID (T.Common.LL.Pred).Common.LL.Succ := To_Address (T);
      Q.Common.LL.Pred := To_Address (T);
   end Insert_R;

   procedure Insert_RF (T : Task_ID) is
      Q : Task_ID := To_Task_ID (Idle_Task.Common.LL.Succ);
   begin
      pragma Debug (Printk ("procedure Insert_RF called" & LF));

      pragma Assert (T.Common.LL.Succ = To_Address (T));
      pragma Assert (T.Common.LL.Pred = To_Address (T));

      --  T is inserted in the queue between a task that has higher
      --  Active_Priority as T and a task that has lower or the same
      --  Active_Priority as T

      while Q /= To_Task_ID (Idle_Task'Address) and then
        T.Common.LL.Active_Priority < Q.Common.LL.Active_Priority
      loop
         Q := To_Task_ID (Q.Common.LL.Succ);
      end loop;

      --  Q is successor of T

      T.Common.LL.Succ := To_Address (Q);
      T.Common.LL.Pred := Q.Common.LL.Pred;
      To_Task_ID (T.Common.LL.Pred).Common.LL.Succ := To_Address (T);
      Q.Common.LL.Pred := To_Address (T);
   end Insert_RF;

   procedure Delete_R (T : Task_ID) is
      Tpred : constant Task_ID := To_Task_ID (T.Common.LL.Pred);
      Tsucc : constant Task_ID := To_Task_ID (T.Common.LL.Succ);

   begin
      pragma Debug (Printk ("procedure Delete_R called" & LF));

      --  checking whether T is in the queue is not necessary because
      --  if T is not in the queue, following statements changes
      --  nothing. But T cannot be in the Timer_Queue, otherwise
      --  activate the check below, note that checking whether T is
      --  in a queue is a relatively expensive operation

      Tpred.Common.LL.Succ := To_Address (Tsucc);
      Tsucc.Common.LL.Pred := To_Address (Tpred);
      T.Common.LL.Succ := To_Address (T);
      T.Common.LL.Pred := To_Address (T);
   end Delete_R;

   procedure Insert_T (T : Task_ID) is
      Q : Task_ID := To_Task_ID (Timer_Queue.Common.LL.Succ);
   begin
      pragma Debug (Printk ("procedure Insert_T called" & LF));

      pragma Assert (T.Common.LL.Succ = To_Address (T));

      while Q /= To_Task_ID (Timer_Queue'Address) and then
        T.Common.LL.Resume_Time > Q.Common.LL.Resume_Time
      loop
         Q := To_Task_ID (Q.Common.LL.Succ);
      end loop;

      --  Q is the task that has Resume_Time equal to or greater than that
      --  of T. If they have the same Resume_Time, continue looking for the
      --  location T is to be inserted using its Active_Priority

      while Q /= To_Task_ID (Timer_Queue'Address) and then
        T.Common.LL.Resume_Time = Q.Common.LL.Resume_Time
      loop
         exit when T.Common.LL.Active_Priority > Q.Common.LL.Active_Priority;
         Q := To_Task_ID (Q.Common.LL.Succ);
      end loop;

      --  Q is successor of T

      T.Common.LL.Succ := To_Address (Q);
      T.Common.LL.Pred := Q.Common.LL.Pred;
      To_Task_ID (T.Common.LL.Pred).Common.LL.Succ := To_Address (T);
      Q.Common.LL.Pred := To_Address (T);
   end Insert_T;

   procedure Delete_T (T : Task_ID) is
      Tpred : constant Task_ID := To_Task_ID (T.Common.LL.Pred);
      Tsucc : constant Task_ID := To_Task_ID (T.Common.LL.Succ);

   begin
      pragma Debug (Printk ("procedure Delete_T called" & LF));

      pragma Assert (T /= To_Task_ID (Timer_Queue'Address));

      Tpred.Common.LL.Succ := To_Address (Tsucc);
      Tsucc.Common.LL.Pred := To_Address (Tpred);
      T.Common.LL.Succ := To_Address (T);
      T.Common.LL.Pred := To_Address (T);
   end Delete_T;

   procedure Move_Top_Task_From_Timer_Queue_To_Ready_Queue is
      Top_Task : Task_ID := To_Task_ID (Timer_Queue.Common.LL.Succ);
   begin
      pragma Debug (Printk ("procedure Move_Top_Task called" & LF));

      if Top_Task /= To_Task_ID (Timer_Queue'Address) then
         Delete_T (Top_Task);
         Top_Task.Common.LL.State := RT_TASK_READY;
         Insert_R (Top_Task);
      end if;
   end  Move_Top_Task_From_Timer_Queue_To_Ready_Queue;

   ----------
   -- Self --
   ----------

   function Self return Task_ID is
   begin
      pragma Debug (Printk ("function Self called" & LF));

      return Current_Task;
   end Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock (Prio : System.Any_Priority; L : access Lock) is
   begin
      pragma Debug (Printk ("procedure Initialize_Lock called" & LF));

      L.Ceiling_Priority := Prio;
      L.Owner := System.Null_Address;
   end Initialize_Lock;

   procedure Initialize_Lock (L : access RTS_Lock; Level : Lock_Level) is
   begin
      pragma Debug (Printk ("procedure Initialize_Lock (RTS) called" & LF));

      L.Ceiling_Priority := System.Any_Priority'Last;
      L.Owner := System.Null_Address;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : access Lock) is
   begin
      pragma Debug (Printk ("procedure Finalize_Lock called" & LF));
      null;
   end Finalize_Lock;

   procedure Finalize_Lock (L : access RTS_Lock) is
   begin
      pragma Debug (Printk ("procedure Finalize_Lock (RTS) called" & LF));
      null;
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
      Prio : constant System.Any_Priority :=
        Current_Task.Common.LL.Active_Priority;

   begin
      pragma Debug (Printk ("procedure Write_Lock called" & LF));

      Ceiling_Violation := False;

      if Prio > L.Ceiling_Priority then
         --  Ceiling violation.
         --  This should never happen, unless something is seriously
         --  wrong with task T or the entire run-time system.
         --  ???? extreme error recovery, e.g. shut down the system or task

         Ceiling_Violation := True;
         pragma Debug (Printk ("Ceiling Violation in Write_Lock" & LF));
         return;
      end if;

      L.Pre_Locking_Priority := Prio;
      L.Owner := To_Address (Current_Task);
      Current_Task.Common.LL.Active_Priority := L.Ceiling_Priority;

      if Current_Task.Common.LL.Outer_Lock = null then
         --  If this lock is not nested, record a pointer to it.

         Current_Task.Common.LL.Outer_Lock :=
           To_RTS_Lock_Ptr (L.all'Unchecked_Access);
      end if;
   end Write_Lock;

   procedure Write_Lock
     (L : access RTS_Lock; Global_Lock : Boolean := False)
   is
      Prio : constant System.Any_Priority :=
        Current_Task.Common.LL.Active_Priority;

   begin
      pragma Debug (Printk ("procedure Write_Lock (RTS) called" & LF));

      if Prio > L.Ceiling_Priority then
         --  Ceiling violation.
         --  This should never happen, unless something is seriously
         --  wrong with task T or the entire runtime system.
         --  ???? extreme error recovery, e.g. shut down the system or task

         Printk ("Ceiling Violation in Write_Lock (RTS)" & LF);
         return;
      end if;

      L.Pre_Locking_Priority := Prio;
      L.Owner := To_Address (Current_Task);
      Current_Task.Common.LL.Active_Priority := L.Ceiling_Priority;

      if Current_Task.Common.LL.Outer_Lock = null then
         Current_Task.Common.LL.Outer_Lock := L.all'Unchecked_Access;
      end if;
   end Write_Lock;

   procedure Write_Lock (T : Task_ID) is
      Prio : constant System.Any_Priority :=
        Current_Task.Common.LL.Active_Priority;

   begin
      pragma Debug (Printk ("procedure Write_Lock (Task_ID) called" & LF));

      if Prio > T.Common.LL.L.Ceiling_Priority then
         --  Ceiling violation.
         --  This should never happen, unless something is seriously
         --  wrong with task T or the entire runtime system.
         --  ???? extreme error recovery, e.g. shut down the system or task

         Printk ("Ceiling Violation in Write_Lock (Task)" & LF);
         return;
      end if;

      T.Common.LL.L.Pre_Locking_Priority := Prio;
      T.Common.LL.L.Owner := To_Address (Current_Task);
      Current_Task.Common.LL.Active_Priority := T.Common.LL.L.Ceiling_Priority;

      if Current_Task.Common.LL.Outer_Lock = null then
         Current_Task.Common.LL.Outer_Lock := T.Common.LL.L'Access;
      end if;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
   begin
      pragma Debug (Printk ("procedure Read_Lock called" & LF));
      Write_Lock (L, Ceiling_Violation);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : access Lock) is
      Flags : Integer;
   begin
      pragma Debug (Printk ("procedure Unlock called" & LF));

      if L.Owner /= To_Address (Current_Task) then
         --  ...error recovery

         null;
         Printk ("The caller is not the owner of the lock" & LF);
         return;
      end if;

      L.Owner := System.Null_Address;

      --  Now that the lock is released, lower own priority,

      if Current_Task.Common.LL.Outer_Lock =
        To_RTS_Lock_Ptr (L.all'Unchecked_Access)
      then
         --  This lock is the outer-most one, reset own priority to
         --  Current_Priority;

         Current_Task.Common.LL.Active_Priority :=
           Current_Task.Common.Current_Priority;
         Current_Task.Common.LL.Outer_Lock := null;

      else
         --  If this lock is nested, pop the old active priority.

         Current_Task.Common.LL.Active_Priority := L.Pre_Locking_Priority;
      end if;

      --  Reschedule the task if necessary. Note we only need to reschedule
      --  the task if its Active_Priority becomes less than the one following
      --  it. The check depends on the fact that Environment_Task (tail of
      --  the ready queue) has the lowest Active_Priority

      if Current_Task.Common.LL.Active_Priority
        < To_Task_ID (Current_Task.Common.LL.Succ).Common.LL.Active_Priority
      then
         R_Save_Flags (Flags);
         R_Cli;
         Delete_R (Current_Task);
         Insert_RF (Current_Task);
         R_Restore_Flags (Flags);
         Rt_Schedule;
      end if;
   end Unlock;

   procedure Unlock (L : access RTS_Lock; Global_Lock : Boolean := False) is
      Flags : Integer;
   begin
      pragma Debug (Printk ("procedure Unlock (RTS_Lock) called" & LF));

      if L.Owner /= To_Address (Current_Task) then
         null;
         Printk ("The caller is not the owner of the lock" & LF);
         return;
      end if;

      L.Owner := System.Null_Address;

      if Current_Task.Common.LL.Outer_Lock = L.all'Unchecked_Access then
         Current_Task.Common.LL.Active_Priority :=
           Current_Task.Common.Current_Priority;
         Current_Task.Common.LL.Outer_Lock := null;

      else
         Current_Task.Common.LL.Active_Priority := L.Pre_Locking_Priority;
      end if;

      --  Reschedule the task if necessary

      if Current_Task.Common.LL.Active_Priority
        < To_Task_ID (Current_Task.Common.LL.Succ).Common.LL.Active_Priority
      then
         R_Save_Flags (Flags);
         R_Cli;
         Delete_R (Current_Task);
         Insert_RF (Current_Task);
         R_Restore_Flags (Flags);
         Rt_Schedule;
      end if;
   end Unlock;

   procedure Unlock (T : Task_ID) is
   begin
      pragma Debug (Printk ("procedure Unlock (Task_ID) called" & LF));
      Unlock (T.Common.LL.L'Access);
   end Unlock;

   -----------
   -- Sleep --
   -----------

   --  Unlock Self_ID.Common.LL.L and suspend Self_ID, atomically.
   --  Before return, lock Self_ID.Common.LL.L again
   --  Self_ID can only be reactivated by calling Wakeup.
   --  Unlock code is repeated intentionally.

   procedure Sleep
     (Self_ID : Task_ID;
      Reason  : ST.Task_States)
   is
      Flags : Integer;
   begin
      pragma Debug (Printk ("procedure Sleep called" & LF));

      --  Note that Self_ID is actually Current_Task, that is, only the
      --  task that is running can put itself into sleep. To preserve
      --  consistency, we use Self_ID throughout the code here

      Self_ID.Common.State := Reason;
      Self_ID.Common.LL.State := RT_TASK_DORMANT;

      R_Save_Flags (Flags);
      R_Cli;

      Delete_R (Self_ID);

      --  Arrange to unlock Self_ID's ATCB lock. The following check
      --  may be unnecessary because the specification of Sleep says
      --  the caller shoud hold its own ATCB lock before calling Sleep

      if Self_ID.Common.LL.L.Owner = To_Address (Self_ID) then
         Self_ID.Common.LL.L.Owner := System.Null_Address;

         if Self_ID.Common.LL.Outer_Lock = Self_ID.Common.LL.L'Access then
            Self_ID.Common.LL.Active_Priority :=
              Self_ID.Common.Current_Priority;
            Self_ID.Common.LL.Outer_Lock := null;

         else
            Self_ID.Common.LL.Active_Priority :=
              Self_ID.Common.LL.L.Pre_Locking_Priority;
         end if;
      end if;

      R_Restore_Flags (Flags);
      Rt_Schedule;

      --  Before leave, regain the lock

      Write_Lock (Self_ID);
   end Sleep;

   -----------------
   -- Timed_Sleep --
   -----------------

   --  Arrange to be awakened after/at Time (depending on Mode) then Unlock
   --  Self_ID.Common.LL.L and suspend self. If the timeout expires first,
   --  that should awaken the task. If it's awakened (by some other task
   --  calling Wakeup) before the timeout expires, the timeout should be
   --  cancelled.

   --  This is for use within the run-time system, so abort is
   --  assumed to be already deferred, and the caller should be
   --  holding its own ATCB lock.

   procedure Timed_Sleep
     (Self_ID  : Task_ID;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
      Flags      : Integer;
      Abs_Time   : RTIME;

   begin
      pragma Debug (Printk ("procedure Timed_Sleep called" & LF));

      Timedout := True;
      Yielded := False;
      --  ??? These two boolean seems not relevant here

      if Mode = Relative then
         Abs_Time := To_RTIME (Time) + Rt_Get_Time;
      else
         Abs_Time := To_RTIME (Time);
      end if;

      Self_ID.Common.LL.Resume_Time := Abs_Time;
      Self_ID.Common.LL.State := RT_TASK_DELAYED;

      R_Save_Flags (Flags);
      R_Cli;
      Delete_R (Self_ID);
      Insert_T (Self_ID);

      --  Check if the timer needs to be set

      if Timer_Queue.Common.LL.Succ = To_Address (Self_ID) then
         Rt_Set_Timer (Abs_Time);
      end if;

      --  Another way to do it
      --
      --  if Abs_Time <
      --    To_Task_ID (Timer_Queue.Common.LL.Succ).Common.LL.Resume_Time
      --  then
      --     Rt_Set_Timer (Abs_Time);
      --  end if;

      --  Arrange to unlock Self_ID's ATCB lock. see comments in Sleep

      if Self_ID.Common.LL.L.Owner = To_Address (Self_ID) then
         Self_ID.Common.LL.L.Owner := System.Null_Address;

         if Self_ID.Common.LL.Outer_Lock = Self_ID.Common.LL.L'Access then
            Self_ID.Common.LL.Active_Priority :=
              Self_ID.Common.Current_Priority;
            Self_ID.Common.LL.Outer_Lock := null;

         else
            Self_ID.Common.LL.Active_Priority :=
              Self_ID.Common.LL.L.Pre_Locking_Priority;
         end if;
      end if;

      R_Restore_Flags (Flags);
      Rt_Schedule;

      --  Before leaving, regain the lock

      Write_Lock (Self_ID);
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   --  This is for use in implementing delay statements, so we assume
   --  the caller is not abort-deferred and is holding no locks.
   --  Self_ID can only be awakened after the timeout, no Wakeup on it.

   procedure Timed_Delay
     (Self_ID  : Task_ID;
      Time     : Duration;
      Mode     : ST.Delay_Modes)
   is
      Flags      : Integer;
      Abs_Time   : RTIME;

   begin
      pragma Debug (Printk ("procedure Timed_Delay called" & LF));

      --  Only the little window between deferring abort and
      --  locking Self_ID is the reason we need to
      --  check for pending abort and priority change below! :(

      Write_Lock (Self_ID);

      --  Take the lock in case its ATCB needs to be modified

      if Mode = Relative then
         Abs_Time := To_RTIME (Time) + Rt_Get_Time;
      else
         Abs_Time := To_RTIME (Time);
      end if;

      Self_ID.Common.LL.Resume_Time := Abs_Time;
      Self_ID.Common.LL.State := RT_TASK_DELAYED;

      R_Save_Flags (Flags);
      R_Cli;
      Delete_R (Self_ID);
      Insert_T (Self_ID);

      --  Check if the timer needs to be set

      if Timer_Queue.Common.LL.Succ = To_Address (Self_ID) then
         Rt_Set_Timer (Abs_Time);
      end if;

      --  Arrange to unlock Self_ID's ATCB lock.
      --  Note that the code below is slightly different from Unlock, so
      --  it is more than inline it.

      if To_Task_ID (Self_ID.Common.LL.L.Owner) = Self_ID then
         Self_ID.Common.LL.L.Owner := System.Null_Address;

         if Self_ID.Common.LL.Outer_Lock = Self_ID.Common.LL.L'Access then
            Self_ID.Common.LL.Active_Priority :=
              Self_ID.Common.Current_Priority;
            Self_ID.Common.LL.Outer_Lock := null;

         else
            Self_ID.Common.LL.Active_Priority :=
              Self_ID.Common.LL.L.Pre_Locking_Priority;
         end if;
      end if;

      R_Restore_Flags (Flags);
      Rt_Schedule;
   end Timed_Delay;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   --  RTIME is represented as a 64-bit signed count of ticks,
   --  where there are 1_193_180 ticks per second.

   --  Let T be a count of ticks and N the corresponding count of nanoseconds.
   --  From the following relationship
   --    T / (ticks_per_second) = N / (ns_per_second)
   --  where ns_per_second is 1_000_000_000 (number of nanoseconds in
   --  a second), we get
   --    T * (ns_per_second) = N * (ticks_per_second)
   --  or
   --    T * 1_000_000_000   = N * 1_193_180
   --  which can be reduced to
   --    T * 50_000_000      = N * 59_659
   --  Let Nano_Count = 50_000_000 and Tick_Count = 59_659, we then have
   --    T * Nano_Count = N * Tick_Count

   --  IMPORTANT FACT:
   --  These numbers are small enough that we can do arithmetic
   --  on them without overflowing 64 bits.  To see this, observe

   --  10**3 = 1000 < 1024 = 2**10
   --  Tick_Count < 60 * 1000 < 64 * 1024 < 2**16
   --  Nano_Count < 50 * 1000 * 1000 < 64 * 1024 * 1024 < 2**26

   --  It follows that if 0 <= R < Tick_Count, we can compute
   --  R * Nano_Count < 2**42 without overflow in 64 bits.
   --  Similarly, if 0 <= R < Nano_Count, we can compute
   --  R * Tick_Count < 2**42 without overflow in 64 bits.

   --  GNAT represents Duration as a count of nanoseconds internally.

   --  To convert T from RTIME to Duration, let
   --    Q = T / Tick_Count, with truncation
   --    R = T - Q * Tick_Count, the remainder 0 <= R < Tick_Count
   --  so
   --    N * Tick_Count
   --      =  T * Nano_Count - Q * Tick_Count * Nano_Count
   --         + Q * Tick_Count * Nano_Count
   --      = (T - Q * Tick_Count) * Nano_Count
   --         + (Q * Nano_Count) * Tick_Count
   --      =  R * Nano_Count + (Q * Nano_Count) * Tick_Count

   --  Now, let
   --    Q1 = R * Nano_Count / Tick_Count, with truncation
   --    R1 = R * Nano_Count - Q1 * Tick_Count, 0 <= R1 <Tick_Count
   --    R * Nano_Count = Q1 * Tick_Count + R1
   --  so
   --    N * Tick_Count
   --      = R * Nano_Count + (Q * Nano_Count) * Tick_Count
   --      = Q1 * Tick_Count + R1 + (Q * Nano_Count) * Tick_Count
   --      = R1 + (Q * Nano_Count + Q1) * Tick_Count
   --  and
   --    N = Q * Nano_Count + Q1 + R1 /Tick_Count,
   --    where 0 <= R1 /Tick_Count < 1

   function To_Duration (T : RTIME) return Duration is
      Q, Q1, RN : RTIME;
   begin
      Q  := T / Tick_Count;
      RN := (T - Q * Tick_Count) * Nano_Count;
      Q1 := RN / Tick_Count;
      return Raw_Duration (Q * Nano_Count + Q1);
   end To_Duration;

   --  To convert D from Duration to RTIME,
   --  Let D be a Duration value, and N be the representation of D as an
   --  integer count of nanoseconds. Let
   --    Q = N / Nano_Count, with truncation
   --    R = N - Q * Nano_Count, the remainder 0 <= R < Nano_Count
   --  so
   --    T * Nano_Count
   --      = N * Tick_Count - Q * Nano_Count * Tick_Count
   --        + Q * Nano_Count * Tick_Count
   --      = (N - Q * Nano_Count) * Tick_Count
   --         + (Q * Tick_Count) * Nano_Count
   --      = R * Tick_Count + (Q * Tick_Count) * Nano_Count
   --  Now, let
   --    Q1 = R * Tick_Count / Nano_Count, with truncation
   --    R1 = R * Tick_Count - Q1 * Nano_Count, 0 <= R1 < Nano_Count
   --    R * Tick_Count = Q1 * Nano_Count + R1
   --  so
   --    T * Nano_Count
   --      = R * Tick_Count + (Q * Tick_Count) * Nano_Count
   --      = Q1 * Nano_Count + R1 + (Q * Tick_Count) * Nano_Count
   --      = (Q * Tick_Count + Q1) * Nano_Count + R1
   --  and
   --    T = Q * Tick_Count + Q1 + R1 / Nano_Count,
   --    where 0 <= R1 / Nano_Count < 1

   function To_RTIME (D : Duration) return RTIME is
      N : RTIME := Raw_RTIME (D);
      Q, Q1, RT : RTIME;

   begin
      Q  := N / Nano_Count;
      RT := (N - Q * Nano_Count) * Tick_Count;
      Q1 := RT / Nano_Count;
      return Q * Tick_Count + Q1;
   end To_RTIME;

   function Monotonic_Clock return Duration is
   begin
      pragma Debug (Printk ("procedure Clock called" & LF));

      return To_Duration (Rt_Get_Time);
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

   procedure Wakeup (T : Task_ID; Reason : ST.Task_States) is
      Flags : Integer;
   begin
      pragma Debug (Printk ("procedure Wakeup called" & LF));

      T.Common.State := Reason;
      T.Common.LL.State := RT_TASK_READY;

      R_Save_Flags (Flags);
      R_Cli;

      if Timer_Queue.Common.LL.Succ = To_Address (T) then
         --  T is the first task in Timer_Queue, further check

         if T.Common.LL.Succ = Timer_Queue'Address then
            --  T is the only task in Timer_Queue, so deactivate timer

            Rt_No_Timer;

         else
            --  T is the first task in Timer_Queue, so set timer to T's
            --  successor's Resume_Time

            Rt_Set_Timer (To_Task_ID (T.Common.LL.Succ).Common.LL.Resume_Time);
         end if;
      end if;

      Delete_T (T);

      --  If T is in Timer_Queue, T is removed. If not, nothing happened

      Insert_R (T);
      R_Restore_Flags (Flags);

      Rt_Schedule;
   end Wakeup;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
      Flags : Integer;
   begin
      pragma Debug (Printk ("procedure Yield called" & LF));

      pragma Assert (Current_Task /= To_Task_ID (Idle_Task'Address));

      R_Save_Flags (Flags);
      R_Cli;
      Delete_R (Current_Task);
      Insert_R (Current_Task);

      --  Remove Current_Task from the top of the Ready_Queue
      --  and reinsert it back at proper position (the end of
      --  tasks with the same active priority).

      R_Restore_Flags (Flags);
      Rt_Schedule;
   end Yield;

   ------------------
   -- Set_Priority --
   ------------------

   --  This version implicitly assume that T is the Current_Task

   procedure Set_Priority
     (T                   : Task_ID;
      Prio                : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
      Flags : Integer;
   begin
      pragma Debug (Printk ("procedure Set_Priority called" & LF));
      pragma Assert (T = Self);

      T.Common.Current_Priority := Prio;

      if T.Common.LL.Outer_Lock /= null then
         --  If the task T is holding any lock, defer the priority change
         --  until the lock is released. That is, T's Active_Priority will
         --  be set to Prio after it unlocks the outer-most lock. See
         --  Unlock for detail.
         --  Nothing needs to be done here for this case

         null;
      else
         --  If T is not holding any lock, change the priority right away.

         R_Save_Flags (Flags);
         R_Cli;
         T.Common.LL.Active_Priority := Prio;
         Delete_R (T);
         Insert_RF (T);

         --  Insert at the front of the queue for its new priority

         R_Restore_Flags (Flags);
      end if;

      Rt_Schedule;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_ID) return System.Any_Priority is
   begin
      pragma Debug (Printk ("procedure Get_Priority called" & LF));

      return T.Common.Current_Priority;
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   --  Do any target-specific initialization that is needed for a new task
   --  that has to be done by the task itself. This is called from the task
   --  wrapper, immediately after the task starts execution.

   procedure Enter_Task (Self_ID : Task_ID) is
   begin
      --  Use this as "hook" to re-enable interrupts.
      pragma Debug (Printk ("procedure Enter_Task called" & LF));

      R_Sti;
   end Enter_Task;

   ----------------
   --  New_ATCB  --
   ----------------

   function New_ATCB (Entry_Num : Task_Entry_Index) return Task_ID is
      T : constant Task_ID := Available_TCBs;
   begin
      pragma Debug (Printk ("function New_ATCB called" & LF));

      if Entry_Num /= 0 then
         --  We are preallocating all TCBs, so they must all have the
         --  same number of entries, which means the value of
         --  Entry_Num must be bounded.  We probably could choose a
         --  non-zero upper bound here, but the Ravenscar Profile
         --  specifies that there be no task entries.
         --  ???
         --  Later, do something better for recovery from this error.

         null;
      end if;

      if T /= null then
         Available_TCBs := To_Task_ID (T.Common.LL.Next);
         T.Common.LL.Next := System.Null_Address;
         Known_Tasks (T.Known_Tasks_Index) := T;
      end if;

      return T;
   end New_ATCB;

   ----------------------
   --  Initialize_TCB  --
   ----------------------

   procedure Initialize_TCB (Self_ID : Task_ID; Succeeded : out Boolean) is
   begin
      pragma Debug (Printk ("procedure Initialize_TCB called" & LF));

      --  Give the task a unique serial number.

      Self_ID.Serial_Number := Next_Serial_Number;
      Next_Serial_Number := Next_Serial_Number + 1;
      pragma Assert (Next_Serial_Number /= 0);

      Self_ID.Common.LL.L.Ceiling_Priority := System.Any_Priority'Last;
      Self_ID.Common.LL.L.Owner := System.Null_Address;
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
      Adjusted_Stack_Size : Integer;
      Bottom              : System.Address;
      Flags               : Integer;

   begin
      pragma Debug (Printk ("procedure Create_Task called" & LF));

      Succeeded := True;

      if T.Common.LL.Magic = RT_TASK_MAGIC then
         Succeeded := False;
         return;
      end if;

      if Stack_Size = Unspecified_Size then
         Adjusted_Stack_Size := To_Integer (Default_Stack_Size);
      elsif Stack_Size < Minimum_Stack_Size then
         Adjusted_Stack_Size := To_Integer (Minimum_Stack_Size);
      else
         Adjusted_Stack_Size := To_Integer (Stack_Size);
      end if;

      Bottom := Kmalloc (Adjusted_Stack_Size, GFP_KERNEL);

      if Bottom = System.Null_Address then
         Succeeded := False;
         return;
      end if;

      T.Common.LL.Uses_Fp          := 1;

      --  This field has to be reset to 1 if T uses FP unit. But, without
      --  a library-level procedure provided by this package, it cannot
      --  be set easily. So temporarily, set it to 1 (which means all the
      --  tasks will use FP unit. ???

      T.Common.LL.Magic            := RT_TASK_MAGIC;
      T.Common.LL.State            := RT_TASK_READY;
      T.Common.LL.Succ             := To_Address (T);
      T.Common.LL.Pred             := To_Address (T);
      T.Common.LL.Active_Priority  := Priority;
      T.Common.Current_Priority    := Priority;

      T.Common.LL.Stack_Bottom := Bottom;
      T.Common.LL.Stack := Bottom + Storage_Offset (Adjusted_Stack_Size);

      --  Store the value T into the stack, so that Task_wrapper (defined
      --  in System.Tasking.Stages) will find that value for its parameter
      --  Self_ID, when the scheduler eventually transfers control to the
      --  new task.

      T.Common.LL.Stack := T.Common.LL.Stack - Addr_Bytes;
      To_Address_Ptr (T.Common.LL.Stack).all := To_Address (T);

      --  Leave space for the return address, which will not be used,
      --  since the task wrapper should never return.

      T.Common.LL.Stack := T.Common.LL.Stack - Addr_Bytes;
      To_Address_Ptr (T.Common.LL.Stack).all := System.Null_Address;

      --  Put the entry point address of the task wrapper
      --  procedure on the new top of the stack.

      T.Common.LL.Stack := T.Common.LL.Stack - Addr_Bytes;
      To_Address_Ptr (T.Common.LL.Stack).all := Wrapper;

      R_Save_Flags (Flags);
      R_Cli;
      Insert_R (T);
      R_Restore_Flags (Flags);
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_ID) is
   begin
      pragma Debug (Printk ("procedure Finalize_TCB called" & LF));

      pragma Assert (T.Common.LL.Succ = To_Address (T));

      if T.Common.LL.State = RT_TASK_DORMANT then
         Known_Tasks (T.Known_Tasks_Index) := null;
         T.Common.LL.Next := To_Address (Available_TCBs);
         Available_TCBs := T;
         Kfree (T.Common.LL.Stack_Bottom);
      end if;
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   procedure Exit_Task is
      Flags : Integer;
   begin
      pragma Debug (Printk ("procedure Exit_Task called" & LF));
      pragma Assert (Current_Task /= To_Task_ID (Idle_Task'Address));
      pragma Assert (Current_Task /= Environment_Task_ID);

      R_Save_Flags (Flags);
      R_Cli;
      Current_Task.Common.LL.State := RT_TASK_DORMANT;
      Current_Task.Common.LL.Magic := 0;
      Delete_R (Current_Task);
      R_Restore_Flags (Flags);
      Rt_Schedule;
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   --  ??? Not implemented for now

   procedure Abort_Task (T : Task_ID) is
   --  Should cause T to raise Abort_Signal the next time it
   --  executes.
   --  ??? Can this ever be called when T = Current_Task?
   --  To be safe, do nothing in this case.
   begin
      pragma Debug (Printk ("procedure Abort_Task called" & LF));
      null;
   end Abort_Task;

   ----------------
   -- Check_Exit --
   ----------------

   --  Dummy versions. The only currently working versions is for solaris
   --  (native).
   --  We should probably copy the working versions over from the Solaris
   --  version of this package, with any appropriate changes, since without
   --  the checks on it will probably be nearly impossible to debug the
   --  run-time system.

   --  Not implemented for now

   function Check_Exit (Self_ID : Task_ID) return Boolean is
   begin
      pragma Debug (Printk ("function Check_Exit called" & LF));

      return True;
   end Check_Exit;

   --------------------
   -- Check_No_Locks --
   --------------------

   function Check_No_Locks (Self_ID : Task_ID) return Boolean is
   begin
      pragma Debug (Printk ("function Check_No_Locks called" & LF));

      if Self_ID.Common.LL.Outer_Lock = null then
         return True;
      else
         return False;
      end if;
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

   -----------------
   -- Stack_Guard --
   -----------------

   --  Not implemented for now

   procedure Stack_Guard (T : Task_ID; On : Boolean) is
   begin
      null;
   end Stack_Guard;

   --------------------
   -- Get_Thread_Id  --
   --------------------

   function Get_Thread_Id (T : Task_ID) return OSI.Thread_Id is
   begin
      return To_Address (T);
   end Get_Thread_Id;

   ------------------
   -- Suspend_Task --
   ------------------

   function Suspend_Task
     (T           : Task_ID;
      Thread_Self : OSI.Thread_Id) return Boolean is
   begin
      return False;
   end Suspend_Task;

   -----------------
   -- Resume_Task --
   -----------------

   function Resume_Task
     (T           : ST.Task_ID;
      Thread_Self : OSI.Thread_Id) return Boolean is
   begin
      return False;
   end Resume_Task;

   -----------------
   -- Init_Module --
   -----------------

   function Init_Module return Integer is
      procedure adainit;
      pragma Import (C, adainit);

   begin
      adainit;
      In_Elab_Code := False;
      Set_Priority (Environment_Task_ID, Any_Priority'First);
      return 0;
   end Init_Module;

   --------------------
   -- Cleanup_Module --
   --------------------

   procedure Cleanup_Module is
      procedure adafinal;
      pragma Import (C, adafinal);

   begin
      adafinal;
   end Cleanup_Module;

   ----------------
   -- Initialize --
   ----------------

   --  The environment task is "special". The TCB of the environment task is
   --  not in the TCB_Array above. Logically, all initialization code for the
   --  runtime system is executed by the environment task, but until the
   --  environment task has initialized its own TCB we dare not execute any
   --  calls that try to access the TCB of Current_Task. It is allocated by
   --  target-independent runtime system code, in System.Tasking.Initializa-
   --  tion.Init_RTS, before the call to this procedure Initialize. The
   --  target-independent runtime system initializes all the components that
   --  are target-independent, but this package needs to be given a chance to
   --  initialize the target-dependent data.  We do that in this procedure.

   --  In the present implementation, Environment_Task is set to be the
   --  regular GNU/Linux kernel task.

   procedure Initialize (Environment_Task : Task_ID) is
   begin
      pragma Debug (Printk ("procedure Initialize called" & LF));

      Environment_Task_ID := Environment_Task;

      --  Build the list of available ATCB's.

      Available_TCBs := To_Task_ID (TCB_Array (1)'Address);

      for J in TCB_Array'First + 1 .. TCB_Array'Last - 1 loop
         --  Note that the zeroth element in TCB_Array is not used, see
         --  comments following the declaration of TCB_Array

         TCB_Array (J).Common.LL.Next := TCB_Array (J + 1)'Address;
      end loop;

      TCB_Array (TCB_Array'Last).Common.LL.Next := System.Null_Address;

      --  Initialize the idle task, which is the head of Ready_Queue.

      Idle_Task.Common.LL.Magic := RT_TASK_MAGIC;
      Idle_Task.Common.LL.State := RT_TASK_READY;
      Idle_Task.Common.Current_Priority := System.Any_Priority'First;
      Idle_Task.Common.LL.Active_Priority  := System.Any_Priority'First;
      Idle_Task.Common.LL.Succ := Idle_Task'Address;
      Idle_Task.Common.LL.Pred := Idle_Task'Address;

      --  Initialize the regular GNU/Linux kernel task.

      Environment_Task.Common.LL.Magic := RT_TASK_MAGIC;
      Environment_Task.Common.LL.State := RT_TASK_READY;
      Environment_Task.Common.Current_Priority := System.Any_Priority'First;
      Environment_Task.Common.LL.Active_Priority  := System.Any_Priority'First;
      Environment_Task.Common.LL.Succ := To_Address (Environment_Task);
      Environment_Task.Common.LL.Pred := To_Address (Environment_Task);

      --  Initialize the head of Timer_Queue

      Timer_Queue.Common.LL.Succ        := Timer_Queue'Address;
      Timer_Queue.Common.LL.Pred        := Timer_Queue'Address;
      Timer_Queue.Common.LL.Resume_Time := Max_Sensible_Delay;

      --  Set the current task to regular GNU/Linux kernel task

      Current_Task := Environment_Task;

      --  Set Timer_Wrapper to be the timer handler

      Rt_Free_Timer;
      Rt_Request_Timer (Timer_Wrapper'Address);

      --  Initialize the lock used to synchronize chain of all ATCBs.

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);

      --  Single_Lock isn't supported in this configuration
      pragma Assert (not Single_Lock);

      Enter_Task (Environment_Task);
   end Initialize;

end System.Task_Primitives.Operations;
