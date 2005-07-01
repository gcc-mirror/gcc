------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

--  This package provides necessary type definitions for compiler interface

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

with Ada.Exceptions;
--  Used for:  Exception_Id

with System.Parameters;
--  used for Size_Type

with System.Task_Info;
--  used for Task_Info_Type

with System.Soft_Links;
--  used for TSD

with System.Task_Primitives;
--  used for Private_Data

with Unchecked_Conversion;

package System.Tasking is

   -------------------
   -- Locking Rules --
   -------------------

   --  The following rules must be followed at all times, to prevent
   --  deadlock and generally ensure correct operation of locking.

   --  Never lock a lock unless abort is deferred

   --  Never undefer abort while holding a lock

   --  Overlapping critical sections must be properly nested, and locks must
   --  be released in LIFO order. e.g., the following is not allowed:

   --         Lock (X);
   --         ...
   --         Lock (Y);
   --         ...
   --         Unlock (X);
   --         ...
   --         Unlock (Y);

   --  Locks with lower (smaller) level number cannot be locked
   --  while holding a lock with a higher level number. (The level

   --  1. System.Tasking.PO_Simple.Protection.L (any PO lock)
   --  2. System.Tasking.Initialization.Global_Task_Lock (in body)
   --  3. System.Task_Primitives.Operations.Single_RTS_Lock
   --  4. System.Tasking.Ada_Task_Control_Block.LL.L (any TCB lock)

   --  Clearly, there can be no circular chain of hold-and-wait
   --  relationships involving locks in different ordering levels.

   --  We used to have Global_Task_Lock before Protection.L but this was
   --  clearly wrong since there can be calls to "new" inside protected
   --  operations. The new ordering prevents these failures.

   --  Sometimes we need to hold two ATCB locks at the same time. To allow us
   --  to order the locking, each ATCB is given a unique serial number. If one
   --  needs to hold locks on several ATCBs at once, the locks with lower
   --  serial numbers must be locked first.

   --  We don't always need to check the serial numbers, since the serial
   --  numbers are assigned sequentially, and so:

   --  . The parent of a task always has a lower serial number.
   --  . The activator of a task always has a lower serial number.
   --  . The environment task has a lower serial number than any other task.
   --  . If the activator of a task is different from the task's parent,
   --    the parent always has a lower serial number than the activator.

   ---------------------------------
   -- Task_Id related definitions --
   ---------------------------------

   type Ada_Task_Control_Block;

   type Task_Id is access all Ada_Task_Control_Block;

   Null_Task : constant Task_Id;

   type Task_List is array (Positive range <>) of Task_Id;

   function Self return Task_Id;
   pragma Inline (Self);
   --  This is the compiler interface version of this function. Do not call
   --  from the run-time system.

   function To_Task_Id is new Unchecked_Conversion (System.Address, Task_Id);
   function To_Address is new Unchecked_Conversion (Task_Id, System.Address);

   -----------------------
   -- Enumeration types --
   -----------------------

   type Task_States is
     (Unactivated,
      --  Task has been created but has not been activated.
      --  It cannot be executing.

      --  Active states
      --  For all states from here down, the task has been activated.
      --  For all states from here down, except for Terminated, the task
      --  may be executing.
      --  Activator = null iff it has not yet completed activating.

      --  For all states from here down,
      --  the task has been activated, and may be executing.

      Runnable,
      --  Task is not blocked for any reason known to Ada.
      --  (It may be waiting for a mutex, though.)
      --  It is conceptually "executing" in normal mode.

      Terminated,
      --  The task is terminated, in the sense of ARM 9.3 (5).
      --  Any dependents that were waiting on terminate
      --  alternatives have been awakened and have terminated themselves.

      Activator_Sleep,
      --  Task is waiting for created tasks to complete activation

      Acceptor_Sleep,
      --  Task is waiting on an accept or selective wait statement

      Entry_Caller_Sleep,
      --  Task is waiting on an entry call

      Async_Select_Sleep,
      --  Task is waiting to start the abortable part of an
      --  asynchronous select statement.

      Delay_Sleep,
      --  Task is waiting on a select statement with only a delay
      --  alternative open.

      Master_Completion_Sleep,
      --  Master completion has two phases.
      --  In Phase 1 the task is sleeping in Complete_Master
      --  having completed a master within itself,
      --  and is waiting for the tasks dependent on that master to become
      --  terminated or waiting on a terminate Phase.

      Master_Phase_2_Sleep,
      --  In Phase 2 the task is sleeping in Complete_Master
      --  waiting for tasks on terminate alternatives to finish
      --  terminating.

      --  The following are special uses of sleep, for server tasks
      --  within the run-time system.

      Interrupt_Server_Idle_Sleep,
      Interrupt_Server_Blocked_Interrupt_Sleep,
      Timer_Server_Sleep,
      AST_Server_Sleep,

      Asynchronous_Hold,
      --  The task has been held by Asynchronous_Task_Control.Hold_Task

      Interrupt_Server_Blocked_On_Event_Flag
      --  The task has been blocked on a system call waiting for the
      --  completion event.
     );

   type Call_Modes is
     (Simple_Call, Conditional_Call, Asynchronous_Call, Timed_Call);

   type Select_Modes is (Simple_Mode, Else_Mode, Terminate_Mode, Delay_Mode);

   subtype Delay_Modes is Integer;

   -------------------------------
   -- Entry related definitions --
   -------------------------------

   Null_Entry : constant := 0;

   Max_Entry : constant := Integer'Last;

   Interrupt_Entry : constant := -2;

   Cancelled_Entry : constant := -1;

   type Entry_Index is range Interrupt_Entry .. Max_Entry;

   Null_Task_Entry : constant := Null_Entry;

   Max_Task_Entry : constant := Max_Entry;

   type Task_Entry_Index is new Entry_Index
     range Null_Task_Entry .. Max_Task_Entry;

   type Entry_Call_Record;

   type Entry_Call_Link is access all Entry_Call_Record;

   type Entry_Queue is record
      Head : Entry_Call_Link;
      Tail : Entry_Call_Link;
   end record;

   type Task_Entry_Queue_Array is
     array (Task_Entry_Index range <>) of Entry_Queue;

   ----------------------------------
   -- Entry_Call_Record definition --
   ----------------------------------

   type Entry_Call_State is
     (Never_Abortable,
      --  the call is not abortable, and never can be

      Not_Yet_Abortable,
      --  the call is not abortable, but may become so

      Was_Abortable,
      --  the call is not abortable, but once was

      Now_Abortable,
      --  the call is abortable

      Done,
      --  the call has been completed

      Cancelled
      --  the call was asynchronous, and was cancelled
     );

   --  Never_Abortable is used for calls that are made in a abort
   --  deferred region (see ARM 9.8(5-11), 9.8 (20)).
   --  Such a call is never abortable.

   --  The Was_ vs. Not_Yet_ distinction is needed to decide whether it
   --  is OK to advance into the abortable part of an async. select stmt.
   --  That is allowed iff the mode is Now_ or Was_.

   --  Done indicates the call has been completed, without cancellation,
   --  or no call has been made yet at this ATC nesting level,
   --  and so aborting the call is no longer an issue.
   --  Completion of the call does not necessarily indicate "success";
   --  the call may be returning an exception if Exception_To_Raise is
   --  non-null.

   --  Cancelled indicates the call was cancelled,
   --  and so aborting the call is no longer an issue.

   --  The call is on an entry queue unless
   --  State >= Done, in which case it may or may not be still Onqueue.

   --  Please do not modify the order of the values, without checking
   --  all uses of this type. We rely on partial "monotonicity" of
   --  Entry_Call_Record.State to avoid locking when we access this
   --  value for certain tests. In particular:

   --  1)  Once State >= Done, we can rely that the call has been
   --      completed. If State >= Done, it will not
   --      change until the task does another entry call at this level.

   --  2)  Once State >= Was_Abortable, we can rely that the call has
   --      been queued abortably at least once, and so the check for
   --      whether it is OK to advance to the abortable part of an
   --      async. select statement does not need to lock anything.

   type Restricted_Entry_Call_Record is record
      Self : Task_Id;
      --  ID of the caller

      Mode : Call_Modes;

      State : Entry_Call_State;
      pragma Atomic (State);
      --  Indicates part of the state of the call.
      --
      --  Protection: If the call is not on a queue, it should only be
      --  accessed by Self, and Self does not need any lock to modify this
      --  field.
      --
      --  Once the call is on a queue, the value should be something other
      --  than Done unless it is cancelled, and access is controller by the
      --  "server" of the queue -- i.e., the lock of Checked_To_Protection
      --  (Call_Target) if the call record is on the queue of a PO, or the
      --  lock of Called_Target if the call is on the queue of a task. See
      --  comments on type declaration for more details.

      Uninterpreted_Data : System.Address;
      --  Data passed by the compiler

      Exception_To_Raise : Ada.Exceptions.Exception_Id;
      --  The exception to raise once this call has been completed without
      --  being aborted.
   end record;
   pragma Suppress_Initialization (Restricted_Entry_Call_Record);

   ------------------------------------
   -- Task related other definitions --
   ------------------------------------

   type Activation_Chain is limited private;
   --  Comment required ???

   type Activation_Chain_Access is access all Activation_Chain;
   --  Comment required ???

   type Task_Procedure_Access is access procedure (Arg : System.Address);

   type Access_Boolean is access all Boolean;

   Detect_Blocking : constant Boolean;
   --  Boolean constant set True iff Detect_Blocking is active

   ----------------------------------------------
   -- Ada_Task_Control_Block (ATCB) definition --
   ----------------------------------------------

   --  Notes on protection (synchronization) of TRTS data structures

   --  Any field of the TCB can be written by the activator of a task when the
   --  task is created, since no other task can access the new task's
   --  state until creation is complete.

   --  The protection for each field is described in a comment starting with
   --  "Protection:".

   --  When a lock is used to protect an ATCB field, this lock is simply named

   --  Some protection is described in terms of tasks related to the
   --  ATCB being protected. These are:

   --    Self:      The task which is controlled by this ATCB
   --    Acceptor:  A task accepting a call from Self
   --    Caller:    A task calling an entry of Self
   --    Parent:    The task executing the master on which Self depends
   --    Dependent: A task dependent on Self
   --    Activator: The task that created Self and initiated its activation
   --    Created:   A task created and activated by Self

   --  Note: The order of the fields is important to implement efficiently
   --  tasking support under gdb.
   --  Currently gdb relies on the order of the State, Parent, Base_Priority,
   --  Task_Image, Task_Image_Len, Call and LL fields.

   -------------------------
   -- Common ATCB section --
   -------------------------

   --  Section used by all GNARL implementations (regular and restricted)

   type Common_ATCB is record
      State : Task_States;
      pragma Atomic (State);
      --  Encodes some basic information about the state of a task,
      --  including whether it has been activated, whether it is sleeping,
      --  and whether it is terminated.
      --
      --  Protection: Self.L

      Parent : Task_Id;
      --  The task on which this task depends.
      --  See also Master_Level and Master_Within.

      Base_Priority : System.Any_Priority;
      --  Base priority, not changed during entry calls, only changed
      --  via dynamic priorities package.
      --
      --  Protection: Only written by Self, accessed by anyone

      Current_Priority : System.Any_Priority;
      --  Active priority, except that the effects of protected object
      --  priority ceilings are not reflected. This only reflects explicit
      --  priority changes and priority inherited through task activation
      --  and rendezvous.
      --
      --  Ada 95 notes: In Ada 95, this field will be transferred to the
      --  Priority field of an Entry_Calls component when an entry call
      --  is initiated. The Priority of the Entry_Calls component will not
      --  change for the duration of the call. The accepting task can
      --  use it to boost its own priority without fear of its changing in
      --  the meantime.
      --
      --  This can safely be used in the priority ordering
      --  of entry queues. Once a call is queued, its priority does not
      --  change.
      --
      --  Since an entry call cannot be made while executing
      --  a protected action, the priority of a task will never reflect a
      --  priority ceiling change at the point of an entry call.
      --
      --  Protection: Only written by Self, and only accessed when Acceptor
      --  accepts an entry or when Created activates, at which points Self is
      --  suspended.

      Protected_Action_Nesting : Natural;
      pragma Atomic (Protected_Action_Nesting);
      --  The dynamic level of protected action nesting for this task. This
      --  field is needed for checking whether potentially blocking operations
      --  are invoked from protected actions. pragma Atomic is used because it
      --  can be read/written from protected interrupt handlers.

      Task_Image : String (1 .. 32);
      --  Hold a string that provides a readable id for task,
      --  built from the variable of which it is a value or component.

      Task_Image_Len : Natural;
      --  Actual length of Task_Image

      Call : Entry_Call_Link;
      --  The entry call that has been accepted by this task.
      --
      --  Protection: Self.L. Self will modify this field when Self.Accepting
      --  is False, and will not need the mutex to do so. Once a task sets
      --  Pending_ATC_Level = 0, no other task can access this field.

      LL : aliased Task_Primitives.Private_Data;
      --  Control block used by the underlying low-level tasking service
      --  (GNULLI).
      --
      --  Protection: This is used only by the GNULLI implementation, which
      --  takes care of all of its synchronization.

      Task_Arg : System.Address;
      --  The argument to task procedure. Provide a handle for discriminant
      --  information
      --
      --  Protection: Part of the synchronization between Self and Activator.
      --  Activator writes it, once, before Self starts executing. Thereafter,
      --  Self only reads it.

      Task_Entry_Point : Task_Procedure_Access;
      --  Information needed to call the procedure containing the code for
      --  the body of this task.
      --
      --  Protection: Part of the synchronization between Self and Activator.
      --  Activator writes it, once, before Self starts executing. Self reads
      --  it, once, as part of its execution.

      Compiler_Data : System.Soft_Links.TSD;
      --  Task-specific data needed by the compiler to store per-task
      --  structures.
      --
      --  Protection: Only accessed by Self

      All_Tasks_Link : Task_Id;
      --  Used to link this task to the list of all tasks in the system
      --
      --  Protection: RTS_Lock

      Activation_Link : Task_Id;
      --  Used to link this task to a list of tasks to be activated
      --
      --  Protection: Only used by Activator

      Activator : Task_Id;
      --  The task that created this task, either by declaring it as a task
      --  object or by executing a task allocator. The value is null iff Self
      --  has completed activation.
      --
      --  Protection: Set by Activator before Self is activated, and only read
      --  and modified by Self after that.

      Wait_Count : Integer;
      --  This count is used by a task that is waiting for other tasks. At all
      --  other times, the value should be zero. It is used differently in
      --  several different states. Since a task cannot be in more than one of
      --  these states at the same time, a single counter suffices.
      --
      --  Protection: Self.L

      --  Activator_Sleep

      --  This is the number of tasks that this task is activating, i.e. the
      --  children that have started activation but have not completed it.
      --
      --  Protection: Self.L and Created.L. Both mutexes must be locked, since
      --  Self.Activation_Count and Created.State must be synchronized.

      --  Master_Completion_Sleep (phase 1)

      --  This is the number dependent tasks of a master being completed by
      --  Self that are not activated, not terminated, and not waiting on a
      --  terminate alternative.

      --  Master_Completion_2_Sleep (phase 2)

      --  This is the count of tasks dependent on a master being completed by
      --  Self which are waiting on a terminate alternative.

      Elaborated : Access_Boolean;
      --  Pointer to a flag indicating that this task's body has been
      --  elaborated. The flag is created and managed by the
      --  compiler-generated code.
      --
      --  Protection: The field itself is only accessed by Activator. The flag
      --  that it points to is updated by Master and read by Activator; access
      --  is assumed to be atomic.

      Activation_Failed : Boolean;
      --  Set to True if activation of a chain of tasks fails,
      --  so that the activator should raise Tasking_Error.

      Task_Info : System.Task_Info.Task_Info_Type;
      --  System-specific attributes of the task as specified by the
      --  Task_Info pragma.
   end record;

   ---------------------------------------
   -- Restricted_Ada_Task_Control_Block --
   ---------------------------------------

   --  This type should only be used by the restricted GNARLI and by
   --  restricted GNULL implementations to allocate an ATCB (see
   --  System.Task_Primitives.Operations.New_ATCB) that will take
   --  significantly less memory.

   --  Note that the restricted GNARLI should only access fields that are
   --  present in the Restricted_Ada_Task_Control_Block structure.

   type Restricted_Ada_Task_Control_Block (Entry_Num : Task_Entry_Index) is
   record
      Common : Common_ATCB;
      --  The common part between various tasking implementations

      Entry_Call : aliased Restricted_Entry_Call_Record;
      --  Protection: This field is used on entry call "queues" associated
      --  with protected objects, and is protected by the protected object
      --  lock.
   end record;
   pragma Suppress_Initialization (Restricted_Ada_Task_Control_Block);

   Interrupt_Manager_ID : Task_Id;
   --  This task ID is declared here to break circular dependencies.
   --  Also declare Interrupt_Manager_ID after Task_Id is known, to avoid
   --  generating unneeded finalization code.

   -----------------------
   -- List of all Tasks --
   -----------------------

   All_Tasks_List : Task_Id;
   --  Global linked list of all tasks

   ------------------------------------------
   -- Regular (non restricted) definitions --
   ------------------------------------------

   --------------------------------
   -- Master Related Definitions --
   --------------------------------

   subtype Master_Level is Integer;
   subtype Master_ID is Master_Level;

   --  Normally, a task starts out with internal master nesting level one
   --  larger than external master nesting level. It is incremented to one by
   --  Enter_Master, which is called in the task body only if the compiler
   --  thinks the task may have dependent tasks. It is set to for the
   --  environment task, the level 2 is reserved for server tasks of the
   --  run-time system (the so called "independent tasks"), and the level 3 is
   --  for the library level tasks.

   Environment_Task_Level : constant Master_Level := 1;
   Independent_Task_Level : constant Master_Level := 2;
   Library_Task_Level     : constant Master_Level := 3;

   ------------------------------
   -- Task size, priority info --
   ------------------------------

   Unspecified_Priority : constant Integer := System.Priority'First - 1;

   Priority_Not_Boosted : constant Integer := System.Priority'First - 1;
   --  Definition of Priority actually has to come from the RTS configuration

   subtype Rendezvous_Priority is Integer
     range Priority_Not_Boosted .. System.Any_Priority'Last;

   ------------------------------------
   -- Rendezvous related definitions --
   ------------------------------------

   No_Rendezvous : constant := 0;

   Max_Select : constant Integer := Integer'Last;
   --  RTS-defined

   subtype Select_Index is Integer range No_Rendezvous .. Max_Select;
   --   type Select_Index is range No_Rendezvous .. Max_Select;

   subtype Positive_Select_Index is
     Select_Index range 1 .. Select_Index'Last;

   type Accept_Alternative is record
      Null_Body : Boolean;
      S         : Task_Entry_Index;
   end record;

   type Accept_List is
     array (Positive_Select_Index range <>) of Accept_Alternative;

   type Accept_List_Access is access constant Accept_List;

   -----------------------------------
   -- ATC_Level related definitions --
   -----------------------------------

   Max_ATC_Nesting : constant Natural := 20;

   subtype ATC_Level_Base is Integer range 0 .. Max_ATC_Nesting;

   ATC_Level_Infinity : constant ATC_Level_Base := ATC_Level_Base'Last;

   subtype ATC_Level is ATC_Level_Base range 0 .. ATC_Level_Base'Last - 1;

   subtype ATC_Level_Index is ATC_Level range 1 .. ATC_Level'Last;

   ----------------------------------
   -- Entry_Call_Record definition --
   ----------------------------------

   type Entry_Call_Record is record
      Self  : Task_Id;
      --  ID of the caller

      Mode : Call_Modes;

      State : Entry_Call_State;
      pragma Atomic (State);
      --  Indicates part of the state of the call
      --
      --  Protection: If the call is not on a queue, it should only be
      --  accessed by Self, and Self does not need any lock to modify this
      --  field. Once the call is on a queue, the value should be something
      --  other than Done unless it is cancelled, and access is controller by
      --  the "server" of the queue -- i.e., the lock of Checked_To_Protection
      --  (Call_Target) if the call record is on the queue of a PO, or the
      --  lock of Called_Target if the call is on the queue of a task. See
      --  comments on type declaration for more details.

      Uninterpreted_Data : System.Address;
      --  Data passed by the compiler

      Exception_To_Raise : Ada.Exceptions.Exception_Id;
      --  The exception to raise once this call has been completed without
      --  being aborted.

      Prev : Entry_Call_Link;

      Next : Entry_Call_Link;

      Level : ATC_Level;
      --  One of Self and Level are redundant in this implementation, since
      --  each Entry_Call_Record is at Self.Entry_Calls (Level). Since we must
      --  have access to the entry call record to be reading this, we could
      --  get Self from Level, or Level from Self. However, this requires
      --  non-portable address arithmetic.

      E : Entry_Index;

      Prio : System.Any_Priority;

      --  The above fields are those that there may be some hope of packing.
      --  They are gathered together to allow for compilers that lay records
      --  out contiguously, to allow for such packing.

      Called_Task : Task_Id;
      pragma Atomic (Called_Task);
      --  Use for task entry calls. The value is null if the call record is
      --  not in use. Conversely, unless State is Done and Onqueue is false,
      --  Called_Task points to an ATCB.
      --
      --  Protection:  Called_Task.L

      Called_PO : System.Address;
      pragma Atomic (Called_PO);
      --  Similar to Called_Task but for protected objects
      --
      --  Note that the previous implementation tried to merge both
      --  Called_Task and Called_PO but this ended up in many unexpected
      --  complications (e.g having to add a magic number in the ATCB, which
      --  caused gdb lots of confusion) with no real gain since the
      --  Lock_Server implementation still need to loop around chasing for
      --  pointer changes even with a single pointer.

      Acceptor_Prev_Call : Entry_Call_Link;
      --  For task entry calls only

      Acceptor_Prev_Priority : Rendezvous_Priority := Priority_Not_Boosted;
      --  For task entry calls only. The priority of the most recent prior
      --  call being serviced. For protected entry calls, this function should
      --  be performed by GNULLI ceiling locking.

      Cancellation_Attempted : Boolean := False;
      pragma Atomic (Cancellation_Attempted);
      --  Cancellation of the call has been attempted.
      --  Consider merging this into State???

      Requeue_With_Abort : Boolean := False;
      --  Temporary to tell caller whether requeue is with abort.
      --  Find a better way of doing this ???

      Needs_Requeue : Boolean := False;
      --  Temporary to tell acceptor of task entry call that
      --  Exceptional_Complete_Rendezvous needs to do requeue.
   end record;

   ------------------------------------
   -- Task related other definitions --
   ------------------------------------

   type Access_Address is access all System.Address;
   --  Comment on what this is used for ???

   pragma No_Strict_Aliasing (Access_Address);
   --  This type is used in contexts where aliasing may be an issue (see
   --  for example s-tataat.adb), so we avoid any incorrect aliasing
   --  assumptions.

   ----------------------------------------------
   -- Ada_Task_Control_Block (ATCB) definition --
   ----------------------------------------------

   type Entry_Call_Array is array (ATC_Level_Index) of
     aliased Entry_Call_Record;

   type Direct_Index is range 0 .. Parameters.Default_Attribute_Count;
   subtype Direct_Index_Range is Direct_Index range 1 .. Direct_Index'Last;
   --  Attributes with indices in this range are stored directly in the task
   --  control block. Such attributes must be Address-sized. Other attributes
   --  will be held in dynamically allocated records chained off of the task
   --  control block.

   type Direct_Attribute_Element is mod Memory_Size;
   pragma Atomic (Direct_Attribute_Element);

   type Direct_Attribute_Array is
     array (Direct_Index_Range) of aliased Direct_Attribute_Element;

   type Direct_Index_Vector is mod 2 ** Parameters.Default_Attribute_Count;
   --  This is a bit-vector type, used to store information about
   --  the usage of the direct attribute fields.

   type Task_Serial_Number is mod 2 ** 64;
   --  Used to give each task a unique serial number

   type Ada_Task_Control_Block (Entry_Num : Task_Entry_Index) is record
      Common : Common_ATCB;
      --  The common part between various tasking implementations

      Entry_Calls : Entry_Call_Array;
      --  An array of entry calls
      --
      --  Protection: The elements of this array are on entry call queues
      --  associated with protected objects or task entries, and are protected
      --  by the protected object lock or Acceptor.L, respectively.

      New_Base_Priority : System.Any_Priority;
      --  New value for Base_Priority (for dynamic priorities package)
      --
      --  Protection: Self.L

      Global_Task_Lock_Nesting : Natural := 0;
      --  This is the current nesting level of calls to
      --  System.Tasking.Stages.Lock_Task_T. This allows a task to call
      --  Lock_Task_T multiple times without deadlocking. A task only locks
      --  All_Task_Lock when its All_Tasks_Nesting goes from 0 to 1, and only
      --  unlocked when it goes from 1 to 0.
      --
      --  Protection: Only accessed by Self

      Open_Accepts : Accept_List_Access;
      --  This points to the Open_Accepts array of accept alternatives passed
      --  to the RTS by the compiler-generated code to Selective_Wait. It is
      --  non-null iff this task is ready to accept an entry call.
      --
      --  Protection: Self.L

      Chosen_Index : Select_Index;
      --  The index in Open_Accepts of the entry call accepted by a selective
      --  wait executed by this task.
      --
      --  Protection: Written by both Self and Caller. Usually protected by
      --  Self.L. However, once the selection is known to have been written it
      --  can be accessed without protection. This happens after Self has
      --  updated it itself using information from a suspended Caller, or
      --  after Caller has updated it and awakened Self.

      Master_of_Task : Master_Level;
      --  The task executing the master of this task, and the ID of this task's
      --  master (unique only among masters currently active within Parent).
      --
      --  Protection: Set by Activator before Self is activated, and read
      --  after Self is activated.

      Master_Within : Master_Level;
      --  The ID of the master currently executing within this task; that is,
      --  the most deeply nested currently active master.
      --
      --  Protection: Only written by Self, and only read by Self or by
      --  dependents when Self is attempting to exit a master. Since Self will
      --  not write this field until the master is complete, the
      --  synchronization should be adequate to prevent races.

      Alive_Count : Integer := 0;
      --  Number of tasks directly dependent on this task (including itself)
      --  that are still "alive", i.e. not terminated.
      --
      --  Protection: Self.L

      Awake_Count : Integer := 0;
      --  Number of tasks directly dependent on this task (including itself)
      --  still "awake", i.e., are not terminated and not waiting on a
      --  terminate alternative.
      --
      --  Invariant: Awake_Count <= Alive_Count

      --  Protection: Self.L

      --  Beginning of flags

      Aborting : Boolean := False;
      pragma Atomic (Aborting);
      --  Self is in the process of aborting. While set, prevents multiple
      --  abort signals from being sent by different aborter while abort
      --  is acted upon. This is essential since an aborter which calls
      --  Abort_To_Level could set the Pending_ATC_Level to yet a lower level
      --  (than the current level), may be preempted and would send the
      --  abort signal when resuming execution. At this point, the abortee
      --  may have completed abort to the proper level such that the
      --  signal (and resulting abort exception) are not handled any more.
      --  In other words, the flag prevents a race between multiple aborters
      --
      --  Protection: protected by atomic access.

      ATC_Hack : Boolean := False;
      pragma Atomic (ATC_Hack);
      --  ?????
      --  Temporary fix, to allow Undefer_Abort to reset Aborting in the
      --  handler for Abort_Signal that encloses an async. entry call.
      --  For the longer term, this should be done via code in the
      --  handler itself.

      Callable : Boolean := True;
      --  It is OK to call entries of this task

      Dependents_Aborted : Boolean := False;
      --  This is set to True by whichever task takes responsibility for
      --  aborting the dependents of this task.
      --
      --  Protection: Self.L

      Interrupt_Entry : Boolean := False;
      --  Indicates if one or more Interrupt Entries are attached to the task.
      --  This flag is needed for cleaning up the Interrupt Entry bindings.

      Pending_Action : Boolean := False;
      --  Unified flag indicating some action needs to be take when abort
      --  next becomes undeferred. Currently set if:
      --  . Pending_Priority_Change is set
      --  . Pending_ATC_Level is changed
      --  . Requeue involving POs
      --    (Abortable field may have changed and the Wait_Until_Abortable
      --     has to recheck the abortable status of the call.)
      --  . Exception_To_Raise is non-null
      --
      --  Protection: Self.L
      --
      --  This should never be reset back to False outside of the procedure
      --  Do_Pending_Action, which is called by Undefer_Abort. It should only
      --  be set to True by Set_Priority and Abort_To_Level.

      Pending_Priority_Change : Boolean := False;
      --  Flag to indicate pending priority change (for dynamic priorities
      --  package). The base priority is updated on the next abort
      --  completion point (aka. synchronization point).
      --
      --  Protection: Self.L

      Terminate_Alternative : Boolean := False;
      --  Task is accepting Select with Terminate Alternative
      --
      --  Protection: Self.L

      --  End of flags

      --  Beginning of counts

      ATC_Nesting_Level : ATC_Level := 1;
      --  The dynamic level of ATC nesting (currently executing nested
      --  asynchronous select statements) in this task.

      --  Protection: Self_ID.L. Only Self reads or updates this field.
      --  Decrementing it deallocates an Entry_Calls component, and care must
      --  be taken that all references to that component are eliminated before
      --  doing the decrement. This in turn will require locking a protected
      --  object (for a protected entry call) or the Acceptor's lock (for a
      --  task entry call). No other task should attempt to read or modify
      --  this value.

      Deferral_Level : Natural := 1;
      --  This is the number of times that Defer_Abortion has been called by
      --  this task without a matching Undefer_Abortion call. Abortion is only
      --  allowed when this zero. It is initially 1, to protect the task at
      --  startup.

      --  Protection: Only updated by Self; access assumed to be atomic

      Pending_ATC_Level : ATC_Level_Base := ATC_Level_Infinity;
      --  The ATC level to which this task is currently being aborted. If the
      --  value is zero, the entire task has "completed". That may be via
      --  abort, exception propagation, or normal exit. If the value is
      --  ATC_Level_Infinity, the task is not being aborted to any level. If
      --  the value is positive, the task has not completed. This should ONLY
      --  be modified by Abort_To_Level and Exit_One_ATC_Level.
      --
      --  Protection: Self.L

      Serial_Number : Task_Serial_Number;
      --  A growing number to provide some way to check locking  rules/ordering

      Known_Tasks_Index : Integer := -1;
      --  Index in the System.Tasking.Debug.Known_Tasks array

      User_State : Long_Integer := 0;
      --  User-writeable location, for use in debugging tasks; also provides a
      --  simple task specific data.

      Direct_Attributes : Direct_Attribute_Array;
      --  For task attributes that have same size as Address

      Is_Defined : Direct_Index_Vector := 0;
      --  Bit I is 1 iff Direct_Attributes (I) is defined

      Indirect_Attributes : Access_Address;
      --  A pointer to chain of records for other attributes that are not
      --  address-sized, including all tagged types.

      Entry_Queues : Task_Entry_Queue_Array (1 .. Entry_Num);
      --  An array of task entry queues
      --
      --  Protection: Self.L. Once a task has set Self.Stage to Completing, it
      --  has exclusive access to this field.
   end record;

   ---------------------
   -- Initialize_ATCB --
   ---------------------

   procedure Initialize_ATCB
     (Self_ID          : Task_Id;
      Task_Entry_Point : Task_Procedure_Access;
      Task_Arg         : System.Address;
      Parent           : Task_Id;
      Elaborated       : Access_Boolean;
      Base_Priority    : System.Any_Priority;
      Task_Info        : System.Task_Info.Task_Info_Type;
      Stack_Size       : System.Parameters.Size_Type;
      T                : Task_Id;
      Success          : out Boolean);
   --  Initialize fields of a TCB and link into global TCB structures Call
   --  this only with abort deferred and holding RTS_Lock. Need more
   --  documentation, mention T, and describe Success ???

private
   Null_Task : constant Task_Id := null;

   GL_Detect_Blocking : Integer;
   pragma Import (C, GL_Detect_Blocking, "__gl_detect_blocking");
   --  Global variable exported by the binder generated file. A value equal to
   --  1 indicates that pragma Detect_Blocking is active, while 0 is used for
   --  the pragma not being present.

   Detect_Blocking : constant Boolean := GL_Detect_Blocking = 1;

   type Activation_Chain is record
      T_ID : Task_Id;
   end record;
   pragma Volatile (Activation_Chain);

   --  Activation_chain is an in-out parameter of initialization procedures
   --  and it must be passed by reference because the init proc may terminate
   --  abnormally after creating task components, and these must be properly
   --  registered for removal (Expunge_Unactivated_Tasks).

end System.Tasking;
