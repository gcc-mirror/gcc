------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S .O P E R A T I O N S     --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.3 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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

--  This package contains all the GNULL primitives that interface directly
--  with the underlying OS.

with System.Parameters;
--  used for Size_Type

with System.Tasking;
--  used for Task_ID

with System.OS_Interface;
--  used for Thread_Id

package System.Task_Primitives.Operations is

   pragma Elaborate_Body;
   package ST renames System.Tasking;
   package OSI renames System.OS_Interface;

   procedure Initialize (Environment_Task : ST.Task_ID);
   pragma Inline (Initialize);
   --  This must be called once, before any other subprograms of this
   --  package are called.

   procedure Create_Task
     (T          : ST.Task_ID;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean);
   pragma Inline (Create_Task);
   --  Create a new low-level task with ST.Task_ID T and place other needed
   --  information in the ATCB.
   --
   --  A new thread of control is created, with a stack of at least Stack_Size
   --  storage units, and the procedure Wrapper is called by this new thread
   --  of control. If Stack_Size = Unspecified_Storage_Size, choose a default
   --  stack size; this may be effectively "unbounded" on some systems.
   --
   --  The newly created low-level task is associated with the ST.Task_ID T
   --  such that any subsequent call to Self from within the context of the
   --  low-level task returns T.
   --
   --  The caller is responsible for ensuring that the storage of the Ada
   --  task control block object pointed to by T persists for the lifetime
   --  of the new task.
   --
   --  Succeeded is set to true unless creation of the task failed,
   --  as it may if there are insufficient resources to create another task.

   procedure Enter_Task (Self_ID : ST.Task_ID);
   pragma Inline (Enter_Task);
   --  Initialize data structures specific to the calling task.
   --  Self must be the ID of the calling task.
   --  It must be called (once) by the task immediately after creation,
   --  while abortion is still deferred.
   --  The effects of other operations defined below are not defined
   --  unless the caller has previously called Initialize_Task.

   procedure Exit_Task;
   pragma Inline (Exit_Task);
   --  Destroy the thread of control.
   --  Self must be the ID of the calling task.
   --  The effects of further calls to operations defined below
   --  on the task are undefined thereafter.

   function New_ATCB (Entry_Num : ST.Task_Entry_Index) return ST.Task_ID;
   pragma Inline (New_ATCB);
   --  Allocate a new ATCB with the specified number of entries.

   procedure Initialize_TCB (Self_ID : ST.Task_ID; Succeeded : out Boolean);
   pragma Inline (Initialize_TCB);
   --  Initialize all fields of the TCB

   procedure Finalize_TCB (T : ST.Task_ID);
   pragma Inline (Finalize_TCB);
   --  Finalizes Private_Data of ATCB, and then deallocates it.
   --  This is also responsible for recovering any storage or other resources
   --  that were allocated by Create_Task (the one in this package).
   --  This should only be called from Free_Task.
   --  After it is called there should be no further
   --  reference to the ATCB that corresponds to T.

   procedure Abort_Task (T : ST.Task_ID);
   pragma Inline (Abort_Task);
   --  Abort the task specified by T (the target task). This causes
   --  the target task to asynchronously raise Abort_Signal if
   --  abort is not deferred, or if it is blocked on an interruptible
   --  system call.
   --
   --  precondition:
   --    the calling task is holding T's lock and has abort deferred
   --
   --  postcondition:
   --    the calling task is holding T's lock and has abort deferred.

   --  ??? modify GNARL to skip wakeup and always call Abort_Task

   function Self return ST.Task_ID;
   pragma Inline (Self);
   --  Return a pointer to the Ada Task Control Block of the calling task.

   type Lock_Level is
     (PO_Level,
      Global_Task_Level,
      All_Attrs_Level,
      All_Tasks_Level,
      Interrupts_Level,
      ATCB_Level);
   --  Type used to describe kind of lock for second form of Initialize_Lock
   --  call specified below.
   --  See locking rules in System.Tasking (spec) for more details.

   procedure Initialize_Lock (Prio : System.Any_Priority; L : access Lock);
   procedure Initialize_Lock (L : access RTS_Lock; Level : Lock_Level);
   pragma Inline (Initialize_Lock);
   --  Initialize a lock object.
   --
   --  For Lock, Prio is the ceiling priority associated with the lock.
   --  For RTS_Lock, the ceiling is implicitly Priority'Last.
   --
   --  If the underlying system does not support priority ceiling
   --  locking, the Prio parameter is ignored.
   --
   --  The effect of either initialize operation is undefined unless L
   --  is a lock object that has not been initialized, or which has been
   --  finalized since it was last initialized.
   --
   --  The effects of the other operations on lock objects
   --  are undefined unless the lock object has been initialized
   --  and has not since been finalized.
   --
   --  Initialization of the per-task lock is implicit in Create_Task.
   --
   --  These operations raise Storage_Error if a lack of storage is detected.

   procedure Finalize_Lock (L : access Lock);
   procedure Finalize_Lock (L : access RTS_Lock);
   pragma Inline (Finalize_Lock);
   --  Finalize a lock object, freeing any resources allocated by the
   --  corresponding Initialize_Lock operation.

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean);
   procedure Write_Lock (L : access RTS_Lock);
   procedure Write_Lock (T : ST.Task_ID);
   pragma Inline (Write_Lock);
   --  Lock a lock object for write access. After this operation returns,
   --  the calling task holds write permission for the lock object. No other
   --  Write_Lock or Read_Lock operation on the same lock object will return
   --  until this task executes an Unlock operation on the same object. The
   --  effect is undefined if the calling task already holds read or write
   --  permission for the lock object L.
   --
   --  For the operation on Lock, Ceiling_Violation is set to true iff the
   --  operation failed, which will happen if there is a priority ceiling
   --  violation.
   --
   --  For the operation on ST.Task_ID, the lock is the special lock object
   --  associated with that task's ATCB. This lock has effective ceiling
   --  priority high enough that it is safe to call by a task with any
   --  priority in the range System.Priority. It is implicitly initialized
   --  by task creation. The effect is undefined if the calling task already
   --  holds T's lock, or has interrupt-level priority. Finalization of the
   --  per-task lock is implicit in Exit_Task.

   procedure Read_Lock (L : access Lock; Ceiling_Violation : out Boolean);
   pragma Inline (Read_Lock);
   --  Lock a lock object for read access. After this operation returns,
   --  the calling task has non-exclusive read permission for the logical
   --  resources that are protected by the lock. No other Write_Lock operation
   --  on the same object will return until this task and any other tasks with
   --  read permission for this lock have executed Unlock operation(s) on the
   --  lock object. A Read_Lock for a lock object may return immediately while
   --  there are tasks holding read permission, provided there are no tasks
   --  holding write permission for the object. The effect is undefined if
   --  the calling task already holds read or write permission for L.
   --
   --  Alternatively: An implementation may treat Read_Lock identically to
   --  Write_Lock. This simplifies the implementation, but reduces the level
   --  of concurrency that can be achieved.
   --
   --  Note that Read_Lock is not defined for RT_Lock and ST.Task_ID.
   --  That is because (1) so far Read_Lock has always been implemented
   --  the same as Write_Lock, (2) most lock usage inside the RTS involves
   --  potential write access, and (3) implementations of priority ceiling
   --  locking that make a reader-writer distinction have higher overhead.

   procedure Unlock (L : access Lock);
   procedure Unlock (L : access RTS_Lock);
   procedure Unlock (T : ST.Task_ID);
   pragma Inline (Unlock);
   --  Unlock a locked lock object.
   --
   --  The effect is undefined unless the calling task holds read or write
   --  permission for the lock L, and L is the lock object most recently
   --  locked by the calling task for which the calling task still holds
   --  read or write permission. (That is, matching pairs of Lock and Unlock
   --  operations on each lock object must be properly nested.)

   --  Note that Write_Lock for RTS_Lock does not have an out-parameter.
   --  RTS_Locks are used in situations where we have not made provision
   --  for recovery from ceiling violations.  We do not expect them to
   --  occur inside the runtime system, because all RTS locks have ceiling
   --  Priority'Last.

   --  There is one way there can be a ceiling violation.
   --  That is if the runtime system is called from a task that is
   --  executing in the Interrupt_Priority range.

   --  It is not clear what to do about ceiling violations due
   --  to RTS calls done at interrupt priority.  In general, it
   --  is not acceptable to give all RTS locks interrupt priority,
   --  since that whould give terrible performance on systems where
   --  this has the effect of masking hardware interrupts, though we
   --  could get away with allowing Interrupt_Priority'last where we
   --  are layered on an OS that does not allow us to mask interrupts.
   --  Ideally, we would like to raise Program_Error back at the
   --  original point of the RTS call, but this would require a lot of
   --  detailed analysis and recoding, with almost certain performance
   --  penalties.

   --  For POSIX systems, we considered just skipping setting a
   --  priority ceiling on RTS locks.  This would mean there is no
   --  ceiling violation, but we would end up with priority inversions
   --  inside the runtime system, resulting in failure to satisfy the
   --  Ada priority rules, and possible missed validation tests.
   --  This could be compensated-for by explicit priority-change calls
   --  to raise the caller to Priority'Last whenever it first enters
   --  the runtime system, but the expected overhead seems high, though
   --  it might be lower than using locks with ceilings if the underlying
   --  implementation of ceiling locks is an inefficient one.

   --  This issue should be reconsidered whenever we get around to
   --  checking for calls to potentially blocking operations from
   --  within protected operations.  If we check for such calls and
   --  catch them on entry to the OS, it may be that we can eliminate
   --  the possibility of ceiling violations inside the RTS.  For this
   --  to work, we would have to forbid explicitly setting the priority
   --  of a task to anything in the Interrupt_Priority range, at least.
   --  We would also have to check that there are no RTS-lock operations
   --  done inside any operations that are not treated as potentially
   --  blocking.

   --  The latter approach seems to be the best, i.e. to check on entry
   --  to RTS calls that may need to use locks that the priority is not
   --  in the interrupt range.  If there are RTS operations that NEED to
   --  be called from interrupt handlers, those few RTS locks should then
   --  be converted to PO-type locks, with ceiling Interrupt_Priority'Last.

   --  For now, we will just shut down the system if there is a
   --  ceiling violation.

   procedure Yield (Do_Yield : Boolean := True);
   pragma Inline (Yield);
   --  Yield the processor. Add the calling task to the tail of the
   --  ready queue for its active_priority.
   --  The Do_Yield argument is only used in some very rare cases very
   --  a yield should have an effect on a specific target and not on regular
   --  ones.

   procedure Set_Priority
     (T : ST.Task_ID;
      Prio : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False);
   pragma Inline (Set_Priority);
   --  Set the priority of the task specified by T to T.Current_Priority.
   --  The priority set is what would correspond to the Ada concept of
   --  "base priority" in the terms of the lower layer system, but
   --  the operation may be used by the upper layer to implement
   --  changes in "active priority" that are not due to lock effects.
   --  The effect should be consistent with the Ada Reference Manual.
   --  In particular, when a task lowers its priority due to the loss of
   --  inherited priority, it goes at the head of the queue for its new
   --  priority (RM D.2.2 par 9).
   --  Loss_Of_Inheritance helps the underlying implementation to do it
   --  right when the OS doesn't.

   function Get_Priority (T : ST.Task_ID) return System.Any_Priority;
   pragma Inline (Get_Priority);
   --  Returns the priority last set by Set_Priority for this task.

   function Monotonic_Clock return Duration;
   pragma Inline (Monotonic_Clock);
   --  Returns "absolute" time, represented as an offset
   --  relative to "the Epoch", which is Jan 1, 1970.
   --  This clock implementation is immune to the system's clock changes.

   function RT_Resolution return Duration;
   pragma Inline (RT_Resolution);
   --  Returns the resolution of the underlying clock used to implement
   --  RT_Clock.

   ------------------
   --  Extensions  --
   ------------------

   --  Whoever calls either of the Sleep routines is responsible
   --  for checking for pending aborts before the call.
   --  Pending priority changes are handled internally.

   procedure Sleep
     (Self_ID  : ST.Task_ID;
      Reason   : System.Tasking.Task_States);
   pragma Inline (Sleep);
   --  Wait until the current task, T,  is signaled to wake up.
   --
   --  precondition:
   --    The calling task is holding its own ATCB lock
   --    and has abort deferred
   --
   --  postcondition:
   --    The calling task is holding its own ATCB lock
   --    and has abort deferred.

   --  The effect is to atomically unlock T's lock and wait, so that another
   --  task that is able to lock T's lock can be assured that the wait has
   --  actually commenced, and that a Wakeup operation will cause the waiting
   --  task to become ready for execution once again. When Sleep returns,
   --  the waiting task will again hold its own ATCB lock. The waiting task
   --  may become ready for execution at any time (that is, spurious wakeups
   --  are permitted), but it will definitely become ready for execution when
   --  a Wakeup operation is performed for the same task.

   procedure Timed_Sleep
     (Self_ID  : ST.Task_ID;
      Time     : Duration;
      Mode     : ST.Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean);
   --  Combination of Sleep (above) and Timed_Delay

   procedure Timed_Delay
     (Self_ID  : ST.Task_ID;
      Time     : Duration;
      Mode     : ST.Delay_Modes);
   --  Implements the semantics of the delay statement. It is assumed that
   --  the caller is not abort-deferred and does not hold any locks.

   procedure Wakeup
     (T      : ST.Task_ID;
      Reason : System.Tasking.Task_States);
   pragma Inline (Wakeup);
   --  Wake up task T if it is waiting on a Sleep call (of ordinary
   --  or timed variety), making it ready for execution once again.
   --  If the task T is not waiting on a Sleep, the operation has no effect.

   function Environment_Task return ST.Task_ID;
   pragma Inline (Environment_Task);
   --  returns the task ID of the environment task
   --  Consider putting this into a variable visible directly
   --  by the rest of the runtime system. ???

   function Get_Thread_Id (T : ST.Task_ID) return OSI.Thread_Id;
   --  returns the thread id of the specified task.

   --------------------
   -- Stack Checking --
   --------------------

   --  Stack checking in GNAT is done using the concept of stack probes. A
   --  stack probe is an operation that will generate a storage error if
   --  an insufficient amount of stack space remains in the current task.

   --  The exact mechanism for a stack probe is target dependent. Typical
   --  possibilities are to use a load from a non-existent page, a store
   --  to a read-only page, or a comparison with some stack limit constant.
   --  Where possible we prefer to use a trap on a bad page access, since
   --  this has less overhead. The generation of stack probes is either
   --  automatic if the ABI requires it (as on for example DEC Unix), or
   --  is controlled by the gcc parameter -fstack-check.

   --  When we are using bad-page accesses, we need a bad page, called a
   --  guard page, at the end of each task stack. On some systems, this
   --  is provided automatically, but on other systems, we need to create
   --  the guard page ourselves, and the procedure Stack_Guard is provided
   --  for this purpose.

   procedure Stack_Guard (T : ST.Task_ID; On : Boolean);
   --  Ensure guard page is set if one is needed and the underlying thread
   --  system does not provide it. The procedure is as follows:
   --
   --    1. When we create a task adjust its size so a guard page can
   --       safely be set at the bottom of the stack
   --
   --    2. When the thread is created (and its stack allocated by the
   --       underlying thread system), get the stack base (and size, depending
   --       how the stack is growing), and create the guard page taking care of
   --       page boundaries issues.
   --
   --    3. When the task is destroyed, remove the guard page.
   --
   --  If On is true then protect the stack bottom (i.e make it read only)
   --  else unprotect it (i.e. On is True for the call when creating a task,
   --  and False when a task is destroyed).
   --
   --  The call to Stack_Guard has no effect if guard pages are not used on
   --  the target, or if guard pages are automatically provided by the system.

   -----------------------------------------
   -- Runtime System Debugging Interfaces --
   -----------------------------------------

   --  These interfaces have been added to assist in debugging the
   --  tasking runtime system.

   function Check_Exit (Self_ID : ST.Task_ID) return Boolean;
   pragma Inline (Check_Exit);
   --  Check that the current task is holding only Global_Task_Lock.

   function Check_No_Locks (Self_ID : ST.Task_ID) return Boolean;
   pragma Inline (Check_No_Locks);
   --  Check that current task is holding no locks.

   function Suspend_Task
     (T           : ST.Task_ID;
      Thread_Self : OSI.Thread_Id)
      return        Boolean;
   --  Suspend a specific task when the underlying thread library provides
   --  such functionality, unless the thread associated with T is Thread_Self.
   --  Such functionality is needed by gdb on some targets (e.g VxWorks)
   --  Return True is the operation is successful

   function Resume_Task
     (T           : ST.Task_ID;
      Thread_Self : OSI.Thread_Id)
      return        Boolean;
   --  Resume a specific task when the underlying thread library provides
   --  such functionality, unless the thread associated with T is Thread_Self.
   --  Such functionality is needed by gdb on some targets (e.g VxWorks)
   --  Return True is the operation is successful

   procedure Lock_All_Tasks_List;
   procedure Unlock_All_Tasks_List;
   --  Lock/Unlock the All_Tasks_L lock which protects
   --  System.Initialization.All_Tasks_List and Known_Tasks
   --  ??? These routines were previousely in System.Tasking.Initialization
   --  but were moved here to avoid dependency problems. That would be
   --  nice to look at it some day and put it back in Initialization.

end System.Task_Primitives.Operations;
