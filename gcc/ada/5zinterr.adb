------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1991-2001 Free Software Foundation, Inc.          --
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

--  Invariants:

--  All user-handleable signals are masked at all times in all
--  tasks/threads except possibly for the Interrupt_Manager task.

--  When a user task wants to have the effect of masking/unmasking an
--  signal, it must call Block_Interrupt/Unblock_Interrupt, which
--  will have the effect of unmasking/masking the signal in the
--  Interrupt_Manager task.  These comments do not apply to vectored
--  hardware interrupts, which may be masked or unmasked using routined
--  interfaced to the relevant VxWorks system calls.

--  Note : Direct calls to sigaction, sigprocmask, pthread_sigsetmask or any
--  other low-level interface that changes the signal action or
--  signal mask needs careful consideration.
--  One may achieve the effect of system calls first masking RTS blocked
--  (by calling Block_Interrupt) for the signal under consideration.
--  This will make all the tasks in RTS blocked for the signal.

--  Once we associate a Signal_Server_Task with an signal, the task never
--  goes away, and we never remove the association. On the other hand, it
--  is more convenient to terminate an associated Interrupt_Server_Task
--  for a vectored hardware interrupt (since we use a binary semaphore
--  for synchronization with the umbrella handler).

--  There is no more than one signal per Signal_Server_Task and no more than
--  one Signal_Server_Task per signal.  The same relation holds for hardware
--  interrupts and Interrupt_Server_Task's at any given time.  That is,
--  only one non-terminated Interrupt_Server_Task exists for a give
--  interrupt at any time.

--  Within this package, the lock L is used to protect the various status
--  tables. If there is a Server_Task associated with a signal or interrupt,
--  we use the per-task lock of the Server_Task instead so that we protect the
--  status between Interrupt_Manager and Server_Task. Protection among
--  service requests are ensured via user calls to the Interrupt_Manager
--  entries.

--  This is the VxWorks version of this package, supporting both signals
--  and vectored hardware interrupts.

with Unchecked_Conversion;

with System.OS_Interface; use System.OS_Interface;

with System.VxWorks;

with Interfaces.VxWorks;

with Ada.Task_Identification;
--  used for Task_ID type

with Ada.Exceptions;
--  used for Raise_Exception

with System.Task_Primitives;
--  used for RTS_Lock
--           Self

with System.Interrupt_Management;
--  used for Reserve
--           Interrupt_ID
--           Interrupt_Mask
--           Abort_Task_Interrupt

with System.Interrupt_Management.Operations;
--  used for Thread_Block_Interrupt
--           Thread_Unblock_Interrupt
--           Install_Default_Action
--           Install_Ignore_Action
--           Copy_Interrupt_Mask
--           Set_Interrupt_Mask
--           Empty_Interrupt_Mask
--           Fill_Interrupt_Mask
--           Add_To_Interrupt_Mask
--           Delete_From_Interrupt_Mask
--           Interrupt_Wait
--           Interrupt_Self_Process
--           Get_Interrupt_Mask
--           Set_Interrupt_Mask
--           IS_Member
--           Environment_Mask
--           All_Tasks_Mask
pragma Elaborate_All (System.Interrupt_Management.Operations);

with System.Error_Reporting;
--  used for Shutdown

with System.Task_Primitives.Operations;
--  used for Write_Lock
--           Unlock
--           Abort
--           Wakeup_Task
--           Sleep
--           Initialize_Lock

with System.Task_Primitives.Interrupt_Operations;
--  used for Set_Interrupt_ID

with System.Storage_Elements;
--  used for To_Address
--           To_Integer
--           Integer_Address

with System.Tasking;
--  used for Task_ID
--           Task_Entry_Index
--           Null_Task
--           Self
--           Interrupt_Manager_ID

with System.Tasking.Utilities;
--  used for Make_Independent

with System.Tasking.Rendezvous;
--  used for Call_Simple
pragma Elaborate_All (System.Tasking.Rendezvous);

with System.Tasking.Initialization;
--  used for Defer_Abort
--           Undefer_Abort

package body System.Interrupts is

   use Tasking;
   use System.Error_Reporting;
   use Ada.Exceptions;

   package PRI renames System.Task_Primitives;
   package POP renames System.Task_Primitives.Operations;
   package PIO renames System.Task_Primitives.Interrupt_Operations;
   package IMNG renames System.Interrupt_Management;
   package IMOP renames System.Interrupt_Management.Operations;

   function To_Ada is new Unchecked_Conversion
     (System.Tasking.Task_ID, Ada.Task_Identification.Task_Id);

   function To_System is new Unchecked_Conversion
     (Ada.Task_Identification.Task_Id, Task_ID);

   -----------------
   -- Local Tasks --
   -----------------

   --  WARNING: System.Tasking.Utilities performs calls to this task
   --  with low-level constructs. Do not change this spec without synchro-
   --  nizing it.

   task Interrupt_Manager is
      entry Initialize (Mask : IMNG.Interrupt_Mask);

      entry Attach_Handler
        (New_Handler : Parameterless_Handler;
         Interrupt   : Interrupt_ID;
         Static      : Boolean;
         Restoration : Boolean := False);

      entry Exchange_Handler
        (Old_Handler : out Parameterless_Handler;
         New_Handler : Parameterless_Handler;
         Interrupt   : Interrupt_ID;
         Static      : Boolean);

      entry Detach_Handler
        (Interrupt : Interrupt_ID;
         Static    : Boolean);

      entry Bind_Interrupt_To_Entry
        (T         : Task_ID;
         E         : Task_Entry_Index;
         Interrupt : Interrupt_ID);

      entry Detach_Interrupt_Entries (T : Task_ID);

      pragma Interrupt_Priority (System.Interrupt_Priority'First);
   end Interrupt_Manager;

   task type Signal_Server_Task (Interrupt : Interrupt_ID) is
      pragma Interrupt_Priority (System.Interrupt_Priority'First + 1);
   end Signal_Server_Task;
   --  Server task for signal handling

   type Signal_Task_Access is access Signal_Server_Task;

   task type Interrupt_Server_Task
     (Interrupt : Interrupt_ID; Int_Sema : SEM_ID) is
      --  Server task for vectored hardware interrupt handling
      pragma Interrupt_Priority (System.Interrupt_Priority'First + 2);
   end Interrupt_Server_Task;

   type Interrupt_Task_Access is access Interrupt_Server_Task;

   -------------------------------
   -- Local Types and Variables --
   -------------------------------

   type Entry_Assoc is record
      T : Task_ID;
      E : Task_Entry_Index;
   end record;

   type Handler_Assoc is record
      H      : Parameterless_Handler;
      Static : Boolean;   --  Indicates static binding;
   end record;

   User_Handler : array (Interrupt_ID) of Handler_Assoc :=
     (others => (null, Static => False));
   pragma Volatile_Components (User_Handler);
   --  Holds the protected procedure handler (if any) and its Static
   --  information  for each interrupt or signal. A handler is static
   --  iff it is specified through the pragma Attach_Handler.

   User_Entry : array (Interrupt_ID) of Entry_Assoc :=
     (others => (T => Null_Task, E => Null_Task_Entry));
   pragma Volatile_Components (User_Entry);
   --  Holds the task and entry index (if any) for each interrupt / signal

   --  Type and Head, Tail of the list containing Registered Interrupt
   --  Handlers. These definitions are used to register the handlers
   --  specified by the pragma Interrupt_Handler.

   type Registered_Handler;
   type R_Link is access all Registered_Handler;

   type Registered_Handler is record
      H    : System.Address := System.Null_Address;
      Next : R_Link := null;
   end record;

   Registered_Handler_Head : R_Link := null;
   Registered_Handler_Tail : R_Link := null;

   Server_ID : array (Interrupt_ID) of System.Tasking.Task_ID :=
     (others => System.Tasking.Null_Task);
   pragma Atomic_Components (Server_ID);
   --  Holds the Task_ID of the Server_Task for each interrupt / signal.
   --  Task_ID is needed to accomplish locking per interrupt base. Also
   --  is needed to determine whether to create a new Server_Task.

   Semaphore_ID_Map : array
     (Interrupt_ID range 0 .. System.VxWorks.Num_HW_Interrupts) of SEM_ID :=
        (others => 0);
   --  Array of binary semaphores associated with vectored interrupts
   --  Note that the last bound should be Max_HW_Interrupt, but this will raise
   --  Storage_Error if Num_HW_Interrupts is null, so use an extra 4 bytes
   --  instead.

   Signal_Access_Hold : Signal_Task_Access;
   --  Variable for allocating a Signal_Server_Task

   Interrupt_Access_Hold : Interrupt_Task_Access;
   --  Variable for allocating an Interrupt_Server_Task

   L : aliased PRI.RTS_Lock;
   --  L protects the contents of the above tables for interrupts / signals
   --  for which Server_ID (I) = Null_Task.
   --
   --  If Server_ID (I) /= Null_Task then protection is via the
   --  per-task (TCB) lock of Server_ID (I).
   --
   --  For deadlock prevention, L should not be locked after
   --  any other lock is held, hence we use PO_Level which is the highest
   --  lock level for error checking.

   Task_Lock : array (Interrupt_ID) of Boolean := (others => False);
   --  Booleans indicating whether the per task lock is used

   Default_Handler : array (HW_Interrupt) of Interfaces.VxWorks.VOIDFUNCPTR;
   --  Vectored interrupt handlers installed prior to program startup.
   --  These are saved only when the umbrella handler is installed for
   --  a given interrupt number.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Reserved_Interrupt (Interrupt : Interrupt_ID);
   --  Check if Id is a reserved interrupt, and if so raise Program_Error
   --  with an appropriate message, otherwise return.

   procedure Finalize_Interrupt_Servers;
   --  Unbind the handlers for hardware interrupt server tasks at program
   --  termination.

   procedure Lock_Interrupt
     (Self_ID   : Task_ID;
      Interrupt : Interrupt_ID);
   --  Protect the tables using L or the per-task lock. Set the Boolean
   --  value Task_Lock if the lock is made using per-task lock.
   --  This information is needed so that Unlock_Interrupt
   --  performs unlocking on the same lock. The situation we are preventing
   --  is, for example, when Attach_Handler is called for the first time
   --  we lock L and create an Server_Task. For a matching unlocking, if we
   --  rely on the fact that there is a Server_Task, we will unlock the
   --  per-task lock.

   procedure Unlock_Interrupt
     (Self_ID   : Task_ID;
      Interrupt : Interrupt_ID);
   --  Unlock interrupt previously locked by Lock_Interrupt

   function Is_Registered (Handler : Parameterless_Handler) return Boolean;
   --  Needs comment ???

   procedure Notify_Interrupt (Param : System.Address);
   --  Umbrella handler for vectored interrupts (not signals)

   procedure Install_Default_Action (Interrupt : HW_Interrupt);
   --  Restore a handler that was in place prior to program execution

   procedure Install_Umbrella_Handler
     (Interrupt : HW_Interrupt;
      Handler   : Interfaces.VxWorks.VOIDFUNCPTR);
   --  Install the runtime umbrella handler for a vectored hardware
   --  interrupt

   function To_Signal (S : Interrupt_ID) return IMNG.Interrupt_ID;
   --  Convert interrupt ID to signal number.

   procedure Unimplemented (Feature : String);
   pragma No_Return (Unimplemented);
   --  Used to mark a call to an unimplemented function. Raises Program_Error
   --  with an appropriate message noting that Feature is unimplemented.

   --------------------
   -- Attach_Handler --
   --------------------

   --  Calling this procedure with New_Handler = null and Static = True
   --  means we want to detach the current handler regardless of the
   --  previous handler's binding status (ie. do not care if it is a
   --  dynamic or static handler).

   --  This option is needed so that during the finalization of a PO, we
   --  can detach handlers attached through pragma Attach_Handler.

   procedure Attach_Handler
     (New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID;
      Static      : Boolean := False)
   is
   begin
      Check_Reserved_Interrupt (Interrupt);
      Interrupt_Manager.Attach_Handler (New_Handler, Interrupt, Static);
   end Attach_Handler;

   -----------------------------
   -- Bind_Interrupt_To_Entry --
   -----------------------------

   --  This procedure raises a Program_Error if it tries to
   --  bind an interrupt to which an Entry or a Procedure is
   --  already bound.

   procedure Bind_Interrupt_To_Entry
     (T       : Task_ID;
      E       : Task_Entry_Index;
      Int_Ref : System.Address)
   is
      Interrupt : constant Interrupt_ID :=
                    Interrupt_ID (Storage_Elements.To_Integer (Int_Ref));

   begin
      Check_Reserved_Interrupt (Interrupt);
      Interrupt_Manager.Bind_Interrupt_To_Entry (T, E, Interrupt);
   end Bind_Interrupt_To_Entry;

   ---------------------
   -- Block_Interrupt --
   ---------------------

   procedure Block_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Unimplemented ("Block_Interrupt");
   end Block_Interrupt;

   ------------------------------
   -- Check_Reserved_Interrupt --
   ------------------------------

   procedure Check_Reserved_Interrupt (Interrupt : Interrupt_ID) is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception
           (Program_Error'Identity,
            "Interrupt" & Interrupt_ID'Image (Interrupt) & " is reserved");
      else
         return;
      end if;
   end Check_Reserved_Interrupt;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler
     (Interrupt : Interrupt_ID)
      return      Parameterless_Handler
   is
   begin
      Check_Reserved_Interrupt (Interrupt);

      --  ??? Since Parameterless_Handler is not Atomic, the
      --  current implementation is wrong. We need a new service in
      --  Interrupt_Manager to ensure atomicity.

      return User_Handler (Interrupt).H;
   end Current_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   --  Calling this procedure with Static = True means we want to Detach the
   --  current handler regardless of the previous handler's binding status
   --  (i.e. do not care if it is a dynamic or static handler).

   --  This option is needed so that during the finalization of a PO, we can
   --  detach handlers attached through pragma Attach_Handler.

   procedure Detach_Handler
     (Interrupt : Interrupt_ID;
      Static    : Boolean := False)
   is
   begin
      Check_Reserved_Interrupt (Interrupt);
      Interrupt_Manager.Detach_Handler (Interrupt, Static);
   end Detach_Handler;

   ------------------------------
   -- Detach_Interrupt_Entries --
   ------------------------------

   procedure Detach_Interrupt_Entries (T : Task_ID) is
   begin
      Interrupt_Manager.Detach_Interrupt_Entries (T);
   end Detach_Interrupt_Entries;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   --  Calling this procedure with New_Handler = null and Static = True
   --  means we want to detach the current handler regardless of the
   --  previous handler's binding status (ie. do not care if it is a
   --  dynamic or static handler).

   --  This option is needed so that during the finalization of a PO, we
   --  can detach handlers attached through pragma Attach_Handler.

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID;
      Static      : Boolean := False)
   is
   begin
      Check_Reserved_Interrupt (Interrupt);
      Interrupt_Manager.Exchange_Handler
        (Old_Handler, New_Handler, Interrupt, Static);
   end Exchange_Handler;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Static_Interrupt_Protection) is
   begin
      --  ??? loop to be executed only when we're not doing library level
      --  finalization, since in this case all interrupt / signal tasks are
      --  gone.

      if not Interrupt_Manager'Terminated then
         for N in reverse Object.Previous_Handlers'Range loop
            Interrupt_Manager.Attach_Handler
              (New_Handler => Object.Previous_Handlers (N).Handler,
               Interrupt   => Object.Previous_Handlers (N).Interrupt,
               Static      => Object.Previous_Handlers (N).Static,
               Restoration => True);
         end loop;
      end if;

      Tasking.Protected_Objects.Entries.Finalize
        (Tasking.Protected_Objects.Entries.Protection_Entries (Object));
   end Finalize;

   --------------------------------
   -- Finalize_Interrupt_Servers --
   --------------------------------

   --  Restore default handlers for interrupt servers.  Signal servers
   --  restore the default handlers when they're aborted.  This is called
   --  by the Interrupt_Manager task when it receives the abort signal
   --  during program finalization.

   procedure Finalize_Interrupt_Servers is
   begin
      if HW_Interrupt'Last >= 0 then
         for Int in HW_Interrupt loop
            if Server_ID (Interrupt_ID (Int)) /= null
              and then
                not Ada.Task_Identification.Is_Terminated
                 (To_Ada (Server_ID (Interrupt_ID (Int))))
            then
               Interrupt_Manager.Attach_Handler
                 (New_Handler => null,
                  Interrupt => Interrupt_ID (Int),
                  Static => True,
                  Restoration => True);
            end if;
         end loop;
      end if;
   end Finalize_Interrupt_Servers;

   -------------------------------------
   -- Has_Interrupt_Or_Attach_Handler --
   -------------------------------------

   function Has_Interrupt_Or_Attach_Handler
     (Object : access Dynamic_Interrupt_Protection)
      return   Boolean
   is
   begin
      return True;
   end Has_Interrupt_Or_Attach_Handler;

   function Has_Interrupt_Or_Attach_Handler
     (Object : access Static_Interrupt_Protection)
      return   Boolean
   is
   begin
      return True;
   end Has_Interrupt_Or_Attach_Handler;

   ----------------------
   -- Ignore_Interrupt --
   ----------------------

   procedure Ignore_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Unimplemented ("Ignore_Interrupt");
   end Ignore_Interrupt;

   ----------------------------
   -- Install_Default_Action --
   ----------------------------

   procedure Install_Default_Action (Interrupt : HW_Interrupt) is
   begin
      --  Restore original interrupt handler

      Interfaces.VxWorks.intVecSet
        (Interfaces.VxWorks.INUM_TO_IVEC (Integer (Interrupt)),
         Default_Handler (Interrupt));
      Default_Handler (Interrupt) := null;
   end Install_Default_Action;

   ----------------------
   -- Install_Handlers --
   ----------------------

   procedure Install_Handlers
     (Object       : access Static_Interrupt_Protection;
      New_Handlers : New_Handler_Array) is
   begin
      for N in New_Handlers'Range loop
         --  We need a lock around this ???

         Object.Previous_Handlers (N).Interrupt := New_Handlers (N).Interrupt;
         Object.Previous_Handlers (N).Static    := User_Handler
           (New_Handlers (N).Interrupt).Static;

         --  We call Exchange_Handler and not directly Interrupt_Manager.
         --  Exchange_Handler so we get the Is_Reserved check.

         Exchange_Handler
           (Old_Handler => Object.Previous_Handlers (N).Handler,
            New_Handler => New_Handlers (N).Handler,
            Interrupt   => New_Handlers (N).Interrupt,
            Static      => True);
      end loop;
   end Install_Handlers;

   ------------------------------
   -- Install_Umbrella_Handler --
   ------------------------------

   procedure Install_Umbrella_Handler
     (Interrupt : HW_Interrupt;
      Handler   : Interfaces.VxWorks.VOIDFUNCPTR)
   is
      use Interfaces.VxWorks;

      Vec         : constant Interrupt_Vector :=
                      INUM_TO_IVEC (Interfaces.VxWorks.int (Interrupt));
      Old_Handler : constant VOIDFUNCPTR :=
                      intVecGet
                        (INUM_TO_IVEC (Interfaces.VxWorks.int (Interrupt)));
      Stat        : Interfaces.VxWorks.STATUS;

   begin
      --  Only install umbrella handler when no Ada handler has already been
      --  installed. Note that the interrupt number is passed as a parameter
      --  when an interrupt occurs, so the umbrella handler has a different
      --  wrapper generated by intConnect for each interrupt number.

      if Default_Handler (Interrupt) = null then
         Stat :=
           intConnect (Vec, VOIDFUNCPTR (Handler), System.Address (Interrupt));
         Default_Handler (Interrupt) := Old_Handler;
      end if;
   end Install_Umbrella_Handler;

   ----------------
   -- Is_Blocked --
   ----------------

   function Is_Blocked (Interrupt : Interrupt_ID) return Boolean is
   begin
      Unimplemented ("Is_Blocked");
      return False;
   end Is_Blocked;

   -----------------------
   -- Is_Entry_Attached --
   -----------------------

   function Is_Entry_Attached (Interrupt : Interrupt_ID) return Boolean is
   begin
      Check_Reserved_Interrupt (Interrupt);
      return User_Entry (Interrupt).T /= Null_Task;
   end Is_Entry_Attached;

   -------------------------
   -- Is_Handler_Attached --
   -------------------------

   function Is_Handler_Attached (Interrupt : Interrupt_ID) return Boolean is
   begin
      Check_Reserved_Interrupt (Interrupt);
      return User_Handler (Interrupt).H /= null;
   end Is_Handler_Attached;

   ----------------
   -- Is_Ignored --
   ----------------

   function Is_Ignored (Interrupt : Interrupt_ID) return Boolean is
   begin
      Unimplemented ("Is_Ignored");
      return False;
   end Is_Ignored;

   -------------------
   -- Is_Registered --
   -------------------

   --  See if Handler has been "pragma"ed using Interrupt_Handler.
   --  Always consider a null handler as registered.

   function Is_Registered (Handler : Parameterless_Handler) return Boolean is
      type Fat_Ptr is record
         Object_Addr  : System.Address;
         Handler_Addr : System.Address;
      end record;

      function To_Fat_Ptr is new Unchecked_Conversion
        (Parameterless_Handler, Fat_Ptr);

      Ptr : R_Link;
      Fat : Fat_Ptr;

   begin
      if Handler = null then
         return True;
      end if;

      Fat := To_Fat_Ptr (Handler);

      Ptr := Registered_Handler_Head;

      while (Ptr /= null) loop
         if Ptr.H = Fat.Handler_Addr then
            return True;
         end if;

         Ptr := Ptr.Next;
      end loop;

      return False;

   end Is_Registered;

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved (Interrupt : Interrupt_ID) return Boolean is
   begin
      if Interrupt < System.VxWorks.Num_HW_Interrupts then
         return False;
      else
         return IMNG.Reserve (To_Signal (Interrupt));
      end if;
   end Is_Reserved;

   --------------------
   -- Lock_Interrupt --
   --------------------

   --  ?????
   --  This package has been modified several times.
   --  Do we still need this fancy locking scheme, now that more operations
   --  are entries of the interrupt manager task?
   --  ?????
   --  More likely, we will need to convert one or more entry calls to
   --  protected operations, because presently we are violating locking order
   --  rules by calling a task entry from within the runtime system.

   procedure Lock_Interrupt
     (Self_ID   : Task_ID;
      Interrupt : Interrupt_ID) is
   begin
      Initialization.Defer_Abort (Self_ID);

      POP.Write_Lock (L'Access);

      if Task_Lock (Interrupt) then
         pragma Assert (Server_ID (Interrupt) /= null,
                        "Task_Lock is true for null server task");
         pragma Assert
           (not Ada.Task_Identification.Is_Terminated
            (To_Ada (Server_ID (Interrupt))),
            "Attempt to lock per task lock of terminated server: " &
            "Task_Lock => True");

         POP.Unlock (L'Access);
         POP.Write_Lock (Server_ID (Interrupt));

      elsif Server_ID (Interrupt) /= Null_Task then
         pragma Assert
           (not Ada.Task_Identification.Is_Terminated
            (To_Ada (Server_ID (Interrupt))),
            "Attempt to lock per task lock of terminated server: " &
            "Task_Lock => False");

         Task_Lock (Interrupt) := True;
         POP.Unlock (L'Access);
         POP.Write_Lock (Server_ID (Interrupt));
      end if;

   end Lock_Interrupt;

   ------------------------
   --  Notify_Interrupt  --
   ------------------------

   --  Umbrella handler for vectored hardware interrupts (as opposed to
   --  signals and exceptions).  As opposed to the signal implementation,
   --  this handler is only installed in the vector table while there is
   --  an active association of an Ada handler to the interrupt.

   --  Otherwise, the handler that existed prior to program startup is
   --  in the vector table.  This ensures that handlers installed by
   --  the BSP are active unless explicitly replaced in the program text.

   --  Each Interrupt_Server_Task has an associated binary semaphore
   --  on which it pends once it's been started.  This routine determines
   --  The appropriate semaphore and and issues a semGive call, waking
   --  the server task.  When a handler is unbound,
   --  System.Interrupts.Unbind_Handler issues a semFlush, and the
   --  server task deletes its semaphore and terminates.

   procedure Notify_Interrupt (Param : System.Address) is
      Interrupt      : Interrupt_ID := Interrupt_ID (Param);
      Discard_Result : STATUS;

   begin
      Discard_Result := semGive (Semaphore_ID_Map (Interrupt));
   end Notify_Interrupt;

   ---------------
   -- Reference --
   ---------------

   function Reference (Interrupt : Interrupt_ID) return System.Address is
   begin
      Check_Reserved_Interrupt (Interrupt);
      return Storage_Elements.To_Address
        (Storage_Elements.Integer_Address (Interrupt));
   end Reference;

   --------------------------------
   -- Register_Interrupt_Handler --
   --------------------------------

   procedure Register_Interrupt_Handler (Handler_Addr : System.Address) is
      New_Node_Ptr : R_Link;
   begin
      --  This routine registers a handler as usable for dynamic
      --  interrupt handler association. Routines attaching and detaching
      --  handlers dynamically should determine whether the handler is
      --  registered. Program_Error should be raised if it is not registered.

      --  Pragma Interrupt_Handler can only appear in a library
      --  level PO definition and instantiation. Therefore, we do not need
      --  to implement an unregister operation. Nor do we need to
      --  protect the queue structure with a lock.

      pragma Assert (Handler_Addr /= System.Null_Address);

      New_Node_Ptr := new Registered_Handler;
      New_Node_Ptr.H := Handler_Addr;

      if Registered_Handler_Head = null then
         Registered_Handler_Head := New_Node_Ptr;
         Registered_Handler_Tail := New_Node_Ptr;

      else
         Registered_Handler_Tail.Next := New_Node_Ptr;
         Registered_Handler_Tail := New_Node_Ptr;
      end if;
   end Register_Interrupt_Handler;

   ---------------
   -- To_Signal --
   ---------------

   function To_Signal (S : Interrupt_ID) return IMNG.Interrupt_ID is
   begin
      return IMNG.Interrupt_ID (S - System.VxWorks.Num_HW_Interrupts);
   end To_Signal;

   -----------------------
   -- Unblock_Interrupt --
   -----------------------

   procedure Unblock_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Unimplemented ("Unblock_Interrupt");
   end Unblock_Interrupt;

   ------------------
   -- Unblocked_By --
   ------------------

   function Unblocked_By
     (Interrupt : Interrupt_ID) return System.Tasking.Task_ID is
   begin
      Unimplemented ("Unblocked_By");
      return Null_Task;
   end Unblocked_By;

   ------------------------
   -- Unignore_Interrupt --
   ------------------------

   procedure Unignore_Interrupt (Interrupt : Interrupt_ID) is
   begin
      Unimplemented ("Unignore_Interrupt");
   end Unignore_Interrupt;

   -------------------
   -- Unimplemented --
   -------------------

   procedure Unimplemented (Feature : String) is
   begin
      Raise_Exception
        (Program_Error'Identity,
         Feature & " not implemented on VxWorks");
   end Unimplemented;

   ----------------------
   -- Unlock_Interrupt --
   ----------------------

   procedure Unlock_Interrupt
     (Self_ID   : Task_ID;
      Interrupt : Interrupt_ID) is
   begin
      if Task_Lock (Interrupt) then
         pragma Assert
           (not Ada.Task_Identification.Is_Terminated
            (To_Ada (Server_ID (Interrupt))),
            "Attempt to unlock per task lock of terminated server");

         POP.Unlock (Server_ID (Interrupt));
      else
         POP.Unlock (L'Access);
      end if;

      Initialization.Undefer_Abort (Self_ID);
   end Unlock_Interrupt;

   -----------------------
   -- Interrupt_Manager --
   -----------------------

   task body Interrupt_Manager is
      ---------------------
      -- Local Variables --
      ---------------------

      Intwait_Mask : aliased IMNG.Interrupt_Mask;
      Old_Mask     : aliased IMNG.Interrupt_Mask;
      Self_ID      : Task_ID := POP.Self;

      --------------------
      -- Local Routines --
      --------------------

      procedure Bind_Handler (Interrupt : Interrupt_ID);
      --  This procedure does not do anything if a signal is blocked.
      --  Otherwise, we have to interrupt Server_Task for status change through
      --  a wakeup signal.

      procedure Unbind_Handler (Interrupt : Interrupt_ID);
      --  This procedure does not do anything if a signal is blocked.
      --  Otherwise, we have to interrupt Server_Task for status change
      --  through an abort signal.

      --  The following two procedures are labelled Unprotected... in order to
      --  indicate that Lock/Unlock_Interrupt operations are needed around
      --  around calls to them.

      procedure Unprotected_Exchange_Handler
        (Old_Handler : out Parameterless_Handler;
         New_Handler : Parameterless_Handler;
         Interrupt   : Interrupt_ID;
         Static      : Boolean;
         Restoration : Boolean := False);

      procedure Unprotected_Detach_Handler
        (Interrupt : Interrupt_ID;
         Static    : Boolean);

      ------------------
      -- Bind_Handler --
      ------------------

      procedure Bind_Handler (Interrupt : Interrupt_ID) is
      begin
         if Interrupt < System.VxWorks.Num_HW_Interrupts then
            Install_Umbrella_Handler
              (HW_Interrupt (Interrupt), Notify_Interrupt'Access);

         else
            --  Mask this task for the given signal so that all tasks
            --  are masked for the signal and the actual delivery of the
            --  signal will be caught using "sigwait" by the
            --  corresponding Server_Task.

            IMOP.Thread_Block_Interrupt (To_Signal (Interrupt));
            --  We have installed a handler or an entry before we called
            --  this procedure. If the handler task is waiting to be
            --  awakened, do it here. Otherwise, the signal will be
            --  discarded.

            POP.Wakeup (Server_ID (Interrupt), Interrupt_Server_Idle_Sleep);
         end if;
      end Bind_Handler;

      --------------------
      -- Unbind_Handler --
      --------------------

      procedure Unbind_Handler (Interrupt : Interrupt_ID) is
         S : STATUS;
         Ret_Interrupt : IMNG.Interrupt_ID;

         use type IMNG.Interrupt_ID;
         use type STATUS;

      begin
         if Interrupt < System.VxWorks.Num_HW_Interrupts then

            --  Hardware interrupt

            Install_Default_Action (HW_Interrupt (Interrupt));

            --  Flush server task off semaphore, allowing it to terminate

            S := semFlush (Semaphore_ID_Map (Interrupt));
            pragma Assert (S = 0);

         else
            --  Currently, there is a handler or an entry attached and
            --  the corresponding Server_Task is waiting on "sigwait."
            --  We have to wake up the Server_Task and make it
            --  wait on a condition variable by sending an
            --  Abort_Task_Interrupt

            --  Make sure corresponding Server_Task is out of its own
            --  sigwait state.

            POP.Abort_Task (Server_ID (Interrupt));
            Ret_Interrupt := IMOP.Interrupt_Wait (Intwait_Mask'Access);
            pragma Assert (Ret_Interrupt = IMNG.Abort_Task_Interrupt);

            IMOP.Install_Default_Action (To_Signal (Interrupt));

            --  Unmake the Interrupt for this task in order to allow default
            --  action again.

            IMOP.Thread_Unblock_Interrupt (To_Signal (Interrupt));
         end if;
      end Unbind_Handler;

      --------------------------------
      -- Unprotected_Detach_Handler --
      --------------------------------

      procedure Unprotected_Detach_Handler
        (Interrupt : Interrupt_ID;
         Static    : Boolean)
      is
         Old_Handler : Parameterless_Handler;
      begin
         if User_Entry (Interrupt).T /= Null_Task then

            --  If an interrupt entry is installed raise
            --  Program_Error. (propagate it to the caller).

            Unlock_Interrupt (Self_ID, Interrupt);
            Raise_Exception (Program_Error'Identity,
              "An interrupt entry is already installed");
         end if;

         --  Note : Static = True will pass the following check. This is the
         --  case when we want to detach a handler regardless of the static
         --  status of the Current_Handler.

         if not Static and then User_Handler (Interrupt).Static then

            --  Trying to detach a static Interrupt Handler.
            --  raise Program_Error.

            Unlock_Interrupt (Self_ID, Interrupt);
            Raise_Exception (Program_Error'Identity,
              "Trying to detach a static Interrupt Handler");
         end if;

         Old_Handler := User_Handler (Interrupt).H;

         --  The new handler

         User_Handler (Interrupt).H := null;
         User_Handler (Interrupt).Static := False;

         if Old_Handler /= null then
            Unbind_Handler (Interrupt);
         end if;

      end Unprotected_Detach_Handler;

      ----------------------------------
      -- Unprotected_Exchange_Handler --
      ----------------------------------

      procedure Unprotected_Exchange_Handler
        (Old_Handler : out Parameterless_Handler;
         New_Handler : Parameterless_Handler;
         Interrupt   : Interrupt_ID;
         Static      : Boolean;
         Restoration : Boolean := False) is
      begin
         if User_Entry (Interrupt).T /= Null_Task then

            --  If an interrupt entry is already installed, raise
            --  Program_Error. (propagate it to the caller).

            Unlock_Interrupt (Self_ID, Interrupt);
            Raise_Exception (Program_Error'Identity,
              "An interrupt is already installed");
         end if;

         --  Note : A null handler with Static = True will
         --  pass the following check. This is the case when we want to
         --  detach a handler regardless of the Static status
         --  of Current_Handler.
         --  We don't check anything if Restoration is True, since we
         --  may be detaching a static handler to restore a dynamic one.

         if not Restoration and then not Static
           and then (User_Handler (Interrupt).Static

                     --  Trying to overwrite a static Interrupt Handler with a
                     --  dynamic Handler

                     --  The new handler is not specified as an
                     --  Interrupt Handler by a pragma.

                     or else not Is_Registered (New_Handler))
         then
            Unlock_Interrupt (Self_ID, Interrupt);
            Raise_Exception
              (Program_Error'Identity,
               "Trying to overwrite a static Interrupt Handler with a " &
               "dynamic Handler");
         end if;

         --  Save the old handler

         Old_Handler := User_Handler (Interrupt).H;

         --  The new handler

         User_Handler (Interrupt).H := New_Handler;

         if New_Handler = null then

            --  The null handler means we are detaching the handler.

            User_Handler (Interrupt).Static := False;

         else
            User_Handler (Interrupt).Static := Static;
         end if;

         --  Invoke a corresponding Server_Task if not yet created.
         --  Place Task_ID info in Server_ID array.

         if New_Handler /= null
           and then
            (Server_ID (Interrupt) = Null_Task
              or else
                Ada.Task_Identification.Is_Terminated
                  (To_Ada (Server_ID (Interrupt))))
         then
            --  When a new Server_Task is created, it should have its
            --  signal mask set to the All_Tasks_Mask.

            IMOP.Set_Interrupt_Mask
              (IMOP.All_Tasks_Mask'Access, Old_Mask'Access);

            if Interrupt < System.VxWorks.Num_HW_Interrupts then

               --  Vectored hardware interrupt

               Interrupt_Access_Hold :=
                 new Interrupt_Server_Task
                   (Interrupt, semBCreate (SEM_Q_FIFO, SEM_EMPTY));
               Server_ID (Interrupt) :=
                 To_System (Interrupt_Access_Hold.all'Identity);

            else
               --  Signal

               Signal_Access_Hold := new Signal_Server_Task (Interrupt);
               Server_ID (Interrupt) :=
                 To_System (Signal_Access_Hold.all'Identity);
            end if;

            IMOP.Set_Interrupt_Mask (Old_Mask'Access);
         end if;

         if (New_Handler = null) and then Old_Handler /= null then

            --  Restore default handler

            Unbind_Handler (Interrupt);

         elsif Old_Handler = null then

            --  Save default handler

            Bind_Handler (Interrupt);
         end if;

      end Unprotected_Exchange_Handler;

      --  Start of processing for Interrupt_Manager

   begin
      --  By making this task independent of any master, when the process
      --  goes away, the Interrupt_Manager will terminate gracefully.

      System.Tasking.Utilities.Make_Independent;

      --  Environment task gets its own interrupt mask, saves it,
      --  and then masks all signals except the Keep_Unmasked set.

      --  During rendezvous, the Interrupt_Manager receives the old
      --  signal mask of the environment task, and sets its own
      --  signal mask to that value.

      --  The environment task will call this entry of Interrupt_Manager
      --  during elaboration of the body of this package.

      accept Initialize (Mask : IMNG.Interrupt_Mask) do
         declare
            The_Mask : aliased IMNG.Interrupt_Mask;

         begin
            IMOP.Copy_Interrupt_Mask (The_Mask, Mask);
            IMOP.Set_Interrupt_Mask (The_Mask'Access);
         end;
      end Initialize;

      --  Note: All tasks in RTS will have all reserved signals
      --  being masked (except the Interrupt_Manager) and Keep_Unmasked
      --  signals unmasked when created.

      --  Abort_Task_Interrupt is one of the signals unmasked
      --  in all tasks. We mask the signal in this particular task
      --  so that "sigwait" is can catch an explicit
      --  Abort_Task_Interrupt from a Server_Task.

      --  This sigwaiting is needed to ensure that a Signal_Server_Task is
      --  out of its own sigwait state. This extra synchronization is
      --  necessary to prevent following scenarios:

      --   1) Interrupt_Manager sends an Abort_Task_Interrupt to a
      --      Signal_Server_Task then changes its own signal mask (OS level).
      --      If a signal (corresponding to the Signal_Server_Task) arrives
      --      in the meantime, we have the Interrupt_Manager umnasked and
      --      the Signal_Server_Task waiting on sigwait.

      --   2) For unbinding a handler, we install a default action in the
      --      Interrupt_Manager. POSIX.1c states that the result of using
      --      "sigwait" and "sigaction" simultaneously on the same signal
      --      is undefined. Therefore, we need to be informed from the
      --      Signal_Server_Task that it is out of its sigwait stage.

      IMOP.Empty_Interrupt_Mask (Intwait_Mask'Access);
      IMOP.Add_To_Interrupt_Mask
        (Intwait_Mask'Access, IMNG.Abort_Task_Interrupt);
      IMOP.Thread_Block_Interrupt (IMNG.Abort_Task_Interrupt);

      loop
         --  A block is needed to absorb Program_Error exception

         declare
            Old_Handler : Parameterless_Handler;

         begin
            select

               accept Attach_Handler
                 (New_Handler : Parameterless_Handler;
                  Interrupt   : Interrupt_ID;
                  Static      : Boolean;
                  Restoration : Boolean := False)
               do
                  Lock_Interrupt (Self_ID, Interrupt);
                  Unprotected_Exchange_Handler
                    (Old_Handler, New_Handler, Interrupt, Static, Restoration);
                  Unlock_Interrupt (Self_ID, Interrupt);
               end Attach_Handler;

            or accept Exchange_Handler
              (Old_Handler : out Parameterless_Handler;
               New_Handler : Parameterless_Handler;
               Interrupt   : Interrupt_ID;
               Static      : Boolean)
            do
               Lock_Interrupt (Self_ID, Interrupt);
               Unprotected_Exchange_Handler
                 (Old_Handler, New_Handler, Interrupt, Static);
               Unlock_Interrupt (Self_ID, Interrupt);
            end Exchange_Handler;

            or accept Detach_Handler
               (Interrupt   : Interrupt_ID;
                Static      : Boolean)
            do
               Lock_Interrupt (Self_ID, Interrupt);
               Unprotected_Detach_Handler (Interrupt, Static);
               Unlock_Interrupt (Self_ID, Interrupt);
            end Detach_Handler;

            or accept Bind_Interrupt_To_Entry
              (T       : Task_ID;
               E       : Task_Entry_Index;
               Interrupt : Interrupt_ID)
            do
               Lock_Interrupt (Self_ID, Interrupt);

               --  If there is a binding already (either a procedure or an
               --  entry), raise Program_Error (propagate it to the caller).

               if User_Handler (Interrupt).H /= null
                 or else User_Entry (Interrupt).T /= Null_Task
               then
                  Unlock_Interrupt (Self_ID, Interrupt);
                  Raise_Exception
                    (Program_Error'Identity,
                     "A binding for this interrupt is already present");
               end if;

               User_Entry (Interrupt) := Entry_Assoc' (T => T, E => E);

               --  Indicate the attachment of interrupt entry in the ATCB.
               --  This is needed  so when an interrupt entry task terminates
               --  the binding can be cleaned. The call to unbinding must be
               --  make by the task before it terminates.

               T.Interrupt_Entry := True;

               --  Invoke a corresponding Server_Task if not yet created.
               --  Place Task_ID info in Server_ID array.

               if Server_ID (Interrupt) = Null_Task or else
                 Ada.Task_Identification.Is_Terminated
                 (To_Ada (Server_ID (Interrupt))) then

                  --  When a new Server_Task is created, it should have its
                  --  signal mask set to the All_Tasks_Mask.

                  IMOP.Set_Interrupt_Mask
                    (IMOP.All_Tasks_Mask'Access, Old_Mask'Access);

                  if Interrupt < System.VxWorks.Num_HW_Interrupts then
                     Interrupt_Access_Hold := new Interrupt_Server_Task
                       (Interrupt, semBCreate (SEM_Q_FIFO, SEM_EMPTY));
                     Server_ID (Interrupt) :=
                       To_System (Interrupt_Access_Hold.all'Identity);

                  else
                     Signal_Access_Hold := new Signal_Server_Task (Interrupt);
                     Server_ID (Interrupt) :=
                       To_System (Signal_Access_Hold.all'Identity);
                  end if;

                  IMOP.Set_Interrupt_Mask (Old_Mask'Access);
               end if;

               Bind_Handler (Interrupt);
               Unlock_Interrupt (Self_ID, Interrupt);
            end Bind_Interrupt_To_Entry;

            or accept Detach_Interrupt_Entries (T : Task_ID)
            do
               for Int in Interrupt_ID'Range loop
                  if not Is_Reserved (Int) then
                     Lock_Interrupt (Self_ID, Int);

                     if User_Entry (Int).T = T then

                        User_Entry (Int) := Entry_Assoc'
                          (T => Null_Task, E => Null_Task_Entry);
                        Unbind_Handler (Int);
                     end if;

                     Unlock_Interrupt (Self_ID, Int);
                  end if;
               end loop;

               --  Indicate in ATCB that no interrupt entries are attached.

               T.Interrupt_Entry := False;
            end Detach_Interrupt_Entries;

            end select;

         exception

            --  If there is a Program_Error we just want to propagate it to
            --  the caller and do not want to stop this task.

            when Program_Error =>
               null;

            when E : others =>
               pragma Assert
                 (Shutdown ("Interrupt_Manager---exception not expected" &
                            ASCII.LF &
                            Exception_Information (E)));
               null;
         end;
      end loop;

      pragma Assert (Shutdown ("Interrupt_Manager---should not get here"));
   exception
      when Standard'Abort_Signal =>
         --  Flush interrupt server semaphores, so they can terminate
         Finalize_Interrupt_Servers;
         raise;
   end Interrupt_Manager;

   ------------------------
   -- Signal_Server_Task --
   ------------------------

   task body Signal_Server_Task is
      Intwait_Mask    : aliased IMNG.Interrupt_Mask;
      Ret_Interrupt   : IMNG.Interrupt_ID;
      Self_ID         : Task_ID := Self;
      Tmp_Handler     : Parameterless_Handler;
      Tmp_ID          : Task_ID;
      Tmp_Entry_Index : Task_Entry_Index;

      use type IMNG.Interrupt_ID;

   begin
      --  By making this task independent of master, when the process
      --  goes away, the Server_Task will terminate gracefully.

      System.Tasking.Utilities.Make_Independent;

      --  Install default action in system level.

      IMOP.Install_Default_Action (To_Signal (Interrupt));

      --  Note: All tasks in RTS will have all reserved signals
      --  masked (except the Interrupt_Manager) and Keep_Unmasked
      --  unmasked when created.

      --  Abort_Task_Interrupt is one of the signals unmasked
      --  in all tasks. We mask it in this particular task
      --  so that "sigwait" can catch an explicit
      --  Abort_Task_Interrupt from the Interrupt_Manager.

      --  There are two signals that this task catches through
      --  "sigwait." One is the signal it is designated to catch
      --  in order to execute an user handler or entry. The other is
      --  Abort_Task_Interrupt. This signal is sent from the
      --  Interrupt_Manager to inform of status changes (e.g: become Blocked,
      --  or a handler or entry is to be detached).

      --  Prepare the mask to be used for sigwait.

      IMOP.Empty_Interrupt_Mask (Intwait_Mask'Access);

      IMOP.Add_To_Interrupt_Mask
        (Intwait_Mask'Access, To_Signal (Interrupt));

      IMOP.Add_To_Interrupt_Mask
        (Intwait_Mask'Access, IMNG.Abort_Task_Interrupt);

      IMOP.Thread_Block_Interrupt (IMNG.Abort_Task_Interrupt);

      PIO.Set_Interrupt_ID (To_Signal (Interrupt), Self_ID);

      loop
         System.Tasking.Initialization.Defer_Abort (Self_ID);
         POP.Write_Lock (Self_ID);

         if User_Handler (Interrupt).H = null
           and then User_Entry (Interrupt).T = Null_Task
         then

            --  No signal binding. If a signal is received,
            --  Interrupt_Manager will take the default action.

            Self_ID.Common.State := Interrupt_Server_Blocked_Interrupt_Sleep;
            POP.Sleep (Self_ID, Interrupt_Server_Idle_Sleep);
            Self_ID.Common.State := Runnable;

         else
            --  A handler or an entry is installed. At this point all tasks
            --  mask for the signal is masked. Catch it using
            --  sigwait.

            --  This task may wake up from sigwait by receiving a signal
            --  (Abort_Task_Interrupt) from the Interrupt_Manager for unbinding
            --  a procedure handler or an entry. Or it could be a wake up
            --  from status change (Unblocked -> Blocked). If that is not
            --  the case, we should execute the attached procedure or entry.

            POP.Unlock (Self_ID);

            Ret_Interrupt := IMOP.Interrupt_Wait (Intwait_Mask'Access);

            if Ret_Interrupt = IMNG.Abort_Task_Interrupt then
               --  Inform the Interrupt_Manager of wakeup from above sigwait.

               POP.Abort_Task (Interrupt_Manager_ID);
               POP.Write_Lock (Self_ID);

            else
               POP.Write_Lock (Self_ID);

               --  Even though we have received a signal, the status may
               --  have changed before we got the Self_ID lock above.
               --  Therefore we make sure a handler or an entry is still
               --  bound and make appropriate call.
               --  If there is no call to make we need to regenerate the
               --  signal in order not to lose it.

               if User_Handler (Interrupt).H /= null then

                  Tmp_Handler := User_Handler (Interrupt).H;

                  --  RTS calls should not be made with self being locked.

                  POP.Unlock (Self_ID);

                  Tmp_Handler.all;
                  POP.Write_Lock (Self_ID);

               elsif User_Entry (Interrupt).T /= Null_Task then

                  Tmp_ID := User_Entry (Interrupt).T;
                  Tmp_Entry_Index := User_Entry (Interrupt).E;

                  --  RTS calls should not be made with self being locked.

                  POP.Unlock (Self_ID);

                  System.Tasking.Rendezvous.Call_Simple
                    (Tmp_ID, Tmp_Entry_Index, System.Null_Address);

                  POP.Write_Lock (Self_ID);
               else
                  --  This is a situation where this task woke up receiving a
                  --  signal and before it got the lock the signal was blocked.
                  --  We do not want to lose the signal so we regenerate it at
                  --  the process level.

                  IMOP.Interrupt_Self_Process (Ret_Interrupt);
               end if;
            end if;
         end if;

         POP.Unlock (Self_ID);
         System.Tasking.Initialization.Undefer_Abort (Self_ID);

         --  Undefer abort here to allow a window for this task
         --  to be aborted at the time of system shutdown.
      end loop;
   end Signal_Server_Task;

   ---------------------------
   -- Interrupt_Server_Task --
   ---------------------------

   --  Server task for vectored hardware interrupt handling

   task body Interrupt_Server_Task is
      Self_ID         : Task_ID := Self;
      Tmp_Handler     : Parameterless_Handler;
      Tmp_ID          : Task_ID;
      Tmp_Entry_Index : Task_Entry_Index;
      S               : STATUS;

      use type STATUS;

   begin
      System.Tasking.Utilities.Make_Independent;
      Semaphore_ID_Map (Interrupt) := Int_Sema;

      loop
         --  Pend on semaphore that will be triggered by the
         --  umbrella handler when the associated interrupt comes in

         S := semTake (Int_Sema, WAIT_FOREVER);
         pragma Assert (S = 0);

         if User_Handler (Interrupt).H /= null then

            --  Protected procedure handler

            Tmp_Handler := User_Handler (Interrupt).H;
            Tmp_Handler.all;

         elsif User_Entry (Interrupt).T /= Null_Task then

            --  Interrupt entry handler

            Tmp_ID := User_Entry (Interrupt).T;
            Tmp_Entry_Index := User_Entry (Interrupt).E;
            System.Tasking.Rendezvous.Call_Simple
              (Tmp_ID, Tmp_Entry_Index, System.Null_Address);

         else
            --  Semaphore has been flushed by an unbind operation in
            --  the Interrupt_Manager. Terminate the server task.

            --  Wait for the Interrupt_Manager to complete its work

            POP.Write_Lock (Self_ID);

            --  Delete the associated semaphore

            S := semDelete (Int_Sema);

            pragma Assert (S = 0);

            --  Set status for the Interrupt_Manager

            Semaphore_ID_Map (Interrupt) := 0;
            Task_Lock (Interrupt) := False;
            Server_ID (Interrupt) := Null_Task;
            POP.Unlock (Self_ID);

            exit;
         end if;
      end loop;
   end Interrupt_Server_Task;

begin
   --  Elaboration code for package System.Interrupts

   --  Get Interrupt_Manager's ID so that Abort_Interrupt can be sent.

   Interrupt_Manager_ID := To_System (Interrupt_Manager'Identity);

   --  Initialize the lock L.

   Initialization.Defer_Abort (Self);
   POP.Initialize_Lock (L'Access, POP.PO_Level);
   Initialization.Undefer_Abort (Self);

   --  During the elaboration of this package body we want the RTS to
   --  inherit its signal mask from the Environment Task.

   --  The Environment Task should have gotten its mask from
   --  the enclosing process during the RTS start up. (See
   --  in s-inmaop.adb). Pass the Interrupt_Mask of the Environment
   --  task to the Interrupt_Manager.

   --  Note : At this point we know that all tasks (including
   --  RTS internal servers) are masked for non-reserved signals
   --  (see s-taprop.adb). Only the Interrupt_Manager will have
   --  masks set up differently, inheriting the original Environment
   --  Task's mask.

   Interrupt_Manager.Initialize (IMOP.Environment_Mask);
end System.Interrupts;
