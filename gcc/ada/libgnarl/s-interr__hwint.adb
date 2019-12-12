------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2019, Free Software Foundation, Inc.          --
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

--  Invariants:

--  All user-handlable signals are masked at all times in all tasks/threads
--  except possibly for the Interrupt_Manager task.

--  When a user task wants to have the effect of masking/unmasking an signal,
--  it must call Block_Interrupt/Unblock_Interrupt, which will have the effect
--  of unmasking/masking the signal in the Interrupt_Manager task. These
--  comments do not apply to vectored hardware interrupts, which may be masked
--  or unmasked using routined interfaced to the relevant embedded RTOS system
--  calls.

--  Once we associate a Signal_Server_Task with an signal, the task never goes
--  away, and we never remove the association. On the other hand, it is more
--  convenient to terminate an associated Interrupt_Server_Task for a vectored
--  hardware interrupt (since we use a binary semaphore for synchronization
--  with the umbrella handler).

--  There is no more than one signal per Signal_Server_Task and no more than
--  one Signal_Server_Task per signal. The same relation holds for hardware
--  interrupts and Interrupt_Server_Task's at any given time. That is, only
--  one non-terminated Interrupt_Server_Task exists for a give interrupt at
--  any time.

--  Within this package, the lock L is used to protect the various status
--  tables. If there is a Server_Task associated with a signal or interrupt,
--  we use the per-task lock of the Server_Task instead so that we protect the
--  status between Interrupt_Manager and Server_Task. Protection among service
--  requests are ensured via user calls to the Interrupt_Manager entries.

--  This is reasonably generic version of this package, supporting vectored
--  hardware interrupts using non-RTOS specific adapter routines which should
--  easily implemented on any RTOS capable of supporting GNAT.

with Ada.Unchecked_Conversion;
with Ada.Task_Identification;

with Interfaces.C; use Interfaces.C;
with System.OS_Interface; use System.OS_Interface;
with System.Interrupt_Management;
with System.Task_Primitives.Operations;
with System.Storage_Elements;
with System.Tasking.Utilities;

with System.Tasking.Rendezvous;
pragma Elaborate_All (System.Tasking.Rendezvous);

package body System.Interrupts is

   use Tasking;

   package POP renames System.Task_Primitives.Operations;

   function To_Ada is new Ada.Unchecked_Conversion
     (System.Tasking.Task_Id, Ada.Task_Identification.Task_Id);

   function To_System is new Ada.Unchecked_Conversion
     (Ada.Task_Identification.Task_Id, Task_Id);

   -----------------
   -- Local Tasks --
   -----------------

   --  WARNING: System.Tasking.Stages performs calls to this task with low-
   --  level constructs. Do not change this spec without synchronizing it.

   task Interrupt_Manager is
      entry Detach_Interrupt_Entries (T : Task_Id);

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
        (T         : Task_Id;
         E         : Task_Entry_Index;
         Interrupt : Interrupt_ID);

      pragma Interrupt_Priority (System.Interrupt_Priority'First);
   end Interrupt_Manager;

   task type Interrupt_Server_Task
     (Interrupt : Interrupt_ID;
      Int_Sema  : Binary_Semaphore_Id)
   is
      --  Server task for vectored hardware interrupt handling

      pragma Interrupt_Priority (System.Interrupt_Priority'First + 2);
   end Interrupt_Server_Task;

   type Interrupt_Task_Access is access Interrupt_Server_Task;

   -------------------------------
   -- Local Types and Variables --
   -------------------------------

   type Entry_Assoc is record
      T : Task_Id;
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
   --  information for each interrupt or signal. A handler is static iff it
   --  is specified through the pragma Attach_Handler.

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

   Server_ID : array (Interrupt_ID) of System.Tasking.Task_Id :=
                 (others => System.Tasking.Null_Task);
   pragma Atomic_Components (Server_ID);
   --  Holds the Task_Id of the Server_Task for each interrupt / signal.
   --  Task_Id is needed to accomplish locking per interrupt base. Also
   --  is needed to determine whether to create a new Server_Task.

   Semaphore_ID_Map : array
     (Interrupt_ID range 0 .. System.OS_Interface.Max_HW_Interrupt) of
        Binary_Semaphore_Id := (others => 0);
   --  Array of binary semaphores associated with vectored interrupts. Note
   --  that the last bound should be Max_HW_Interrupt, but this will raise
   --  Storage_Error if Num_HW_Interrupts is null so use extra 4 bytes instead.

   Interrupt_Access_Hold : Interrupt_Task_Access;
   --  Variable for allocating an Interrupt_Server_Task

   Handler_Installed : array (HW_Interrupt) of Boolean := (others => False);
   --  True if Notify_Interrupt was connected to the interrupt. Handlers can
   --  be connected but disconnection is not possible on VxWorks. Therefore
   --  we ensure Notify_Installed is connected at most once.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Reserved_Interrupt (Interrupt : Interrupt_ID);
   --  Check if Id is a reserved interrupt, and if so raise Program_Error
   --  with an appropriate message, otherwise return.

   procedure Finalize_Interrupt_Servers;
   --  Unbind the handlers for hardware interrupt server tasks at program
   --  termination.

   function Is_Registered (Handler : Parameterless_Handler) return Boolean;
   --  See if Handler has been "pragma"ed using Interrupt_Handler.
   --  Always consider a null handler as registered.

   procedure Notify_Interrupt (Param : System.Address);
   pragma Convention (C, Notify_Interrupt);
   --  Umbrella handler for vectored interrupts (not signals)

   procedure Install_Umbrella_Handler
     (Interrupt : HW_Interrupt;
      Handler   : System.OS_Interface.Interrupt_Handler);
   --  Install the runtime umbrella handler for a vectored hardware
   --  interrupt

   procedure Unimplemented (Feature : String);
   pragma No_Return (Unimplemented);
   --  Used to mark a call to an unimplemented function. Raises Program_Error
   --  with an appropriate message noting that Feature is unimplemented.

   --------------------
   -- Attach_Handler --
   --------------------

   --  Calling this procedure with New_Handler = null and Static = True
   --  means we want to detach the current handler regardless of the previous
   --  handler's binding status (i.e. do not care if it is a dynamic or static
   --  handler).

   --  This option is needed so that during the finalization of a PO, we can
   --  detach handlers attached through pragma Attach_Handler.

   procedure Attach_Handler
     (New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID;
      Static      : Boolean := False) is
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
     (T       : Task_Id;
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
         raise Program_Error with
           "interrupt" & Interrupt_ID'Image (Interrupt) & " is reserved";
      else
         return;
      end if;
   end Check_Reserved_Interrupt;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler
     (Interrupt : Interrupt_ID) return Parameterless_Handler
   is
   begin
      Check_Reserved_Interrupt (Interrupt);

      --  ??? Since Parameterless_Handler is not Atomic, the current
      --  implementation is wrong. We need a new service in Interrupt_Manager
      --  to ensure atomicity.

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

   procedure Detach_Interrupt_Entries (T : Task_Id) is
   begin
      Interrupt_Manager.Detach_Interrupt_Entries (T);
   end Detach_Interrupt_Entries;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   --  Calling this procedure with New_Handler = null and Static = True
   --  means we want to detach the current handler regardless of the previous
   --  handler's binding status (i.e. we do not care if it is a dynamic or
   --  static handler).

   --  This option is needed so that during the finalization of a PO, we can
   --  detach handlers attached through pragma Attach_Handler.

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

   --  Restore default handlers for interrupt servers

   --  This is called by the Interrupt_Manager task when it receives the abort
   --  signal during program finalization.

   procedure Finalize_Interrupt_Servers is
      HW_Interrupts : constant Boolean := HW_Interrupt'Last >= 0;
   begin
      if HW_Interrupts then
         for Int in HW_Interrupt loop
            if Server_ID (Interrupt_ID (Int)) /= null
              and then
                not Ada.Task_Identification.Is_Terminated
                 (To_Ada (Server_ID (Interrupt_ID (Int))))
            then
               Interrupt_Manager.Attach_Handler
                 (New_Handler => null,
                  Interrupt   => Interrupt_ID (Int),
                  Static      => True,
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
      pragma Unreferenced (Object);
   begin
      return True;
   end Has_Interrupt_Or_Attach_Handler;

   function Has_Interrupt_Or_Attach_Handler
     (Object : access Static_Interrupt_Protection)
      return   Boolean
   is
      pragma Unreferenced (Object);
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

   ----------------------
   -- Install_Handlers --
   ----------------------

   procedure Install_Handlers
     (Object       : access Static_Interrupt_Protection;
      New_Handlers : New_Handler_Array)
   is
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

   ---------------------------------
   -- Install_Restricted_Handlers --
   ---------------------------------

   procedure Install_Restricted_Handlers
      (Prio     : Any_Priority;
       Handlers : New_Handler_Array)
   is
      pragma Unreferenced (Prio);
   begin
      for N in Handlers'Range loop
         Attach_Handler (Handlers (N).Handler, Handlers (N).Interrupt, True);
      end loop;
   end Install_Restricted_Handlers;

   ------------------------------
   -- Install_Umbrella_Handler --
   ------------------------------

   procedure Install_Umbrella_Handler
     (Interrupt : HW_Interrupt;
      Handler   : System.OS_Interface.Interrupt_Handler)
   is
      Vec : constant Interrupt_Vector :=
              Interrupt_Number_To_Vector (int (Interrupt));

      Status : int;

   begin
      --  Only install umbrella handler when no Ada handler has already been
      --  installed. Note that the interrupt number is passed as a parameter
      --  when an interrupt occurs, so the umbrella handler has a different
      --  wrapper generated by intConnect for each interrupt number.

      if not Handler_Installed (Interrupt) then
         Status :=
            Interrupt_Connect (Vec, Handler, System.Address (Interrupt));
         pragma Assert (Status = 0);

         Handler_Installed (Interrupt) := True;
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

   function Is_Registered (Handler : Parameterless_Handler) return Boolean is

      type Acc_Proc is access procedure;

      type Fat_Ptr is record
         Object_Addr  : System.Address;
         Handler_Addr : Acc_Proc;
      end record;

      function To_Fat_Ptr is new Ada.Unchecked_Conversion
        (Parameterless_Handler, Fat_Ptr);

      Ptr : R_Link;
      Fat : Fat_Ptr;

   begin
      if Handler = null then
         return True;
      end if;

      Fat := To_Fat_Ptr (Handler);

      Ptr := Registered_Handler_Head;
      while Ptr /= null loop
         if Ptr.H = Fat.Handler_Addr.all'Address then
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
      use System.Interrupt_Management;
   begin
      return Reserve (System.Interrupt_Management.Interrupt_ID (Interrupt));
   end Is_Reserved;

   ----------------------
   -- Notify_Interrupt --
   ----------------------

   --  Umbrella handler for vectored hardware interrupts (as opposed to signals
   --  and exceptions). As opposed to the signal implementation, this handler
   --  is installed in the vector table when the first Ada handler is attached
   --  to the interrupt. However because VxWorks don't support disconnecting
   --  handlers, this subprogram always test whether or not an Ada handler is
   --  effectively attached.

   --  Otherwise, the handler that existed prior to program startup is in the
   --  vector table. This ensures that handlers installed by the BSP are active
   --  unless explicitly replaced in the program text.

   --  Each Interrupt_Server_Task has an associated binary semaphore on which
   --  it pends once it's been started. This routine determines The appropriate
   --  semaphore and issues a semGive call, waking the server task. When
   --  a handler is unbound, System.Interrupts.Unbind_Handler issues a
   --  Binary_Semaphore_Flush, and the server task deletes its semaphore
   --  and terminates.

   procedure Notify_Interrupt (Param : System.Address) is
      Interrupt : constant Interrupt_ID := Interrupt_ID (Param);
      Id        : constant Binary_Semaphore_Id := Semaphore_ID_Map (Interrupt);
      Status    : int;
   begin
      if Id /= 0 then
         Status := Binary_Semaphore_Release (Id);
         pragma Assert (Status = 0);
      end if;
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
      --  This routine registers a handler as usable for dynamic interrupt
      --  handler association. Routines attaching and detaching handlers
      --  dynamically should determine whether the handler is registered.
      --  Program_Error should be raised if it is not registered.

      --  Pragma Interrupt_Handler can only appear in a library level PO
      --  definition and instantiation. Therefore, we do not need to implement
      --  an unregister operation. Nor do we need to protect the queue
      --  structure with a lock.

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
     (Interrupt : Interrupt_ID) return System.Tasking.Task_Id
   is
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
      raise Program_Error with Feature & " not implemented on VxWorks";
   end Unimplemented;

   -----------------------
   -- Interrupt_Manager --
   -----------------------

   task body Interrupt_Manager is
      --  By making this task independent of any master, when the process goes
      --  away, the Interrupt_Manager will terminate gracefully.

      Ignore : constant Boolean := System.Tasking.Utilities.Make_Independent;
      pragma Unreferenced (Ignore);

      --------------------
      -- Local Routines --
      --------------------

      procedure Bind_Handler (Interrupt : Interrupt_ID);
      --  This procedure does not do anything if a signal is blocked.
      --  Otherwise, we have to interrupt Server_Task for status change
      --  through a wakeup signal.

      procedure Unbind_Handler (Interrupt : Interrupt_ID);
      --  This procedure does not do anything if a signal is blocked.
      --  Otherwise, we have to interrupt Server_Task for status change
      --  through an abort signal.

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
         Install_Umbrella_Handler
           (HW_Interrupt (Interrupt), Notify_Interrupt'Access);
      end Bind_Handler;

      --------------------
      -- Unbind_Handler --
      --------------------

      procedure Unbind_Handler (Interrupt : Interrupt_ID) is
         Status : int;

      begin
         --  Flush server task off semaphore, allowing it to terminate

         Status := Binary_Semaphore_Flush (Semaphore_ID_Map (Interrupt));
         pragma Assert (Status = 0);
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

            --  If an interrupt entry is installed raise Program_Error
            --  (propagate it to the caller).

            raise Program_Error with
              "an interrupt entry is already installed";
         end if;

         --  Note : Static = True will pass the following check. This is the
         --  case when we want to detach a handler regardless of the static
         --  status of the Current_Handler.

         if not Static and then User_Handler (Interrupt).Static then

            --  Trying to detach a static Interrupt Handler, raise
            --  Program_Error.

            raise Program_Error with
              "trying to detach a static Interrupt Handler";
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
         Restoration : Boolean := False)
      is
      begin
         if User_Entry (Interrupt).T /= Null_Task then

            --  If an interrupt entry is already installed, raise
            --  Program_Error (propagate it to the caller).

            raise Program_Error with "an interrupt is already installed";
         end if;

         --  Note : A null handler with Static = True will pass the following
         --  check. This is the case when we want to detach a handler
         --  regardless of the Static status of Current_Handler.

         --  We don't check anything if Restoration is True, since we may be
         --  detaching a static handler to restore a dynamic one.

         if not Restoration and then not Static
           and then (User_Handler (Interrupt).Static

            --  Trying to overwrite a static Interrupt Handler with a dynamic
            --  Handler

            --  The new handler is not specified as an Interrupt Handler by a
            --  pragma.

           or else not Is_Registered (New_Handler))
         then
            raise Program_Error with
               "trying to overwrite a static interrupt handler with a "
               & "dynamic handler";
         end if;

         --  Save the old handler

         Old_Handler := User_Handler (Interrupt).H;

         --  The new handler

         User_Handler (Interrupt).H := New_Handler;

         if New_Handler = null then

            --  The null handler means we are detaching the handler

            User_Handler (Interrupt).Static := False;

         else
            User_Handler (Interrupt).Static := Static;
         end if;

         --  Invoke a corresponding Server_Task if not yet created. Place
         --  Task_Id info in Server_ID array.

         if New_Handler /= null
           and then
            (Server_ID (Interrupt) = Null_Task
              or else
                Ada.Task_Identification.Is_Terminated
                  (To_Ada (Server_ID (Interrupt))))
         then
            Interrupt_Access_Hold :=
              new Interrupt_Server_Task (Interrupt, Binary_Semaphore_Create);
            Server_ID (Interrupt) :=
              To_System (Interrupt_Access_Hold.all'Identity);
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
                  Unprotected_Exchange_Handler
                    (Old_Handler, New_Handler, Interrupt, Static, Restoration);
               end Attach_Handler;

            or
               accept Exchange_Handler
                 (Old_Handler : out Parameterless_Handler;
                  New_Handler : Parameterless_Handler;
                  Interrupt   : Interrupt_ID;
                  Static      : Boolean)
               do
                  Unprotected_Exchange_Handler
                    (Old_Handler, New_Handler, Interrupt, Static);
               end Exchange_Handler;

            or
               accept Detach_Handler
                  (Interrupt : Interrupt_ID;
                   Static    : Boolean)
               do
                  Unprotected_Detach_Handler (Interrupt, Static);
               end Detach_Handler;

            or
               accept Bind_Interrupt_To_Entry
                 (T         : Task_Id;
                  E         : Task_Entry_Index;
                  Interrupt : Interrupt_ID)
               do
                  --  If there is a binding already (either a procedure or an
                  --  entry), raise Program_Error (propagate it to the caller).

                  if User_Handler (Interrupt).H /= null
                    or else User_Entry (Interrupt).T /= Null_Task
                  then
                     raise Program_Error with
                       "a binding for this interrupt is already present";
                  end if;

                  User_Entry (Interrupt) := Entry_Assoc'(T => T, E => E);

                  --  Indicate the attachment of interrupt entry in the ATCB.
                  --  This is needed so when an interrupt entry task terminates
                  --  the binding can be cleaned. The call to unbinding must be
                  --  make by the task before it terminates.

                  T.Interrupt_Entry := True;

                  --  Invoke a corresponding Server_Task if not yet created.
                  --  Place Task_Id info in Server_ID array.

                  if Server_ID (Interrupt) = Null_Task
                    or else
                      Ada.Task_Identification.Is_Terminated
                        (To_Ada (Server_ID (Interrupt)))
                  then
                     Interrupt_Access_Hold := new Interrupt_Server_Task
                       (Interrupt, Binary_Semaphore_Create);
                     Server_ID (Interrupt) :=
                       To_System (Interrupt_Access_Hold.all'Identity);
                  end if;

                  Bind_Handler (Interrupt);
               end Bind_Interrupt_To_Entry;

            or
               accept Detach_Interrupt_Entries (T : Task_Id) do
                  for Int in Interrupt_ID'Range loop
                     if not Is_Reserved (Int) then
                        if User_Entry (Int).T = T then
                           User_Entry (Int) :=
                             Entry_Assoc'
                               (T => Null_Task, E => Null_Task_Entry);
                           Unbind_Handler (Int);
                        end if;
                     end if;
                  end loop;

                  --  Indicate in ATCB that no interrupt entries are attached

                  T.Interrupt_Entry := False;
               end Detach_Interrupt_Entries;
            end select;

         exception
            --  If there is a Program_Error we just want to propagate it to
            --  the caller and do not want to stop this task.

            when Program_Error =>
               null;

            when others =>
               pragma Assert (False);
               null;
         end;
      end loop;

   exception
      when Standard'Abort_Signal =>

         --  Flush interrupt server semaphores, so they can terminate

         Finalize_Interrupt_Servers;
         raise;
   end Interrupt_Manager;

   ---------------------------
   -- Interrupt_Server_Task --
   ---------------------------

   --  Server task for vectored hardware interrupt handling

   task body Interrupt_Server_Task is
      Ignore : constant Boolean := System.Tasking.Utilities.Make_Independent;

      Self_Id         : constant Task_Id := Self;
      Tmp_Handler     : Parameterless_Handler;
      Tmp_ID          : Task_Id;
      Tmp_Entry_Index : Task_Entry_Index;
      Status          : int;

   begin
      Semaphore_ID_Map (Interrupt) := Int_Sema;

      loop
         --  Pend on semaphore that will be triggered by the umbrella handler
         --  when the associated interrupt comes in.

         Status := Binary_Semaphore_Obtain (Int_Sema);
         pragma Assert (Status = 0);

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
            --  Semaphore has been flushed by an unbind operation in the
            --  Interrupt_Manager. Terminate the server task.

            --  Wait for the Interrupt_Manager to complete its work

            POP.Write_Lock (Self_Id);

            --  Unassociate the interrupt handler

            Semaphore_ID_Map (Interrupt) := 0;

            --  Delete the associated semaphore

            Status := Binary_Semaphore_Delete (Int_Sema);

            pragma Assert (Status = 0);

            --  Set status for the Interrupt_Manager

            Server_ID (Interrupt) := Null_Task;
            POP.Unlock (Self_Id);

            exit;
         end if;
      end loop;
   end Interrupt_Server_Task;

begin
   --  Get Interrupt_Manager's ID so that Abort_Interrupt can be sent

   Interrupt_Manager_ID := To_System (Interrupt_Manager'Identity);
end System.Interrupts;
