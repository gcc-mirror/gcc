------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2004, Free Software Foundation, Inc.          --
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

--  This is an OpenVMS/Alpha version of this package.

--  Invariants:

--  Once we associate a Server_Task with an interrupt, the task never
--  goes away, and we never remove the association.

--  There is no more than one interrupt per Server_Task and no more than
--  one Server_Task per interrupt.

--  Within this package, the lock L is used to protect the various status
--  tables. If there is a Server_Task associated with an interrupt, we use
--  the per-task lock of the Server_Task instead so that we protect the
--  status between Interrupt_Manager and Server_Task. Protection among
--  service requests are done using User Request to Interrupt_Manager
--  rendezvous.

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
pragma Elaborate_All (System.Interrupt_Management.Operations);

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

with System.Parameters;
--  used for Single_Lock

with Unchecked_Conversion;

package body System.Interrupts is

   use Tasking;
   use System.Parameters;
   use Ada.Exceptions;

   package POP renames System.Task_Primitives.Operations;
   package PIO renames System.Task_Primitives.Interrupt_Operations;
   package IMNG renames System.Interrupt_Management;
   package IMOP renames System.Interrupt_Management.Operations;

   function To_System is new Unchecked_Conversion
     (Ada.Task_Identification.Task_Id, Task_ID);

   -----------------
   -- Local Tasks --
   -----------------

   --  WARNING: System.Tasking.Stages performs calls to this task
   --  with low-level constructs. Do not change this spec without synchro-
   --  nizing it.

   task Interrupt_Manager is
      entry Detach_Interrupt_Entries (T : Task_ID);

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
        (Interrupt   : Interrupt_ID;
         Static      : Boolean);

      entry Bind_Interrupt_To_Entry
        (T         : Task_ID;
         E         : Task_Entry_Index;
         Interrupt : Interrupt_ID);

      entry Block_Interrupt (Interrupt : Interrupt_ID);

      entry Unblock_Interrupt (Interrupt : Interrupt_ID);

      entry Ignore_Interrupt (Interrupt : Interrupt_ID);

      entry Unignore_Interrupt (Interrupt : Interrupt_ID);

      pragma Interrupt_Priority (System.Interrupt_Priority'Last);
   end Interrupt_Manager;

   task type Server_Task (Interrupt : Interrupt_ID) is
      pragma Priority (System.Interrupt_Priority'Last);
      --  Note: the above pragma Priority is strictly speaking improper
      --  since it is outside the range of allowed priorities, but the
      --  compiler treats system units specially and does not apply
      --  this range checking rule to system units.

   end Server_Task;

   type Server_Task_Access is access Server_Task;

   --------------------------------
   --  Local Types and Variables --
   --------------------------------

   type Entry_Assoc is record
      T : Task_ID;
      E : Task_Entry_Index;
   end record;

   type Handler_Assoc is record
      H      : Parameterless_Handler;
      Static : Boolean;   --  Indicates static binding;
   end record;

   User_Handler : array (Interrupt_ID'Range) of Handler_Assoc :=
                    (others => (null, Static => False));
   pragma Volatile_Components (User_Handler);
   --  Holds the protected procedure handler (if any) and its Static
   --  information  for each interrupt. A handler is a Static one if
   --  it is specified through the pragma Attach_Handler.
   --  Attach_Handler. Otherwise, not static)

   User_Entry : array (Interrupt_ID'Range) of Entry_Assoc :=
                  (others => (T => Null_Task, E => Null_Task_Entry));
   pragma Volatile_Components (User_Entry);
   --  Holds the task and entry index (if any) for each interrupt

   Blocked : constant array (Interrupt_ID'Range) of Boolean :=
     (others => False);
--  ??? pragma Volatile_Components (Blocked);
   --  True iff the corresponding interrupt is blocked in the process level

   Ignored : array (Interrupt_ID'Range) of Boolean := (others => False);
   pragma Volatile_Components (Ignored);
   --  True iff the corresponding interrupt is blocked in the process level

   Last_Unblocker : constant array (Interrupt_ID'Range) of Task_ID :=
     (others => Null_Task);
--  ??? pragma Volatile_Components (Last_Unblocker);
   --  Holds the ID of the last Task which Unblocked this Interrupt.
   --  It contains Null_Task if no tasks have ever requested the
   --  Unblocking operation or the Interrupt is currently Blocked.

   Server_ID : array (Interrupt_ID'Range) of Task_ID :=
                 (others => Null_Task);
   pragma Atomic_Components (Server_ID);
   --  Holds the Task_ID of the Server_Task for each interrupt.
   --  Task_ID is needed to accomplish locking per Interrupt base. Also
   --  is needed to decide whether to create a new Server_Task.

   --  Type and Head, Tail of the list containing Registered Interrupt
   --  Handlers. These definitions are used to register the handlers
   --  specified by the pragma Interrupt_Handler.

   type Registered_Handler;
   type R_Link is access all Registered_Handler;

   type Registered_Handler is record
      H :    System.Address := System.Null_Address;
      Next : R_Link := null;
   end record;

   Registered_Handler_Head : R_Link := null;
   Registered_Handler_Tail : R_Link := null;

   Access_Hold : Server_Task_Access;
   --  variable used to allocate Server_Task using "new".

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Is_Registered (Handler : Parameterless_Handler) return Boolean;
   --  See if the Handler has been "pragma"ed using Interrupt_Handler.
   --  Always consider a null handler as registered.

   --------------------------------
   -- Register_Interrupt_Handler --
   --------------------------------

   procedure Register_Interrupt_Handler (Handler_Addr : System.Address) is
      New_Node_Ptr : R_Link;
   begin
      --  This routine registers the Handler as usable for Dynamic
      --  Interrupt Handler. Routines attaching and detaching Handler
      --  dynamically should first consult if the Handler is rgistered.
      --  A Program Error should be raised if it is not registered.

      --  The pragma Interrupt_Handler can only appear in the library
      --  level PO definition and instantiation. Therefore, we do not need
      --  to implement Unregistering operation. Neither we need to
      --  protect the queue structure using a Lock.

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

   -------------------
   -- Is_Registered --
   -------------------

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

      while Ptr /= null loop
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
      return IMNG.Reserve (IMNG.Interrupt_ID (Interrupt));
   end Is_Reserved;

   -----------------------
   -- Is_Entry_Attached --
   -----------------------

   function Is_Entry_Attached (Interrupt : Interrupt_ID) return Boolean is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      return User_Entry (Interrupt).T /= Null_Task;
   end Is_Entry_Attached;

   -------------------------
   -- Is_Handler_Attached --
   -------------------------

   function Is_Handler_Attached (Interrupt : Interrupt_ID) return Boolean is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      return User_Handler (Interrupt).H /= null;
   end Is_Handler_Attached;

   ----------------
   -- Is_Blocked --
   ----------------

   function Is_Blocked (Interrupt : Interrupt_ID) return Boolean is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      return Blocked (Interrupt);
   end Is_Blocked;

   ----------------
   -- Is_Ignored --
   ----------------

   function Is_Ignored (Interrupt : Interrupt_ID) return Boolean is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      return Ignored (Interrupt);
   end Is_Ignored;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (Interrupt : Interrupt_ID)
     return Parameterless_Handler is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      --  ??? Since Parameterless_Handler is not Atomic, the
      --  current implementation is wrong. We need a new service in
      --  Interrupt_Manager to ensure atomicity.

      return User_Handler (Interrupt).H;
   end Current_Handler;

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
      Static      : Boolean := False) is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      Interrupt_Manager.Attach_Handler (New_Handler, Interrupt, Static);

   end Attach_Handler;

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
      Static      : Boolean := False) is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      Interrupt_Manager.Exchange_Handler
        (Old_Handler, New_Handler, Interrupt, Static);

   end Exchange_Handler;

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
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      Interrupt_Manager.Detach_Handler (Interrupt, Static);
   end Detach_Handler;

   ---------------
   -- Reference --
   ---------------

   function Reference (Interrupt : Interrupt_ID) return System.Address is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      return Storage_Elements.To_Address
        (Storage_Elements.Integer_Address (Interrupt));
   end Reference;

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
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      Interrupt_Manager.Bind_Interrupt_To_Entry (T, E, Interrupt);

   end Bind_Interrupt_To_Entry;

   ------------------------------
   -- Detach_Interrupt_Entries --
   ------------------------------

   procedure Detach_Interrupt_Entries (T : Task_ID) is
   begin
      Interrupt_Manager.Detach_Interrupt_Entries (T);
   end Detach_Interrupt_Entries;

   ---------------------
   -- Block_Interrupt --
   ---------------------

   procedure Block_Interrupt (Interrupt : Interrupt_ID) is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      Interrupt_Manager.Block_Interrupt (Interrupt);
   end Block_Interrupt;

   -----------------------
   -- Unblock_Interrupt --
   -----------------------

   procedure Unblock_Interrupt (Interrupt : Interrupt_ID) is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      Interrupt_Manager.Unblock_Interrupt (Interrupt);
   end Unblock_Interrupt;

   ------------------
   -- Unblocked_By --
   ------------------

   function Unblocked_By
     (Interrupt : Interrupt_ID) return System.Tasking.Task_ID is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      return Last_Unblocker (Interrupt);
   end Unblocked_By;

   ----------------------
   -- Ignore_Interrupt --
   ----------------------

   procedure Ignore_Interrupt (Interrupt : Interrupt_ID) is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      Interrupt_Manager.Ignore_Interrupt (Interrupt);
   end Ignore_Interrupt;

   ------------------------
   -- Unignore_Interrupt --
   ------------------------

   procedure Unignore_Interrupt (Interrupt : Interrupt_ID) is
   begin
      if Is_Reserved (Interrupt) then
         Raise_Exception (Program_Error'Identity, "Interrupt" &
           Interrupt_ID'Image (Interrupt) & " is reserved");
      end if;

      Interrupt_Manager.Unignore_Interrupt (Interrupt);
   end Unignore_Interrupt;

   -----------------------
   -- Interrupt_Manager --
   -----------------------

   task body Interrupt_Manager is

      ---------------------
      --  Local Routines --
      ---------------------

      procedure Unprotected_Exchange_Handler
        (Old_Handler : out Parameterless_Handler;
         New_Handler : Parameterless_Handler;
         Interrupt   : Interrupt_ID;
         Static      : Boolean;
         Restoration : Boolean := False);

      procedure Unprotected_Detach_Handler
        (Interrupt : Interrupt_ID;
         Static    : Boolean);

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
            --  In case we have an Interrupt Entry already installed.
            --  raise a program error. (propagate it to the caller).

            Raise_Exception (Program_Error'Identity,
              "An interrupt is already installed");
         end if;

         --  Note : A null handler with Static = True will
         --  pass the following check. That is the case when we want to
         --  Detach a handler regardless of the Static status
         --  of the current_Handler.
         --  We don't check anything if Restoration is True, since we
         --  may be detaching a static handler to restore a dynamic one.

         if not Restoration and then not Static
            --  Tries to overwrite a static Interrupt Handler with a
            --  dynamic Handler

           and then (User_Handler (Interrupt).Static

                        --  The new handler is not specified as an
                        --  Interrupt Handler by a pragma.

                        or else not Is_Registered (New_Handler))
         then
            Raise_Exception (Program_Error'Identity,
              "Trying to overwrite a static Interrupt Handler with a " &
              "dynamic Handler");
         end if;

         --  The interrupt should no longer be ingnored if
         --  it was ever ignored.

         Ignored (Interrupt) := False;

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

         if Server_ID (Interrupt) = Null_Task then
            Access_Hold := new Server_Task (Interrupt);
            Server_ID (Interrupt) := To_System (Access_Hold.all'Identity);
         else
            POP.Wakeup (Server_ID (Interrupt), Interrupt_Server_Idle_Sleep);
         end if;

      end Unprotected_Exchange_Handler;

      --------------------------------
      -- Unprotected_Detach_Handler --
      --------------------------------

      procedure Unprotected_Detach_Handler
        (Interrupt   : Interrupt_ID;
         Static      : Boolean)
      is
      begin
         if User_Entry (Interrupt).T /= Null_Task then
            --  In case we have an Interrupt Entry installed.
            --  raise a program error. (propagate it to the caller).

            Raise_Exception (Program_Error'Identity,
              "An interrupt entry is already installed");
         end if;

         --  Note : Static = True will pass the following check. That is the
         --  case when we want to detach a handler regardless of the static
         --  status of the current_Handler.

         if not Static and then User_Handler (Interrupt).Static then
            --  Tries to detach a static Interrupt Handler.
            --  raise a program error.

            Raise_Exception (Program_Error'Identity,
              "Trying to detach a static Interrupt Handler");
         end if;

         --  The interrupt should no longer be ignored if
         --  it was ever ignored.

         Ignored (Interrupt) := False;

         --  The new handler

         User_Handler (Interrupt).H := null;
         User_Handler (Interrupt).Static := False;
         IMOP.Interrupt_Self_Process (IMNG.Interrupt_ID (Interrupt));

      end Unprotected_Detach_Handler;

   --  Start of processing for Interrupt_Manager

   begin
      --  By making this task independent of master, when the process
      --  goes away, the Interrupt_Manager will terminate gracefully.

      System.Tasking.Utilities.Make_Independent;

      --  Environmen task gets its own interrupt mask, saves it,
      --  and then masks all interrupts except the Keep_Unmasked set.

      --  During rendezvous, the Interrupt_Manager receives the old
      --  interrupt mask of the environment task, and sets its own
      --  interrupt mask to that value.

      --  The environment task will call the entry of Interrupt_Manager some
      --  during elaboration of the body of this package.

      accept Initialize (Mask : IMNG.Interrupt_Mask) do
         pragma Warnings (Off, Mask);
         null;
      end Initialize;

      --  Note: All tasks in RTS will have all the Reserve Interrupts
      --  being masked (except the Interrupt_Manager) and Keep_Unmasked
      --  unmasked when created.

      --  Abort_Task_Interrupt is one of the Interrupt unmasked
      --  in all tasks. We mask the Interrupt in this particular task
      --  so that "sigwait" is possible to catch an explicitely sent
      --  Abort_Task_Interrupt from the Server_Tasks.

      --  This sigwaiting is needed so that we make sure a Server_Task is
      --  out of its own sigwait state. This extra synchronization is
      --  necessary to prevent following senarios.

      --   1) Interrupt_Manager sends an Abort_Task_Interrupt to the
      --      Server_Task then changes its own interrupt mask (OS level).
      --      If an interrupt (corresponding to the Server_Task) arrives
      --      in the nean time we have the Interrupt_Manager umnasked and
      --      the Server_Task waiting on sigwait.

      --   2) For unbinding handler, we install a default action in the
      --      Interrupt_Manager. POSIX.1c states that the result of using
      --      "sigwait" and "sigaction" simaltaneously on the same interrupt
      --      is undefined. Therefore, we need to be informed from the
      --      Server_Task of the fact that the Server_Task is out of its
      --      sigwait stage.

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

            or accept Exchange_Handler
               (Old_Handler : out Parameterless_Handler;
                New_Handler : Parameterless_Handler;
                Interrupt   : Interrupt_ID;
                Static      : Boolean)
            do
               Unprotected_Exchange_Handler
                 (Old_Handler, New_Handler, Interrupt, Static);
            end Exchange_Handler;

            or accept Detach_Handler
               (Interrupt   : Interrupt_ID;
                Static      : Boolean)
            do
               Unprotected_Detach_Handler (Interrupt, Static);
            end Detach_Handler;

            or accept Bind_Interrupt_To_Entry
              (T       : Task_ID;
               E       : Task_Entry_Index;
               Interrupt : Interrupt_ID)
            do
               --  if there is a binding already (either a procedure or an
               --  entry), raise Program_Error (propagate it to the caller).

               if User_Handler (Interrupt).H /= null
                 or else User_Entry (Interrupt).T /= Null_Task
               then
                  Raise_Exception (Program_Error'Identity,
                    "A binding for this interrupt is already present");
               end if;

               --  The interrupt should no longer be ingnored if
               --  it was ever ignored.

               Ignored (Interrupt) := False;
               User_Entry (Interrupt) := Entry_Assoc'(T => T, E => E);

               --  Indicate the attachment of Interrupt Entry in ATCB.
               --  This is need so that when an Interrupt Entry task
               --  terminates the binding can be cleaned.
               --  The call to unbinding must be
               --  make by the task before it terminates.

               T.Interrupt_Entry := True;

               --  Invoke a corresponding Server_Task if not yet created.
               --  Place Task_ID info in Server_ID array.

               if Server_ID (Interrupt) = Null_Task then

                  Access_Hold := new Server_Task (Interrupt);
                  Server_ID (Interrupt) :=
                    To_System (Access_Hold.all'Identity);
               else
                  POP.Wakeup (Server_ID (Interrupt),
                              Interrupt_Server_Idle_Sleep);
               end if;
            end Bind_Interrupt_To_Entry;

            or accept Detach_Interrupt_Entries (T : Task_ID)
            do
               for J in Interrupt_ID'Range loop
                  if not Is_Reserved (J) then
                     if User_Entry (J).T = T then

                        --  The interrupt should no longer be ignored if
                        --  it was ever ignored.

                        Ignored (J) := False;
                        User_Entry (J) :=
                          Entry_Assoc'(T => Null_Task, E => Null_Task_Entry);
                        IMOP.Interrupt_Self_Process (IMNG.Interrupt_ID (J));
                     end if;
                  end if;
               end loop;

               --  Indicate in ATCB that no Interrupt Entries are attached.

               T.Interrupt_Entry := False;
            end Detach_Interrupt_Entries;

            or accept Block_Interrupt (Interrupt : Interrupt_ID) do
               pragma Warnings (Off, Interrupt);
               raise Program_Error;
            end Block_Interrupt;

            or accept Unblock_Interrupt (Interrupt : Interrupt_ID) do
               pragma Warnings (Off, Interrupt);
               raise Program_Error;
            end Unblock_Interrupt;

            or accept Ignore_Interrupt (Interrupt : Interrupt_ID) do
               pragma Warnings (Off, Interrupt);
               raise Program_Error;
            end Ignore_Interrupt;

            or accept Unignore_Interrupt (Interrupt : Interrupt_ID) do
               pragma Warnings (Off, Interrupt);
               raise Program_Error;
            end Unignore_Interrupt;

            end select;

         exception
            --  If there is a program error we just want to propagate it
            --  to the caller and do not want to stop this task.

            when Program_Error =>
               null;

            when others =>
               pragma Assert (False);
               null;
         end;
      end loop;
   end Interrupt_Manager;

   -----------------
   -- Server_Task --
   -----------------

   task body Server_Task is
      Self_ID         : Task_ID := Self;
      Tmp_Handler     : Parameterless_Handler;
      Tmp_ID          : Task_ID;
      Tmp_Entry_Index : Task_Entry_Index;
      Intwait_Mask    : aliased IMNG.Interrupt_Mask;

   begin
      --  By making this task independent of master, when the process
      --  goes away, the Server_Task will terminate gracefully.

      System.Tasking.Utilities.Make_Independent;

      --  Install default action in system level.

      IMOP.Install_Default_Action (IMNG.Interrupt_ID (Interrupt));

      --  Set up the mask (also clears the event flag)

      IMOP.Empty_Interrupt_Mask (Intwait_Mask'Access);
      IMOP.Add_To_Interrupt_Mask
        (Intwait_Mask'Access, IMNG.Interrupt_ID (Interrupt));

      --  Remember the Interrupt_ID for Abort_Task.

      PIO.Set_Interrupt_ID (IMNG.Interrupt_ID (Interrupt), Self_ID);

      --  Note: All tasks in RTS will have all the Reserve Interrupts
      --  being masked (except the Interrupt_Manager) and Keep_Unmasked
      --  unmasked when created.

      loop
         System.Tasking.Initialization.Defer_Abort (Self_ID);

         --  A Handler or an Entry is installed. At this point all tasks
         --  mask for the Interrupt is masked. Catch the Interrupt using
         --  sigwait.

         --  This task may wake up from sigwait by receiving an interrupt
         --  (Abort_Task_Interrupt) from the Interrupt_Manager for unbinding
         --  a Procedure Handler or an Entry. Or it could be a wake up
         --  from status change (Unblocked -> Blocked). If that is not
         --  the case, we should exceute the attached Procedure or Entry.

         if Single_Lock then
            POP.Lock_RTS;
         end if;

         POP.Write_Lock (Self_ID);

         if User_Handler (Interrupt).H = null
           and then User_Entry (Interrupt).T = Null_Task
         then
            --  No Interrupt binding. If there is an interrupt,
            --  Interrupt_Manager will take default action.

            Self_ID.Common.State := Interrupt_Server_Idle_Sleep;
            POP.Sleep (Self_ID, Interrupt_Server_Idle_Sleep);
            Self_ID.Common.State := Runnable;

         else
            Self_ID.Common.State := Interrupt_Server_Blocked_On_Event_Flag;
            Self_ID.Common.State := Runnable;

            if not (Self_ID.Deferral_Level = 0
                    and then Self_ID.Pending_ATC_Level
                             < Self_ID.ATC_Nesting_Level)
            then
               if User_Handler (Interrupt).H /= null then
                  Tmp_Handler := User_Handler (Interrupt).H;

                  --  RTS calls should not be made with self being locked.

                  POP.Unlock (Self_ID);

                  if Single_Lock then
                     POP.Unlock_RTS;
                  end if;

                  Tmp_Handler.all;

                  if Single_Lock then
                     POP.Lock_RTS;
                  end if;

                  POP.Write_Lock (Self_ID);

               elsif User_Entry (Interrupt).T /= Null_Task then
                  Tmp_ID := User_Entry (Interrupt).T;
                  Tmp_Entry_Index := User_Entry (Interrupt).E;

                  --  RTS calls should not be made with self being locked.

                  POP.Unlock (Self_ID);

                  if Single_Lock then
                     POP.Unlock_RTS;
                  end if;

                  System.Tasking.Rendezvous.Call_Simple
                    (Tmp_ID, Tmp_Entry_Index, System.Null_Address);

                  if Single_Lock then
                     POP.Lock_RTS;
                  end if;

                  POP.Write_Lock (Self_ID);
               end if;
            end if;
         end if;

         POP.Unlock (Self_ID);

         if Single_Lock then
            POP.Unlock_RTS;
         end if;

         System.Tasking.Initialization.Undefer_Abort (Self_ID);

         --  Undefer abort here to allow a window for this task
         --  to be aborted  at the time of system shutdown.
      end loop;
   end Server_Task;

   -------------------------------------
   -- Has_Interrupt_Or_Attach_Handler --
   -------------------------------------

   function Has_Interrupt_Or_Attach_Handler
     (Object : access Dynamic_Interrupt_Protection)
      return   Boolean
   is
      pragma Warnings (Off, Object);

   begin
      return True;
   end Has_Interrupt_Or_Attach_Handler;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Object : in out Static_Interrupt_Protection) is
   begin
      --  ??? loop to be executed only when we're not doing library level
      --  finalization, since in this case all interrupt tasks are gone.
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

   -------------------------------------
   -- Has_Interrupt_Or_Attach_Handler --
   -------------------------------------

   function Has_Interrupt_Or_Attach_Handler
     (Object : access Static_Interrupt_Protection)
      return   Boolean
   is
      pragma Warnings (Off, Object);
   begin
      return True;
   end Has_Interrupt_Or_Attach_Handler;

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

--  Elaboration code for package System.Interrupts
begin

   --  Get Interrupt_Manager's ID so that Abort_Interrupt can be sent.

   Interrupt_Manager_ID := To_System (Interrupt_Manager'Identity);

   --  During the elaboration of this package body we want RTS to
   --  inherit the interrupt mask from the Environment Task.

   --  The Environment Task should have gotten its mask from
   --  the enclosing process during the RTS start up. (See
   --  in s-inmaop.adb). Pass the Interrupt_Mask of the Environment
   --  task to the Interrupt_Manager.

   --  Note : At this point we know that all tasks (including
   --  RTS internal servers) are masked for non-reserved signals
   --  (see s-taprop.adb). Only the Interrupt_Manager will have
   --  masks set up differently inheriting the original Environment
   --  Task's mask.

   Interrupt_Manager.Initialize (IMOP.Environment_Mask);
end System.Interrupts;
