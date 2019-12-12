------------------------------------------------------------------------------
--                                                                          --
--                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  --
--                                                                          --
--                SYSTEM.TASKING.PROTECTED_OBJECTS.ENTRIES                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--          Copyright (C) 1998-2019, Free Software Foundation, Inc.         --
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

--  This package contains all the simple primitives related to protected
--  objects with entries (i.e init, lock, unlock).

--  The handling of protected objects with no entries is done in
--  System.Tasking.Protected_Objects, the complex routines for protected
--  objects with entries in System.Tasking.Protected_Objects.Operations.

--  The split between Entries and Operations is needed to break circular
--  dependencies inside the run time.

--  Note: the compiler generates direct calls to this interface, via Rtsfind

with System.Task_Primitives.Operations;
with System.Restrictions;
with System.Parameters;

with System.Tasking.Initialization;
pragma Elaborate_All (System.Tasking.Initialization);
--  To insure that tasking is initialized if any protected objects are created

package body System.Tasking.Protected_Objects.Entries is

   package STPO renames System.Task_Primitives.Operations;

   use Parameters;
   use Task_Primitives.Operations;

   ----------------
   -- Local Data --
   ----------------

   Locking_Policy : Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Protection_Entries) is
      Entry_Call        : Entry_Call_Link;
      Caller            : Task_Id;
      Ceiling_Violation : Boolean;
      Self_ID           : constant Task_Id := STPO.Self;
      Old_Base_Priority : System.Any_Priority;

   begin
      if Object.Finalized then
         return;
      end if;

      STPO.Write_Lock (Object.L'Unrestricted_Access, Ceiling_Violation);

      if Single_Lock then
         Lock_RTS;
      end if;

      if Ceiling_Violation then

         --  Dip our own priority down to ceiling of lock. See similar code in
         --  Tasking.Entry_Calls.Lock_Server.

         STPO.Write_Lock (Self_ID);
         Old_Base_Priority := Self_ID.Common.Base_Priority;
         Self_ID.New_Base_Priority := Object.Ceiling;
         Initialization.Change_Base_Priority (Self_ID);
         STPO.Unlock (Self_ID);

         if Single_Lock then
            Unlock_RTS;
         end if;

         STPO.Write_Lock (Object.L'Unrestricted_Access, Ceiling_Violation);

         if Ceiling_Violation then
            raise Program_Error with "ceiling violation";
         end if;

         if Single_Lock then
            Lock_RTS;
         end if;

         Object.Old_Base_Priority := Old_Base_Priority;
         Object.Pending_Action := True;
      end if;

      --  Send program_error to all tasks still queued on this object

      for E in Object.Entry_Queues'Range loop
         Entry_Call := Object.Entry_Queues (E).Head;

         while Entry_Call /= null loop
            Caller := Entry_Call.Self;
            Entry_Call.Exception_To_Raise := Program_Error'Identity;

            STPO.Write_Lock (Caller);
            Initialization.Wakeup_Entry_Caller (Self_ID, Entry_Call, Done);
            STPO.Unlock (Caller);

            exit when Entry_Call = Object.Entry_Queues (E).Tail;
            Entry_Call := Entry_Call.Next;
         end loop;
      end loop;

      Object.Finalized := True;

      if Single_Lock then
         Unlock_RTS;
      end if;

      STPO.Unlock (Object.L'Unrestricted_Access);

      STPO.Finalize_Lock (Object.L'Unrestricted_Access);
   end Finalize;

   -----------------
   -- Get_Ceiling --
   -----------------

   function Get_Ceiling
     (Object : Protection_Entries_Access) return System.Any_Priority is
   begin
      return Object.New_Ceiling;
   end Get_Ceiling;

   -------------------------------------
   -- Has_Interrupt_Or_Attach_Handler --
   -------------------------------------

   function Has_Interrupt_Or_Attach_Handler
     (Object : Protection_Entries_Access)
      return   Boolean
   is
      pragma Warnings (Off, Object);
   begin
      return False;
   end Has_Interrupt_Or_Attach_Handler;

   -----------------------------------
   -- Initialize_Protection_Entries --
   -----------------------------------

   procedure Initialize_Protection_Entries
     (Object            : Protection_Entries_Access;
      Ceiling_Priority  : Integer;
      Compiler_Info     : System.Address;
      Entry_Queue_Maxes : Protected_Entry_Queue_Max_Access;
      Entry_Bodies      : Protected_Entry_Body_Access;
      Find_Body_Index   : Find_Body_Index_Access)
   is
      Init_Priority : Integer := Ceiling_Priority;
      Self_ID       : constant Task_Id := STPO.Self;

   begin
      if Init_Priority = Unspecified_Priority then
         Init_Priority := System.Priority'Last;
      end if;

      if Locking_Policy = 'C'
        and then Has_Interrupt_Or_Attach_Handler (Object)
        and then Init_Priority not in System.Interrupt_Priority
      then
         --  Required by C.3.1(11)

         raise Program_Error;
      end if;

      --  If a PO is created from a controlled operation, abort is already
      --  deferred at this point, so we need to use Defer_Abort_Nestable. In
      --  some cases, the following assertion can help to spot inconsistencies,
      --  outside the above scenario involving controlled types.

      --  pragma Assert (Self_Id.Deferral_Level = 0);

      Initialization.Defer_Abort_Nestable (Self_ID);
      Initialize_Lock (Init_Priority, Object.L'Access);
      Initialization.Undefer_Abort_Nestable (Self_ID);

      Object.Ceiling           := System.Any_Priority (Init_Priority);
      Object.New_Ceiling       := System.Any_Priority (Init_Priority);
      Object.Owner             := Null_Task;
      Object.Compiler_Info     := Compiler_Info;
      Object.Pending_Action    := False;
      Object.Call_In_Progress  := null;
      Object.Entry_Queue_Maxes := Entry_Queue_Maxes;
      Object.Entry_Bodies      := Entry_Bodies;
      Object.Find_Body_Index   := Find_Body_Index;

      for E in Object.Entry_Queues'Range loop
         Object.Entry_Queues (E).Head := null;
         Object.Entry_Queues (E).Tail := null;
      end loop;
   end Initialize_Protection_Entries;

   ------------------
   -- Lock_Entries --
   ------------------

   procedure Lock_Entries (Object : Protection_Entries_Access) is
      Ceiling_Violation : Boolean;

   begin
      Lock_Entries_With_Status (Object, Ceiling_Violation);

      if Ceiling_Violation then
         raise Program_Error with "ceiling violation";
      end if;
   end Lock_Entries;

   ------------------------------
   -- Lock_Entries_With_Status --
   ------------------------------

   procedure Lock_Entries_With_Status
     (Object            : Protection_Entries_Access;
      Ceiling_Violation : out Boolean)
   is
   begin
      if Object.Finalized then
         raise Program_Error with "protected object is finalized";
      end if;

      --  If pragma Detect_Blocking is active then, as described in the ARM
      --  9.5.1, par. 15, we must check whether this is an external call on a
      --  protected subprogram with the same target object as that of the
      --  protected action that is currently in progress (i.e., if the caller
      --  is already the protected object's owner). If this is the case hence
      --  Program_Error must be raised.

      if Detect_Blocking and then Object.Owner = Self then
         raise Program_Error;
      end if;

      --  The lock is made without deferring abort

      --  Therefore the abort has to be deferred before calling this routine.
      --  This means that the compiler has to generate a Defer_Abort call
      --  before the call to Lock.

      --  The caller is responsible for undeferring abort, and compiler
      --  generated calls must be protected with cleanup handlers to ensure
      --  that abort is undeferred in all cases.

      pragma Assert
        (STPO.Self.Deferral_Level > 0
          or else not Restrictions.Abort_Allowed);

      Write_Lock (Object.L'Access, Ceiling_Violation);

      --  We are entering in a protected action, so that we increase the
      --  protected object nesting level (if pragma Detect_Blocking is
      --  active), and update the protected object's owner.

      if Detect_Blocking then
         declare
            Self_Id : constant Task_Id := Self;

         begin
            --  Update the protected object's owner

            Object.Owner := Self_Id;

            --  Increase protected object nesting level

            Self_Id.Common.Protected_Action_Nesting :=
              Self_Id.Common.Protected_Action_Nesting + 1;
         end;
      end if;
   end Lock_Entries_With_Status;

   ----------------------------
   -- Lock_Read_Only_Entries --
   ----------------------------

   procedure Lock_Read_Only_Entries (Object : Protection_Entries_Access) is
      Ceiling_Violation : Boolean;

   begin
      if Object.Finalized then
         raise Program_Error with "protected object is finalized";
      end if;

      --  If pragma Detect_Blocking is active then, as described in the ARM
      --  9.5.1, par. 15, we must check whether this is an external call on a
      --  protected subprogram with the same target object as that of the
      --  protected action that is currently in progress (i.e., if the caller
      --  is already the protected object's owner). If this is the case hence
      --  Program_Error must be raised.

      --  Note that in this case (getting read access), several tasks may
      --  have read ownership of the protected object, so that this method of
      --  storing the (single) protected object's owner does not work
      --  reliably for read locks. However, this is the approach taken for two
      --  major reasons: first, this function is not currently being used (it
      --  is provided for possible future use), and second, it largely
      --  simplifies the implementation.

      if Detect_Blocking and then Object.Owner = Self then
         raise Program_Error;
      end if;

      Read_Lock (Object.L'Access, Ceiling_Violation);

      if Ceiling_Violation then
         raise Program_Error with "ceiling violation";
      end if;

      --  We are entering in a protected action, so that we increase the
      --  protected object nesting level (if pragma Detect_Blocking is
      --  active), and update the protected object's owner.

      if Detect_Blocking then
         declare
            Self_Id : constant Task_Id := Self;

         begin
            --  Update the protected object's owner

            Object.Owner := Self_Id;

            --  Increase protected object nesting level

            Self_Id.Common.Protected_Action_Nesting :=
              Self_Id.Common.Protected_Action_Nesting + 1;
         end;
      end if;
   end Lock_Read_Only_Entries;

   -----------------------
   -- Number_Of_Entries --
   -----------------------

   function Number_Of_Entries
     (Object : Protection_Entries_Access) return Entry_Index
   is
   begin
      return Entry_Index (Object.Num_Entries);
   end Number_Of_Entries;

   -----------------
   -- Set_Ceiling --
   -----------------

   procedure Set_Ceiling
     (Object : Protection_Entries_Access;
      Prio   : System.Any_Priority) is
   begin
      Object.New_Ceiling := Prio;
   end Set_Ceiling;

   --------------------
   -- Unlock_Entries --
   --------------------

   procedure Unlock_Entries (Object : Protection_Entries_Access) is
   begin
      --  We are exiting from a protected action, so that we decrease the
      --  protected object nesting level (if pragma Detect_Blocking is
      --  active), and remove ownership of the protected object.

      if Detect_Blocking then
         declare
            Self_Id : constant Task_Id := Self;

         begin
            --  Calls to this procedure can only take place when being within
            --  a protected action and when the caller is the protected
            --  object's owner.

            pragma Assert (Self_Id.Common.Protected_Action_Nesting > 0
                             and then Object.Owner = Self_Id);

            --  Remove ownership of the protected object

            Object.Owner := Null_Task;

            Self_Id.Common.Protected_Action_Nesting :=
              Self_Id.Common.Protected_Action_Nesting - 1;
         end;
      end if;

      --  Before releasing the mutex we must actually update its ceiling
      --  priority if it has been changed.

      if Object.New_Ceiling /= Object.Ceiling then
         if Locking_Policy = 'C' then
            System.Task_Primitives.Operations.Set_Ceiling
              (Object.L'Access, Object.New_Ceiling);
         end if;

         Object.Ceiling := Object.New_Ceiling;
      end if;

      Unlock (Object.L'Access);
   end Unlock_Entries;

end System.Tasking.Protected_Objects.Entries;
