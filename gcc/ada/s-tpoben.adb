------------------------------------------------------------------------------
--                                                                          --
--               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                 SYSTEM.TASKING.PROTECTED_OBJECTS.ENTRIES                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1998-2001, Free Software Foundation, Inc.          --
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

--  This package contains all the simple primitives related to
--  Protected_Objects with entries (i.e init, lock, unlock).

--  The handling of protected objects with no entries is done in
--  System.Tasking.Protected_Objects, the complex routines for protected
--  objects with entries in System.Tasking.Protected_Objects.Operations.
--  The split between Entries and Operations is needed to break circular
--  dependencies inside the run time.

--  Note: the compiler generates direct calls to this interface, via Rtsfind.

with Ada.Exceptions;
--  used for Exception_Occurrence_Access

with System.Task_Primitives.Operations;
--  used for Initialize_Lock
--           Write_Lock
--           Unlock
--           Get_Priority
--           Wakeup

with System.Tasking.Initialization;
--  used for Defer_Abort,
--           Undefer_Abort,
--           Change_Base_Priority

pragma Elaborate_All (System.Tasking.Initialization);
--  this insures that tasking is initialized if any protected objects are
--  created.

with System.Parameters;
--  used for Single_Lock

package body System.Tasking.Protected_Objects.Entries is

   package STPO renames System.Task_Primitives.Operations;

   use Parameters;
   use Task_Primitives.Operations;
   use Ada.Exceptions;

   Locking_Policy : Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Protection_Entries) is
      Entry_Call        : Entry_Call_Link;
      Caller            : Task_ID;
      Ceiling_Violation : Boolean;
      Self_ID           : constant Task_ID := STPO.Self;
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
         --  Dip our own priority down to ceiling of lock.
         --  See similar code in Tasking.Entry_Calls.Lock_Server.

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
            Raise_Exception (Program_Error'Identity, "Ceiling Violation");
         end if;

         if Single_Lock then
            Lock_RTS;
         end if;

         Object.Old_Base_Priority := Old_Base_Priority;
         Object.Pending_Action := True;
      end if;

      --  Send program_error to all tasks still queued on this object.

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
      Entry_Bodies      : Protected_Entry_Body_Access;
      Find_Body_Index   : Find_Body_Index_Access)
   is
      Init_Priority : Integer := Ceiling_Priority;
      Self_ID       : constant Task_ID := STPO.Self;

   begin
      if Init_Priority = Unspecified_Priority then
         Init_Priority  := System.Priority'Last;
      end if;

      if Locking_Policy = 'C'
        and then Has_Interrupt_Or_Attach_Handler (Object)
        and then Init_Priority not in System.Interrupt_Priority
      then
         --  Required by C.3.1(11)

         raise Program_Error;
      end if;

      Initialization.Defer_Abort (Self_ID);
      Initialize_Lock (Init_Priority, Object.L'Access);
      Initialization.Undefer_Abort (Self_ID);
      Object.Ceiling := System.Any_Priority (Init_Priority);
      Object.Compiler_Info := Compiler_Info;
      Object.Pending_Action := False;
      Object.Call_In_Progress := null;
      Object.Entry_Bodies := Entry_Bodies;
      Object.Find_Body_Index :=  Find_Body_Index;

      for E in Object.Entry_Queues'Range loop
         Object.Entry_Queues (E).Head := null;
         Object.Entry_Queues (E).Tail := null;
      end loop;
   end Initialize_Protection_Entries;

   ------------------
   -- Lock_Entries --
   ------------------

   procedure Lock_Entries
     (Object : Protection_Entries_Access; Ceiling_Violation : out Boolean) is
   begin
      if Object.Finalized then
         Raise_Exception
           (Program_Error'Identity, "Protected Object is finalized");
      end if;

      --  The lock is made without defering abortion.

      --  Therefore the abortion has to be deferred before calling this
      --  routine. This means that the compiler has to generate a Defer_Abort
      --  call before the call to Lock.

      --  The caller is responsible for undeferring abortion, and compiler
      --  generated calls must be protected with cleanup handlers to ensure
      --  that abortion is undeferred in all cases.

      pragma Assert (STPO.Self.Deferral_Level > 0);
      Write_Lock (Object.L'Access, Ceiling_Violation);
   end Lock_Entries;

   procedure Lock_Entries (Object : Protection_Entries_Access) is
      Ceiling_Violation : Boolean;
   begin
      if Object.Finalized then
         Raise_Exception
           (Program_Error'Identity, "Protected Object is finalized");
      end if;

      pragma Assert (STPO.Self.Deferral_Level > 0);
      Write_Lock (Object.L'Access, Ceiling_Violation);

      if Ceiling_Violation then
         Raise_Exception (Program_Error'Identity, "Ceiling Violation");
      end if;
   end Lock_Entries;

   ----------------------------
   -- Lock_Read_Only_Entries --
   ----------------------------

   procedure Lock_Read_Only_Entries (Object : Protection_Entries_Access) is
      Ceiling_Violation : Boolean;
   begin
      if Object.Finalized then
         Raise_Exception
           (Program_Error'Identity, "Protected Object is finalized");
      end if;

      Read_Lock (Object.L'Access, Ceiling_Violation);

      if Ceiling_Violation then
         Raise_Exception (Program_Error'Identity, "Ceiling Violation");
      end if;
   end Lock_Read_Only_Entries;

   --------------------
   -- Unlock_Entries --
   --------------------

   procedure Unlock_Entries (Object : Protection_Entries_Access) is
   begin
      Unlock (Object.L'Access);
   end Unlock_Entries;

end System.Tasking.Protected_Objects.Entries;
