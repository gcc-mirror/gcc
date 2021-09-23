------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--      S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S     --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--            Copyright (C) 1991-2017, Florida State University             --
--                     Copyright (C) 1995-2021, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
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

with System.Task_Primitives.Operations;
with System.Soft_Links.Tasking;

with System.Secondary_Stack;
pragma Elaborate_All (System.Secondary_Stack);
pragma Unreferenced (System.Secondary_Stack);
--  Make sure the body of Secondary_Stack is elaborated before calling
--  Init_Tasking_Soft_Links. See comments for this routine for explanation.

package body System.Tasking.Protected_Objects is

   use System.Task_Primitives.Operations;

   ----------------
   -- Local Data --
   ----------------

   Locking_Policy : Character;
   pragma Import (C, Locking_Policy, "__gl_locking_policy");

   -------------------------
   -- Finalize_Protection --
   -------------------------

   procedure Finalize_Protection (Object : in out Protection) is
   begin
      Finalize_Lock (Object.L'Unrestricted_Access);
   end Finalize_Protection;

   ---------------------------
   -- Initialize_Protection --
   ---------------------------

   procedure Initialize_Protection
     (Object           : Protection_Access;
      Ceiling_Priority : Integer)
   is
      Init_Priority : Integer := Ceiling_Priority;

   begin
      if Init_Priority = Unspecified_Priority then
         Init_Priority := System.Priority'Last;
      end if;

      Initialize_Lock (Init_Priority, Object.L'Access);
      Object.Ceiling := System.Any_Priority (Init_Priority);
      Object.New_Ceiling := System.Any_Priority (Init_Priority);
      Object.Owner := Null_Task;
   end Initialize_Protection;

   -----------------
   -- Get_Ceiling --
   -----------------

   function Get_Ceiling
     (Object : Protection_Access) return System.Any_Priority is
   begin
      return Object.New_Ceiling;
   end Get_Ceiling;

   ----------
   -- Lock --
   ----------

   procedure Lock (Object : Protection_Access) is
      Ceiling_Violation : Boolean;

   begin
      --  The lock is made without deferring abort

      --  Therefore the abort has to be deferred before calling this routine.
      --  This means that the compiler has to generate a Defer_Abort call
      --  before the call to Lock.

      --  The caller is responsible for undeferring abort, and compiler
      --  generated calls must be protected with cleanup handlers to ensure
      --  that abort is undeferred in all cases.

      --  If pragma Detect_Blocking is active then, as described in the ARM
      --  9.5.1, par. 15, we must check whether this is an external call on a
      --  protected subprogram with the same target object as that of the
      --  protected action that is currently in progress (i.e., if the caller
      --  is already the protected object's owner). If this is the case hence
      --  Program_Error must be raised.

      if Detect_Blocking and then Object.Owner = Self then
         raise Program_Error;
      end if;

      Write_Lock (Object.L'Access, Ceiling_Violation);

      if Ceiling_Violation then
         raise Program_Error;
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
   end Lock;

   --------------------
   -- Lock_Read_Only --
   --------------------

   procedure Lock_Read_Only (Object : Protection_Access) is
      Ceiling_Violation : Boolean;

   begin
      --  If pragma Detect_Blocking is active then, as described in the ARM
      --  9.5.1, par. 15, we must check whether this is an external call on
      --  protected subprogram with the same target object as that of the
      --  protected action that is currently in progress (i.e., if the caller
      --  is already the protected object's owner). If this is the case hence
      --  Program_Error must be raised.
      --
      --  Note that in this case (getting read access), several tasks may have
      --  read ownership of the protected object, so that this method of
      --  storing the (single) protected object's owner does not work reliably
      --  for read locks. However, this is the approach taken for two major
      --  reasons: first, this function is not currently being used (it is
      --  provided for possible future use), and second, it largely simplifies
      --  the implementation.

      if Detect_Blocking and then Object.Owner = Self then
         raise Program_Error;
      end if;

      Read_Lock (Object.L'Access, Ceiling_Violation);

      if Ceiling_Violation then
         raise Program_Error;
      end if;

      --  We are entering in a protected action, so we increase the protected
      --  object nesting level (if pragma Detect_Blocking is active).

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
   end Lock_Read_Only;

   -----------------
   -- Set_Ceiling --
   -----------------

   procedure Set_Ceiling
     (Object : Protection_Access;
      Prio   : System.Any_Priority) is
   begin
      Object.New_Ceiling := Prio;
   end Set_Ceiling;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Object : Protection_Access) is
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

            --  We are exiting from a protected action, so we decrease the
            --  protected object nesting level.

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

   end Unlock;

begin
   --  Ensure that tasking is initialized, as well as tasking soft links
   --  when using protected objects.

   Tasking.Initialize;
   System.Soft_Links.Tasking.Init_Tasking_Soft_Links;
end System.Tasking.Protected_Objects;
