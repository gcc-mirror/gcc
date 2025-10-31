------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . S Y N C H R O N O U S _ T A S K _ C O N T R O L          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;

with System.Soft_Links;
with System.Task_Primitives.Operations; use System.Task_Primitives.Operations;

package body Ada.Synchronous_Task_Control with
  SPARK_Mode => Off
is
   use type System.Tasking.Task_Id;

   package SSL renames System.Soft_Links;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : in out Suspension_Object) is
   begin
      Initialize_Lock (S.L'Access, PO_Level);

      S.State := False;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (S : in out Suspension_Object) is
   begin
      Finalize_Lock (S.L'Access);
   end Finalize;

   -------------------
   -- Current_State --
   -------------------

   function Current_State (S : Suspension_Object) return Boolean is
   begin
      return S.State;
   end Current_State;

   ---------------
   -- Set_False --
   ---------------

   procedure Set_False (S : in out Suspension_Object) is
   begin
      SSL.Abort_Defer.all;
      Write_Lock (S.L'Access);

      S.State := False;

      Unlock (S.L'Access);
      SSL.Abort_Undefer.all;
   end Set_False;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
      Suspended_Task : System.Tasking.Task_Id := null;
   begin
      if Is_Task_Context then
         SSL.Abort_Defer.all;
      end if;

      Write_Lock (S.L'Access);

      if S.Suspended_Task /= null then
         --  We copy the suspended task's ID to a local object. We'll wake the
         --  task up right after we unlock the suspension object.
         Suspended_Task := S.Suspended_Task;
         S.Suspended_Task := null;
      else
         S.State := True;
      end if;

      Unlock (S.L'Access);

      if Suspended_Task /= null then
         Write_Lock (Suspended_Task);

         Wakeup (Suspended_Task, System.Tasking.Runnable);

         Unlock (Suspended_Task);
      end if;

      if Is_Task_Context then
         SSL.Abort_Undefer.all;
      end if;
   end Set_True;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
      Self_ID : constant System.Tasking.Task_Id := Self;
   begin
      --  This is a potentially blocking (see ARM D.10, par. 10), so that
      --  if pragma Detect_Blocking is active then Program_Error must be
      --  raised if this operation is called from a protected action.

      if System.Tasking.Detect_Blocking
        and then System.Tasking.Self.Common.Protected_Action_Nesting > 0
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity, "potentially blocking operation");
      end if;

      SSL.Abort_Defer.all;
      Write_Lock (S.L'Access);

      if S.Suspended_Task /= null then
         Unlock (S.L'Access);
         SSL.Abort_Undefer.all;

         raise Program_Error;
      else
         if S.State then
            S.State := False;

            Unlock (S.L'Access);
         else
            Write_Lock (Self_ID);

            --  We treat starting to block in Suspend_Until_True as an abort
            --  completion point, even if the language does not require it.
            if Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level then
               Unlock (Self_ID);
               Unlock (S.L'Access);
               SSL.Abort_Undefer.all;
               return;
            end if;

            S.Suspended_Task := Self_ID;

            Unlock (S.L'Access);

            Self_ID.Common.State := System.Tasking.Suspension_Object_Sleep;

            --  We sleep until at least one of the following propositions
            --  becomes true:
            --
            --  1. We have been unsuspended by some other task calling
            --  Set_True.
            --  2. We have received an abort.
            loop
               Sleep (Self_ID, System.Tasking.Suspension_Object_Sleep);

               Write_Lock (S.L'Access);

               --  If S.Suspended_Task /= Self_ID, we've been unsuspended by a
               --  call to Set_True. S.Suspended_Task is not necessarily null
               --  because some other task might have started waiting on the
               --  suspension object.
               if S.Suspended_Task /= Self_ID then
                  exit;

               --  Otherwise if we have received an abort, we must free the
               --  waiting slot on the suspension object.
               elsif Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level then
                  S.Suspended_Task := null;
                  exit;
               end if;

               Unlock (S.L'Access);
            end loop;

            Self_ID.Common.State := System.Tasking.Runnable;
            Unlock (S.L'Access);
            Unlock (Self_ID);
         end if;
         SSL.Abort_Undefer.all;
      end if;

   end Suspend_Until_True;

end Ada.Synchronous_Task_Control;
