------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                 A D A . D Y N A M I C _ P R I O R I T I E S              --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

with System.Task_Primitives.Operations;
with System.Tasking;
with System.Parameters;
with System.Soft_Links;

with Ada.Unchecked_Conversion;

package body Ada.Dynamic_Priorities is

   package STPO renames System.Task_Primitives.Operations;
   package SSL renames System.Soft_Links;

   use System.Parameters;
   use System.Tasking;

   function Convert_Ids is new
     Ada.Unchecked_Conversion
       (Task_Identification.Task_Id, System.Tasking.Task_Id);

   ------------------
   -- Get_Priority --
   ------------------

   --  Inquire base priority of a task

   function Get_Priority
     (T : Ada.Task_Identification.Task_Id :=
        Ada.Task_Identification.Current_Task) return System.Any_Priority
   is
      Target : constant Task_Id := Convert_Ids (T);
      Error_Message : constant String := "Trying to get the priority of a ";

   begin
      if Target = Convert_Ids (Ada.Task_Identification.Null_Task_Id) then
         raise Program_Error with Error_Message & "null task";
      end if;

      if Task_Identification.Is_Terminated (T) then
         raise Tasking_Error with Error_Message & "terminated task";
      end if;

      return Target.Common.Base_Priority;
   end Get_Priority;

   ------------------
   -- Set_Priority --
   ------------------

   --  Change base priority of a task dynamically

   procedure Set_Priority
     (Priority : System.Any_Priority;
      T        : Ada.Task_Identification.Task_Id :=
        Ada.Task_Identification.Current_Task)
   is
      Target        : constant Task_Id := Convert_Ids (T);
      Error_Message : constant String := "Trying to set the priority of a ";
      Yield_Needed  : Boolean;

   begin
      if Target = Convert_Ids (Ada.Task_Identification.Null_Task_Id) then
         raise Program_Error with Error_Message & "null task";
      end if;

      --  Setting the priority of an already-terminated task doesn't do
      --  anything (see RM-D.5.1(7)). Note that Get_Priority is different in
      --  this regard.

      if Task_Identification.Is_Terminated (T) then
         return;
      end if;

      SSL.Abort_Defer.all;

      if Single_Lock then
         STPO.Lock_RTS;
      end if;

      STPO.Write_Lock (Target);

      Target.Common.Base_Priority := Priority;

      if Target.Common.Call /= null
        and then
          Target.Common.Call.Acceptor_Prev_Priority /= Priority_Not_Boosted
      then
         --  Target is within a rendezvous, so ensure the correct priority
         --  will be reset when finishing the rendezvous, and only change the
         --  priority immediately if the new priority is greater than the
         --  current (inherited) priority.

         Target.Common.Call.Acceptor_Prev_Priority := Priority;

         if Priority >= Target.Common.Current_Priority then
            Yield_Needed := True;
            STPO.Set_Priority (Target, Priority);
         else
            Yield_Needed := False;
         end if;

      else
         Yield_Needed := True;
         STPO.Set_Priority (Target, Priority);

         if Target.Common.State = Entry_Caller_Sleep then
            Target.Pending_Priority_Change := True;
            STPO.Wakeup (Target, Target.Common.State);
         end if;
      end if;

      STPO.Unlock (Target);

      if Single_Lock then
         STPO.Unlock_RTS;
      end if;

      if STPO.Self = Target and then Yield_Needed then

         --  Yield is needed to enforce FIFO task dispatching

         --  LL Set_Priority is made while holding the RTS lock so that it is
         --  inheriting high priority until it release all the RTS locks.

         --  If this is used in a system where Ceiling Locking is not enforced
         --  we may end up getting two Yield effects.

         STPO.Yield;
      end if;

      SSL.Abort_Undefer.all;
   end Set_Priority;

end Ada.Dynamic_Priorities;
