------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                 A D A . D Y N A M I C _ P R I O R I T I E S              --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.25 $
--                                                                          --
--             Copyright (C) 1991-2001 Florida State University             --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Task_Identification;
--  used for Task_Id
--           Current_Task
--           Null_Task_Id
--           Is_Terminated

with System.Task_Primitives.Operations;
--  used for Write_Lock
--           Unlock
--           Set_Priority
--           Wakeup
--           Self

with System.Tasking;
--  used for Task_ID

with Ada.Exceptions;
--  used for Raise_Exception

with System.Tasking.Initialization;
--  used for Defer/Undefer_Abort

with Unchecked_Conversion;

package body Ada.Dynamic_Priorities is

   use System.Tasking;
   use Ada.Exceptions;

   function Convert_Ids is new
     Unchecked_Conversion
       (Task_Identification.Task_Id, System.Tasking.Task_ID);

   ------------------
   -- Get_Priority --
   ------------------

   --  Inquire base priority of a task

   function Get_Priority
     (T : Ada.Task_Identification.Task_Id :=
          Ada.Task_Identification.Current_Task)
      return System.Any_Priority is

      Target : constant Task_ID := Convert_Ids (T);
      Error_Message : constant String := "Trying to get the priority of a ";

   begin
      if Target = Convert_Ids (Ada.Task_Identification.Null_Task_Id) then
         Raise_Exception (Program_Error'Identity,
           Error_Message & "null task");
      end if;

      if Task_Identification.Is_Terminated (T) then
         Raise_Exception (Tasking_Error'Identity,
           Error_Message & "null task");
      end if;

      return Target.Common.Base_Priority;
   end Get_Priority;

   ------------------
   -- Set_Priority --
   ------------------

   --  Change base priority of a task dynamically

   procedure Set_Priority
     (Priority : System.Any_Priority;
      T : Ada.Task_Identification.Task_Id :=
          Ada.Task_Identification.Current_Task)
   is
      Target  : constant Task_ID := Convert_Ids (T);
      Self_ID : constant Task_ID := System.Task_Primitives.Operations.Self;
      Error_Message : constant String := "Trying to set the priority of a ";

   begin
      if Target = Convert_Ids (Ada.Task_Identification.Null_Task_Id) then
         Raise_Exception (Program_Error'Identity,
           Error_Message & "null task");
      end if;

      if Task_Identification.Is_Terminated (T) then
         Raise_Exception (Tasking_Error'Identity,
           Error_Message & "terminated task");
      end if;

      System.Tasking.Initialization.Defer_Abort (Self_ID);
      System.Task_Primitives.Operations.Write_Lock (Target);

      if Self_ID = Target then
         Target.Common.Base_Priority := Priority;
         System.Task_Primitives.Operations.Set_Priority (Target, Priority);
         System.Task_Primitives.Operations.Unlock (Target);
         System.Task_Primitives.Operations.Yield;
         --  Yield is needed to enforce FIFO task dispatching.
         --  LL Set_Priority is made while holding the RTS lock so that
         --  it is inheriting high priority until it release all the RTS
         --  locks.
         --  If this is used in a system where Ceiling Locking is
         --  not enforced we may end up getting two Yield effects.
      else
         Target.New_Base_Priority := Priority;
         Target.Pending_Priority_Change := True;
         Target.Pending_Action := True;

         System.Task_Primitives.Operations.Wakeup
           (Target, Target.Common.State);
         --  If the task is suspended, wake it up to perform the change.
         --  check for ceiling violations ???
         System.Task_Primitives.Operations.Unlock (Target);

      end if;
      System.Tasking.Initialization.Undefer_Abort (Self_ID);

   end Set_Priority;

end Ada.Dynamic_Priorities;
