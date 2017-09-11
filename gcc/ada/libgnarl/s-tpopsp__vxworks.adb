------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                SYSTEM.TASK_PRIMITIVES.OPERATIONS.SPECIFIC                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1992-2017, Free Software Foundation, Inc.          --
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

--  This is a VxWorks version of this package where foreign threads are
--  recognized. The implementation is based on VxWorks taskVarLib.

separate (System.Task_Primitives.Operations)
package body Specific is

   ATCB_Key : aliased System.Address := System.Null_Address;
   --  Key used to find the Ada Task_Id associated with a thread

   ATCB_Key_Addr : System.Address := ATCB_Key'Address;
   pragma Export (Ada, ATCB_Key_Addr, "__gnat_ATCB_key_addr");
   --  Exported to support the temporary AE653 task registration
   --  implementation. This mechanism is used to minimize impact on other
   --  targets.

   Stack_Limit : aliased System.Address;

   pragma Import (C, Stack_Limit, "__gnat_stack_limit");

   type Set_Stack_Limit_Proc_Acc is access procedure;
   pragma Convention (C, Set_Stack_Limit_Proc_Acc);

   Set_Stack_Limit_Hook : Set_Stack_Limit_Proc_Acc;
   pragma Import (C, Set_Stack_Limit_Hook, "__gnat_set_stack_limit_hook");
   --  Procedure to be called when a task is created to set stack limit if
   --  limit checking is used.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   -------------------
   -- Is_Valid_Task --
   -------------------

   function Is_Valid_Task return Boolean is
   begin
      return taskVarGet (taskIdSelf, ATCB_Key'Access) /= ERROR;
   end Is_Valid_Task;

   ---------
   -- Set --
   ---------

   procedure Set (Self_Id : Task_Id) is
      Result : STATUS;

   begin
      --  If argument is null, destroy task specific data, to make API
      --  consistent with other platforms, and thus compatible with the
      --  shared version of s-tpoaal.adb.

      if Self_Id = null then
         Result := taskVarDelete (taskIdSelf, ATCB_Key'Access);
         pragma Assert (Result /= ERROR);
         return;
      end if;

      if not Is_Valid_Task then
         Result := taskVarAdd (Self_Id.Common.LL.Thread, ATCB_Key'Access);
         pragma Assert (Result = OK);

         if Stack_Check_Limits
           and then Result /= ERROR
           and then Set_Stack_Limit_Hook /= null
         then
            --  This will be initialized from taskInfoGet() once the task is
            --  is running.

            Result :=
              taskVarAdd (Self_Id.Common.LL.Thread, Stack_Limit'Access);
            pragma Assert (Result /= ERROR);
         end if;
      end if;

      Result :=
        taskVarSet
          (Self_Id.Common.LL.Thread,
           ATCB_Key'Access,
           To_Address (Self_Id));
      pragma Assert (Result /= ERROR);
   end Set;

   ----------
   -- Self --
   ----------

   function Self return Task_Id is
   begin
      return To_Task_Id (ATCB_Key);
   end Self;

end Specific;
