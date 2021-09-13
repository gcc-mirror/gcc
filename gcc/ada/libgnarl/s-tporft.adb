------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--         SYSTEM.TASK_PRIMITIVES.OPERATIONS.REGISTER_FOREIGN_THREAD        --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--          Copyright (C) 2002-2021, Free Software Foundation, Inc.         --
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

with System.Multiprocessors;
with System.Soft_Links;
with System.Task_Info;

separate (System.Task_Primitives.Operations)
function Register_Foreign_Thread
  (Thread         : Thread_Id;
   Sec_Stack_Size : Size_Type := Unspecified_Size)
   return Task_Id
is
   Local_ATCB : aliased Ada_Task_Control_Block (0);
   Self_Id    : Task_Id;
   Succeeded  : Boolean;

begin
   --  This section is tricky. We must not call anything that might require
   --  an ATCB, until the new ATCB is in place. In order to get an ATCB
   --  immediately, we fake one, so that it is then possible to e.g allocate
   --  memory (which might require accessing self).

   --  Record this as the Task_Id for the thread

   Local_ATCB.Common.LL.Thread := Thread;
   Local_ATCB.Common.Current_Priority := System.Priority'First;
   Local_ATCB.Common.Global_Task_Lock_Nesting := 0;
   Specific.Set (Local_ATCB'Unchecked_Access);

   --  It is now safe to use an allocator

   Self_Id := new Ada_Task_Control_Block (0);

   --  Finish initialization

   Lock_RTS;
   System.Tasking.Initialize_ATCB
     (Self_Id, null, Null_Address, Null_Task,
      Foreign_Task_Elaborated'Access,
      System.Priority'First, System.Multiprocessors.Not_A_Specific_CPU, null,
      Task_Info.Unspecified_Task_Info, 0, Self_Id, Succeeded);
   Unlock_RTS;
   pragma Assert (Succeeded);

   Self_Id.Master_Of_Task := 0;
   Self_Id.Master_Within := Self_Id.Master_Of_Task + 1;

   for L in Self_Id.Entry_Calls'Range loop
      Self_Id.Entry_Calls (L).Self := Self_Id;
      Self_Id.Entry_Calls (L).Level := L;
   end loop;

   Self_Id.Common.State := Runnable;
   Self_Id.Awake_Count := 1;

   Self_Id.Common.Task_Image (1 .. 14) := "foreign thread";
   Self_Id.Common.Task_Image_Len := 14;

   --  Since this is not an ordinary Ada task, we will start out undeferred

   Self_Id.Deferral_Level := 0;

   --  We do not provide an alternate stack for foreign threads

   Self_Id.Common.Task_Alternate_Stack := Null_Address;

   --  Create the TSD for the task

   System.Soft_Links.Create_TSD
     (Self_Id.Common.Compiler_Data, null, Sec_Stack_Size);

   Enter_Task (Self_Id);

   return Self_Id;
end Register_Foreign_Thread;
