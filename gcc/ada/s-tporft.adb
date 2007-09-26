------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--    S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S .   --
--               R E G I S T E R _ F O R E I G N _ T H R E A D              --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--          Copyright (C) 2002-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

with System.Task_Info;
--  Use for Unspecified_Task_Info

with System.Soft_Links;
--  used to initialize TSD for a C thread, in function Self

separate (System.Task_Primitives.Operations)
function Register_Foreign_Thread (Thread : Thread_Id) return Task_Id is
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
   Specific.Set (Local_ATCB'Unchecked_Access);

   --  It is now safe to use an allocator

   Self_Id := new Ada_Task_Control_Block (0);

   --  Finish initialization

   Lock_RTS;
   System.Tasking.Initialize_ATCB
     (Self_Id, null, Null_Address, Null_Task,
      Foreign_Task_Elaborated'Access,
      System.Priority'First, Task_Info.Unspecified_Task_Info, 0, Self_Id,
      Succeeded);
   Unlock_RTS;
   pragma Assert (Succeeded);

   Self_Id.Master_of_Task := 0;
   Self_Id.Master_Within := Self_Id.Master_of_Task + 1;

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

   System.Soft_Links.Create_TSD (Self_Id.Common.Compiler_Data);

   --  ???
   --  The following call is commented out to avoid dependence on
   --  the System.Tasking.Initialization package.
   --  It seems that if we want Ada.Task_Attributes to work correctly
   --  for C threads we will need to raise the visibility of this soft
   --  link to System.Soft_Links.
   --  We are putting that off until this new functionality is otherwise
   --  stable.

   --  System.Tasking.Initialization.Initialize_Attributes_Link.all (T);

   Enter_Task (Self_Id);

   return Self_Id;
end Register_Foreign_Thread;
