------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2003, Free Software Foundation, Inc.         --
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

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Task_Primitives.Operations;
--  used for Self

with Unchecked_Deallocation;
--  To recover from failure of ATCB initialization.

with System.Storage_Elements;
--  Needed for initializing Stack_Info.Size

with System.Parameters;
--  Used for Adjust_Storage_Size

package body System.Tasking is

   package STPO renames System.Task_Primitives.Operations;

   procedure Free is new
     Unchecked_Deallocation (Ada_Task_Control_Block, Task_ID);

   ----------
   -- Self --
   ----------

   function Self return Task_ID renames STPO.Self;

   ---------------------
   -- Initialize_ATCB --
   ---------------------

   procedure Initialize_ATCB
     (Self_ID          : Task_ID;
      Task_Entry_Point : Task_Procedure_Access;
      Task_Arg         : System.Address;
      Parent           : Task_ID;
      Elaborated       : Access_Boolean;
      Base_Priority    : System.Any_Priority;
      Task_Info        : System.Task_Info.Task_Info_Type;
      Stack_Size       : System.Parameters.Size_Type;
      T                : in out Task_ID;
      Success          : out Boolean) is
   begin
      T.Common.State := Unactivated;

      --  Initialize T.Common.LL

      STPO.Initialize_TCB (T, Success);

      if not Success then
         Free (T);
         return;
      end if;

      T.Common.Parent := Parent;
      T.Common.Base_Priority := Base_Priority;
      T.Common.Current_Priority := 0;
      T.Common.Call := null;
      T.Common.Task_Arg := Task_Arg;
      T.Common.Task_Entry_Point := Task_Entry_Point;
      T.Common.Activator := Self_ID;
      T.Common.Wait_Count := 0;
      T.Common.Elaborated := Elaborated;
      T.Common.Activation_Failed := False;
      T.Common.Task_Info := Task_Info;

      if T.Common.Parent = null then
         --  For the environment task, the adjusted stack size is
         --  meaningless. For example, an unspecified Stack_Size means
         --  that the stack size is determined by the environment, or
         --  can grow dynamically. The Stack_Checking algorithm
         --  therefore needs to use the requested size, or 0 in
         --  case of an unknown size.

         T.Common.Compiler_Data.Pri_Stack_Info.Size :=
            Storage_Elements.Storage_Offset (Stack_Size);

      else
         T.Common.Compiler_Data.Pri_Stack_Info.Size :=
           Storage_Elements.Storage_Offset
             (Parameters.Adjust_Storage_Size (Stack_Size));
      end if;

      --  Link the task into the list of all tasks.

      T.Common.All_Tasks_Link := All_Tasks_List;
      All_Tasks_List := T;
   end Initialize_ATCB;

   Main_Task_Image : constant String := "main_task";
   --  Image of environment task.

   Main_Priority : Integer;
   pragma Import (C, Main_Priority, "__gl_main_priority");
   --  Priority for main task. Note that this is of type Integer, not
   --  Priority, because we use the value -1 to indicate the default
   --  main priority, and that is of course not in Priority'range.

   ----------------------------
   -- Tasking Initialization --
   ----------------------------

   --  This block constitutes the first part of the initialization of the
   --  GNARL. This includes creating data structures to make the initial thread
   --  into the environment task. The last part of the initialization is done
   --  in System.Tasking.Initialization or System.Tasking.Restricted.Stages.
   --  All the initializations used to be in Tasking.Initialization, but this
   --  is no longer possible with the run time simplification (including
   --  optimized PO and the restricted run time) since one cannot rely on
   --  System.Tasking.Initialization being present, as was done before.

begin
   declare
      T             : Task_ID;
      Success       : Boolean;
      Base_Priority : Any_Priority;

   begin
      --  Initialize Environment Task

      if Main_Priority = Unspecified_Priority then
         Base_Priority := Default_Priority;
      else
         Base_Priority := Priority (Main_Priority);
      end if;

      Success := True;
      T := STPO.New_ATCB (0);
      Initialize_ATCB
        (null, null, Null_Address, Null_Task, null, Base_Priority,
         Task_Info.Unspecified_Task_Info, 0, T, Success);
      pragma Assert (Success);

      STPO.Initialize (T);
      STPO.Set_Priority (T, T.Common.Base_Priority);
      T.Common.State := Runnable;
      T.Common.Task_Image_Len := Main_Task_Image'Length;
      T.Common.Task_Image (Main_Task_Image'Range) := Main_Task_Image;

      --  Only initialize the first element since others are not relevant
      --  in ravenscar mode. Rest of the initialization is done in Init_RTS.

      T.Entry_Calls (1).Self := T;
   end;
end System.Tasking;
