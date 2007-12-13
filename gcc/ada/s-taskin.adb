------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Task_Primitives.Operations;
--  used for Self

with System.Storage_Elements;
--  Needed for initializing Stack_Info.Size

package body System.Tasking is

   package STPO renames System.Task_Primitives.Operations;

   ---------------------
   -- Detect_Blocking --
   ---------------------

   function Detect_Blocking return Boolean is
      GL_Detect_Blocking : Integer;
      pragma Import (C, GL_Detect_Blocking, "__gl_detect_blocking");
      --  Global variable exported by the binder generated file.
      --  A value equal to 1 indicates that pragma Detect_Blocking is active,
      --  while 0 is used for the pragma not being present.

   begin
      return GL_Detect_Blocking = 1;
   end Detect_Blocking;

   ----------
   -- Self --
   ----------

   function Self return Task_Id renames STPO.Self;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size (T : Task_Id) return System.Parameters.Size_Type is
   begin
      return
         System.Parameters.Size_Type
           (T.Common.Compiler_Data.Pri_Stack_Info.Size);
   end Storage_Size;

   ---------------------
   -- Initialize_ATCB --
   ---------------------

   procedure Initialize_ATCB
     (Self_ID          : Task_Id;
      Task_Entry_Point : Task_Procedure_Access;
      Task_Arg         : System.Address;
      Parent           : Task_Id;
      Elaborated       : Access_Boolean;
      Base_Priority    : System.Any_Priority;
      Task_Info        : System.Task_Info.Task_Info_Type;
      Stack_Size       : System.Parameters.Size_Type;
      T                : Task_Id;
      Success          : out Boolean) is
   begin
      T.Common.State := Unactivated;

      --  Initialize T.Common.LL

      STPO.Initialize_TCB (T, Success);

      if not Success then
         return;
      end if;

      T.Common.Parent := Parent;
      T.Common.Base_Priority := Base_Priority;
      T.Common.Current_Priority := 0;
      T.Common.Protected_Action_Nesting := 0;
      T.Common.Call := null;
      T.Common.Task_Arg := Task_Arg;
      T.Common.Task_Entry_Point := Task_Entry_Point;
      T.Common.Activator := Self_ID;
      T.Common.Wait_Count := 0;
      T.Common.Elaborated := Elaborated;
      T.Common.Activation_Failed := False;
      T.Common.Task_Info := Task_Info;
      T.Common.Global_Task_Lock_Nesting := 0;
      T.Common.Fall_Back_Handler := null;
      T.Common.Specific_Handler  := null;

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

      --  Link the task into the list of all tasks

      T.Common.All_Tasks_Link := All_Tasks_List;
      All_Tasks_List := T;
   end Initialize_ATCB;

   ----------------
   -- Initialize --
   ----------------

   Main_Task_Image : constant String := "main_task";
   --  Image of environment task

   Main_Priority : Integer;
   pragma Import (C, Main_Priority, "__gl_main_priority");
   --  Priority for main task. Note that this is of type Integer, not
   --  Priority, because we use the value -1 to indicate the default
   --  main priority, and that is of course not in Priority'range.

   Initialized : Boolean := False;
   --  Used to prevent multiple calls to Initialize

   procedure Initialize is
      T             : Task_Id;
      Base_Priority : Any_Priority;

      Success : Boolean;
      pragma Warnings (Off, Success);

   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

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
   end Initialize;

end System.Tasking;
