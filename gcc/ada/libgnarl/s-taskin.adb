------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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
with System.Storage_Elements;

package body System.Tasking is

   package STPO renames System.Task_Primitives.Operations;

   ---------------------
   -- Detect_Blocking --
   ---------------------

   function Detect_Blocking return Boolean is
      GL_Detect_Blocking : Integer;
      pragma Import (C, GL_Detect_Blocking, "__gl_detect_blocking");
      --  Global variable exported by the binder generated file. A value equal
      --  to 1 indicates that pragma Detect_Blocking is active, while 0 is used
      --  for the pragma not being present.

   begin
      return GL_Detect_Blocking = 1;
   end Detect_Blocking;

   -----------------------
   -- Number_Of_Entries --
   -----------------------

   function Number_Of_Entries (Self_Id : Task_Id) return Entry_Index is
   begin
      return Entry_Index (Self_Id.Entry_Num);
   end Number_Of_Entries;

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
      Base_CPU         : System.Multiprocessors.CPU_Range;
      Domain           : Dispatching_Domain_Access;
      Task_Info        : System.Task_Info.Task_Info_Type;
      Stack_Size       : System.Parameters.Size_Type;
      T                : Task_Id;
      Success          : out Boolean)
   is
   begin
      T.Common.State := Unactivated;

      --  Initialize T.Common.LL

      STPO.Initialize_TCB (T, Success);

      if not Success then
         return;
      end if;

      --  Note that use of an aggregate here for this assignment
      --  would be illegal, because Common_ATCB is limited because
      --  Task_Primitives.Private_Data is limited.

      T.Common.Parent                   := Parent;
      T.Common.Base_Priority            := Base_Priority;
      T.Common.Base_CPU                 := Base_CPU;

      --  The Domain defaults to that of the activator. But that can be null in
      --  the case of foreign threads (see Register_Foreign_Thread), in which
      --  case we default to the System_Domain.

      if Domain /= null then
         T.Common.Domain := Domain;
      elsif Self_ID.Common.Domain /= null then
         T.Common.Domain := Self_ID.Common.Domain;
      else
         T.Common.Domain := System_Domain;
      end if;
      pragma Assert (T.Common.Domain /= null);

      T.Common.Current_Priority         := Priority'First;
      T.Common.Protected_Action_Nesting := 0;
      T.Common.Call                     := null;
      T.Common.Task_Arg                 := Task_Arg;
      T.Common.Task_Entry_Point         := Task_Entry_Point;
      T.Common.Activator                := Self_ID;
      T.Common.Wait_Count               := 0;
      T.Common.Elaborated               := Elaborated;
      T.Common.Activation_Failed        := False;
      T.Common.Task_Info                := Task_Info;
      T.Common.Global_Task_Lock_Nesting := 0;
      T.Common.Fall_Back_Handler        := null;
      T.Common.Specific_Handler         := null;
      T.Common.Debug_Events             := [others => False];
      T.Common.Task_Image_Len           := 0;

      if T.Common.Parent = null then

         --  For the environment task, the adjusted stack size is meaningless.
         --  For example, an unspecified Stack_Size means that the stack size
         --  is determined by the environment, or can grow dynamically. The
         --  Stack_Checking algorithm therefore needs to use the requested
         --  size, or 0 in case of an unknown size.

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

   Main_Priority : constant Integer;
   pragma Import (C, Main_Priority, "__gl_main_priority");
   --  Priority for main task. Note that this is of type Integer, not Priority,
   --  because we use the value -1 to indicate the default main priority, and
   --  that is of course not in Priority'range.

   Main_CPU : constant Integer;
   pragma Import (C, Main_CPU, "__gl_main_cpu");
   --  Affinity for main task. Note that this is of type Integer, not
   --  CPU_Range, because we use the value -1 to indicate the unassigned
   --  affinity, and that is of course not in CPU_Range'Range.

   Initialized : Boolean := False;
   --  Used to prevent multiple calls to Initialize

   procedure Initialize is
      T             : Task_Id;
      Base_Priority : Any_Priority;
      Base_CPU      : System.Multiprocessors.CPU_Range;
      Success       : Boolean;

      use type System.Multiprocessors.CPU_Range;

   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      --  Initialize Environment Task

      Base_Priority :=
        (if Main_Priority = Unspecified_Priority
         then Default_Priority
         else Priority (Main_Priority));

      Base_CPU :=
        (if Main_CPU = Unspecified_CPU
         then System.Multiprocessors.Not_A_Specific_CPU
         else System.Multiprocessors.CPU_Range (Main_CPU));

      --  At program start-up the environment task is allocated to the default
      --  system dispatching domain.
      --  Make sure that the processors which are not available are not taken
      --  into account. Use Number_Of_CPUs to know the exact number of
      --  processors in the system at execution time.

      System_Domain :=
        new Dispatching_Domain'
          (Multiprocessors.CPU'First .. Multiprocessors.Number_Of_CPUs =>
             True);

      T := STPO.New_ATCB (0);
      Initialize_ATCB
        (Self_ID          => null,
         Task_Entry_Point => null,
         Task_Arg         => Null_Address,
         Parent           => Null_Task,
         Elaborated       => null,
         Base_Priority    => Base_Priority,
         Base_CPU         => Base_CPU,
         Domain           => System_Domain,
         Task_Info        => Task_Info.Unspecified_Task_Info,
         Stack_Size       => 0,
         T                => T,
         Success          => Success);
      pragma Assert (Success);

      STPO.Initialize (T);
      STPO.Set_Priority (T, T.Common.Base_Priority);
      T.Common.State := Runnable;
      T.Common.Task_Image_Len := Main_Task_Image'Length;
      T.Common.Task_Image (Main_Task_Image'Range) := Main_Task_Image;

      Dispatching_Domain_Tasks :=
        new Array_Allocated_Tasks'
          (Multiprocessors.CPU'First .. Multiprocessors.Number_Of_CPUs => 0);

      --  Signal that this task is being allocated to a processor

      if Base_CPU /= System.Multiprocessors.Not_A_Specific_CPU then

         --  Increase the number of tasks attached to the CPU to which this
         --  task is allocated.

         Dispatching_Domain_Tasks (Base_CPU) :=
           Dispatching_Domain_Tasks (Base_CPU) + 1;
      end if;

      --  The full initialization of the environment task's Entry_Calls array
      --  is deferred to Init_RTS because only the first element of the array
      --  is used by the restricted Ravenscar runtime.

      T.Entry_Calls (T.Entry_Calls'First).Self := T;
      T.Entry_Calls (T.Entry_Calls'First).Level := T.Entry_Calls'First;

   end Initialize;
end System.Tasking;
