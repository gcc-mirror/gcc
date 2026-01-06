------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--            S Y S T E M . S T A C K _ U S A G E . T A S K I N G           --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--           Copyright (C) 2009-2026, Free Software Foundation, Inc.        --
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

--  This is why this package is part of GNARL:

with System.Tasking.Debug;
with System.Tasking.Stages;
with System.Task_Primitives.Operations;

with System.IO;

package body System.Stack_Usage.Tasking is
   use System.IO;

   procedure Report_For_Task (Id : System.Tasking.Task_Id);
   --  A generic procedure calculating stack usage for a given task

   procedure Compute_All_Tasks;
   --  Compute the stack usage for all tasks and saves it in
   --  System.Stack_Usage.Result_Array

   procedure Compute_Current_Task;
   --  Compute the stack usage for a given task and saves it in the precise
   --  slot in System.Stack_Usage.Result_Array;

   procedure Report_Impl
     (All_Tasks   : Boolean;
      Do_Print    : Boolean;
      Result_Data : out Stack_Usage_Result_Array);
   --  Report the stack usage of either all tasks (All_Tasks = True) or of the
   --  current task (All_Task = False). If Print is True, then results are
   --  printed on stderr. Otherwise, we fill the referred structure with the
   --  stack information for later processing. We do this copy to avoid reading
   --  System.Stack_Usage.Result_Array without locking the runtime.

   procedure Convert
     (TS : System.Stack_Usage.Task_Result; Res : out Stack_Usage_Result);
   --  Convert an object of type System.Stack_Usage in a Stack_Usage_Result

   -------------
   -- Convert --
   -------------

   procedure Convert
     (TS : System.Stack_Usage.Task_Result; Res : out Stack_Usage_Result) is
   begin
      Res := TS;
   end Convert;

   ---------------------
   -- Report_For_Task --
   ---------------------

   procedure Report_For_Task (Id : System.Tasking.Task_Id) is
      use type System.Tasking.Task_Id;
   begin
      --  Special treatment of the environment task that uses a Stack_Analyzer
      --  object that is not part of its ATCB.
      if Id = System.Task_Primitives.Operations.Environment_Task then

         --  Check whether we are tracking stack usage for the environment task
         if Compute_Environment_Task then
            Compute_Result (Environment_Task_Analyzer);
            Report_Result (Environment_Task_Analyzer);

         else
            Put_Line
              ("Stack usage for environment task needs GNAT_STACK_LIMIT");
         end if;

      --  Regular task

      else
         declare
            Name_Length : constant Natural :=
              Natural'Min (Id.Common.Task_Image_Len, Task_Name_Length);
         begin
            --  Skip the task if it hasn't initialized the stack pattern yet
            if Id.Common.Task_Image (1 .. Name_Length) =
               Id.Common.Analyzer.Task_Name (1 .. Name_Length)
            then
               System.Stack_Usage.Compute_Result (Id.Common.Analyzer);
               System.Stack_Usage.Report_Result (Id.Common.Analyzer);
            end if;
         end;
      end if;
   end Report_For_Task;

   -----------------------
   -- Compute_All_Tasks --
   -----------------------

   procedure Compute_All_Tasks is
      Id : System.Tasking.Task_Id;
      use type System.Tasking.Task_Id;
   begin
      if not System.Stack_Usage.Is_Enabled then
         Put_Line ("Stack Usage not enabled: bind with -uNNN switch");
      else

         --  Loop over all tasks

         for J in System.Tasking.Debug.Known_Tasks'First + 1
           .. System.Tasking.Debug.Known_Tasks'Last
         loop
            Id := System.Tasking.Debug.Known_Tasks (J);
            exit when Id = null;

            --  Calculate the task usage for a given task

            --  Skip if the task is terminated because the ATCB can be already
            --  destroyed.

            if not System.Tasking.Stages.Terminated (Id) then
               Report_For_Task (Id);
            end if;
         end loop;

      end if;
   end Compute_All_Tasks;

   --------------------------
   -- Compute_Current_Task --
   --------------------------

   procedure Compute_Current_Task is
   begin
      if not System.Stack_Usage.Is_Enabled then
         Put_Line ("Stack Usage not enabled: bind with -uNNN switch");
      else

         --  The current task

         Report_For_Task (System.Tasking.Self);

      end if;
   end Compute_Current_Task;

   -----------------
   -- Report_Impl --
   -----------------

   procedure Report_Impl
     (All_Tasks   : Boolean;
      Do_Print    : Boolean;
      Result_Data : out Stack_Usage_Result_Array) is
   begin

      --  Lock the runtime to compute and display stack usage

      System.Task_Primitives.Operations.Lock_RTS;

      --  Calculate results

      if All_Tasks then
         Compute_All_Tasks;
      else
         Compute_Current_Task;
      end if;

      --  Output results, either printing it or in the out parameter

      if Do_Print then
         System.Stack_Usage.Output_Results;

      else
         --  Extract data from the snapshot in System.Stack_Usage.Result_Array

         pragma Assert
           (System.Stack_Usage.Result_Array = null or else
            (System.Stack_Usage.Result_Array'First = Result_Data'First and then
             System.Stack_Usage.Result_Array'Last = Result_Data'Last));

         for J in Result_Data'Range loop
            Convert (System.Stack_Usage.Result_Array (J), Result_Data (J));
         end loop;
      end if;

      --  Unlock the runtime

      System.Task_Primitives.Operations.Unlock_RTS;

   end Report_Impl;

   ----------------------
   -- Report_All_Tasks --
   ----------------------

   procedure Report_All_Tasks is
      Empty_Result_Array : Stack_Usage_Result_Array (1 .. 0);
   begin
      Report_Impl (True, True, Empty_Result_Array);
   end Report_All_Tasks;

   -------------------------
   -- Report_Current_Task --
   -------------------------

   procedure Report_Current_Task is
      Res : Stack_Usage_Result;
   begin
      Res := Get_Current_Task_Usage;
      Print (Res);
   end Report_Current_Task;

   -------------------------
   -- Get_All_Tasks_Usage --
   -------------------------

   function Get_All_Tasks_Usage return Stack_Usage_Result_Array is
      Res : Stack_Usage_Result_Array
        (1 ..
           (if System.Stack_Usage.Result_Array = null then 0
            else System.Stack_Usage.Result_Array'Length));
   begin
      Report_Impl (True, False, Res);

      return Res;
   end Get_All_Tasks_Usage;

   ----------------------------
   -- Get_Current_Task_Usage --
   ----------------------------

   function Get_Current_Task_Usage return Stack_Usage_Result is
      use type System.Tasking.Task_Id;

      Self_ID     : constant System.Tasking.Task_Id := System.Tasking.Self;
      Is_Env_Task : constant Boolean :=
        Self_ID = System.Task_Primitives.Operations.Environment_Task;

      Res_Array : Stack_Usage_Result_Array
        (1 ..
           (if System.Stack_Usage.Result_Array = null then 0
            else System.Stack_Usage.Result_Array'Length));
      Res       : Stack_Usage_Result;
      Found     : Boolean := False;

   begin
      Report_Impl (False, False, Res_Array);

      --  Look for the task info in the copy of System.Stack_Usage.Result_Array
      --  (the search is based on task name).

      for Stack_Usage of Res_Array loop
         if Stack_Usage.Task_Name = Self_ID.Common.Analyzer.Task_Name or else
           (Is_Env_Task and then
            Stack_Usage.Task_Name (1 .. 16) = "ENVIRONMENT TASK")
         then
            Res := Stack_Usage;
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         --  Not found because the task is not part of those for which we store
         --  the results. Hence we need to compute now.

         --  Environment task
         if Is_Env_Task then
            Res.Task_Name :=
              "ENVIRONMENT TASK" & (1 .. Task_Name_Length - 16 => ASCII.NUL);

            if Compute_Environment_Task then
               Res.Stack_Size := Environment_Task_Analyzer.Stack_Size;
               Res.Value :=
                 Stack_Size
                   (To_Stack_Address
                      (Environment_Task_Analyzer.Topmost_Touched_Mark),
                    To_Stack_Address (Environment_Task_Analyzer.Stack_Base));
            else
               Res.Stack_Size := 0;
               Res.Value := 0;
            end if;

         --  Other tasks

         else
            Res.Task_Name := Self_ID.Common.Analyzer.Task_Name;
            Res.Stack_Size := Self_ID.Common.Analyzer.Stack_Size;
            Res.Value :=
              Stack_Size
                (To_Stack_Address
                   (Self_ID.Common.Analyzer.Topmost_Touched_Mark),
                 To_Stack_Address (Self_ID.Common.Analyzer.Stack_Base));
         end if;
      end if;

      return Res;
   end Get_Current_Task_Usage;

   -----------
   -- Print --
   -----------

   procedure Print (Obj : Stack_Usage_Result) is
      Pos : Positive := Obj.Task_Name'Last;

   begin
      --  Simply trim the string containing the task name

      for S in Obj.Task_Name'Range loop
         if Obj.Task_Name (S) = ' ' then
            Pos := S;
            exit;
         end if;
      end loop;

      declare
         T_Name : constant String :=
           Obj.Task_Name (Obj.Task_Name'First .. Pos);
      begin
         --  Notify when we don't know stack usage
         Put_Line
           ("| " & T_Name & "|" &
            (if Obj.Stack_Size = 0 then " NA | NA"
             else Natural'Image (Obj.Stack_Size) & " |" &
                  Natural'Image (Obj.Value)));
      end;
   end Print;

end System.Stack_Usage.Tasking;
