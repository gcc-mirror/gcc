------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--            S Y S T E M . S T A C K _ U S A G E . T A S K I N G           --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--           Copyright (C) 2009-2018, Free Software Foundation, Inc.        --
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

with System.Stack_Usage;

--  This is why this package is part of GNARL:

with System.Tasking.Debug;
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

   procedure Report_Impl (All_Tasks : Boolean; Do_Print : Boolean);
   --  Report the stack usage of either all tasks (All_Tasks = True) or of the
   --  current task (All_Task = False). If Print is True, then results are
   --  printed on stderr

   procedure Convert
     (TS  : System.Stack_Usage.Task_Result;
      Res : out Stack_Usage_Result);
   --  Convert an object of type System.Stack_Usage in a Stack_Usage_Result

   -------------
   -- Convert --
   -------------

   procedure Convert
     (TS  : System.Stack_Usage.Task_Result;
      Res : out Stack_Usage_Result) is
   begin
      Res := TS;
   end Convert;

   ---------------------
   -- Report_For_Task --
   ---------------------

   procedure Report_For_Task (Id : System.Tasking.Task_Id) is
   begin
      System.Stack_Usage.Compute_Result (Id.Common.Analyzer);
      System.Stack_Usage.Report_Result (Id.Common.Analyzer);
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

            Report_For_Task (Id);
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

   procedure Report_Impl (All_Tasks : Boolean; Do_Print : Boolean) is
   begin

      --  Lock the runtime

      System.Task_Primitives.Operations.Lock_RTS;

      --  Calculate results

      if All_Tasks then
         Compute_All_Tasks;
      else
         Compute_Current_Task;
      end if;

      --  Output results
      if Do_Print then
         System.Stack_Usage.Output_Results;
      end if;

      --  Unlock the runtime

      System.Task_Primitives.Operations.Unlock_RTS;

   end Report_Impl;

   ---------------------
   -- Report_All_Task --
   ---------------------

   procedure Report_All_Tasks is
   begin
      Report_Impl (True, True);
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
        (1 .. System.Stack_Usage.Result_Array'Length);
   begin
      Report_Impl (True, False);

      for J in Res'Range loop
         Convert (System.Stack_Usage.Result_Array (J), Res (J));
      end loop;

      return Res;
   end Get_All_Tasks_Usage;

   ----------------------------
   -- Get_Current_Task_Usage --
   ----------------------------

   function Get_Current_Task_Usage return Stack_Usage_Result is
      Res : Stack_Usage_Result;
      Original : System.Stack_Usage.Task_Result;
      Found : Boolean := False;
   begin

      Report_Impl (False, False);

      --  Look for the task info in System.Stack_Usage.Result_Array;
      --  the search is based on task name

      for T in System.Stack_Usage.Result_Array'Range loop
         if System.Stack_Usage.Result_Array (T).Task_Name =
           System.Tasking.Self.Common.Analyzer.Task_Name
         then
            Original := System.Stack_Usage.Result_Array (T);
            Found := True;
            exit;
         end if;
      end loop;

      --  Be sure a task has been found

      pragma Assert (Found);

      Convert (Original, Res);
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
         Put_Line
           ("| " & T_Name & " | " & Natural'Image (Obj.Stack_Size) &
            Natural'Image (Obj.Value));
      end;
   end Print;

end System.Stack_Usage.Tasking;
