------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M - S T A C K _ U S A G E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2009, Free Software Foundation, Inc.          --
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

with System.Parameters;
with System.CRTL;
with System.IO;

package body System.Stack_Usage is
   use System.Storage_Elements;
   use System;
   use System.IO;
   use Interfaces;

   -----------------
   -- Stack_Slots --
   -----------------

   --  Stackl_Slots is an internal data type to represent a sequence of real
   --  stack slots initialized with a provided pattern, with operations to
   --  abstract away the target call stack growth direction.

   type Stack_Slots is array (Integer range <>) of Pattern_Type;
   for Stack_Slots'Component_Size use Pattern_Type'Object_Size;

   --  We will carefully handle the initializations ourselves and might want
   --  to remap an initialized overlay later on with an address clause.

   pragma Suppress_Initialization (Stack_Slots);

   --  The abstract Stack_Slots operations all operate over the simple array
   --  memory model:

   --  memory addresses increasing ---->

   --  Slots('First)                                           Slots('Last)
   --    |                                                             |
   --    V                                                             V
   --  +------------------------------------------------------------------+
   --  |####|                                                        |####|
   --  +------------------------------------------------------------------+

   --  What we call Top or Bottom always denotes call chain leaves or entry
   --  points respectively, and their relative positions in the stack array
   --  depends on the target stack growth direction:

   --                           Stack_Grows_Down

   --                <----- calls push frames towards decreasing addresses

   --   Top(most) Slot                                   Bottom(most) Slot
   --    |                                                            |
   --    V                                                            V
   --  +------------------------------------------------------------------+
   --  |####|                            | leaf frame | ... | entry frame |
   --  +------------------------------------------------------------------+

   --                           Stack_Grows_Up

   --   calls push frames towards increasing addresses ----->

   --   Bottom(most) Slot                                    Top(most) Slot
   --    |                                                             |
   --    V                                                             V
   --  +------------------------------------------------------------------+
   --  | entry frame | ... | leaf frame |                            |####|
   --  +------------------------------------------------------------------+

   function Top_Slot_Index_In (Stack : Stack_Slots) return Integer;
   --  Index of the stack Top slot in the Slots array, denoting the latest
   --  possible slot available to call chain leaves.

   function Bottom_Slot_Index_In (Stack : Stack_Slots) return Integer;
   --  Index of the stack Bottom slot in the Slots array, denoting the first
   --  possible slot available to call chain entry points.

   function Push_Index_Step_For (Stack : Stack_Slots) return Integer;
   --  By how much do we need to update a Slots index to Push a single slot on
   --  the stack.

   function Pop_Index_Step_For (Stack : Stack_Slots) return Integer;
   --  By how much do we need to update a Slots index to Pop a single slot off
   --  the stack.

   pragma Inline_Always (Top_Slot_Index_In);
   pragma Inline_Always (Bottom_Slot_Index_In);
   pragma Inline_Always (Push_Index_Step_For);
   pragma Inline_Always (Pop_Index_Step_For);

   -----------------------
   -- Top_Slot_Index_In --
   -----------------------

   function Top_Slot_Index_In (Stack : Stack_Slots) return Integer is
   begin
      if System.Parameters.Stack_Grows_Down then
         return Stack'First;
      else
         return Stack'Last;
      end if;
   end Top_Slot_Index_In;

   ----------------------------
   --  Bottom_Slot_Index_In  --
   ----------------------------

   function Bottom_Slot_Index_In (Stack : Stack_Slots) return Integer is
   begin
      if System.Parameters.Stack_Grows_Down then
         return Stack'Last;
      else
         return Stack'First;
      end if;
   end Bottom_Slot_Index_In;

   -------------------------
   -- Push_Index_Step_For --
   -------------------------

   function Push_Index_Step_For (Stack : Stack_Slots) return Integer is
      pragma Unreferenced (Stack);
   begin
      if System.Parameters.Stack_Grows_Down then
         return -1;
      else
         return +1;
      end if;
   end Push_Index_Step_For;

   ------------------------
   -- Pop_Index_Step_For --
   ------------------------

   function Pop_Index_Step_For (Stack : Stack_Slots) return Integer is
   begin
      return -Push_Index_Step_For (Stack);
   end Pop_Index_Step_For;

   -------------------
   -- Unit Services --
   -------------------

   --  Now the implementation of the services offered by this unit, on top of
   --  the Stack_Slots abstraction above.

   Index_Str       : constant String  := "Index";
   Task_Name_Str   : constant String  := "Task Name";
   Stack_Size_Str  : constant String  := "Stack Size";
   Actual_Size_Str : constant String  := "Stack usage";

   function Get_Usage_Range (Result : Task_Result) return String;
   --  Return string representing the range of possible result of stack usage

   procedure Output_Result
     (Result_Id          : Natural;
      Result             : Task_Result;
      Max_Stack_Size_Len : Natural;
      Max_Actual_Use_Len : Natural);
   --  Prints the result on the standard output. Result Id is the number of
   --  the result in the array, and Result the contents of the actual result.
   --  Max_Stack_Size_Len and Max_Actual_Use_Len are used for displaying the
   --  proper layout. They hold the maximum length of the string representing
   --  the Stack_Size and Actual_Use values.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Buffer_Size : Natural) is
      Bottom_Of_Stack  : aliased Integer;
      Stack_Size_Chars : System.Address;

   begin
      --  Initialize the buffered result array

      Result_Array := new Result_Array_Type (1 .. Buffer_Size);
      Result_Array.all :=
        (others =>
           (Task_Name => (others => ASCII.NUL),
            Variation => 0,
            Value     => 0,
            Max_Size  => 0));

      --  Set the Is_Enabled flag to true, so that the task wrapper knows that
      --  it has to handle dynamic stack analysis

      Is_Enabled := True;

      Stack_Size_Chars := System.CRTL.getenv ("GNAT_STACK_LIMIT" & ASCII.NUL);

      --  If variable GNAT_STACK_LIMIT is set, then we will take care of the
      --  environment task, using GNAT_STASK_LIMIT as the size of the stack.
      --  It doesn't make sens to process the stack when no bound is set (e.g.
      --  limit is typically up to 4 GB).

      if Stack_Size_Chars /= Null_Address then
         declare
            My_Stack_Size : Integer;

         begin
            My_Stack_Size := System.CRTL.atoi (Stack_Size_Chars) * 1024;

            Initialize_Analyzer
              (Environment_Task_Analyzer,
               "ENVIRONMENT TASK",
               My_Stack_Size,
               My_Stack_Size,
               System.Storage_Elements.To_Integer (Bottom_Of_Stack'Address));

            Fill_Stack (Environment_Task_Analyzer);

            Compute_Environment_Task := True;
         end;

      --  GNAT_STACK_LIMIT not set

      else
         Compute_Environment_Task := False;
      end if;
   end Initialize;

   ----------------
   -- Fill_Stack --
   ----------------

   procedure Fill_Stack (Analyzer : in out Stack_Analyzer) is
      --  Change the local variables and parameters of this function with
      --  super-extra care. The more the stack frame size of this function is
      --  big, the more an "instrumentation threshold at writing" error is
      --  likely to happen.

      Stack_Used_When_Filling : Integer;
      Current_Stack_Level     : aliased Integer;

   begin
      --  Readjust the pattern size. When we arrive in this function, there is
      --  already a given amount of stack used, that we won't analyze.

      Stack_Used_When_Filling :=
        Stack_Size
         (Analyzer.Bottom_Of_Stack,
          To_Stack_Address (Current_Stack_Level'Address))
          + Natural (Current_Stack_Level'Size);

      if Stack_Used_When_Filling > Analyzer.Pattern_Size then
         --  In this case, the known size of the stack is too small, we've
         --  already taken more than expected, so there's no possible
         --  computation

         Analyzer.Pattern_Size := 0;
      else
         Analyzer.Pattern_Size :=
           Analyzer.Pattern_Size - Stack_Used_When_Filling;
      end if;

      declare
         Stack : aliased Stack_Slots
                           (1 .. Analyzer.Pattern_Size / Bytes_Per_Pattern);

      begin
         Stack := (others => Analyzer.Pattern);

         Analyzer.Stack_Overlay_Address := Stack'Address;

         if Analyzer.Pattern_Size /= 0 then
            Analyzer.Bottom_Pattern_Mark :=
              To_Stack_Address (Stack (Bottom_Slot_Index_In (Stack))'Address);
            Analyzer.Top_Pattern_Mark :=
              To_Stack_Address (Stack (Top_Slot_Index_In (Stack))'Address);
         else
            Analyzer.Bottom_Pattern_Mark := To_Stack_Address (Stack'Address);
            Analyzer.Top_Pattern_Mark := To_Stack_Address (Stack'Address);
         end if;

         --  If Arr has been packed, the following assertion must be true (we
         --  add the size of the element whose address is:
         --    Min (Analyzer.Inner_Pattern_Mark, Analyzer.Outer_Pattern_Mark)):

         pragma Assert
           (Analyzer.Pattern_Size = 0 or else
            Analyzer.Pattern_Size =
              Stack_Size
                (Analyzer.Top_Pattern_Mark, Analyzer.Bottom_Pattern_Mark));
      end;
   end Fill_Stack;

   -------------------------
   -- Initialize_Analyzer --
   -------------------------

   procedure Initialize_Analyzer
     (Analyzer         : in out Stack_Analyzer;
      Task_Name        : String;
      My_Stack_Size    : Natural;
      Max_Pattern_Size : Natural;
      Bottom           : Stack_Address;
      Pattern          : Unsigned_32 := 16#DEAD_BEEF#)
   is
   begin
      --  Initialize the analyzer fields

      Analyzer.Bottom_Of_Stack := Bottom;
      Analyzer.Stack_Size      := My_Stack_Size;
      Analyzer.Pattern_Size    := Max_Pattern_Size;
      Analyzer.Pattern         := Pattern;
      Analyzer.Result_Id       := Next_Id;
      Analyzer.Task_Name       := (others => ' ');

      --  Compute the task name, and truncate if bigger than Task_Name_Length

      if Task_Name'Length <= Task_Name_Length then
         Analyzer.Task_Name (1 .. Task_Name'Length) := Task_Name;
      else
         Analyzer.Task_Name :=
           Task_Name (Task_Name'First ..
                        Task_Name'First + Task_Name_Length - 1);
      end if;

      Next_Id := Next_Id + 1;
   end Initialize_Analyzer;

   ----------------
   -- Stack_Size --
   ----------------

   function Stack_Size
     (SP_Low  : Stack_Address;
      SP_High : Stack_Address) return Natural
   is
   begin
      if SP_Low > SP_High then
         return Natural (SP_Low - SP_High + 4);
      else
         return Natural (SP_High - SP_Low + 4);
      end if;
   end Stack_Size;

   --------------------
   -- Compute_Result --
   --------------------

   procedure Compute_Result (Analyzer : in out Stack_Analyzer) is

      --  Change the local variables and parameters of this function with
      --  super-extra care. The larger the stack frame size of this function
      --  is, the more an "instrumentation threshold at reading" error is
      --  likely to happen.

      Stack : Stack_Slots (1 .. Analyzer.Pattern_Size / Bytes_Per_Pattern);
      for Stack'Address use Analyzer.Stack_Overlay_Address;

   begin
      Analyzer.Topmost_Touched_Mark := Analyzer.Bottom_Pattern_Mark;

      if Analyzer.Pattern_Size = 0 then
         return;
      end if;

      --  Look backward from the topmost possible end of the marked stack to
      --  the bottom of it. The first index not equals to the patterns marks
      --  the beginning of the used stack.

      declare
         Top_Index    : constant Integer := Top_Slot_Index_In (Stack);
         Bottom_Index : constant Integer := Bottom_Slot_Index_In (Stack);
         Step         : constant Integer := Pop_Index_Step_For (Stack);
         J            : Integer;

      begin
         J := Top_Index;
         loop
            if Stack (J) /= Analyzer.Pattern then
               Analyzer.Topmost_Touched_Mark
                 := To_Stack_Address (Stack (J)'Address);
               exit;
            end if;

            exit when J = Bottom_Index;
            J := J + Step;
         end loop;
      end;
   end Compute_Result;

   ---------------------
   -- Get_Usage_Range --
   ---------------------

   function Get_Usage_Range (Result : Task_Result) return String is
      Variation_Used_Str : constant String :=
                             Natural'Image (Result.Variation);
      Value_Used_Str     : constant String :=
                             Natural'Image (Result.Value);
   begin
      return Value_Used_Str & " +/- " & Variation_Used_Str;
   end Get_Usage_Range;

   ---------------------
   --  Output_Result --
   ---------------------

   procedure Output_Result
     (Result_Id          : Natural;
      Result             : Task_Result;
      Max_Stack_Size_Len : Natural;
      Max_Actual_Use_Len : Natural)
   is
      Result_Id_Str     : constant String := Natural'Image (Result_Id);
      My_Stack_Size_Str : constant String := Natural'Image (Result.Max_Size);
      Actual_Use_Str    : constant String := Get_Usage_Range (Result);

      Result_Id_Blanks  : constant
        String (1 .. Index_Str'Length - Result_Id_Str'Length)    :=
          (others => ' ');

      Stack_Size_Blanks : constant
        String (1 .. Max_Stack_Size_Len - My_Stack_Size_Str'Length) :=
          (others => ' ');

      Actual_Use_Blanks : constant
        String (1 .. Max_Actual_Use_Len - Actual_Use_Str'Length) :=
          (others => ' ');

   begin
      Set_Output (Standard_Error);
      Put (Result_Id_Blanks & Natural'Image (Result_Id));
      Put (" | ");
      Put (Result.Task_Name);
      Put (" | ");
      Put (Stack_Size_Blanks & My_Stack_Size_Str);
      Put (" | ");
      Put (Actual_Use_Blanks & Actual_Use_Str);
      New_Line;
   end Output_Result;

   ---------------------
   --  Output_Results --
   ---------------------

   procedure Output_Results is
      Max_Stack_Size                         : Natural := 0;
      Max_Actual_Use_Result_Id               : Natural := Result_Array'First;
      Max_Stack_Size_Len, Max_Actual_Use_Len : Natural := 0;

      Task_Name_Blanks : constant
        String (1 .. Task_Name_Length - Task_Name_Str'Length) :=
          (others => ' ');

   begin
      Set_Output (Standard_Error);

      if Compute_Environment_Task then
         Compute_Result (Environment_Task_Analyzer);
         Report_Result (Environment_Task_Analyzer);
      end if;

      if Result_Array'Length > 0 then

         --  Computes the size of the largest strings that will get displayed,
         --  in order to do correct column alignment.

         for J in Result_Array'Range loop
            exit when J >= Next_Id;

            if Result_Array (J).Value >
               Result_Array (Max_Actual_Use_Result_Id).Value
            then
               Max_Actual_Use_Result_Id := J;
            end if;

            if Result_Array (J).Max_Size > Max_Stack_Size then
               Max_Stack_Size := Result_Array (J).Max_Size;
            end if;
         end loop;

         Max_Stack_Size_Len := Natural'Image (Max_Stack_Size)'Length;

         Max_Actual_Use_Len :=
           Get_Usage_Range (Result_Array (Max_Actual_Use_Result_Id))'Length;

         --  Display the output header. Blanks will be added in front of the
         --  labels if needed.

         declare
            Stack_Size_Blanks  : constant
              String (1 .. Max_Stack_Size_Len - Stack_Size_Str'Length) :=
                (others => ' ');

            Stack_Usage_Blanks : constant
              String (1 .. Max_Actual_Use_Len - Actual_Size_Str'Length) :=
                (others => ' ');

         begin
            if Stack_Size_Str'Length > Max_Stack_Size_Len then
               Max_Stack_Size_Len := Stack_Size_Str'Length;
            end if;

            if Actual_Size_Str'Length > Max_Actual_Use_Len then
               Max_Actual_Use_Len := Actual_Size_Str'Length;
            end if;

            Put
              (Index_Str & " | " & Task_Name_Str & Task_Name_Blanks & " | "
               & Stack_Size_Str & Stack_Size_Blanks & " | "
               & Stack_Usage_Blanks & Actual_Size_Str);
         end;

         New_Line;

         --  Now display the individual results

         for J in Result_Array'Range loop
            exit when J >= Next_Id;
            Output_Result
              (J, Result_Array (J), Max_Stack_Size_Len, Max_Actual_Use_Len);
         end loop;

      --  Case of no result stored, still display the labels

      else
         Put
           (Index_Str & " | " & Task_Name_Str & Task_Name_Blanks & " | "
            & Stack_Size_Str & " | " & Actual_Size_Str);
         New_Line;
      end if;
   end Output_Results;

   -------------------
   -- Report_Result --
   -------------------

   procedure Report_Result (Analyzer : Stack_Analyzer) is
      Result  : Task_Result :=
                  (Task_Name      => Analyzer.Task_Name,
                   Max_Size       => Analyzer.Stack_Size,
                   Variation    => 0,
                   Value    => 0);

      Overflow_Guard : constant Integer :=
        Analyzer.Stack_Size
          - Stack_Size (Analyzer.Top_Pattern_Mark, Analyzer.Bottom_Of_Stack);
      Max, Min : Positive;

   begin
      if Analyzer.Pattern_Size = 0 then

         --  If we have that result, it means that we didn't do any computation
         --  at all. In other words, we used at least everything (and possibly
         --  more).

         Min := Analyzer.Stack_Size - Overflow_Guard;
         Max := Analyzer.Stack_Size;

      else
         Min :=
           Stack_Size
             (Analyzer.Topmost_Touched_Mark, Analyzer.Bottom_Of_Stack);
         Max := Min + Overflow_Guard;
      end if;

      Result.Value := (Max + Min) / 2;
      Result.Variation := (Max - Min) / 2;

      if Analyzer.Result_Id in Result_Array'Range then

         --  If the result can be stored, then store it in Result_Array

         Result_Array (Analyzer.Result_Id) := Result;

      else
         --  If the result cannot be stored, then we display it right away

         declare
            Result_Str_Len : constant Natural :=
                               Get_Usage_Range (Result)'Length;
            Size_Str_Len   : constant Natural :=
                               Natural'Image (Analyzer.Stack_Size)'Length;

            Max_Stack_Size_Len : Natural;
            Max_Actual_Use_Len : Natural;

         begin
            --  Take either the label size or the number image size for the
            --  size of the column "Stack Size".

            Max_Stack_Size_Len :=
              (if Size_Str_Len > Stack_Size_Str'Length
               then Size_Str_Len
               else Stack_Size_Str'Length);

            --  Take either the label size or the number image size for the
            --  size of the column "Stack Usage".

            Max_Actual_Use_Len :=
              (if Result_Str_Len > Actual_Size_Str'Length
               then Result_Str_Len
               else Actual_Size_Str'Length);

            Output_Result
              (Analyzer.Result_Id,
               Result,
               Max_Stack_Size_Len,
               Max_Actual_Use_Len);
         end;
      end if;
   end Report_Result;

end System.Stack_Usage;
