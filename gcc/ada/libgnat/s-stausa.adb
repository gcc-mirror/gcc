------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M - S T A C K _ U S A G E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2017, Free Software Foundation, Inc.          --
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

   -------------------
   -- Unit Services --
   -------------------

   --  Now the implementation of the services offered by this unit, on top of
   --  the Stack_Slots abstraction above.

   Index_Str       : constant String  := "Index";
   Task_Name_Str   : constant String  := "Task Name";
   Stack_Size_Str  : constant String  := "Stack Size";
   Actual_Size_Str : constant String  := "Stack usage";

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
      Stack_Size_Chars : System.Address;

   begin
      --  Initialize the buffered result array

      Result_Array := new Result_Array_Type (1 .. Buffer_Size);
      Result_Array.all :=
        (others =>
           (Task_Name   => (others => ASCII.NUL),
            Value       => 0,
            Stack_Size  => 0));

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
               0,
               My_Stack_Size);

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

      Current_Stack_Level : aliased Integer;

      Guard : constant := 256;
      --  Guard space between the Current_Stack_Level'Address and the last
      --  allocated byte on the stack.
   begin
      if Parameters.Stack_Grows_Down then
         if Analyzer.Stack_Base - Stack_Address (Analyzer.Pattern_Size) >
              To_Stack_Address (Current_Stack_Level'Address) - Guard
         then
            --  No room for a pattern

            Analyzer.Pattern_Size := 0;
            return;
         end if;

         Analyzer.Pattern_Limit :=
           Analyzer.Stack_Base - Stack_Address (Analyzer.Pattern_Size);

         if Analyzer.Stack_Base >
              To_Stack_Address (Current_Stack_Level'Address) - Guard
         then
            --  Reduce pattern size to prevent local frame overwrite

            Analyzer.Pattern_Size :=
              Integer (To_Stack_Address (Current_Stack_Level'Address) - Guard
                         - Analyzer.Pattern_Limit);
         end if;

         Analyzer.Pattern_Overlay_Address :=
           To_Address (Analyzer.Pattern_Limit);
      else
         if Analyzer.Stack_Base + Stack_Address (Analyzer.Pattern_Size) <
              To_Stack_Address (Current_Stack_Level'Address) + Guard
         then
            --  No room for a pattern

            Analyzer.Pattern_Size := 0;
            return;
         end if;

         Analyzer.Pattern_Limit :=
           Analyzer.Stack_Base + Stack_Address (Analyzer.Pattern_Size);

         if Analyzer.Stack_Base <
           To_Stack_Address (Current_Stack_Level'Address) + Guard
         then
            --  Reduce pattern size to prevent local frame overwrite

            Analyzer.Pattern_Size :=
              Integer
                (Analyzer.Pattern_Limit -
                  (To_Stack_Address (Current_Stack_Level'Address) + Guard));
         end if;

         Analyzer.Pattern_Overlay_Address :=
           To_Address (Analyzer.Pattern_Limit -
                         Stack_Address (Analyzer.Pattern_Size));
      end if;

      --  Declare and fill the pattern buffer

      declare
         Pattern : aliased Stack_Slots
                     (1 .. Analyzer.Pattern_Size / Bytes_Per_Pattern);
         for Pattern'Address use Analyzer.Pattern_Overlay_Address;

      begin
         if System.Parameters.Stack_Grows_Down then
            for J in reverse Pattern'Range loop
               Pattern (J) := Analyzer.Pattern;
            end loop;

         else
            for J in Pattern'Range loop
               Pattern (J) := Analyzer.Pattern;
            end loop;
         end if;
      end;
   end Fill_Stack;

   -------------------------
   -- Initialize_Analyzer --
   -------------------------

   procedure Initialize_Analyzer
     (Analyzer         : in out Stack_Analyzer;
      Task_Name        : String;
      Stack_Size       : Natural;
      Stack_Base       : Stack_Address;
      Pattern_Size     : Natural;
      Pattern          : Interfaces.Unsigned_32 := 16#DEAD_BEEF#)
   is
   begin
      --  Initialize the analyzer fields

      Analyzer.Stack_Base    := Stack_Base;
      Analyzer.Stack_Size    := Stack_Size;
      Analyzer.Pattern_Size  := Pattern_Size;
      Analyzer.Pattern       := Pattern;
      Analyzer.Result_Id     := Next_Id;
      Analyzer.Task_Name     := (others => ' ');

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
         return Natural (SP_Low - SP_High);
      else
         return Natural (SP_High - SP_Low);
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
      for Stack'Address use Analyzer.Pattern_Overlay_Address;

   begin
      --  Value if the pattern was not modified

      if Parameters.Stack_Grows_Down then
         Analyzer.Topmost_Touched_Mark :=
           Analyzer.Pattern_Limit + Stack_Address (Analyzer.Pattern_Size);
      else
         Analyzer.Topmost_Touched_Mark :=
           Analyzer.Pattern_Limit - Stack_Address (Analyzer.Pattern_Size);
      end if;

      if Analyzer.Pattern_Size = 0 then
         return;
      end if;

      --  Look backward from the topmost possible end of the marked stack to
      --  the bottom of it. The first index not equals to the patterns marks
      --  the beginning of the used stack.

      if System.Parameters.Stack_Grows_Down then
         for J in Stack'Range loop
            if Stack (J) /= Analyzer.Pattern then
               Analyzer.Topmost_Touched_Mark :=
                 To_Stack_Address (Stack (J)'Address);
               exit;
            end if;
         end loop;

      else
         for J in reverse Stack'Range loop
            if Stack (J) /= Analyzer.Pattern then
               Analyzer.Topmost_Touched_Mark :=
                 To_Stack_Address (Stack (J)'Address);
               exit;
            end if;
         end loop;

      end if;
   end Compute_Result;

   ---------------------
   --  Output_Result --
   ---------------------

   procedure Output_Result
     (Result_Id          : Natural;
      Result             : Task_Result;
      Max_Stack_Size_Len : Natural;
      Max_Actual_Use_Len : Natural)
   is
      Result_Id_Str  : constant String := Natural'Image (Result_Id);
      Stack_Size_Str : constant String := Natural'Image (Result.Stack_Size);
      Actual_Use_Str : constant String := Natural'Image (Result.Value);

      Result_Id_Blanks  : constant
        String (1 .. Index_Str'Length - Result_Id_Str'Length)    :=
          (others => ' ');

      Stack_Size_Blanks : constant
        String (1 .. Max_Stack_Size_Len - Stack_Size_Str'Length) :=
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
      Put (Stack_Size_Blanks & Stack_Size_Str);
      Put (" | ");
      Put (Actual_Use_Blanks & Actual_Use_Str);
      New_Line;
   end Output_Result;

   ---------------------
   --  Output_Results --
   ---------------------

   procedure Output_Results is
      Max_Stack_Size                         : Natural := 0;
      Max_Stack_Usage                        : Natural := 0;
      Max_Stack_Size_Len, Max_Actual_Use_Len : Natural := 0;

      Task_Name_Blanks : constant
                           String
                             (1 .. Task_Name_Length - Task_Name_Str'Length) :=
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

            if Result_Array (J).Value > Max_Stack_Usage then
               Max_Stack_Usage := Result_Array (J).Value;
            end if;

            if Result_Array (J).Stack_Size > Max_Stack_Size then
               Max_Stack_Size := Result_Array (J).Stack_Size;
            end if;
         end loop;

         Max_Stack_Size_Len := Natural'Image (Max_Stack_Size)'Length;

         Max_Actual_Use_Len := Natural'Image (Max_Stack_Usage)'Length;

         --  Display the output header. Blanks will be added in front of the
         --  labels if needed.

         declare
            Stack_Size_Blanks  : constant
                                   String (1 .. Max_Stack_Size_Len -
                                                  Stack_Size_Str'Length) :=
                                      (others => ' ');

            Stack_Usage_Blanks : constant
                                   String (1 .. Max_Actual_Use_Len -
                                                  Actual_Size_Str'Length) :=
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
      Result : Task_Result := (Task_Name  => Analyzer.Task_Name,
                               Stack_Size => Analyzer.Stack_Size,
                               Value      => 0);
   begin
      if Analyzer.Pattern_Size = 0 then

         --  If we have that result, it means that we didn't do any computation
         --  at all (i.e. we used at least everything (and possibly more).

         Result.Value := Analyzer.Stack_Size;

      else
         Result.Value := Stack_Size (Analyzer.Topmost_Touched_Mark,
                                     Analyzer.Stack_Base);
      end if;

      if Analyzer.Result_Id in Result_Array'Range then

         --  If the result can be stored, then store it in Result_Array

         Result_Array (Analyzer.Result_Id) := Result;

      else
         --  If the result cannot be stored, then we display it right away

         declare
            Result_Str_Len : constant Natural :=
                               Natural'Image (Result.Value)'Length;
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
