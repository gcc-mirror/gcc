------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M - S T A C K _ U S A G E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with System.Parameters;
with System.CRTL;
with System.IO;

package body System.Stack_Usage is
   use System.Storage_Elements;
   use System;
   use System.IO;

   procedure Output_Result (Result_Id : Natural; Result : Task_Result);

   function Report_Result (Analyzer : Stack_Analyzer) return Natural;

   function Inner_Than
     (A1 : Stack_Address;
      A2 : Stack_Address) return Boolean;
   pragma Inline (Inner_Than);
   --  Return True if, according to the direction of the stack growth, A1 is
   --  inner than A2. Inlined to reduce the size of the stack used by the
   --  instrumentation code.

   ----------------
   -- Inner_Than --
   ----------------

   function Inner_Than
     (A1 : Stack_Address;
      A2 : Stack_Address) return Boolean
   is
   begin
      if System.Parameters.Stack_Grows_Down then
         return A1 > A2;
      else
         return A2 > A1;
      end if;
   end Inner_Than;

   ----------------
   -- Initialize --
   ----------------

   --  Add comments to this procedure ???
   --  Other subprograms also need more comment in code???

   procedure Initialize (Buffer_Size : Natural) is
      Bottom_Of_Stack : aliased Integer;

      Stack_Size_Chars : System.Address;
   begin
      Result_Array := new Result_Array_Type (1 .. Buffer_Size);
      Result_Array.all :=
        (others =>
           (Task_Name =>
              (others => ASCII.NUL),
            Measure => 0,
            Max_Size => 0));

      Is_Enabled := True;

      Stack_Size_Chars := System.CRTL.getenv ("GNAT_STACK_LIMIT" & ASCII.NUL);

      --  If variable GNAT_STACK_LIMIT is set, then we will take care of the
      --  environment task, using GNAT_STASK_LIMIT as the size of the stack.
      --  It doens't make sens to process the stack when no bound is set (e.g.
      --  limit is typically up to 4 GB).

      if Stack_Size_Chars /= Null_Address then
         declare
            Stack_Size : Integer;

         begin
            Stack_Size := System.CRTL.atoi (Stack_Size_Chars) * 1024;

            Initialize_Analyzer (Environment_Task_Analyzer,
                                 "ENVIRONMENT TASK",
                                 Stack_Size,
                                 System.Storage_Elements.To_Integer
                                   (Bottom_Of_Stack'Address));

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

      type Word_32_Arr is
        array (1 .. Analyzer.Size / (Word_32_Size / Byte_Size)) of Word_32;
      pragma Pack (Word_32_Arr);

      package Arr_Addr is
        new System.Address_To_Access_Conversions (Word_32_Arr);

      Arr : aliased Word_32_Arr;

   begin
      for J in Word_32_Arr'Range loop
         Arr (J) := Analyzer.Pattern;
      end loop;
      Analyzer.Array_Address := Arr_Addr.To_Address (Arr'Access);
      Analyzer.Inner_Pattern_Mark := To_Stack_Address (Arr (1)'Address);
      Analyzer.Outer_Pattern_Mark :=
        To_Stack_Address (Arr (Word_32_Arr'Last)'Address);

      if Inner_Than (Analyzer.Outer_Pattern_Mark,
                     Analyzer.Inner_Pattern_Mark) then
         Analyzer.Inner_Pattern_Mark := Analyzer.Outer_Pattern_Mark;
         Analyzer.Outer_Pattern_Mark := To_Stack_Address (Arr (1)'Address);
         Analyzer.First_Is_Outermost := True;
      else
         Analyzer.First_Is_Outermost := False;
      end if;

      --  If Arr has been packed, the following assertion must be true (we add
      --  the size of the element whose address is:
      --
      --    Min (Analyzer.Inner_Pattern_Mark, Analyzer.Outer_Pattern_Mark)):

      pragma Assert
        (Analyzer.Size =
           Stack_Size
             (Analyzer.Outer_Pattern_Mark, Analyzer.Inner_Pattern_Mark) +
           Word_32_Size / Byte_Size);
   end Fill_Stack;

   -------------------------
   -- Initialize_Analyzer --
   -------------------------

   procedure Initialize_Analyzer
     (Analyzer  : in out Stack_Analyzer;
      Task_Name : String;
      Size      : Natural;
      Bottom    : Stack_Address;
      Pattern   : Word_32 := 16#DEAD_BEEF#)
   is
   begin
      Analyzer.Bottom_Of_Stack := Bottom;
      Analyzer.Size := Size;
      Analyzer.Pattern := Pattern;
      Analyzer.Result_Id := Next_Id;

      Analyzer.Task_Name := (others => ' ');

      if Task_Name'Length <= Task_Name_Length then
         Analyzer.Task_Name (1 .. Task_Name'Length) := Task_Name;
      else
         Analyzer.Task_Name :=
           Task_Name (Task_Name'First ..
                        Task_Name'First + Task_Name_Length - 1);
      end if;

      if Next_Id in Result_Array'Range then
         Result_Array (Analyzer.Result_Id).Task_Name := Analyzer.Task_Name;
      end if;

      Result_Array (Analyzer.Result_Id).Max_Size := Size;
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

      type Word_32_Arr is
        array (1 .. Analyzer.Size / (Word_32_Size / Byte_Size)) of Word_32;
      pragma Pack (Word_32_Arr);

      package Arr_Addr is
        new System.Address_To_Access_Conversions (Word_32_Arr);

      Arr_Access : Arr_Addr.Object_Pointer;

   begin
      Arr_Access := Arr_Addr.To_Pointer (Analyzer.Array_Address);
      Analyzer.Outermost_Touched_Mark := Analyzer.Inner_Pattern_Mark;

      for J in Word_32_Arr'Range loop
         if Arr_Access (J) /= Analyzer.Pattern then
            Analyzer.Outermost_Touched_Mark :=
              To_Stack_Address (Arr_Access (J)'Address);

            if Analyzer.First_Is_Outermost then
               exit;
            end if;
         end if;
      end loop;
   end Compute_Result;

   ---------------------
   --  Output_Result --
   ---------------------

   procedure Output_Result (Result_Id : Natural; Result : Task_Result) is
   begin
      Set_Output (Standard_Error);
      Put (Natural'Image (Result_Id));
      Put (" | ");
      Put (Result.Task_Name);
      Put (" | ");
      Put (Natural'Image (Result.Max_Size));
      Put (" | ");
      Put (Natural'Image (Result.Measure));
      New_Line;
   end Output_Result;

   ---------------------
   --  Output_Results --
   ---------------------

   procedure Output_Results is
   begin
      if Compute_Environment_Task then
         Compute_Result (Environment_Task_Analyzer);
         Report_Result (Environment_Task_Analyzer);
      end if;

      Set_Output (Standard_Error);
      Put ("Index | Task Name | Stack Size | Actual Use");
      New_Line;

      for J in Result_Array'Range loop
         exit when J >= Next_Id;

         Output_Result (J, Result_Array (J));
      end loop;
   end Output_Results;

   -------------------
   -- Report_Result --
   -------------------

   procedure Report_Result (Analyzer : Stack_Analyzer) is
   begin
      if Analyzer.Result_Id in Result_Array'Range then
         Result_Array (Analyzer.Result_Id).Measure := Report_Result (Analyzer);
      else
         Output_Result
           (Analyzer.Result_Id,
            (Task_Name => Analyzer.Task_Name,
             Max_Size  => Analyzer.Size,
             Measure   => Report_Result (Analyzer)));
      end if;
   end Report_Result;

   function Report_Result (Analyzer : Stack_Analyzer) return Natural is
   begin
      if Analyzer.Outermost_Touched_Mark = Analyzer.Inner_Pattern_Mark then
         return Stack_Size (Analyzer.Inner_Pattern_Mark,
                            Analyzer.Bottom_Of_Stack);

      else
         return Stack_Size (Analyzer.Outermost_Touched_Mark,
                            Analyzer.Bottom_Of_Stack);
      end if;
   end Report_Result;

end System.Stack_Usage;
