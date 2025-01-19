------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M - S T A C K _ U S A G E                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2025, Free Software Foundation, Inc.          --
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

with System.Storage_Elements;
with System.Address_To_Access_Conversions;
with Interfaces;

package System.Stack_Usage is
   pragma Preelaborate;

   package SSE renames System.Storage_Elements;

   subtype Stack_Address is SSE.Integer_Address;
   --  Address on the stack

   function To_Stack_Address
     (Value : System.Address) return Stack_Address
      renames System.Storage_Elements.To_Integer;

   Task_Name_Length : constant := 32;
   --  The maximum length of task name displayed.
   --  ??? Consider merging this variable with Max_Task_Image_Length.

   type Task_Result is record
      Task_Name : String (1 .. Task_Name_Length);

      Value : Natural;
      --  Amount of stack used. The value is calculated on the basis of the
      --  mechanism used by GNAT to allocate it, and it is NOT a precise value.

      Stack_Size : Natural;
      --  Size of the stack
   end record;

   type Result_Array_Type is array (Positive range <>) of Task_Result;

   type Stack_Analyzer is private;
   --  Type of the stack analyzer tool. It is used to fill a portion of the
   --  stack with Pattern, and to compute the stack used after some execution.

   --  Note that Fill_Stack writes data past the current top of the stack
   --  (i.e. at addresses less than the stack pointer register, assuming the
   --  stack grows downward). Therefore, this package is incompatible with
   --  tools like Valgrind and DrMemory.

   --  Usage:

   --  A typical use of the package is something like:

   --  A : Stack_Analyzer;

   --  task T is
   --     pragma Storage_Size (A_Storage_Size);
   --  end T;

   --  [...]

   --     Bottom_Of_Stack : aliased Integer;
   --     --  Bottom_Of_Stack'Address will be used as an approximation of
   --     --  the bottom of stack. A good practise is to avoid allocating
   --     --  other local variables on this stack, as it would degrade
   --     --  the quality of this approximation.

   --  begin
   --     Initialize_Analyzer (A,
   --                          "Task t",
   --                          A_Storage_Size,
   --                          0,
   --                          A_Storage_Size - A_Guard,
   --                          To_Stack_Address (Bottom_Of_Stack'Address));
   --     Fill_Stack (A);
   --     Some_User_Code;
   --     Compute_Result (A);
   --     Report_Result (A);
   --  end T;

   --  Errors:
   --
   --  We are instrumenting the code to measure the stack used by the user
   --  code. This method has a number of systematic errors, but several methods
   --  can be used to evaluate or reduce those errors. Here are those errors
   --  and the strategy that we use to deal with them:

   --  Bottom offset:

   --     Description: The procedure used to fill the stack with a given
   --       pattern will itself have a stack frame. The value of the stack
   --       pointer in this procedure is, therefore, different from the value
   --       before the call to the instrumentation procedure.

   --     Strategy: The user of this package should measure the bottom of stack
   --       before the call to Fill_Stack and pass it in parameter. The impact
   --       is very minor unless the stack used is very small, but in this case
   --       you aren't very interested by the figure.

   --  Instrumentation threshold at writing:

   --     Description: The procedure used to fill the stack with a given
   --       pattern will itself have a stack frame.  Therefore, it will
   --       fill the stack after this stack frame. This part of the stack will
   --       appear as used in the final measure.

   --     Strategy: As the user passes the value of the bottom of stack to
   --       the instrumentation to deal with the bottom offset error, and as
   --       the instrumentation procedure knows where the pattern filling start
   --       on the stack, the difference between the two values is the minimum
   --       stack usage that the method can measure. If, when the results are
   --       computed, the pattern zone has been left untouched, we conclude
   --       that the stack usage is inferior to this minimum stack usage.

   --  Instrumentation threshold at reading:

   --    Description: The procedure used to read the stack at the end of the
   --      execution clobbers the stack by allocating its stack frame. If this
   --      stack frame is bigger than the total stack used by the user code at
   --      this point, it will increase the measured stack size.

   --    Strategy: We could augment this stack frame and see if it changes the
   --      measure. However, this error should be negligible.

   --   Pattern zone overflow:

   --     Description: The stack grows outer than the topmost bound of the
   --       pattern zone. In that case, the topmost region modified in the
   --       pattern is not the maximum value of the stack pointer during the
   --       execution.

   --     Strategy: At the end of the execution, the difference between the
   --       topmost memory region modified in the pattern zone and the
   --       topmost bound of the pattern zone can be understood as the
   --       biggest allocation that the method could have detect, provided
   --       that there is no "Untouched allocated zone" error and no "Pattern
   --       usage in user code" error. If no object in the user code is likely
   --       to have this size, this is not likely to happen.

   --   Pattern usage in user code:

   --     Description: The pattern can be found in the object of the user code.
   --       Therefore, the address space where this object has been allocated
   --       will appear as untouched.

   --     Strategy: Choose a pattern that is uncommon. 16#0000_0000# is the
   --       worst choice; 16#DEAD_BEEF# can be a good one. A good choice is an
   --       address which is not a multiple of 2, and which is not in the
   --       target address space. You can also change the pattern to see if it
   --       changes the measure. Note that this error *very* rarely influence
   --       the measure of the total stack usage: to have some influence, the
   --       pattern has to be used in the object that has been allocated on the
   --       topmost address of the used stack.

   --   Stack overflow:

   --     Description: The pattern zone does not fit on the stack. This may
   --       lead to an erroneous execution.

   --     Strategy: Specify a storage size that is bigger than the size of the
   --       pattern. 2 times bigger should be enough.

   --   Augmentation of the user stack frames:

   --     Description: The use of instrumentation object or procedure may
   --       augment the stack frame of the caller.

   --     Strategy: Do *not* inline the instrumentation procedures. Do *not*
   --       allocate the Stack_Analyzer object on the stack.

   --   Untouched allocated zone:

   --     Description: The user code may allocate objects that it will never
   --       touch. In that case, the pattern will not be changed.

   --     Strategy: There are no way to detect this error. Fortunately, this
   --       error is really rare, and it is most probably a bug in the user
   --       code, e.g. some uninitialized variable. It is (most of the time)
   --       harmless: it influences the measure only if the untouched allocated
   --       zone happens to be located at the topmost value of the stack
   --       pointer for the whole execution.

   procedure Initialize (Buffer_Size : Natural);
   pragma Export (C, Initialize, "__gnat_stack_usage_initialize");
   --  Initializes the size of the buffer that stores the results. Only the
   --  first Buffer_Size results are stored. Any results that do not fit in
   --  this buffer will be displayed on the fly.

   procedure Fill_Stack (Analyzer : in out Stack_Analyzer);
   --  Fill an area of the stack with the pattern Analyzer.Pattern. The size
   --  of this area is Analyzer.Size. After the call to this procedure,
   --  the memory will look like that:
   --
   --                                                             Stack growing
   --  ---------------------------------------------------------------------->
   --  |<--------------------->|<----------------------------------->|
   --  |  Stack frames to      | Memory filled with Analyzer.Pattern |
   --  |  Fill_Stack           |                                     |
   --  ^                       |                                     ^
   --  Analyzer.Stack_Base     |                      Analyzer.Pattern_Limit
   --                          ^
   --                    Analyzer.Pattern_Limit +/- Analyzer.Pattern_Size
   --

   procedure Initialize_Analyzer
     (Analyzer         : in out Stack_Analyzer;
      Task_Name        : String;
      Stack_Size       : Natural;
      Stack_Base       : Stack_Address;
      Pattern_Size     : Natural;
      Pattern          : Interfaces.Unsigned_32 := 16#DEAD_BEEF#);
   --  Should be called before any use of a Stack_Analyzer, to initialize it.
   --  Max_Pattern_Size is the size of the pattern zone, might be smaller than
   --  the full stack size Stack_Size in order to take into account e.g. the
   --  secondary stack and a guard against overflow. The actual size taken
   --  will be readjusted with data already used at the time the stack is
   --  actually filled.

   Is_Enabled : Boolean := False;
   --  When this flag is true, then stack analysis is enabled

   procedure Compute_Result (Analyzer : in out Stack_Analyzer);
   --  Read the pattern zone and deduce the stack usage. It should be called
   --  from the same frame as Fill_Stack. If Analyzer.Probe is not null, an
   --  array of Unsigned_32 with Analyzer.Probe elements is allocated on
   --  Compute_Result's stack frame. Probe can be used to detect  the error:
   --  "instrumentation threshold at reading". See above. After the call
   --  to this procedure, the memory will look like:
   --
   --                                                             Stack growing
   --  ----------------------------------------------------------------------->
   --  |<---------------------->|<-------------->|<--------->|<--------->|
   --  |  Stack frames          | Array of       | used      |  Memory   |
   --  |  to Compute_Result     | Analyzer.Probe | during    |   filled  |
   --  |                        | elements       |  the      |    with   |
   --  |                        |                | execution |  pattern  |
   --  |                                                     |           |
   --  |<---------------------------------------------------->           |
   --                  Stack used                                        ^
   --                                                           Pattern_Limit

   procedure Report_Result (Analyzer : Stack_Analyzer);
   --  Store the results of the computation in memory, at the address
   --  corresponding to the symbol __gnat_stack_usage_results. This is not
   --  done inside Compute_Result in order to use as less stack as possible
   --  within a task.

   procedure Output_Results;
   --  Print the results computed so far on the standard output. Should be
   --  called when all tasks are dead.

   pragma Export (C, Output_Results, "__gnat_stack_usage_output_results");

private

   package Unsigned_32_Addr is
     new System.Address_To_Access_Conversions (Interfaces.Unsigned_32);

   subtype Pattern_Type is Interfaces.Unsigned_32;
   Bytes_Per_Pattern : constant := Pattern_Type'Object_Size / Storage_Unit;

   type Stack_Analyzer is record
      Task_Name : String (1 .. Task_Name_Length);
      --  Name of the task

      Stack_Base : Stack_Address;
      --  Address of the base of the stack, as given by the caller of
      --  Initialize_Analyzer.

      Stack_Size : Natural;
      --  Entire size of the analyzed stack

      Pattern_Size : Natural;
      --  Size of the pattern zone

      Pattern : Pattern_Type;
      --  Pattern used to recognize untouched memory

      Pattern_Limit : Stack_Address;
      --  Bound of the pattern area farthest to the base

      Topmost_Touched_Mark : Stack_Address;
      --  Topmost address of the pattern area whose value it is pointing
      --  at has been modified during execution. If the systematic error are
      --  compensated, it is the topmost value of the stack pointer during
      --  the execution.

      Pattern_Overlay_Address : System.Address;
      --  Address of the stack abstraction object we overlay over a
      --  task's real stack, typically a pattern-initialized array.

      Result_Id : Positive;
      --  Id of the result. If less than value given to gnatbind -u corresponds
      --  to the location in the result array of result for the current task.
   end record;

   Environment_Task_Analyzer : Stack_Analyzer;

   Compute_Environment_Task  : Boolean;

   type Result_Array_Ptr is access all Result_Array_Type;

   Result_Array : Result_Array_Ptr;
   pragma Export (C, Result_Array, "__gnat_stack_usage_results");
   --  Exported in order to have an easy accessible symbol in when debugging

   Next_Id : Positive := 1;
   --  Id of the next stack analyzer

   function Stack_Size
     (SP_Low  : Stack_Address;
      SP_High : Stack_Address) return Natural;
   pragma Inline (Stack_Size);
   --  Return the size of a portion of stack delimited by SP_High and SP_Low
   --  (), i.e. the difference between SP_High and SP_Low. The storage element
   --  pointed by SP_Low is not included in the size. Inlined to reduce the
   --  size of the stack used by the instrumentation code.

end System.Stack_Usage;
