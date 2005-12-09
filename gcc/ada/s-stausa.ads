------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M - S T A C K _ U S A G E                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2005, Free Software Foundation, Inc.          --
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

with System;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;

package System.Stack_Usage is
   pragma Preelaborate;

   package SSE renames System.Storage_Elements;

   Byte_Size : constant := 8;
   Word_32_Size : constant := 4 * Byte_Size;

   type Word_32 is mod 2 ** Word_32_Size;
   for Word_32'Alignment use 4;

   subtype Stack_Address is SSE.Integer_Address;
   --  Address on the stack
   --
   --  Note: in this package, when comparing two addresses on the stack, the
   --  comments use the terms "outer", "inner", "outermost" and "innermost"
   --  instead of the ambigous "higher", "lower", "highest" and "lowest".
   --  "inner" means "closer to the bottom of stack" and is the contrary of
   --  "outer". "innermost" means "closest address to the bottom of stack". The
   --  stack is growing from the inner to the outer.

   --  Top/Bottom would be much better than inner and outer ???

   function To_Stack_Address (Value : System.Address) return Stack_Address
                              renames System.Storage_Elements.To_Integer;

   type Stack_Analyzer is private;
   --  Type of the stack analyzer tool. It is used to fill a portion of
   --  the stack with Pattern, and to compute the stack used after some
   --  execution.

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
   --  code. This method has a number of systematic errors, but several
   --  methods can be used to evaluate or reduce those errors. Here are
   --  those errors and the strategy that we use to deal with them:

   --  Bottom offset:

   --     Description: The procedure used to fill the stack with a given
   --       pattern will itself have a stack frame. The value of the stack
   --       pointer in this procedure is, therefore, different from the value
   --       before the call to the instrumentation procedure.

   --     Strategy: The user of this package should measure the bottom of stack
   --       before the call to Fill_Stack and pass it in parameter.

   --  Instrumentation threshold at writing:

   --     Description: The procedure used to fill the stack with a given
   --       pattern will itself have a stack frame.  Therefore, it will
   --       fill the stack after this stack frame. This part of the stack will
   --       appear as used in the final measure.

   --     Strategy: As the user passes the value of the bottom of stack to
   --       the instrumentation to deal with the bottom offset error, and as as
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
   --      measure. However, this error should be negligeable.

   --   Pattern zone overflow:

   --     Description: The stack grows outer than the outermost bound of the
   --       pattern zone. In that case, the outermost region modified in the
   --       pattern is not the maximum value of the stack pointer during the
   --       execution.

   --     Strategy: At the end of the execution, the difference between the
   --       outermost memory region modified in the pattern zone and the
   --       outermost bound of the pattern zone can be understood as the
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
   --       outermost address of the used stack.

   --   Stack overflow:

   --     Description: The pattern zone does not fit on the stack. This may
   --       lead to an erroneous execution.

   --    Strategy: Specify a storage size that is bigger than the size of the
   --      pattern. 2 times bigger should be enough.

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
   --       zone happens to be located at the outermost value of the stack
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
   --  ----------------------------------------------------------------------->
   --  |<---------------------->|<----------------------------------->|
   --  |  Stack frame           | Memory filled with Analyzer.Pattern |
   --  |  of Fill_Stack         |                                     |
   --  |  (deallocated at       |                                     |
   --  |  the end of the call)  |                                     |
   --  ^                        |                                     |
   --  Analyzer.Bottom_Of_Stack ^                                     |
   --                    Analyzer.Inner_Pattern_Mark                  ^
   --                                            Analyzer.Outer_Pattern_Mark

   procedure Initialize_Analyzer
     (Analyzer  : in out Stack_Analyzer;
      Task_Name : String;
      Size      : Natural;
      Bottom    : Stack_Address;
      Pattern   : Word_32 := 16#DEAD_BEEF#);
   --  Should be called before any use of a Stack_Analyzer, to initialize it.
   --  Size is the size of the pattern zone. Bottom should be a close
   --  approximation of the caller base frame address.

   Is_Enabled : Boolean := False;
   --  When this flag is true, then stack analysis is enabled

   procedure Compute_Result (Analyzer : in out Stack_Analyzer);
   --  Read the patern zone and deduce the stack usage. It should be called
   --  from the same frame as Fill_Stack. If Analyzer.Probe is not null, an
   --  array of Word_32 with Analyzer.Probe elements is allocated on
   --  Compute_Result's stack frame. Probe can be used to detect  the error:
   --  "instrumentation threshold at reading". See above. After the call
   --  to this procedure, the memory will look like:
   --
   --                                                             Stack growing
   --  ----------------------------------------------------------------------->
   --  |<---------------------->|<-------------->|<--------->|<--------->|
   --  |  Stack frame           | Array of       | used      |  Memory   |
   --  |  of Compute_Result     | Analyzer.Probe | during    |   filled  |
   --  |  (deallocated at       | elements       |  the      |    with   |
   --  |  the end of the call)  |                | execution |  pattern  |
   --  |                        ^                |           |           |
   --  |                   Inner_Pattern_Mark    |           |           |
   --  |                                                     |           |
   --  |<---------------------------------------------------->           |
   --                  Stack used                                        ^
   --                                                     Outer_Pattern_Mark

   procedure Report_Result (Analyzer : Stack_Analyzer);
   --  Store the results of the computation in memory, at the address
   --  corresponding to the symbol __gnat_stack_usage_results. This is not
   --  done inside Compute_Resuls in order to use as less stack as possible
   --  within a task.

   procedure Output_Results;
   --  Print the results computed so far on the standard output. Should be
   --  called when all tasks are dead.

   pragma Export (C, Output_Results, "__gnat_stack_usage_output_results");

private

   Task_Name_Length : constant := 32;

   package Word_32_Addr is
     new System.Address_To_Access_Conversions (Word_32);

   type Stack_Analyzer is record
      Task_Name : String (1 .. Task_Name_Length);
      --  Name of the task

      Size : Natural;
      --  Size of the pattern zone

      Pattern : Word_32;
      --  Pattern used to recognize untouched memory

      Inner_Pattern_Mark : Stack_Address;
      --  Innermost bound of the pattern area on the stack

      Outer_Pattern_Mark : Stack_Address;
      --  Outermost bound of the pattern area on the stack

      Outermost_Touched_Mark : Stack_Address;
      --  Outermost address of the pattern area whose value it is pointing
      --  at has been modified during execution. If the systematic error are
      --  compensated, it is the outermost value of the stack pointer during
      --  the execution.

      Bottom_Of_Stack : Stack_Address;
      --  Address of the bottom of the stack, as given by the caller of
      --  Initialize_Analyzer.

      Array_Address : System.Address;
      --  Address of the array of Word_32 that represents the pattern zone

      First_Is_Outermost : Boolean;
      --  Set to true if the first element of the array of Word_32 that
      --  represents the pattern zone is at the outermost address of the
      --  pattern zone; false if it is the innermost address.

      Result_Id : Positive;
      --  Id of the result. If less than value given to gnatbind -u corresponds
      --  to the location in the result array of result for the current task.
   end record;

   Environment_Task_Analyzer : Stack_Analyzer;

   Compute_Environment_Task  : Boolean;

   type Task_Result is record
      Task_Name : String (1 .. Task_Name_Length);
      Measure   : Natural;
      Max_Size  : Natural;
   end record;

   type Result_Array_Type is array (Positive range <>) of Task_Result;
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
   --  Return the size of a portion of stack delimeted by SP_High and SP_Low
   --  (), i.e. the difference between SP_High and SP_Low. The storage element
   --  pointed by SP_Low is not included in the size. Inlined to reduce the
   --  size of the stack used by the instrumentation code.

end System.Stack_Usage;
