-- CXAF001.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that an implementation supports the functionality defined
--      in Package Ada.Command_Line.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the subprograms
--      contained in package Ada.Command_Line.  Each of the subprograms
--      is exercised in a general sense, to ensure that it is available,
--      and that it provides the prescribed results in a known test 
--      environment.  Function Argument_Count must return zero, or the 
--      number of arguments passed to the program calling it.  Function
--      Argument is called with a parameter value one greater than the
--      actual number of arguments passed to the executing program, which
--      must result in Constraint_Error being raised.  Function Command_Name
--      should return the name of the executing program that called it
--      (specifically, this test name).  Function Set_Exit_Status is called
--      with two different parameter values, the constants Failure and 
--      Success defined in package Ada.Command_Line.
--
--      The setting of the variable TC_Verbose allows for some additional
--      output to be displayed during the running of the test as an aid in
--      tracing the processing flow of the test.
--
-- APPLICABILITY CRITERIA: 
--      This test is applicable to implementations that support the 
--      declaration of package Command_Line as defined in the Ada Reference 
--      manual. 
--      An alternative declaration is allowed for package Command_Line if 
--      different functionality is appropriate for the external execution
--      environment.
--
--       
-- CHANGE HISTORY:
--      10 Jul 95   SAIC    Initial prerelease version.
--      02 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      05 AUG 98   EDS     Allow Null string result to be returned from
--                          Function Command
--!

with Ada.Command_Line;
with Ada.Exceptions;
with Report;

procedure CXAF001 is
begin

   Report.Test ("CXAF001", "Check that an implementation supports the " &
                           "functionality defined in Package "          &
                           "Ada.Command_Line");

   Test_Block:
   declare

      use Ada.Exceptions;

      type String_Access is access all String;

      TC_Verbose           : Boolean := False;
      Number_Of_Arguments  : Natural := Natural'Last;
      Name_Of_Command      : String_Access;

   begin

      -- Check the result of function Argument_Count.
      -- Note: If the external environment does not support passing arguments
      --       to the program invoking the function, the function result
      --       will be zero.

      Number_Of_Arguments := Ada.Command_Line.Argument_Count;
      if Number_Of_Arguments = Natural'Last then
         Report.Failed("Argument_Count did not provide a return result");
      end if;
      if TC_Verbose then
         Report.Comment
           ("Argument_Count = " & Integer'Image(Number_Of_Arguments));
      end if;


      -- Check that the result of Function Argument is Constraint_Error
      -- when the Number argument is outside the range of 1..Argument_Count.

      Test_Function_Argument_1 :
      begin
         declare

            -- Define a value that will be outside the range of 
            -- 1..Argument_Count. 
            -- Note: If the external execution environment does not support
            --       passing arguments to a program, then Argument(N) for 
            --       any N will raise Constraint_Error, since 
            --       Argument_Count = 0;

            Arguments_Plus_One : Positive := 
              Ada.Command_Line.Argument_Count + 1;

            -- Using the above value in a call to Argument must result in
            -- the raising of Constraint_Error.

            Argument_String    : constant String := 
              Ada.Command_Line.Argument(Arguments_Plus_One);

         begin
            Report.Failed("Constraint_Error not raised by Function "  &
                          "Argument when provided a Number argument " &
                          "out of range");
         end;
      exception
         when Constraint_Error => null;  -- OK, expected exception.
            if TC_Verbose then
              Report.Comment ("Argument_Count raised Constraint_Error");
            end if;
         when others => 
            Report.Failed ("Unexpected exception raised by Argument " &
                           "in Test_Function_Argument_1 block");
      end Test_Function_Argument_1;


      -- Check that Function Argument returns a string result.

      Test_Function_Argument_2 :
      begin
         if Ada.Command_Line.Argument_Count > 0 then
            Report.Comment
              ("Last argument is: " &
               Ada.Command_Line.Argument(Ada.Command_Line.Argument_Count));
         elsif TC_Verbose then
            Report.Comment("Argument_Count is zero, no test of Function " &
                           "Argument for string result");
         end if;
      exception
         when others => 
            Report.Failed ("Unexpected exception raised by Argument " &
                           "in Test_Function_Argument_2 block");
      end Test_Function_Argument_2;


      -- Check the result of Function Command_Name.

      Name_Of_Command := new String'(Ada.Command_Line.Command_Name);

      if Name_Of_Command = null  then
         Report.Failed("Null string pointer returned from Function Command");
      elsif Name_Of_Command.all = "" then
         Report.Comment("Null string result returned from Function Command");
      elsif TC_Verbose then
         Report.Comment("Invoking command is " & Name_Of_Command.all);
      end if;


      -- Check that procedure Set_Exit_Status is available.
      -- Note: If the external execution environment does not support
      --       returning an exit value from a program, then Set_Exit_Status
      --       does nothing.
      
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
      if TC_Verbose then
         Report.Comment("Exit status set to Failure");
      end if;

      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);
      if TC_Verbose then
         Report.Comment("Exit status set to Success");
      end if;


   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXAF001;
