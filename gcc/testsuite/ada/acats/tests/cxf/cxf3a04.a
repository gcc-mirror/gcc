-- CXF3A04.A 
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
--      Check that the edited output string value returned by Function Image
--      is correct.
--
-- TEST DESCRIPTION:
--      This test is structured using tables of data, consisting of
--      numerical values, picture strings, and expected image
--      result strings.  These data tables are found in package FXF3A00.
--      
--      The results of the Image function are examined under a number of 
--      circumstances.  The generic package Decimal_Output is instantiated
--      twice, for decimal data with delta 0.01 and delta 1.0.  Each version
--      of Image is called with both default parameters and user-provided
--      parameters.  The results of each call to Image are compared to an
--      expected edited output result string.
--      
--      In addition, three calls to Image are designed to raise Layout_Error,
--      due to the combination of decimal value and picture string provided
--      as input parameters.  If Layout_Error is not raised, or an alternate
--      exception is raised instead, test failure results.
--      
-- TEST FILES:
--      The following files comprise this test:
--
--         FXF3A00.A   (foundation code)
--      => CXF3A04.A
--
--       
-- CHANGE HISTORY:
--      22 JAN 95   SAIC    Initial prerelease version.
--      11 MAR 97   PWB.CTA Corrected incorrect index expression
--!

with FXF3A00;
with Ada.Text_IO.Editing;
with Report;

procedure CXF3A04 is
begin

   Report.Test ("CXF3A04", "Check that the string value returned by " &
                           "Function Image is correct, based on the " &
                           "numerical data and picture formatting "   &
                           "parameters provided to the function");

   Test_Block:
   declare

      use Ada.Text_IO;

      -- Instantiate the Decimal_Output generic package for the two data
      -- types, using the default values for the Default_Currency,
      -- Default_Fill, Default_Separator, and Default_Radix_Mark 
      -- parameters.

      package Pack_NDP is 
        new Editing.Decimal_Output (FXF3A00.Decimal_Type_NDP);

      package Pack_2DP is 
        new Editing.Decimal_Output (FXF3A00.Decimal_Type_2DP);

      TC_Currency   : constant String    := "$";
      TC_Fill       : constant Character := '*';
      TC_Separator  : constant Character := ',';
      TC_Radix_Mark : constant Character := '.';

      TC_Picture    : Editing.Picture;


   begin

      Two_Decimal_Place_Data:
      -- Use a decimal fixed point type with delta 0.01 (two decimal places) 
      -- and valid picture strings. Evaluate the result of function Image 
      -- with the expected edited output result string.
      declare  

         TC_Loop_End : constant :=                                     -- 10
           FXF3A00.Number_Of_2DP_Items - FXF3A00.Number_Of_Foreign_Strings;

      begin
         -- The first 10 picture strings in the Valid_Strings array 
         -- correspond to data values of a decimal type with delta 0.01. 

         -- Compare string result of Image with expected edited output 
         -- string.  Evaluate data using both default parameters of Image
         -- and user-provided parameter values.
         for i in 1..TC_Loop_End loop

            -- Create the picture object from the picture string.
            TC_Picture := Editing.To_Picture(FXF3A00.Valid_Strings(i).all);

            -- Use the default parameters for this loop evaluation of Image.
            if Pack_2DP.Image(FXF3A00.Data_With_2DP(i), TC_Picture) /=
               FXF3A00.Edited_Output(i).all
            then
               Report.Failed("Incorrect result from Function Image, "    &
                             "when used with a decimal type with delta " &
                             "0.01, picture string "                     &
                             FXF3A00.Valid_Strings(i).all                &
                             ", and the default parameters of Image");
            end if;

            -- Use user-provided parameters for this loop evaluation of Image.

            if Pack_2DP.Image(Item       => FXF3A00.Data_With_2DP(i), 
                              Pic        => TC_Picture,
                              Currency   => TC_Currency,
                              Fill       => TC_Fill,
                              Separator  => TC_Separator,
                              Radix_Mark => TC_Radix_Mark) /=
               FXF3A00.Edited_Output(i).all
            then
               Report.Failed("Incorrect result from Function Image, "    &
                             "when used with a decimal type with delta " &
                             "0.01, picture string "                     &
                             FXF3A00.Valid_Strings(i).all                &
                             ", and user-provided parameters");
            end if;

         end loop;

      exception
         when others => 
           Report.Failed("Exception raised in Two_Decimal_Place_Data block");
      end Two_Decimal_Place_Data;



      No_Decimal_Place_Data:
      -- Use a decimal fixed point type with delta 1.00 (no decimal places) 
      -- and valid picture strings. Evaluate the result of function Image 
      -- with the expected result string.
      declare

         use Editing, FXF3A00;

         TC_Offset     : constant := 10;
         TC_Loop_Start : constant := TC_Offset + 1;                   -- 11
         TC_Loop_End   : constant := TC_Loop_Start +
                                     Number_Of_NDP_Items - 1;         -- 22

      begin
         -- The following evaluations correspond to data values of a 
         -- decimal type with delta 1.0. 

         -- Compare string result of Image with expected edited output 
         -- string.  Evaluate data using both default parameters of Image
         -- and user-provided parameter values.
         -- Note: TC_Offset is used to align corresponding data the various
         --       data tables in foundation package FXF3A00.

         for i in TC_Loop_Start..TC_Loop_End loop

            -- Create the picture object from the picture string.
            TC_Picture := To_Picture(Valid_Strings(i).all);

            -- Use the default parameters for this loop evaluation of Image.
            if not (Pack_NDP.Image(Data_With_NDP(i-TC_Offset), TC_Picture) =
                    Edited_Output(TC_Offset+i).all)
            then
               Report.Failed("Incorrect result from Function Image, "    &
                             "when used with a decimal type with delta " &
                             "1.0, picture string "                      &
                             Valid_Strings(i).all                        &
                             ", and the default parameters of Image");
            end if;

            -- Use user-provided parameters for this loop evaluation of Image.
            if Pack_NDP.Image(Item       => Data_With_NDP(i - TC_Offset), 
                              Pic        => TC_Picture,
                              Currency   => TC_Currency,
                              Fill       => TC_Fill,
                              Separator  => TC_Separator,
                              Radix_Mark => TC_Radix_Mark) /=
               Edited_Output(TC_Offset+i).all
            then
               Report.Failed("Incorrect result from Function Image, "    &
                             "when used with a decimal type with delta " &
                             "1.0, picture string "                      &
                             Valid_Strings(i).all                        &
                             ", and user-provided parameters");
            end if;

         end loop;

      exception
         when others => 
           Report.Failed("Exception raised in No_Decimal_Place_Data block");
      end No_Decimal_Place_Data;



      Exception_Block:
      -- The following three calls of Function Image, using the specific
      -- decimal values and picture strings provided, will cause 
      -- a Layout_Error to be raised.
      -- The first two evaluations use the instantiation of Decimal_Output
      -- with a decimal type with delta 0.01, while the last evaluation
      -- uses the instantiation with decimal type with delta 1.0.

      -- Note: The data and the picture strings used in the following 
      --       evaluations are not themselves erroneous, but when used in
      --       combination will cause Layout_Error to be raised.

      begin

         for i in 1..FXF3A00.Number_Of_Erroneous_Conditions loop    -- 1..3
            begin
               -- Create the picture object from the picture string.
               TC_Picture :=
                 Editing.To_Picture(FXF3A00.Erroneous_Strings(i).all);

               -- Layout_Error must be raised by the following calls to
               -- Function Image.

               if i < 3 then  -- Choose the appropriate instantiation.
                  declare
                     N : constant Natural := Pack_2DP.Length(TC_Picture);
                     TC_String : String(1..N);
                  begin
                     TC_String := Pack_2DP.Image(FXF3A00.Erroneous_Data(i),
                                                 TC_Picture);
                  end;
               else
                  declare
                     use FXF3A00;
                     N : constant Natural := Pack_NDP.Length(TC_Picture,
                                                             TC_Currency);
                     TC_String : String(1..N);
                  begin
                     TC_String := 
                       Pack_NDP.Image(Item       => Decimal_Type_NDP(
                                                      Erroneous_Data(i)),
                                      Pic        => TC_Picture,
                                      Currency   => TC_Currency,
                                      Fill       => TC_Fill,
                                      Separator  => TC_Separator,
                                      Radix_Mark => TC_Radix_Mark);
                  end;
               end if;

               Report.Failed("Layout_Error not raised by combination "  &
                             "# " & Integer'Image(i) & " "              &
                             "of decimal data and picture string");

            exception
               when Layout_Error => null;    -- Expected exception.
               when others       =>
                 Report.Failed("Incorrect exception raised by combination " &
                               "# " & Integer'Image(i) & " "                &
                               "of decimal data and picture string");
            end;
         end loop;

      exception
         when others => 
           Report.Failed("Unexpected exception raised in Exception_Block");
      end Exception_Block;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXF3A04;
