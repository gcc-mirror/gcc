-- CXF3A08.A 
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
--      Check that the version of Ada.Text_IO.Editing.Put with an out 
--      String parameter propagates Layout_Error if the edited output string 
--      result of Put exceeds the length of the out String parameter.
--      
-- TEST DESCRIPTION:
--      This test is structured using tables of data, consisting of
--      numerical values, picture strings, and expected image
--      result strings.  These data tables are found in package FXF3A00.
--      
--      This test examines the case of the out string parameter to Procedure
--      Put being insufficiently long to hold the entire edited output
--      string result of the procedure.  In this case, Layout_Error is to be
--      raised.  Test failure results if Layout_Error is not raised, or if an
--      exception other than Layout_Error is raised.
--      
--      A number of data combinations are examined, using instantiations 
--      of Package Decimal_Output with different decimal data types and 
--      both default and non-default parameters as generic actual parameters.
--      In addition, calls to Procedure Put are performed using default 
--      parameters, non-default parameters, and non-default parameters that 
--      override the generic actual parameters provided at the time of 
--      instantiation of Decimal_Output.
--      
-- TEST FILES:
--      The following files comprise this test:
--
--         FXF3A00.A   (foundation code)
--      => CXF3A08.A
--
--       
-- CHANGE HISTORY:
--      31 JAN 95   SAIC    Initial prerelease version.
--
--!

with FXF3A00;
with Ada.Text_IO.Editing;
with Report;

procedure CXF3A08 is
begin

   Report.Test ("CXF3A08", "Check that the version of "                &
                           "Ada.Text_IO.Editing.Put with an out "      &
                           "String parameter propagates Layout_Error " &
                           "if the output string exceeds the length "  &
                           "of the out String parameter");

   Test_Block:
   declare

      use Ada.Text_IO;

      -- Instantiate the Decimal_Output generic package for two
      -- different decimal data types.
                                   -- Uses decimal type with delta 0.01 and
      package Pack_2DP is          -- non-default generic actual parameters.
        new Editing.Decimal_Output(Num => FXF3A00.Decimal_Type_2DP,
                                   Default_Currency   => "$",
                                   Default_Fill       => '*',
                                   Default_Separator  => ',',
                                   Default_Radix_Mark => '.');

      package Pack_NDP is          -- Uses decimal type with delta 1.0.
        new Editing.Decimal_Output(FXF3A00.Decimal_Type_NDP);

      TC_Picture       : Editing.Picture;
      TC_Start_Loop    : Integer := 0;
      TC_End_Loop      : Integer := 0;
      TC_Offset        : Integer := 0;

      TC_Short_String  : String(1..4);   -- Shorter than the shortest edited
                                         -- output string result.

   begin

      -- Examine cases where the out string parameter is shorter than 
      -- the length of the edited output result.  Use the instantiation of 
      -- Decimal_Output specific to data with two decimal places.

      TC_Start_Loop := 1;
      TC_End_Loop   := FXF3A00.Number_of_2DP_Items -              -- 10
                       FXF3A00.Number_Of_Foreign_Strings;

      for i in TC_Start_Loop..TC_End_Loop loop                    -- 1..10

         -- Create the picture object from the picture string.

         TC_Picture := 
           Editing.To_Picture(Pic_String => FXF3A00.Valid_Strings(i).all,
                              Blank_When_Zero => False);

         -- The out parameter string provided in the call to Put is 
         -- shorter than the edited output result of the procedure.  
         -- This will result in a Layout_Error being raised and handled.
         -- Test failure results from no exception being raised, or from
         -- the wrong exception being raised.

         begin

            -- Use the instantiation of Decimal_Output specific to decimal
            -- data with two decimal places, as well as non-default 
            -- parameters and named parameter association.

            Pack_2DP.Put(To         => TC_Short_String,
                         Item       => FXF3A00.Data_With_2DP(i),
                         Pic        => TC_Picture,
                         Currency   => "$",
                         Fill       => '*',
                         Separator  => ',',
                         Radix_Mark => '.');

            -- Test failure if exception not raised.

            Report.Failed
              ("Layout_Error not raised, decimal data with two decimal " &
               "places, loop #" & Integer'Image(i));

         exception
            when Layout_Error => null;  -- OK, expected exception.
            when others       =>
               Report.Failed
                 ("Incorrect exception raised, Layout_Error expected, " &
                  "decimal data with two decimal places, loop #"        &
                  Integer'Image(i));
         end;
      end loop;


      -- Perform similar evaluations as above, but use the instantiation
      -- of Decimal_Output specific to decimal data with no decimal places.

      TC_Start_Loop := TC_End_Loop + 1;                           -- 11
      TC_End_Loop   := TC_Start_Loop +                            -- 22
                       FXF3A00.Number_of_NDP_Items - 1;               
      TC_Offset     := FXF3A00.Number_of_Foreign_Strings;         -- 10
      -- This offset is required due to the arrangement of data within the
      -- tables found in FXF3A00.

      for i in TC_Start_Loop..TC_End_Loop loop                    -- 11..22

         -- Create the picture object from the picture string.

         TC_Picture := Editing.To_Picture(FXF3A00.Valid_Strings(i).all);

         begin

            -- Use the instantiation of Decimal_Output specific to decimal
            -- data with no decimal places, as well as default parameters
            -- and positional parameter association.

            Pack_NDP.Put(TC_Short_String,
                         FXF3A00.Data_With_NDP(i-TC_Offset),
                         TC_Picture);

            -- Test failure if exception not raised.

            Report.Failed
              ("Layout_Error not raised, decimal data with no decimal " &
               "places, loop #" & Integer'Image(i));

         exception
            when Layout_Error => null;  -- OK, expected exception.
            when others       =>
               Report.Failed
                 ("Incorrect exception raised, Layout_Error expected, " &
                  "decimal data with no decimal places, loop #"         &
                  Integer'Image(i));
         end;

      end loop;


      -- Check that Layout_Error is raised by Put resulting from an
      -- instantiation of Decimal_Output specific to foreign currency
      -- representations. 
      -- Note: Both of the following evaluation sets use decimal data with 
      --       two decimal places.

      declare

         package Pack_FF is 
           new Editing.Decimal_Output(Num => FXF3A00.Decimal_Type_2DP,
                                      Default_Currency   => "FF",
                                      Default_Fill       => '*',
                                      Default_Separator  => '.',
                                      Default_Radix_Mark => ',');

      begin

         TC_Offset := FXF3A00.Number_Of_2DP_Items -                 -- 10
                      FXF3A00.Number_Of_Foreign_Strings;

         for i in 1..FXF3A00.Number_Of_FF_Strings loop              -- 1..4
            begin

               -- Create the picture object from the picture string.
               TC_Picture := 
                 Editing.To_Picture(FXF3A00.Foreign_Strings(i).all);

               Pack_FF.Put(To   => TC_Short_String,
                           Item => FXF3A00.Data_With_2DP(i+TC_Offset),
                           Pic  => TC_Picture);

               Report.Failed("Layout_Error was not raised by Put from "  &
                             "an instantiation of Decimal_Output using " &
                             "non-default parameters specific to FF "    &
                             "currency, loop #" & Integer'Image(i));

            exception
               when Layout_Error => null;  -- OK, expected exception.
               when others       =>
                  Report.Failed("Incorrect exception raised by Put from "   &
                                "an instantiation of Decimal_Output using " &
                                "non-default parameters specific to FF "    &
                                "currency, loop #" & Integer'Image(i));
            end;
         end loop;


         -- These evaluations use a version of Put resulting from a 
         -- non-default instantiation of Decimal_Output, but which has 
         -- specific foreign currency parameters provided in the call that
         -- override the generic actual parameters provided at instantiation.

         TC_Offset := TC_Offset + FXF3A00.Number_Of_FF_Strings;      -- 14

         for i in 1..FXF3A00.Number_Of_DM_Strings loop               -- 1..5
            begin
               TC_Picture := 
                 Editing.To_Picture(FXF3A00.Foreign_Strings
                                      (i+FXF3A00.Number_Of_FF_Strings).all);

               Pack_2DP.Put(To         => TC_Short_String,
                            Item       => FXF3A00.Data_With_2DP(i+TC_Offset),
                            Pic        => TC_Picture,
                            Currency   => "DM",
                            Fill       => '*',
                            Separator  => ',',
                            Radix_Mark => '.');

               Report.Failed("Layout_Error was not raised by Put using " &
                             "non-default parameters specific to DM "    &
                             "currency, loop #" & Integer'Image(i));

            exception
               when Layout_Error => null;  -- OK, expected exception.
               when others       =>
                  Report.Failed("Incorrect exception raised by Put using " &
                                "non-default parameters specific to DM "   &
                                "currency, loop #" & Integer'Image(i));
            end;
         end loop;

      end;

   exception
      when others => Report.Failed("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXF3A08;
