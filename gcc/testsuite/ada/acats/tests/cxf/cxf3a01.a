-- CXF3A01.A 
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
--      Check that the function Ada.Text_IO.Editing.Valid returns False if
--         a) Pic_String is not a well-formed Picture string, or
--         b) the length of Pic_String exceeds Max_Picture_Length, or
--         c) Blank_When_Zero is True and Pic_String contains '*';
--      Check that Valid otherwise returns True.
--
-- TEST DESCRIPTION:
--      This test validates the results of function Editing.Valid under a 
--      variety of conditions.  Both valid and invalid picture strings are
--      provided as input parameters to the function.  The use of the 
--      Blank_When_Zero parameter is evaluated with strings that contain the
--      zero suppression character '*'.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXF3A00.A   (foundation code)
--      => CXF3A01.A
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with FXF3A00;
with Ada.Text_IO.Editing;
with Report;

procedure CXF3A01 is
begin

   Report.Test ("CXF3A01", "Check that the Valid function from package "    &
                           "Ada.Text_IO.Editing returns False for strings " &
                           "that fail to comply with the composition "      &
                           "constraints defined for picture strings. "      &
                           "Check that the Valid function returns True "    &
                           "for strings that conform to the composition "   &
                           "constraints defined for picture strings");

   Test_Block:
   declare
      use FXF3A00;
      use Ada.Text_IO;
   begin

      -- Use a series of picture strings that conform to the composition
      -- constraints to validate the Ada.Text_IO.Editing.Valid function.
      -- The result for each of these calls should be True.
      -- In all the following cases, the default value of the Blank_When_Zero
      -- parameter is used.

      for i in 1..FXF3A00.Number_Of_Valid_Strings loop

         if not Editing.Valid(Pic_String => FXF3A00.Valid_Strings(i).all) 
         then
            Report.Failed("Incorrect result from Function Valid using " &
                          "Valid_String = " & FXF3A00.Valid_Strings(i).all); 
         end if;

      end loop;


      for i in 1..FXF3A00.Number_Of_Foreign_Strings loop

         if not Editing.Valid(Pic_String => FXF3A00.Foreign_Strings(i).all) 
         then
            Report.Failed("Incorrect result from Function Valid using " &
                          "Foreign_String = " & 
                          FXF3A00.Foreign_Strings(i).all);
         end if;

      end loop;


      -- Use a series of picture strings that violate one or more of the 
      -- composition constraints to validate the Ada.Text_IO.Editing.Valid
      -- function.  The result for each of these calls should be False.
      -- In all the following cases, the default value of the Blank_When_Zero
      -- parameter is used.

      for i in 1..FXF3A00.Number_Of_Invalid_Strings loop

         if Editing.Valid(Pic_String => FXF3A00.Invalid_Strings(i).all) 
         then
            Report.Failed("Incorrect result from Function Valid using " &
                          "Invalid_String = " & 
                          FXF3A00.Invalid_Strings(i).all);
         end if;

      end loop;


      -- In all the following cases, the default value of the Blank_When_Zero
      -- parameter is overridden with a True actual parameter value.  Using
      -- valid picture strings that contain the '*' zero suppression character 
      -- when this parameter value is True must result in a False result
      -- from function Valid.  Valid picture strings that do not contain the
      -- '*' character should return a function result of True with True
      -- provided as the actual parameter to Blank_When_Zero.

      -- Check entries 1, 2, 25, 36 from the Valid_Strings array, all of
      -- which contain the '*' zero suppression character.

      if Editing.Valid(Valid_Strings(1).all,  Blank_When_Zero => True) or
         Editing.Valid(Valid_Strings(2).all,  Blank_When_Zero => True) or
         Editing.Valid(Valid_Strings(25).all, Blank_When_Zero => True) or
         Editing.Valid(Valid_Strings(36).all, Blank_When_Zero => True) 
      then
         Report.Failed
           ("Incorrect result from Function Valid when setting "   &
            "the value of the Blank_When_Zero parameter to True, " &
            "and using picture strings with the '*' character");
      end if;


      -- Check entries from the Valid_Strings array, none of
      -- which contain the '*' zero suppression character.

      for i in 3..24 loop

         if not Editing.Valid(Pic_String      => Valid_Strings(i).all,  
                              Blank_When_Zero => True)
         then
            Report.Failed("Incorrect result from Function Valid when "    &
                          "setting the value of the Blank_When_Zero "     &
                          "parameter to True, and using picture strings " &
                          "without the '*' character, Valid_String = "    &
                          FXF3A00.Valid_Strings(i).all);
         end if;

      end loop;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXF3A01;
