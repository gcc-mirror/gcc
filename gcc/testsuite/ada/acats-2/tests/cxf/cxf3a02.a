-- CXF3A02.A 
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
--      Check that the function Ada.Text_IO.Editing.To_Picture raises
--      Picture_Error if the picture string provided as input parameter does
--      not conform to the composition constraints defined for picture 
--      strings.
--      Check that when Pic_String is applied to To_Picture, the result
--      is equivalent to the actual string parameter of To_Picture; 
--      Check that when Blank_When_Zero is applied to To_Picture, the result
--      is the same value as the Blank_When_Zero parameter of To_Picture.
--      
-- TEST DESCRIPTION:
--      This test validates that function Editing.To_Picture returns a 
--      Picture result when provided a valid picture string, and raises a
--      Picture_Error exception when provided an invalid picture string
--      input parameter.  In addition, the Picture result of To_Picture is
--      converted back to a picture string value using function Pic_String,
--      and the result of function Blank_When_Zero is validated based on the
--      value of parameter Blank_When_Zero used in the formation of the Picture
--      by function To_Picture.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FXF3A00.A   (foundation code)
--      => CXF3A02.A
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      11 Mar 97   PWB.CTA Corrected invalid picture string and uppercase
--                          problem.
--!

with FXF3A00;
with Ada.Text_IO.Editing;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Report;

procedure CXF3A02 is

   Lower_Alpha : constant String := "abcdefghijklmnopqrstuvwxyz";
   Upper_Alpha : constant String := "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   function UpperCase ( Source : String ) return String is
   begin
      return
         Ada.Strings.Fixed.Translate
            ( Source  => Source, 
              Mapping => Ada.Strings.Maps.To_Mapping 
                            ( From => Lower_Alpha, 
                              To => Upper_Alpha ) );
   end UpperCase;

begin

   Report.Test ("CXF3A02", "Check that the function "                       &
                           "Ada.Text_IO.Editing.To_Picture raises "         &
                           "Picture_Error if the picture string provided "  &
                           "as input parameter does not conform to the "    &
                           "composition constraints defined for picture "   &
                           "strings");

   Test_Block:
   declare

      use Ada.Text_IO;
      use FXF3A00;

      TC_Picture         : Editing.Picture;
      TC_Blank_When_Zero : Boolean;

   begin


      -- Validate that function To_Picture does not raise Picture_Error when
      -- provided a valid picture string as an input parameter.

      for i in 1..FXF3A00.Number_Of_Valid_Strings loop
         begin
            TC_Picture := 
              Editing.To_Picture(Pic_String      => Valid_Strings(i).all,
                                 Blank_When_Zero => False );
         exception
            when Editing.Picture_Error => 
              Report.Failed
              ("Picture_Error raised by function To_Picture "     &
               "with a valid picture string as input parameter, " &
               "Valid_String = " & FXF3A00.Valid_Strings(i).all);
            when others => 
              Report.Failed("Unexpected exception raised - 1, " &
                            "Valid_String = " & FXF3A00.Valid_Strings(i).all);
         end;
      end loop;



      -- Validate that function To_Picture raises Picture_Error when an
      -- invalid picture string is provided as an input parameter.
      -- Default value used for parameter Blank_When_Zero.

      for i in 1..FXF3A00.Number_Of_Invalid_Strings loop
         begin
            TC_Picture := 
              Editing.To_Picture(Pic_String => FXF3A00.Invalid_Strings(i).all);
            Report.Failed
              ("Picture_Error not raised by function To_Picture "    &
               "with an invalid picture string as input parameter, " &
               "Invalid_String = " & FXF3A00.Invalid_Strings(i).all);
         exception
            when Editing.Picture_Error => null;  -- OK, expected exception.
            when others                => 
              Report.Failed("Unexpected exception raised, " & 
                            "Invalid_String = "             &
                            FXF3A00.Invalid_Strings(i).all);
         end;
      end loop;



      -- Validate that To_Picture and Pic_String/Blank_When_Zero provide 
      -- "inverse" results.

      -- Use the default value of the Blank_When_Zero parameter (False) for 
      -- these evaluations (some valid strings have the '*' zero suppression
      -- character, which would result in an invalid string if used with a
      -- True value for the Blank_When_Zero parameter).

      for i in 1..FXF3A00.Number_Of_Valid_Strings loop
         begin

            -- Format a picture string using function To_Picture.

            TC_Picture := Editing.To_Picture(FXF3A00.Valid_Strings(i).all);

            -- Reconvert the Picture result from To_Picture to a string value
            -- using function Pic_String, and compare to the original string.

            if Editing.Pic_String(Pic => TC_Picture) /= 
               Uppercase (FXF3A00.Valid_Strings(i).all)
            then
               Report.Failed
                 ("Inverse result incorrect from Editing.Pic_String, " &
                  "Valid_String = " & FXF3A00.Valid_Strings(i).all);
            end if;

            -- Ensure that function Blank_When_Zero returns the correct value
            -- of the Blank_When_Zero parameter used in forming the Picture 
            -- (default parameter value False used in call to To_Picture 
            -- above).

            if Editing.Blank_When_Zero(Pic => TC_Picture) then
               Report.Failed
                 ("Inverse result incorrect from Editing.Blank_When_Zero, " &
                  "Valid_String = " & FXF3A00.Valid_Strings(i).all);
            end if;

         exception
            when others => 
              Report.Failed("Unexpected exception raised - 2, " &
                            "Valid_String = " & FXF3A00.Valid_Strings(i).all);
         end;
      end loop;


      -- Specifically check that any lower case letters in the original
      -- picture string have been converted to upper case form following
      -- the To_Picture/Pic_String conversion (as shown in previous loop).

      declare
         The_Picture         : Editing.Picture;
         The_Picture_String  : constant String := "+bBbZz_zZz_Zz9.99";
         The_Expected_Result : constant String := "+BBBZZ_ZZZ_ZZ9.99";
      begin
         -- Convert Picture String to Picture.
         The_Picture := Editing.To_Picture(Pic_String => The_Picture_String);

         declare
            -- Reconvert the Picture to a Picture String.
            The_Result : constant String := Editing.Pic_String(The_Picture);
         begin
            if The_Result /= The_Expected_Result then
               Report.Failed("Conversion to Picture/Reconversion to String " &
                             "did not produce expected result when Picture " &
                             "String had lower case letters");
            end if;
         end;
      end;


      -- Use a value of True for the Blank_When_Zero parameter for the 
      -- following evaluations (picture strings that do not have the '*' zero 
      -- suppression character, which would result in an invalid string when 
      -- used here with a True value for the Blank_When_Zero parameter).

      for i in 3..24 loop
         begin

            -- Format a picture string using function To_Picture.

            TC_Picture := 
              Editing.To_Picture(Pic_String      => Valid_Strings(i).all,
                                 Blank_When_Zero => True);

            -- Reconvert the Picture result from To_Picture to a string value
            -- using function Pic_String, and compare to the original string.

            if Editing.Pic_String(Pic => TC_Picture) /= 
               UpperCase (FXF3A00.Valid_Strings(i).all)
            then
               Report.Failed
                 ("Inverse result incorrect from Editing.Pic_String, used "   &
                  "on Picture formed with parameter Blank_When_Zero = True, " &
                  "Valid_String = " & FXF3A00.Valid_Strings(i).all);
            end if;

            -- Ensure that function Blank_When_Zero returns the correct value
            -- of the Blank_When_Zero parameter used in forming the Picture 
            -- (default parameter value False overridden in call to 
            -- To_Picture above).

            if not Editing.Blank_When_Zero(Pic => TC_Picture) then
               Report.Failed
                 ("Inverse result incorrect from Editing.Blank_When_Zero, "  &
                  "used on a Picture formed with parameter Blank_When_Zero " & 
                  "= True, Valid_String = " & FXF3A00.Valid_Strings(i).all);
            end if;

         exception
            when others => 
              Report.Failed("Unexpected exception raised - 3, " &
                            "Valid_String = " & FXF3A00.Valid_Strings(i).all);
         end;
      end loop;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXF3A02;
