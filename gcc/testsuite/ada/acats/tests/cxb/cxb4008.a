-- CXB4008.A
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
--      Check that the function To_Decimal with Binary parameter will return
--      the corresponding value of the decimal type Num.
--
--      Check that the function To_Decimal with Long_Binary parameter will 
--      return the corresponding value of the decimal type Num.
--
--      Check that both of the To_Decimal functions described above will 
--      propagate Conversion_Error if the converted value Item is outside 
--      the range of type Num.
--
--      Check that the function To_Binary converts a value of the Ada 
--      decimal type Num into a Binary type value.
--
--      Check that the function To_Long_Binary converts a value of the Ada 
--      decimal type Num into a Long_Binary type value.
--      
-- TEST DESCRIPTION:
--      This test uses several instantiations of generic package 
--      Decimal_Conversions to provide appropriate test material.
--      Two of the instantiations use decimal types as generic actuals
--      that include the implementation defined constants Max_Digits_Binary 
--      and Max_Digits_Long_Binary in their definition.
--
--      Subtests are included for both versions of function To_Decimal,
--      (Binary and Long_Binary parameters), and include checks that
--      Conversion_Error is propagated under the appropriate circumstances.
--      Functions To_Binary and To_Long_Binary are "sanity" checked, to  
--      ensure that the functions are available, and that the results are 
--      appropriate based on their parameter input.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.COBOL.  If an implementation provides
--      package Interfaces.COBOL, this test must compile, execute, and 
--      report "PASSED".
--
--       
-- CHANGE HISTORY:
--      21 Feb 96   SAIC    Initial release for 2.1.
--      10 Jun 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Ada.Exceptions;
with Interfaces.COBOL;                                          -- N/A => ERROR

procedure CXB4008 is
begin

   Report.Test ("CXB4008", "Check that functions To_Decimal, To_Binary, and " &
                           "To_Long_Binary produce the correct results");

   Test_Block:
   declare

      use Interfaces.COBOL;
      use Ada.Exceptions;
      use type Interfaces.COBOL.Numeric;

      type Decimal_Type_1 is delta 0.1    digits  6;
      type Decimal_Type_2 is delta 0.01   digits Max_Digits_Binary;
      type Decimal_Type_3 is delta 0.001  digits 10;
      type Decimal_Type_4 is delta 0.0001 digits Max_Digits_Long_Binary;

      package Pack_1 is new Decimal_Conversions(Decimal_Type_1);
      package Pack_2 is new Decimal_Conversions(Decimal_Type_2);
      package Pack_3 is new Decimal_Conversions(Decimal_Type_3);
      package Pack_4 is new Decimal_Conversions(Decimal_Type_4);

      TC_Dec_1       : Decimal_Type_1 := 12345.0;
      TC_Dec_2       : Decimal_Type_2 := 123456.00;
      TC_Dec_3       : Decimal_Type_3 := 1234567.000;
      TC_Dec_4       : Decimal_Type_4 := 12345678.0000;
      TC_Binary      : Interfaces.COBOL.Binary;
      TC_Long_Binary : Interfaces.COBOL.Long_Binary;

   begin

      -- Check that the function To_Decimal with Binary parameter will 
      -- return the corresponding value of the decimal type Num.

      if Pack_1.To_Decimal(Item => Pack_1.To_Binary(TC_Dec_1)) /= TC_Dec_1 or
         Pack_2.To_Decimal(Pack_2.To_Binary(TC_Dec_2))         /= TC_Dec_2 
      then
         Report.Failed("Incorrect result from function To_Decimal with " &
                       "Binary parameter - 1");
      end if;

      if Pack_1.To_Decimal(Item => Pack_1.To_Binary(1234.0)) /= 1234.0 then
         Report.Failed("Incorrect result from function To_Decimal with " &
                       "Binary parameter - 2");
      end if;

      TC_Binary := Pack_2.To_Binary(TC_Dec_2);
      if Pack_2.To_Decimal(TC_Binary) /= TC_Dec_2 then
         Report.Failed("Incorrect result from function To_Decimal with " &
                       "Binary parameter - 3");
      end if;



      -- Check that the function To_Decimal with Long_Binary parameter 
      -- will return the corresponding value of the decimal type Num.

      if Pack_3.To_Decimal(Item => Pack_3.To_Long_Binary(TC_Dec_3)) /= 
         TC_Dec_3                                                      or
         Pack_4.To_Decimal(Pack_4.To_Long_Binary(TC_Dec_4))         /= 
         TC_Dec_4 
      then
         Report.Failed("Incorrect result from function To_Decimal with " &
                       "Long_Binary parameter - 1");
      end if;

      if Pack_3.To_Decimal(Pack_3.To_Long_Binary(1234567.0)) /= 1234567.0 then
         Report.Failed("Incorrect result from function To_Decimal with " &
                       "Long_Binary parameter - 2");
      end if;

      TC_Long_Binary := Pack_4.To_Long_Binary(TC_Dec_4);
      if Pack_4.To_Decimal(TC_Long_Binary) /= TC_Dec_4 then
         Report.Failed("Incorrect result from function To_Decimal with " &
                       "Long_Binary parameter - 3");
      end if;



      -- Check that both of the To_Decimal functions described above 
      -- will propagate Conversion_Error if the converted value Item is 
      -- outside the range of type Num.
      -- Note: Binary/Long_Binary parameter values are created by an 
      --       instantiation of To_Binary/To_Long_Binary with a larger 
      --       Num type as the generic formal.

      Binary_Parameter:
      begin
         TC_Dec_1 := Pack_1.To_Decimal(Pack_2.To_Binary(123456.78));
         Report.Failed("Conversion_Error was not raised by function " &
                       "To_Decimal with Binary parameter, when the "  &
                       "converted value Item was outside the range "  &
                       "of type Num");
         if TC_Dec_1 = 12345.6 then  -- Avoid dead assignment optimization.
            Report.Comment("Should never be printed");
         end if;
      exception
         when Conversion_Error   => null;  -- OK, expected exception.
         when The_Error : others =>
            Report.Failed(Ada.Exceptions.Exception_Name(The_Error) & " "   &
                          "was incorrectly raised by function To_Decimal " &
                          "with Binary parameter, when the converted "     &
                          "value Item was outside the range of type Num");
      end Binary_Parameter;

      Long_Binary_Parameter:
      begin
         TC_Dec_3 := Pack_3.To_Decimal(Pack_4.To_Long_Binary(TC_Dec_4));
         Report.Failed("Conversion_Error was not raised by function "     &
                       "To_Decimal with Long_Binary parameter, when "     &
                       "the converted value Item was outside the range "  &
                       "of type Num");
         if TC_Dec_3 = 123456.78 then  -- Avoid dead assignment optimization.
            Report.Comment("Should never be printed");
         end if;
      exception
         when Conversion_Error   => null;  -- OK, expected exception.
         when The_Error : others =>
            Report.Failed(Ada.Exceptions.Exception_Name(The_Error) & " "    &
                          "was incorrectly raised by function To_Decimal "  &
                          "with Long_Binary parameter, when the converted " &
                          "value Item was outside the range of type Num");
      end Long_Binary_Parameter;



      -- Check that the function To_Binary converts a value of the Ada 
      -- decimal type Num into a Binary type value.

      TC_Dec_1 := 123.4;
      TC_Dec_2 := 9.99;
      if Pack_1.To_Binary(TC_Dec_1) = Pack_1.To_Binary(-TC_Dec_1) or
         Pack_2.To_Binary(TC_Dec_2) = Pack_2.To_Binary(-TC_Dec_2) 
      then
         Report.Failed("Incorrect result from function To_Binary - 1");
      end if;

      if Pack_1.To_Binary(1.1)     = Pack_1.To_Binary(-1.1)      or
         Pack_2.To_Binary(9999.99) = Pack_2.To_Binary(-9999.99) 
      then
         Report.Failed("Incorrect result from function To_Binary - 2");
      end if;


      -- Check that the function To_Long_Binary converts a value of the 
      -- Ada decimal type Num into a Long_Binary type value.

      TC_Dec_3 := 9.001;
      TC_Dec_4 := 123.4567;
      if Pack_3.To_Long_Binary(TC_Dec_3) = Pack_3.To_Long_Binary(-TC_Dec_3) or
         Pack_4.To_Long_Binary(TC_Dec_4) = Pack_4.To_Long_Binary(-TC_Dec_4)
      then
         Report.Failed("Incorrect result from function To_Long_Binary - 1");
      end if;

      if Pack_3.To_Long_Binary(1.011)        = 
         Pack_3.To_Long_Binary(-1.011)         or
         Pack_4.To_Long_Binary(2345678.9012) = 
         Pack_4.To_Long_Binary(-2345678.9012)
      then
         Report.Failed("Incorrect result from function To_Long_Binary - 2");
      end if;


   exception
      when The_Error : others => 
         Report.Failed("The following exception was raised in the " &
                       "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB4008;
