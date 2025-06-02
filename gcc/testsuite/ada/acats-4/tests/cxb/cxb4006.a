-- CXB4006.A
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
--      Check that the function Valid with Packed_Decimal and Packed_Format
--      parameters returns True if Item (the Packed_Decimal parameter) has
--      a value consistent with the Packed_Format parameter.
--
--      Check that the function Length with Packed_Format parameter returns
--      the minimal length of a Packed_Decimal value sufficient to hold any
--      value of type Num when represented according to parameter Format.
--
--      Check that the function To_Decimal with Packed_Decimal and 
--      Packed_Format parameters produces a decimal type value corresponding 
--      to the Packed_Decimal parameter value Item, under the conditions of 
--      the Packed_Format parameter Format.
--
--      Check that the function To_Packed with Decimal (Num) and 
--      Packed_Format parameters produces a Packed_Decimal result that 
--      corresponds to the decimal parameter under conditions of the 
--      Packed_Format parameter.
--
--      Check that Conversion_Error is propagated by function To_Packed if 
--      the value of the decimal parameter Item is negative and the specified
--      Packed_Format parameter is Packed_Unsigned.
--
--      
-- TEST DESCRIPTION:
--      This test checks the results from instantiated versions of 
--      several functions that deal with parameters or results of type
--      Packed_Decimal.  Since the rules for the formation of Packed_Decimal
--      values are implementation defined, several of the subtests cannot
--      directly check the accuracy of the results produced. Instead, they
--      verify that the result is within a range of possible values, or
--      that the result of one function can be converted back to the original 
--      actual parameter using a "mirror image" conversion function.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.COBOL.  If an implementation provides
--      package Interfaces.COBOL, this test must compile, execute, and 
--      report "PASSED".
--
--       
-- CHANGE HISTORY:
--      12 Feb 96   SAIC    Initial release for 2.1.
--      30 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Ada.Exceptions;
with Interfaces.COBOL;                                          -- N/A => ERROR

procedure CXB4006 is
begin

   Report.Test ("CXB4006", "Check that the functions Valid, Length, "   &
                           "To_Decimal, and To_Packed specific to "     &
                           "Packed_Decimal parameters produce correct " &
                           "results");

   Test_Block:
   declare

      use Interfaces.COBOL;
      use Ada.Exceptions;
      use type Interfaces.COBOL.Numeric;

      type Decimal_Type_1 is delta 0.1    digits  6;
      type Decimal_Type_2 is delta 0.01   digits  8;
      type Decimal_Type_3 is delta 0.001  digits 10;
      type Decimal_Type_4 is delta 0.0001 digits 12;

      package Pack_1 is new Decimal_Conversions(Decimal_Type_1);
      package Pack_2 is new Decimal_Conversions(Decimal_Type_2);
      package Pack_3 is new Decimal_Conversions(Decimal_Type_3);
      package Pack_4 is new Decimal_Conversions(Decimal_Type_4);

      TC_Dec_1      : Decimal_Type_1 := 12345.6;
      TC_Dec_2      : Decimal_Type_2 := 123456.78;
      TC_Dec_3      : Decimal_Type_3 := 1234567.890;
      TC_Dec_4      : Decimal_Type_4 := 12345678.9012;
      TC_Min_Length : Natural        := 1;
      TC_Max_Length : Natural        := 16;

   begin

      -- Check that the function Valid with Packed_Decimal and Packed_Format
      -- parameters returns True if Item (the Packed_Decimal parameter) has 
      -- a value consistent with the Packed_Format parameter.
      -- Note: Since the formation rules for Packed_Decimal values are
      --       implementation defined, the parameter values here are 
      --       created by function To_Packed.

      TC_Dec_1 := 1434.3;
      if not Pack_1.Valid(Item   => Pack_1.To_Packed(TC_Dec_1, 
                                                     Packed_Unsigned),
                          Format => Packed_Unsigned)
      then
         Report.Failed("Incorrect result from function Valid - 1");
      end if;

      TC_Dec_2 := -4321.03;
      if not Pack_2.Valid(Pack_2.To_Packed(TC_Dec_2, Packed_Signed),
                          Format => Packed_Signed)                   or
         Pack_2.Valid(Pack_2.To_Packed(TC_Dec_2, Packed_Signed),
                      Format => Packed_Unsigned)
      then
         Report.Failed("Incorrect result from function Valid - 2");
      end if;

      TC_Dec_3 := 1234567.890;
      if not Pack_3.Valid(Pack_3.To_Packed(TC_Dec_3, Packed_Unsigned),
                          Packed_Unsigned)
      then
         Report.Failed("Incorrect result from function Valid - 3");
      end if;

      TC_Dec_4 := -234.6789;
      if not Pack_4.Valid(Item   => Pack_4.To_Packed(TC_Dec_4, 
                                                     Packed_Signed),
                          Format => Packed_Signed)                        or
         Pack_4.Valid(Item   => Pack_4.To_Packed(TC_Dec_4, Packed_Signed),
                      Format => Packed_Unsigned)
      then
         Report.Failed("Incorrect result from function Valid - 4");
      end if;



      -- Check that the function Length with Packed_Format parameter returns
      -- the minimal length of a Packed_Decimal value sufficient to hold any
      -- value of type Num when represented according to parameter Format.

      if NOT (Pack_1.Length(Packed_Signed)   >= TC_Min_Length AND
              Pack_1.Length(Packed_Signed)   <= TC_Max_Length AND
              Pack_1.Length(Packed_Unsigned) >= TC_Min_Length AND
              Pack_1.Length(Packed_Unsigned) <= TC_Max_Length)
      then
         Report.Failed("Incorrect result from function Length - 1");
      end if;

      if NOT (Pack_2.Length(Packed_Signed)   >= TC_Min_Length AND
              Pack_2.Length(Packed_Signed)   <= TC_Max_Length AND
              Pack_2.Length(Packed_Unsigned) >= TC_Min_Length AND
              Pack_2.Length(Packed_Unsigned) <= TC_Max_Length)
      then
         Report.Failed("Incorrect result from function Length - 2");
      end if;

      if NOT (Pack_3.Length(Packed_Signed)   >= TC_Min_Length AND
              Pack_3.Length(Packed_Signed)   <= TC_Max_Length AND
              Pack_3.Length(Packed_Unsigned) >= TC_Min_Length AND
              Pack_3.Length(Packed_Unsigned) <= TC_Max_Length)
      then
         Report.Failed("Incorrect result from function Length - 3");
      end if;

      if NOT (Pack_4.Length(Packed_Signed)   >= TC_Min_Length AND
              Pack_4.Length(Packed_Signed)   <= TC_Max_Length AND
              Pack_4.Length(Packed_Unsigned) >= TC_Min_Length AND
              Pack_4.Length(Packed_Unsigned) <= TC_Max_Length)
      then
         Report.Failed("Incorrect result from function Length - 4");
      end if;



      -- Check that the function To_Decimal with Packed_Decimal and 
      -- Packed_Format parameters produces a decimal type value corresponding 
      -- to the Packed_Decimal parameter value Item, under the conditions of 
      -- the Packed_Format parameter Format.

      begin
         TC_Dec_1 := 1234.5;
         if Pack_1.To_Decimal(Item   => Pack_1.To_Packed(TC_Dec_1,
                                                         Packed_Unsigned),
                              Format => Packed_Unsigned) /= TC_Dec_1
         then
            Report.Failed("Incorrect result from function To_Decimal - 1");
         end if;
      exception
         when The_Error : others => 
            Report.Failed("The following exception was raised in " &
                          "subtest 1 of function To_Decimal: "     & 
                          Exception_Name(The_Error));
      end;

      begin
         TC_Dec_2 := -123456.50;
         if Pack_2.To_Decimal(Pack_2.To_Packed(TC_Dec_2, Packed_Signed),
                              Format => Packed_Signed) /= TC_Dec_2
         then
            Report.Failed("Incorrect result from function To_Decimal - 2");
         end if;
      exception
         when The_Error : others => 
            Report.Failed("The following exception was raised in " &
                          "subtest 2 of function To_Decimal: "     & 
                          Exception_Name(The_Error));
      end;

      begin
         TC_Dec_3 := 1234567.809;
         if Pack_3.To_Decimal(Pack_3.To_Packed(TC_Dec_3, Packed_Unsigned),
                              Packed_Unsigned) /= TC_Dec_3
         then
            Report.Failed("Incorrect result from function To_Decimal - 3");
         end if;
      exception
         when The_Error : others => 
            Report.Failed("The following exception was raised in " &
                          "subtest 3 of function To_Decimal: "     & 
                          Exception_Name(The_Error));
      end;

      begin
         TC_Dec_4 := -789.1234;
         if Pack_4.To_Decimal(Item   => Pack_4.To_Packed(TC_Dec_4,
                                                         Packed_Signed),
                              Format => Packed_Signed) /= TC_Dec_4
         then
            Report.Failed("Incorrect result from function To_Decimal - 4");
         end if;
      exception
         when The_Error : others => 
            Report.Failed("The following exception was raised in " &
                          "subtest 4 of function To_Decimal: "     & 
                          Exception_Name(The_Error));
      end;



      -- Check that the function To_Packed with Decimal (Num) and 
      -- Packed_Format parameters produces a Packed_Decimal result that 
      -- corresponds to the decimal parameter under conditions of the 
      -- Packed_Format parameter.

      if Pack_1.To_Packed(Item =>  123.4, Format => Packed_Unsigned) =
         Pack_1.To_Packed(Item => -123.4, Format => Packed_Signed)
      then
         Report.Failed("Incorrect result from function To_Packed - 1");
      end if; 

      if Pack_2.To_Packed( 123.45, Format => Packed_Unsigned) =
         Pack_2.To_Packed(-123.45, Format => Packed_Signed)
      then
         Report.Failed("Incorrect result from function To_Packed - 2");
      end if; 

      if Pack_3.To_Packed(Item =>  123.456, Format => Packed_Unsigned) =
         Pack_3.To_Packed(Item => -123.456, Format => Packed_Signed)
      then
         Report.Failed("Incorrect result from function To_Packed - 3");
      end if; 

      if (Pack_4.To_Packed( 123.4567, Packed_Unsigned)     =
          Pack_4.To_Packed(-123.4567, Packed_Signed))       or
         (Pack_4.To_Packed(12345678.9012, Packed_Unsigned) =
          Pack_4.To_Packed(12345678.9013, Packed_Unsigned)) or
         (Pack_4.To_Packed(12345678.9012, Packed_Unsigned) =
          Pack_4.To_Packed(22345678.9012, Packed_Unsigned)) 
      then
         Report.Failed("Incorrect result from function To_Packed - 4");
      end if; 


      -- Check that Conversion_Error is propagated by function To_Packed if 
      -- the value of the decimal parameter Item is negative and the 
      -- specified Packed_Format parameter is Packed_Unsigned.

      begin
         if Pack_1.To_Packed(Item => -12.3, Format => Packed_Unsigned) =
            Pack_1.To_Packed(Item =>  12.3, Format => Packed_Signed)
         then
            Report.Comment("Should never be printed");
         end if;
         Report.Failed("Conversion_Error not raised following call to " & 
                       "function To_Packed with a negative parameter "  &  
                       "Item and Packed_Format parameter Packed_Unsigned");
      exception
         when Conversion_Error   => null;  -- OK, expected exception.
         when The_Error : others =>
            Report.Failed(Exception_Name(The_Error) & " was incorrectly " &
                          "raised following call to function To_Packed "  & 
                          "with a negative parameter Item and "           &
                          "Packed_Format parameter Packed_Unsigned"); 
      end;

   exception
      when The_Error : others => 
         Report.Failed("The following exception was raised in the " &
                       "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB4006;
