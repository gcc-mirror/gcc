-- CXB4007.A
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
--      Check that the function Valid with Byte_Array and Binary_Format 
--      parameters returns True if the Byte_Array parameter corresponds 
--      to any value inside the range of type Num.
--      Check that function Valid returns False if the Byte_Array parameter
--      corresponds to a value outside the range of Num. 
--
--      Check that function Length with Binary_Format parameter will return
--      the minimum length of a Byte_Array value required to hold any value
--      of decimal type Num.
--
--      Check that function To_Decimal with Byte_Array and Binary_Format
--      parameters will return a decimal type value that corresponds to
--      parameter Item (of type Byte_Array) under the specified Format.
--
--      Check that Conversion_Error is propagated by function To_Decimal if
--      the Byte_Array parameter Item represents a decimal value outside the
--      range of decimal type Num.
--
--      Check that function To_Binary will produce a Byte_Array result that
--      corresponds to the decimal type parameter Item, under the specified
--      Binary_Format.
--      
-- TEST DESCRIPTION:
--      This test uses several instantiations of generic package 
--      Decimal_Conversions to provide appropriate test material.
--      This test uses the function To_Binary to create all Byte_Array 
--      parameter values used in calls to functions Valid and To_Decimal.
--      The function Valid is tested with parameters to provide both
--      valid and invalid expected results.  This test also checks that
--      Function To_Decimal produces expected results in cases where each
--      of the three predefined Binary_Format constants are used in the
--      function calls. In addition, the prescribed propagation of 
--      Conversion_Error by function To_Decimal is verified.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.COBOL.  If an implementation provides
--      package Interfaces.COBOL, this test must compile, execute, and 
--      report "PASSED".
--
--       
-- CHANGE HISTORY:
--      14 Feb 96   SAIC    Initial release for 2.1.
--      30 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--      05 JAN 98   EDS     Remove incorrect subtest.
--!

with Report;
with Ada.Exceptions;
with Interfaces.COBOL;                                          -- N/A => ERROR

procedure CXB4007 is
begin

   Report.Test ("CXB4007", "Check that functions Valid, Length, To_Decimal " &
                           "and To_Binary specific to Byte_Array and "       &
                           "Binary_Format parameters produce correct results");

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
      TC_Valid      : Boolean        := False;

   begin

      -- Check that the function Valid with Byte_Array and Binary_Format 
      -- parameters returns True if the Byte_Array parameter corresponds to 
      -- any value inside the range of type Num.

      if not Pack_1.Valid(Item   => Pack_1.To_Binary(TC_Dec_1,
                                                     High_Order_First),
                          Format => High_Order_First)                   or
         not Pack_1.Valid(Pack_1.To_Binary(0.0, Low_Order_First), 
                          Format => Low_Order_First)                    
      then
         Report.Failed("Incorrect result from function Valid, using " &
                       "parameters that should return a positive result - 1");
      end if;

      TC_Valid := (Pack_2.Valid(Pack_2.To_Binary(TC_Dec_2, High_Order_First),
                                Format => High_Order_First)             and
                   Pack_2.Valid(Pack_2.To_Binary(0.01, Low_Order_First), 
                                Format => Low_Order_First));              
      if not TC_Valid then
         Report.Failed("Incorrect result from function Valid, using " &
                       "parameters that should return a positive result - 2");
      end if;

      if not Pack_3.Valid(Item   => Pack_3.To_Binary(TC_Dec_3,
                                                     Low_Order_First),
                          Format => Low_Order_First)                  or
         not Pack_3.Valid(Pack_3.To_Binary(0.001, High_Order_First), 
                          Format => High_Order_First)                 or
         not Pack_3.Valid(Pack_3.To_Binary(123.456, Native_Binary),
                          Native_Binary)
      then
         Report.Failed("Incorrect result from function Valid, using " &
                       "parameters that should return a positive result - 3");
      end if;


      -- Check that function Valid returns False if the Byte_Array parameter
      -- corresponds to a value outside the range of Num. 
      -- Note: use a Byte_Array value Item created by an instantiation of 
      --       To_Binary with a larger Num type as the generic formal.

      if Pack_1.Valid(Item   => Pack_2.To_Binary(TC_Dec_2, Low_Order_First),
                      Format => Low_Order_First)                or
         Pack_2.Valid(Pack_3.To_Binary(TC_Dec_3, High_Order_First), 
                      Format => High_Order_First)               or
         Pack_3.Valid(Pack_4.To_Binary(TC_Dec_4, Native_Binary),
                      Native_Binary)
      then
         Report.Failed("Incorrect result from function Valid, using " &
                       "parameters that should return a negative result");
      end if;


      -- Check that function Length with Binary_Format parameter will return
      -- the minimum length of a Byte_Array value required to hold any value
      -- of decimal type Num.

      if not (Pack_1.Length(Native_Binary)    >= TC_Min_Length and
              Pack_1.Length(Low_Order_First)  <= TC_Max_Length and
              Pack_2.Length(High_Order_First) >= TC_Min_Length and
              Pack_2.Length(Native_Binary)    <= TC_Max_Length and
              Pack_3.Length(Low_Order_First)  >= TC_Min_Length and
              Pack_3.Length(High_Order_First) <= TC_Max_Length and
              Pack_4.Length(Native_Binary)    >= TC_Min_Length and
              Pack_4.Length(Low_Order_First)  <= TC_Max_Length)
      then
         Report.Failed("Incorrect result from function Length");
      end if;



      -- Check that function To_Decimal with Byte_Array and Binary_Format
      -- parameters will return a decimal type value that corresponds to
      -- parameter Item (of type Byte_Array) under the specified Format.

      if Pack_1.To_Decimal(Item   => Pack_1.To_Binary(Item   => TC_Dec_1, 
                                                      Format => Native_Binary),
                           Format => Native_Binary) /=
         TC_Dec_1
      then
         Report.Failed("Incorrect result from function To_Decimal - 1");
      end if;

      if Pack_3.To_Decimal(Pack_3.To_Binary(TC_Dec_3, High_Order_First),
                           Format => High_Order_First) /=
         TC_Dec_3
      then
         Report.Failed("Incorrect result from function To_Decimal - 2");
      end if;

      if Pack_4.To_Decimal(Pack_4.To_Binary(TC_Dec_4, Low_Order_First),
                           Low_Order_First) /=
         TC_Dec_4
      then
         Report.Failed("Incorrect result from function To_Decimal - 3");
      end if;



      -- Check that Conversion_Error is propagated by function To_Decimal
      -- if the Byte_Array parameter Item represents a decimal value outside
      -- the range of decimal type Num.
      -- Note: use a Byte_Array value Item created by an instantiation of 
      --       To_Binary with a larger Num type as the generic formal.

      begin
         TC_Dec_4 := 99999.9001;
         TC_Dec_1 := Pack_1.To_Decimal(Pack_4.To_Binary(TC_Dec_4,
                                                        Native_Binary),
                                       Format => Native_Binary); 
         if TC_Dec_1 = 99999.9 then
            Report.Comment("Minimize dead assignment optimization -- " &
                           "Should never be printed");
         end if;
         Report.Failed("Conversion_Error not raised following call to "   &
                       "function To_Decimal if the Byte_Array parameter " &
                       "Item represents a decimal value outside the "     &
                       "range of decimal type Num");
      exception
         when Conversion_Error   => null;  -- OK, expected exception.
         when The_Error : others =>
            Report.Failed(Exception_Name(The_Error) & " was incorrectly " &
                          "raised following call to function To_Decimal " &
                          "if the Byte_Array parameter Item represents "  &
                          "a decimal value outside the range of decimal " &
                          "type Num");
      end;



      -- Check that function To_Binary will produce a Byte_Array result that
      -- corresponds to the decimal type parameter Item, under the specified
      -- Binary_Format.

      -- Different ordering.
      TC_Dec_1 := 12345.6;
      if Pack_1.To_Binary(TC_Dec_1, Low_Order_First)  =
         Pack_1.To_Binary(TC_Dec_1, High_Order_First)
      then
         Report.Failed("Incorrect result from function To_Binary - 1");
      end if;

      -- Variable vs. literal.
      TC_Dec_2 := 12345.00;
      if Pack_2.To_Binary(TC_Dec_2, Native_Binary) /=
         Pack_2.To_Binary(12345.00, Native_Binary)
      then
         Report.Failed("Incorrect result from function To_Binary - 2");
      end if;

   exception
      when The_Error : others => 
         Report.Failed("The following exception was raised in the " &
                       "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB4007;
