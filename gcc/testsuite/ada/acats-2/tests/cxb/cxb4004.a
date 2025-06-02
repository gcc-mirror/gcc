-- CXB4004.A
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
--      Check that function Length, with Display_Format parameter, will 
--      return the minimal length of a Numeric value that will be required 
--      to hold the largest value of type Num represented as Format. 
--
--      Check that function To_Decimal will produce a decimal type Num 
--      result that corresponds to parameter Item as represented by 
--      parameter Format.
--
--      Check that function To_Decimal propagates Conversion_Error when 
--      the value represented by parameter Item is outside the range of 
--      the Decimal_Type Num used to instantiate the package 
--      Decimal_Conversions 
--
--      Check that function To_Display returns a Numeric type result that 
--      represents Item under the specific Display_Format.
--
--      Check that function To_Display propagates Conversion_Error when 
--      parameter Item is negative and the specified Display_Format 
--      parameter is Unsigned.
--
-- TEST DESCRIPTION:
--      This test checks the results from instantiated versions of three
--      functions within generic package Interfaces.COBOL.Decimal_Conversions.
--      This generic package is instantiated twice, with decimal types having
--      four and ten digits representation. 
--      The function Length is validated with the Unsigned, Leading_Separate,
--      and Trailing_Separate Display_Format specifiers.
--      The results of function To_Decimal are verified in cases where it
--      is given a variety of Numeric and Display_Format type parameters.
--      Function To_Decimal is also checked to propagate Conversion_Error
--      when the value represented by parameter Item is outside the range 
--      of the type used to instantiate the package.
--      The results of function To_Display are verified in cases where it
--      is given a variety of Num and Display_Format parameters.  It is also
--      checked to ensure that it propagates Conversion_Error if parameter
--      Num is negative and the Format parameter is Unsigned.
--      
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.COBOL.COBOL_Character:
--      ' ', '0'..'9', '+', '-', and '.'.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.COBOL.  If an implementation provides
--      package Interfaces.COBOL, this test must compile, execute, and 
--      report "PASSED".
--
--       
-- CHANGE HISTORY:
--      06 Feb 96   SAIC    Initial release for 2.1.
--      30 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Interfaces.COBOL;                                          -- N/A => ERROR
with Ada.Exceptions;

procedure CXB4004 is
begin

   Report.Test ("CXB4004", "Check that the functions Length, To_Decimal, " &
                           "and To_Display produce correct results");

   Test_Block:
   declare

      use Interfaces;
      use Ada.Exceptions;
      use type Interfaces.COBOL.Numeric;

      Number_Of_Unsigned_Items          : constant := 6;
      Number_Of_Leading_Separate_Items  : constant := 6;
      Number_Of_Trailing_Separate_Items : constant := 6;
      Number_Of_Decimal_Items           : constant := 9;

      type Decimal_Type_1     is delta 0.01 digits  4;
      type Decimal_Type_2     is delta 1.0 digits  10;
      type Numeric_Access     is access COBOL.Numeric;
      type Numeric_Items_Type is array(Integer range <>) of Numeric_Access;

      Correct_Result : Boolean        := False;
      TC_Num_1       : Decimal_Type_1 := 0.0;
      TC_Num_2       : Decimal_Type_2 := 0.0;

      package Package_1 is new COBOL.Decimal_Conversions(Decimal_Type_1);
      package Package_2 is new COBOL.Decimal_Conversions(Decimal_Type_2);


      Package_1_Numeric_Items :
        Numeric_Items_Type(1..Number_Of_Decimal_Items) :=
          (new COBOL.Numeric'("0"),
           new COBOL.Numeric'("591"),
           new COBOL.Numeric'("6342"),
           new COBOL.Numeric'("+0"),
           new COBOL.Numeric'("-1539"),
           new COBOL.Numeric'("+9199"),
           new COBOL.Numeric'("0-"),
           new COBOL.Numeric'("8934+"),
           new COBOL.Numeric'("9949-"));

      Package_2_Numeric_Items :
        Numeric_Items_Type(1..Number_Of_Decimal_Items) :=
          (new COBOL.Numeric'("3"),
           new COBOL.Numeric'("105"),
           new COBOL.Numeric'("1234567899"),
           new COBOL.Numeric'("+8"),
           new COBOL.Numeric'("-12345601"),
           new COBOL.Numeric'("+9123459999"),
           new COBOL.Numeric'("1-"),
           new COBOL.Numeric'("123456781+"),
           new COBOL.Numeric'("9499999999-"));


      Decimal_Type_1_Items : array (1..Number_Of_Decimal_Items) 
                               of Decimal_Type_1 :=
        (0.0,  5.91,  63.42, 0.0, -15.39,  91.99, 0.0,  89.34, -99.49);

      Decimal_Type_2_Items : array (1..Number_Of_Decimal_Items) 
                               of Decimal_Type_2 :=
        ( 3.0,        105.0,  1234567899.0, 
          8.0,  -12345601.0,  9123459999.0,
         -1.0,  123456781.0, -9499999999.0);

   begin

      -- Check that function Length with Display_Format parameter will 
      -- return the minimal length of a Numeric value (number of 
      -- COBOL_Characters) that will be required to hold the largest 
      -- value of type Num. 

      if Package_1.Length(COBOL.Unsigned) /=  4 or
         Package_2.Length(COBOL.Unsigned) /= 10
      then
         Report.Failed("Incorrect results from function Length when " &
                       "used with Display_Format parameter Unsigned");
      end if;

      if Package_1.Length(Format => COBOL.Leading_Separate) /=  5 or
         Package_2.Length(Format => COBOL.Leading_Separate) /= 11
      then
         Report.Failed("Incorrect results from function Length when " &
                       "used with Display_Format parameter "          &
                       "Leading_Separate");
      end if;

      if Package_1.Length(COBOL.Trailing_Separate) /=  5 or
         Package_2.Length(COBOL.Trailing_Separate) /= 11
      then
         Report.Failed("Incorrect results from function Length when " &
                       "used with Display_Format parameter "          &
                       "Trailing_Separate");
      end if;


      -- Check that function To_Decimal with Numeric and Display_Format 
      -- parameters will produce a decimal type Num result that corresponds 
      -- to parameter Item as represented by parameter Format.

      for i in 1..Number_Of_Decimal_Items loop
         case i is
            when 1..3 =>  -- Unsigned Display_Format parameter.

               if Package_1.To_Decimal(Package_1_Numeric_Items(i).all,
                                       Format => COBOL.Unsigned) /=
                  Decimal_Type_1_Items(i)
               then
                  Report.Failed
                    ("Incorrect result from function To_Decimal "    &
                     "from an instantiation of Decimal_Conversions " &
                     "using a four-digit Decimal type, with Format " &
                     "parameter Unsigned, subtest index: "           &
                     Integer'Image(i)); 
               end if;

               if Package_2.To_Decimal(Package_2_Numeric_Items(i).all,
                                       Format => COBOL.Unsigned) /=
                  Decimal_Type_2_Items(i)
               then
                  Report.Failed
                    ("Incorrect result from function To_Decimal "    &
                     "from an instantiation of Decimal_Conversions " &
                     "using a ten-digit Decimal type, with Format "  &
                     "parameter Unsigned, subtest index: "           &
                     Integer'Image(i)); 
               end if;

            when 4..6 =>  -- Leading_Separate Display_Format parameter.

               if Package_1.To_Decimal(Package_1_Numeric_Items(i).all,
                                       Format => COBOL.Leading_Separate) /=
                  Decimal_Type_1_Items(i)
               then
                  Report.Failed
                    ("Incorrect result from function To_Decimal "    &
                     "from an instantiation of Decimal_Conversions " &
                     "using a four-digit Decimal type, with Format " &
                     "parameter Leading_Separate, subtest index: "   &
                     Integer'Image(i)); 
               end if;

               if Package_2.To_Decimal(Package_2_Numeric_Items(i).all,
                                       Format => COBOL.Leading_Separate) /=
                  Decimal_Type_2_Items(i)
               then
                  Report.Failed
                    ("Incorrect result from function To_Decimal "    &
                     "from an instantiation of Decimal_Conversions " &
                     "using a ten-digit Decimal type, with Format "  &
                     "parameter Leading_Separate, subtest index: "   &
                     Integer'Image(i)); 
               end if;

            when 7..9 =>  -- Trailing_Separate Display_Format parameter.

               if Package_1.To_Decimal(Package_1_Numeric_Items(i).all,
                                       COBOL.Trailing_Separate) /=
                  Decimal_Type_1_Items(i)
               then
                  Report.Failed
                    ("Incorrect result from function To_Decimal "    &
                     "from an instantiation of Decimal_Conversions " &
                     "using a four-digit Decimal type, with Format " &
                     "parameter Trailing_Separate, subtest index: "  &
                     Integer'Image(i)); 
               end if;

               if Package_2.To_Decimal(Package_2_Numeric_Items(i).all,
                                       COBOL.Trailing_Separate) /=
                  Decimal_Type_2_Items(i)
               then
                  Report.Failed
                    ("Incorrect result from function To_Decimal "    &
                     "from an instantiation of Decimal_Conversions " &
                     "using a ten-digit Decimal type, with Format "  &
                     "parameter Trailing_Separate, subtest index: "  &
                     Integer'Image(i)); 
               end if;

         end case;
      end loop;


      -- Check that function To_Decimal propagates Conversion_Error when 
      -- the value represented by Numeric type parameter Item is outside 
      -- the range of the Decimal_Type Num used to instantiate the package 
      -- Decimal_Conversions.

      declare
         TC_Numeric_1 : Decimal_Type_1 := Decimal_Type_1_Items(1);
      begin
         -- The COBOL.Numeric type used as parameter Item represents a
         -- Decimal value that is outside the range of the Decimal type 
         -- used to instantiate Package_1.
         TC_Numeric_1 := 
           Package_1.To_Decimal(Item   => Package_2_Numeric_Items(8).all,
                                Format => COBOL.Trailing_Separate);
         Report.Failed("Conversion_Error not raised by To_Decimal "     &
                       "when the value represented by parameter "       &
                       "Item is outside the range of the Decimal_Type " &
                       "used to instantiate the package "               &
                       "Decimal_Conversions");
         if TC_Numeric_1 = Decimal_Type_1_Items(1) then
            Report.Comment("To Guard Against Dead Assignment Elimination " &
                           "-- Should never be printed");
         end if;
      exception
         when COBOL.Conversion_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Incorrect exception raised by To_Decimal "      &
                          "when the value represented by parameter "       &
                          "Item is outside the range of the Decimal_Type " &
                          "used to instantiate the package "               &
                          "Decimal_Conversions");
      end;


      -- Check that function To_Display with decimal type Num and 
      -- Display_Format parameters returns a Numeric type result that 
      -- represents Item under the specific Display_Format.

      -- Unsigned Display_Format parameter.
      TC_Num_1       := 13.04;
      Correct_Result := (Package_1.To_Display(TC_Num_1, COBOL.Unsigned) =
                         "1304") AND
                        (Package_1.To_Display(TC_Num_1, COBOL.Unsigned) /= 
                         "13.04");
      if not Correct_Result then
         Report.Failed("Incorrect result from function To_Display with " &
                       "Unsigned Display_Format parameter - 1");
      end if;

      TC_Num_2       := 1234567890.0;
      Correct_Result := Package_2.To_Display(TC_Num_2, 
                                             COBOL.Unsigned) = "1234567890";
      if not Correct_Result then
         Report.Failed("Incorrect result from function To_Display with " &
                       "Unsigned Display_Format parameter - 2");
      end if;

      -- Leading_Separate Display_Format parameter.
      TC_Num_1       := -34.29;
      Correct_Result := (Package_1.To_Display(TC_Num_1, 
                                              COBOL.Leading_Separate) = 
                         "-3429") AND
                        (Package_1.To_Display(TC_Num_1,
                                              COBOL.Leading_Separate) /=
                         "-34.29");
      if not Correct_Result then
         Report.Failed("Incorrect result from function To_Display with " &
                       "Leading_Separate Display_Format parameter - 1");
      end if;

      TC_Num_1       := 19.01;
      Correct_Result := Package_1.To_Display(TC_Num_1, 
                                             COBOL.Leading_Separate) = 
                        "+1901";
      if not Correct_Result then
         Report.Failed("Incorrect result from function To_Display with " &
                       "Leading_Separate Display_Format parameter - 2");
      end if;

      TC_Num_2       := 1234567890.0;
      Correct_Result := Package_2.To_Display(TC_Num_2, 
                                             COBOL.Leading_Separate) = 
                        "+1234567890";
      if not Correct_Result then
         Report.Failed("Incorrect result from function To_Display with " &
                       "Leading_Separate Display_Format parameter - 3");
      end if;

      TC_Num_2       := -1234567890.0;
      Correct_Result := Package_2.To_Display(TC_Num_2, 
                                             COBOL.Leading_Separate) = 
                        "-1234567890";
      if not Correct_Result then
         Report.Failed("Incorrect result from function To_Display with " &
                       "Leading_Separate Display_Format parameter - 4");
      end if;

      -- Trailing_Separate Display_Format parameter.
      TC_Num_1       := -99.91;
      Correct_Result := (Package_1.To_Display(TC_Num_1, 
                                             COBOL.Trailing_Separate) = 
                         "9991-") AND
                        (Package_1.To_Display(TC_Num_1,
                                              COBOL.Trailing_Separate) /=
                         "99.91-");
      if not Correct_Result then
         Report.Failed("Incorrect result from function To_Display with " &
                       "Trailing_Separate Display_Format parameter - 1");
      end if;

      TC_Num_1       := 51.99;
      Correct_Result := Package_1.To_Display(TC_Num_1, 
                                             COBOL.Trailing_Separate) = 
                        "5199+";
      if not Correct_Result then
         Report.Failed("Incorrect result from function To_Display with " &
                       "Trailing_Separate Display_Format parameter - 2");
      end if;

      TC_Num_2       := 1234567890.0;
      Correct_Result := Package_2.To_Display(TC_Num_2, 
                                             COBOL.Trailing_Separate) = 
                        "1234567890+";
      if not Correct_Result then
         Report.Failed("Incorrect result from function To_Display with " &
                       "Trailing_Separate Display_Format parameter - 3");
      end if;

      TC_Num_2       := -1234567890.0;
      Correct_Result := Package_2.To_Display(TC_Num_2, 
                                             COBOL.Trailing_Separate) = 
                        "1234567890-";
      if not Correct_Result then
         Report.Failed("Incorrect result from function To_Display with " &
                       "Trailing_Separate Display_Format parameter - 4");
      end if;


      -- Check that function To_Display propagates Conversion_Error when
      -- parameter Item is negative and the specified Display_Format 
      -- parameter is Unsigned.

      begin
         if Package_2.To_Display(Item   => Decimal_Type_2_Items(9),
                                 Format => COBOL.Unsigned) = 
            Package_2_Numeric_Items(2).all 
         then
            Report.Comment("To Guard Against Dead Assignment Elimination " &
                           "-- Should never be printed");
         end if;
         Report.Failed("Conversion_Error not raised by To_Display " &
                       "when the value represented by parameter "   &
                       "Item is negative and the Display_Format "   &
                       "parameter is Unsigned");
      exception
         when COBOL.Conversion_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Incorrect exception raised by To_Display " &
                          "when the value represented by parameter "  &
                          "Item is negative and the Display_Format "  &
                          "parameter is Unsigned");
      end;


   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB4004;
