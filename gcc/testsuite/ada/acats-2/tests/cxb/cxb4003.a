-- CXB4003.A
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
--      Check that function Valid, with the Display_Format parameter 
--      set to Unsigned, will return True if Numeric parameter Item 
--      comprises one or more decimal digit characters; check that it 
--      returns False if the parameter Item is otherwise comprised.
--
--      Check that function Valid, with Display_Format parameter set to 
--      Leading_Separate, will return True if Numeric parameter Item 
--      comprises a single occurrence of a Plus_Sign or Minus_Sign 
--      character, and then by one or more decimal digit characters; 
--      check that it returns False if the parameter Item is otherwise 
--      comprised.
--
--      Check that function Valid, with Display_Format parameter set to 
--      Trailing_Separate, will return True if Numeric parameter Item 
--      comprises one or more decimal digit characters, and then by a 
--      single occurrence of the Plus_Sign or Minus_Sign character; 
--      check that it returns False if the parameter Item is otherwise
--      comprised.
--
-- TEST DESCRIPTION:
--      This test checks that a version of function Valid, from an instance
--      of the generic package Decimal_Conversions, will produce correct
--      results based on the particular Numeric and Display_Format
--      parameters provided.  Arrays of both valid and invalid Numeric
--      data items have been created to correspond to a particular
--      value of Display_Format.  The result of the function is compared
--      against the expected result for each appropriate combination of
--      Numeric and Display_Format parameter.
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.COBOL.COBOL_Character:
--      ' ', 'A'..'Z', '+', '-', '.', '$'.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.COBOL.  If an implementation provides
--      package Interfaces.COBOL, this test must compile, execute, and 
--      report "PASSED".
--
--      
--       
-- CHANGE HISTORY:
--      18 Jan 96   SAIC    Initial version for 2.1. 
--      30 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Ada.Exceptions;
with Interfaces.COBOL;                                          -- N/A => ERROR

procedure CXB4003 is
begin

   Report.Test ("CXB4003", "Check that function Valid, with various "     &
                           "Display_Format parameters, produces correct " &
                           "results");

   Test_Block:
   declare

      use Interfaces;
      use Ada.Exceptions;

      type A_Numeric_Type     is delta 0.01 digits 16;
      type Numeric_Access     is access COBOL.Numeric;
      type Numeric_Items_Type is array(Integer range <>) of Numeric_Access;

      package Display_Format is 
        new COBOL.Decimal_Conversions(Num => A_Numeric_Type);


      Number_Of_Valid_Unsigned_Items            : constant :=  5;
      Number_Of_Invalid_Unsigned_Items          : constant := 21;
      Number_Of_Valid_Leading_Separate_Items    : constant :=  5;
      Number_Of_Invalid_Leading_Separate_Items  : constant := 23;
      Number_Of_Valid_Trailing_Separate_Items   : constant :=  5;
      Number_Of_Invalid_Trailing_Separate_Items : constant := 22;

      Valid_Unsigned_Items :
        Numeric_Items_Type(1..Number_Of_Valid_Unsigned_Items) :=
          (new COBOL.Numeric'("0"),
           new COBOL.Numeric'("1"),
           new COBOL.Numeric'("0000000001"),
           new COBOL.Numeric'("1234567890123456"),
           new COBOL.Numeric'("0000"));

      Invalid_Unsigned_Items :        
        Numeric_Items_Type(1..Number_Of_Invalid_Unsigned_Items) :=
          (new COBOL.Numeric'(" 12345"),
           new COBOL.Numeric'("    12345"),
           new COBOL.Numeric'("1234567890 "),
           new COBOL.Numeric'("1234567890   "),
           new COBOL.Numeric'("1.01"),
           new COBOL.Numeric'(".0000000001"),
           new COBOL.Numeric'("12345 6"),
           new COBOL.Numeric'("MCXVIII"),
           new COBOL.Numeric'("15F"),
           new COBOL.Numeric'("+12345"),
           new COBOL.Numeric'("$12.30"),
           new COBOL.Numeric'("1234-"),
           new COBOL.Numeric'("12--"),
           new COBOL.Numeric'("+12-"),
           new COBOL.Numeric'("++99--"),
           new COBOL.Numeric'("-1.01"),
           new COBOL.Numeric'("(1.01)"),
           new COBOL.Numeric'("123,456"),
           new COBOL.Numeric'("101."),
           new COBOL.Numeric'(""),
           new COBOL.Numeric'("1.0000"));

      Valid_Leading_Separate_Items : 
        Numeric_Items_Type(1..Number_Of_Valid_Leading_Separate_Items) :=
          (new COBOL.Numeric'("+1000"),
           new COBOL.Numeric'("-1"),
           new COBOL.Numeric'("-0000000001"),
           new COBOL.Numeric'("+1234567890123456"),
           new COBOL.Numeric'("-0000"));

      Invalid_Leading_Separate_Items :
        Numeric_Items_Type(1..Number_Of_Invalid_Leading_Separate_Items) :=
          (new COBOL.Numeric'("123456"),
           new COBOL.Numeric'(" +12345"),
           new COBOL.Numeric'("    +12345"),
           new COBOL.Numeric'("- 0000000001"),
           new COBOL.Numeric'("1234567890- "),
           new COBOL.Numeric'("1234567890+   "),
           new COBOL.Numeric'("123-456"),
           new COBOL.Numeric'("+15F"),
           new COBOL.Numeric'("++123"),
           new COBOL.Numeric'("12--"),
           new COBOL.Numeric'("+12-"),
           new COBOL.Numeric'("+/-12"),
           new COBOL.Numeric'("++99--"),
           new COBOL.Numeric'("1.01"),
           new COBOL.Numeric'("(1.01)"),
           new COBOL.Numeric'("+123,456"),
           new COBOL.Numeric'("+15FF"),
           new COBOL.Numeric'("- 123"),
           new COBOL.Numeric'("+$123"),
           new COBOL.Numeric'(""),
           new COBOL.Numeric'("-"),
           new COBOL.Numeric'("-1.01"),
           new COBOL.Numeric'("1.0000+"));

      Valid_Trailing_Separate_Items : 
        Numeric_Items_Type(1..Number_Of_Valid_Trailing_Separate_Items) :=
          (new COBOL.Numeric'("1001-"),
           new COBOL.Numeric'("1+"),
           new COBOL.Numeric'("0000000001+"),
           new COBOL.Numeric'("1234567890123456-"),
           new COBOL.Numeric'("0000-"));

      Invalid_Trailing_Separate_Items :
        Numeric_Items_Type(1..Number_Of_Invalid_Trailing_Separate_Items) :=
          (new COBOL.Numeric'("123456"),
           new COBOL.Numeric'("+12345"),
           new COBOL.Numeric'("12345 "),
           new COBOL.Numeric'("123- "),
           new COBOL.Numeric'("123-   "),
           new COBOL.Numeric'("12345 +"),
           new COBOL.Numeric'("12345+   "),
           new COBOL.Numeric'("-0000000001"),
           new COBOL.Numeric'("123-456"),
           new COBOL.Numeric'("12--"),
           new COBOL.Numeric'("+12-"),
           new COBOL.Numeric'("99+-"),
           new COBOL.Numeric'("12+/-"),
           new COBOL.Numeric'("12.01-"),
           new COBOL.Numeric'("$12.01+"),
           new COBOL.Numeric'("(1.01)"),
           new COBOL.Numeric'("DM12-"),
           new COBOL.Numeric'("123,456+"),
           new COBOL.Numeric'(""),
           new COBOL.Numeric'("-"),
           new COBOL.Numeric'("1.01-"),
           new COBOL.Numeric'("+1.0000"));

   begin

      -- Check that function Valid, with the Display_Format parameter 
      -- set to Unsigned, will return True if Numeric parameter Item 
      -- comprises one or more decimal digit characters; check that it 
      -- returns False if the parameter Item is otherwise comprised.

      for i in 1..Number_of_Valid_Unsigned_Items loop
         -- Fail if the Item parameter is _NOT_ considered Valid.
         if not Display_Format.Valid(Item   => Valid_Unsigned_Items(i).all, 
                                     Format => COBOL.Unsigned)
         then
            Report.Failed("Incorrect result from function Valid, with "  &
                          "Format parameter set to Unsigned, for valid " &
                          "format item number " & Integer'Image(i));
         end if;
      end loop;


      for i in 1..Number_of_Invalid_Unsigned_Items loop
         -- Fail if the Item parameter _IS_ considered Valid.
         if Display_Format.Valid(Item   => Invalid_Unsigned_Items(i).all, 
                                 Format => COBOL.Unsigned)
         then
            Report.Failed("Incorrect result from function Valid, with "    &
                          "Format parameter set to Unsigned, for invalid " &
                          "format item number " & Integer'Image(i));
         end if;
      end loop;



      -- Check that function Valid, with Display_Format parameter set to 
      -- Leading_Separate, will return True if Numeric parameter Item 
      -- comprises a single occurrence of a Plus_Sign or Minus_Sign 
      -- character, and then by one or more decimal digit characters; 
      -- check that it returns False if the parameter Item is otherwise 
      -- comprised.

      for i in 1..Number_of_Valid_Leading_Separate_Items loop
         -- Fail if the Item parameter is _NOT_ considered Valid.
         if not Display_Format.Valid(Valid_Leading_Separate_Items(i).all, 
                                     Format => COBOL.Leading_Separate)
         then
            Report.Failed("Incorrect result from function Valid, with " &
                          "Format parameter set to Leading_Separate, "  &
                          "for valid format item number " & Integer'Image(i));
         end if;
      end loop;


      for i in 1..Number_of_Invalid_Leading_Separate_Items loop
         -- Fail if the Item parameter _IS_ considered Valid.
         if Display_Format.Valid(Invalid_Leading_Separate_Items(i).all, 
                                 Format => COBOL.Leading_Separate)
         then
            Report.Failed("Incorrect result from function Valid, with " &
                          "Format parameter set to Leading_Separate, "  &
                          "for invalid format item number "             &
                          Integer'Image(i));
         end if;
      end loop;



      -- Check that function Valid, with Display_Format parameter set to 
      -- Trailing_Separate, will return True if Numeric parameter Item 
      -- comprises one or more decimal digit characters, and then by a 
      -- single occurrence of the Plus_Sign or Minus_Sign character; 
      -- check that it returns False if the parameter Item is otherwise
      -- comprised.

      for i in 1..Number_of_Valid_Trailing_Separate_Items loop
         -- Fail if the Item parameter is _NOT_ considered Valid.
         if not Display_Format.Valid(Valid_Trailing_Separate_Items(i).all, 
                                     COBOL.Trailing_Separate)
         then
            Report.Failed("Incorrect result from function Valid, with " &
                          "Format parameter set to Trailing_Separate, " &
                          "for valid format item number " & Integer'Image(i));
         end if;
      end loop;


      for i in 1..Number_of_Invalid_Trailing_Separate_Items loop
         -- Fail if the Item parameter _IS_ considered Valid.
         if Display_Format.Valid(Invalid_Trailing_Separate_Items(i).all, 
                                 COBOL.Trailing_Separate)
         then
            Report.Failed("Incorrect result from function Valid, with " &
                          "Format parameter set to Trailing_Separate, " &
                          "for invalid format item number "             &
                          Integer'Image(i));
         end if;
      end loop;


   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB4003;
