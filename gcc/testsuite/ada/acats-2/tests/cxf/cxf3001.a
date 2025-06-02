-- CXF3001.A 
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
--      result strings.  
--
--      Each picture string is checked for validity, and an invalid picture 
--      string will cause immediate test failure on its first pass through
--      the evaluation loop.  Inside the evaluation loop, each decimal data 
--      item is combined with each of the picture strings as parameters to a
--      call to Image, and the result of each call is compared to an 
--      expected edited output result string.
--      
--       
-- CHANGE HISTORY:
--      24 Feb 95   SAIC    Initial prerelease version.
--      23 Jun 95   SAIC    Corrected call to functions Valid and To_Picture.
--      22 Aug 95   SAIC    Test name changed to CXF3001 (from CXF3301) to
--                          conform to naming conventions.
--      24 Feb 97   CTA.PWB Corrected picture strings and expected results.
--!

with Ada.Text_IO.Editing;
with Report;

procedure CXF3001 is
begin

   Report.Test ("CXF3001", "Check that the string value returned by " &
                           "Function Image is correct");

   Test_Block:
   declare

      use Ada.Text_IO;

      Number_Of_Decimal_Items    : constant := 5;
      Number_Of_Picture_Strings  : constant := 4;
      Number_Of_Expected_Results : constant := Number_Of_Decimal_Items *
                                               Number_Of_Picture_Strings;

      type String_Pointer_Type is access String;

      -- Define a decimal data type, and instantiate the Decimal_Output 
      -- generic package for the data type.

      type Decimal_Data_Type is delta 0.01 digits 16;
      package Ed_Out is new Editing.Decimal_Output (Decimal_Data_Type);

      -- Define types for the arrays of data that will hold the decimal data 
      -- values, picture strings, and expected edited output results.

      type Decimal_Data_Array_Type is 
        array (Integer range <>) of Decimal_Data_Type;

      type Picture_String_Array_Type is 
        array (Integer range <>) of String_Pointer_Type;

      type Edited_Output_Results_Array_Type is 
        array (Integer range <>) of String_Pointer_Type;

      -- Define the data arrays for this test.

      Decimal_Data : 
        Decimal_Data_Array_Type(1..Number_Of_Decimal_Items) :=
          ( 1 =>  5678.90,
            2 => -6789.01,
            3 =>     0.00,
            4 =>     0.20,
            5 =>     3.45
          );

      Picture_Strings : 
        Picture_String_Array_Type(1..Number_Of_Picture_Strings) :=
          ( 1 => new String'("-$$_$$9.99"),
            2 => new String'("-$$_$$$.$$"),
            3 => new String'("-ZZZZ.ZZ"),
            4 => new String'("-$$$_999.99")
          );

      Edited_Output :
        Edited_Output_Results_Array_Type(1..Number_Of_Expected_Results) := 
          ( 1 => new String'(" $5,678.90"),    
            2 => new String'(" $5,678.90"),
            3 => new String'(" 5678.90"),     
            4 => new String'("  $5,678.90"),  

            5 => new String'("-$6,789.01"),
            6 => new String'("-$6,789.01"),
            7 => new String'("-6789.01"),
            8 => new String'("- $6,789.01"),

            9 => new String'("     $0.00"),
           10 => new String'("          "),    
           11 => new String'("        "),     
           12 => new String'("   $ 000.00"),  

           13 => new String'("     $0.20"),
           14 => new String'("      $.20"),    
           15 => new String'("     .20"),     
           16 => new String'("   $ 000.20"),

           17 => new String'("     $3.45"),
           18 => new String'("     $3.45"),
           19 => new String'("    3.45"),
           20 => new String'("   $ 003.45")
          );

      TC_Picture    : Editing.Picture;
      TC_Loop_Count : Natural := 0;

   begin

      -- Compare string result of Image with expected edited output string.  

      Evaluate_Edited_Output:
      for i in 1..Number_Of_Decimal_Items loop
         for j in 1..Number_Of_Picture_Strings loop

            TC_Loop_Count := TC_Loop_Count + 1;

            -- Check on the validity of the picture strings prior to
            -- processing.

            if Editing.Valid(Picture_Strings(j).all) then

               -- Create the picture object from the picture string.
               TC_Picture := Editing.To_Picture(Picture_Strings(j).all);

               -- Compare actual edited output result of Function Image with
               -- the expected result.

               if Ed_Out.Image(Decimal_Data(i), TC_Picture) /=
                  Edited_Output(TC_Loop_Count).all
               then
                  Report.Failed("Incorrect result from Function Image, " &
                                "when used with decimal data item # "    &
                                Integer'Image(i)                         &
                                " and picture string # "                  &
                                Integer'Image(j));
               end if;

            else
               Report.Failed("Picture String # " & Integer'Image(j) &
                             "reported as being invalid");
               -- Immediate test failure if a string is invalid.
               exit Evaluate_Edited_Output;
            end if;

         end loop;
      end loop Evaluate_Edited_Output;

   exception
      when Editing.Picture_Error =>
         Report.Failed ("Picture_Error raised in Test_Block");
      when Layout_Error          =>
         Report.Failed ("Layout_Error raised in Test_Block");
      when others                => 
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXF3001;
