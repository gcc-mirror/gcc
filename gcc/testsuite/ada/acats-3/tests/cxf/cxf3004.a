-- CXF3004.A
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
--      Check that statically identifiable picture strings can be used
--      in conjunction with function Image to produce output strings 
--      appropriate to foreign currency representations.
--
--      Check that statically identifiable picture strings will cause
--      function Image to raise Layout_Error under the appropriate 
--      conditions.
--      
-- TEST DESCRIPTION:
--      This test defines several picture strings that are statically
--      identifiable, (i.e.,  Pic : Picture := To_Picture("..."); ).
--      These picture strings are used in conjunction with decimal data
--      as parameters in calls to function Image.  
--
--       
-- CHANGE HISTORY:
--      11 Apr 96   SAIC    Initial release for 2.1.
--
--!

with Report;
with Ada.Text_IO.Editing;
with Ada.Exceptions;

procedure CXF3004 is
begin

   Report.Test ("CXF3004", "Check that statically identifiable "        &
                           "picture strings will cause function Image " &
                           "to raise Layout_Error under appropriate "   &
                           "conditions");

   Test_Block:
   declare

      use Ada.Exceptions;
      use Ada.Text_IO.Editing;

      FF_Currency  : constant String    := "FF";
      DM_Currency  : constant String    := "DM";
      FF_Separator : constant Character := '.';
      DM_Separator : constant Character := ',';
      FF_Radix     : constant Character := ',';
      DM_Radix     : constant Character := '.';
      Blank_Fill   : constant Character := ' ';
      Star_Fill    : constant Character := '*';


      -- Define a decimal data type, and instantiate the Decimal_Output 
      -- generic package for the data type.

      type Decimal_Data_Type is delta 0.01 digits 16;

      package Image_IO is 
        new Decimal_Output(Num                => Decimal_Data_Type,
                           Default_Currency   => "$",
                           Default_Fill       => Star_Fill,
                           Default_Separator  => Default_Separator,
                           Default_Radix_Mark => DM_Radix);



      -- The following decimal data items are used with picture strings 
      -- in evaluating use of foreign currency symbols.

      Dec_Data_1 : Decimal_Data_Type := 123456.78;
      Dec_Data_2 : Decimal_Data_Type :=     32.10;
      Dec_Data_3 : Decimal_Data_Type :=  -1234.57;
      Dec_Data_4 : Decimal_Data_Type := 123456.78;
      Dec_Data_5 : Decimal_Data_Type :=     12.34;
      Dec_Data_6 : Decimal_Data_Type :=     12.34;
      Dec_Data_7 : Decimal_Data_Type :=  12345.67;


      -- Statically identifiable picture strings.
      -- These strings are used in conjunction with non-default values 
      -- for Currency string, Radix mark, and Separator in calls to
      -- function Image.

      Picture_1 : Picture := To_Picture("-###**_***_**9.99");    -- FF
      Picture_2 : Picture := To_Picture("###z_ZZ9.99");          -- FF
      Picture_3 : Picture := To_Picture("<<<<_<<<.<<###>");      -- DM
      Picture_4 : Picture := To_Picture("-$_$$$_$$$_$$9.99");    -- DM
      Picture_5 : Picture := To_Picture("$Zz9.99");              -- DM
      Picture_6 : Picture := To_Picture("$$$9.99");              -- DM
      Picture_7 : Picture := To_Picture("###_###_##9.99");       -- CHF


      -- The following ten edited output strings correspond to the ten
      -- foreign currency picture strings.

      Output_1 : constant String := "  FF***123.456,78";
      Output_2 : constant String := " FF   32,10";
      Output_3 : constant String := "  (1,234.57DM )";
      Output_4 : constant String := "      DM123,456.78"; 
      Output_5 : constant String := "DM 12.34";
      Output_6 : constant String := " DM12.34";
      Output_7 : constant String := "  CHF12,345.67";


   begin

      -- Check the results of function Image, using the picture strings
      -- constructed above, in creating foreign currency edited output
      -- strings.

      if Image_IO.Image(Item       => Dec_Data_1,
                        Pic        => Picture_1,
                        Currency   => FF_Currency,
                        Fill       => Star_Fill,
                        Separator  => FF_Separator,
                        Radix_Mark => FF_Radix) /= Output_1
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_1");
      end if;

      if Image_IO.Image(Item       => Dec_Data_2,
                        Pic        => Picture_2,
                        Currency   => FF_Currency,
                        Fill       => Blank_Fill,
                        Separator  => FF_Separator,
                        Radix_Mark => FF_Radix) /= Output_2
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_2");
      end if;

      if Image_IO.Image(Item       => Dec_Data_3,
                        Pic        => Picture_3,
                        Currency   => DM_Currency,
                        Fill       => Blank_Fill,
                        Separator  => DM_Separator,
                        Radix_Mark => DM_Radix) /= Output_3
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_3");
      end if;

      if Image_IO.Image(Item       => Dec_Data_4,
                        Pic        => Picture_4,
                        Currency   => DM_Currency,
                        Fill       => Blank_Fill,
                        Separator  => DM_Separator,
                        Radix_Mark => DM_Radix) /= Output_4
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_4");
      end if;

      if Image_IO.Image(Item       => Dec_Data_5,
                        Pic        => Picture_5,
                        Currency   => DM_Currency,
                        Fill       => Blank_Fill,
                        Separator  => DM_Separator,
                        Radix_Mark => DM_Radix) /= Output_5
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_5");
      end if;

      if Image_IO.Image(Item       => Dec_Data_6,
                        Pic        => Picture_6,
                        Currency   => DM_Currency,
                        Fill       => Blank_Fill,
                        Separator  => DM_Separator,
                        Radix_Mark => DM_Radix) /= Output_6
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_6");
      end if;

      if Image_IO.Image(Item       => Dec_Data_7,
                        Pic        => Picture_7,
                        Currency   => "CHF",
                        Fill       => Blank_Fill,
                        Separator  => ',',
                        Radix_Mark => '.') /= Output_7
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_7");
      end if;


      -- The following calls of Function Image, using the specific
      -- decimal values and picture strings provided, will cause 
      -- a Layout_Error to be raised.
      -- Note: The data and the picture strings used in the following 
      --       evaluations are not themselves erroneous, but when used in
      --       combination will cause Layout_Error to be raised.

      Exception_Block_1 :
      declare
         Erroneous_Data_1    : Decimal_Data_Type :=  12.34;
         Erroneous_Picture_1 : Picture := To_Picture("9.99");
         N         : constant Natural := Image_IO.Length(Erroneous_Picture_1);
         TC_String : String(1..N);
      begin
         TC_String := Image_IO.Image(Erroneous_Data_1, Erroneous_Picture_1);
         Report.Failed("Layout_Error not raised by combination of "  &
                       "Erroneous_Picture_1 and Erroneous_Data_1");
         Report.Comment("Should never be printed: " & TC_String);
      exception
         when Ada.Text_IO.Layout_Error => null; -- OK, expected exception.
         when The_Error : others       => 
            Report.Failed
              ("The following exception was incorrectly raised in " &
               "Exception_Block_1: " & Exception_Name(The_Error));
      end Exception_Block_1;
  
      Exception_Block_2 :
      declare
         Erroneous_Data_2    : Decimal_Data_Type :=  -12.34;
         Erroneous_Picture_2 : Picture := To_Picture("99.99");
         N         : constant Natural := Image_IO.Length(Erroneous_Picture_2);
         TC_String : String(1..N);
      begin
         TC_String := Image_IO.Image(Erroneous_Data_2, Erroneous_Picture_2);
         Report.Failed("Layout_Error not raised by combination of "  &
                       "Erroneous_Picture_2 and Erroneous_Data_2");
         Report.Comment("Should never be printed: " & TC_String);
      exception
         when Ada.Text_IO.Layout_Error => null; -- OK, expected exception.
         when The_Error : others       => 
            Report.Failed
              ("The following exception was incorrectly raised in " &
               "Exception_Block_2: " & Exception_Name(The_Error));
      end Exception_Block_2;
  
   exception
      when The_Error : others => 
         Report.Failed("The following exception was raised in the " &
                       "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXF3004;
