-- CXF3003.A
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
--      Check that statically identifiable picture strings can be used to
--      produce correctly formatted edited output.
--      
-- TEST DESCRIPTION:
--      This test defines several picture strings that are statically
--      identifiable, (i.e.,  Pic : Picture := To_Picture("..."); ).
--      These picture strings are used in conjunction with decimal data
--      as parameters in calls to functions Valid and Image.  These
--      functions are created by an instantiation of the generic package
--      Ada.Text_IO.Editing.Decimal_Output.
--
--       
-- CHANGE HISTORY:
--      04 Apr 96   SAIC    Initial release for 2.1.
--      13 Feb 97   PWB.CTA corrected incorrect picture strings.
--!

with Report;
with Ada.Text_IO.Editing;
with Ada.Exceptions;

procedure CXF3003 is
begin

   Report.Test ("CXF3003", "Check that statically identifiable "     &
                           "picture strings can be used to produce " &
                           "correctly formatted edited output");

   Test_Block:
   declare

      use Ada.Exceptions;
      use Ada.Text_IO.Editing;

      Def_Cur   : constant String    := "$";
      Def_Fill  : constant Character := '*';
      Def_Sep   : constant Character := Default_Separator;
      Def_Radix : constant Character := 
                    Ada.Text_IO.Editing.Default_Radix_Mark;

      type Str_Ptr is access String;
      type Edited_Output_Array_Type is array (Integer range <>) of Str_Ptr;

      -- Define a decimal data type, and instantiate the Decimal_Output 
      -- generic package for the data type.

      type Decimal_Data_Type is delta 0.01 digits 16;

      package Image_IO is 
        new Decimal_Output(Num                => Decimal_Data_Type,
                           Default_Currency   => Def_Cur,
                           Default_Fill       => '*',
                           Default_Separator  => Default_Separator,
                           Default_Radix_Mark => Def_Radix);


      type Decimal_Data_Array_Type is 
        array (Integer range <>) of Decimal_Data_Type;

      Decimal_Data : Decimal_Data_Array_Type(1..5) :=
          (1 =>  1357.99,
           2 => -9029.01,
           3 =>     0.00,
           4 =>     0.20,
           5 =>     3.45);

      -- Statically identifiable picture strings.

      Picture_1  : Picture := To_Picture("-$$_$$9.99"); 
      Picture_2  : Picture := To_Picture("-$$_$$$.$$"); 
      Picture_3  : Picture := To_Picture("-ZZZZ.ZZ");
      Picture_5  : Picture := To_Picture("-$$$_999.99");
      Picture_6  : Picture := To_Picture("-###**_***_**9.99");
      Picture_7  : Picture := To_Picture("-$**_***_**9.99");
      Picture_8  : Picture := To_Picture("-$$$$$$.$$");
      Picture_9  : Picture := To_Picture("-$$$$$$.$$");
      Picture_10 : Picture := To_Picture("+BBBZZ_ZZZ_ZZZ.ZZ");
      Picture_11 : Picture := To_Picture("--_---_---_--9");
      Picture_12 : Picture := To_Picture("-$_$$$_$$$_$$9.99");
      Picture_14 : Picture := To_Picture("$_$$9.99");
      Picture_15 : Picture := To_Picture("$$9.99");


      Picture_1_Output : Edited_Output_Array_Type(1..5) :=
          ( 1 => new String'(" $1,357.99"),      
            2 => new String'("-$9,029.01"),    
            3 => new String'("     $0.00"),
            4 => new String'("     $0.20"),
            5 => new String'("     $3.45"));

      Picture_2_Output : Edited_Output_Array_Type(1..5) :=
           (1 => new String'(" $1,357.99"),      
            2 => new String'("-$9,029.01"),    
            3 => new String'("          "),    
            4 => new String'("      $.20"),    
            5 => new String'("     $3.45"));

      Picture_3_Output : Edited_Output_Array_Type(1..5) :=
           (1 => new String'(" 1357.99"),       
            2 => new String'("-9029.01"),
            3 => new String'("        "),     
            4 => new String'("     .20"),     
            5 => new String'("    3.45"));

      Picture_5_Output : Edited_Output_Array_Type(1..5) := 
           (1 => new String'("  $1,357.99"),
            2 => new String'("- $9,029.01"),
            3 => new String'("   $ 000.00"),  
            4 => new String'("   $ 000.20"),
            5 => new String'("   $ 003.45"));

   begin

      -- Check the results of function Valid, using the first five decimal
      -- data items and picture strings.

      if not Image_IO.Valid(Decimal_Data(1), Picture_1) then
         Report.Failed("Picture string 1 not valid");
      elsif not Image_IO.Valid(Decimal_Data(2), Picture_2) then
         Report.Failed("Picture string 2 not valid");
      elsif not Image_IO.Valid(Decimal_Data(3), Picture_3) then
         Report.Failed("Picture string 3 not valid");
      elsif not Image_IO.Valid(Decimal_Data(5), Picture_5) then
         Report.Failed("Picture string 5 not valid");
      end if;


      -- Check the results of function Image, using the picture strings
      -- constructed above, with a variety of named vs. positional
      -- parameter notation and defaulted parameters.

      for i in 1..5 loop
         if Image_IO.Image(Item => Decimal_Data(i), Pic => Picture_1) /=
            Picture_1_Output(i).all
         then
            Report.Failed("Incorrect result from function Image with "    &
                          "decimal data item #" & Integer'Image(i) & ", " &
                          "combined with Picture_1 picture string."       &
                          "Expected: " & Picture_1_Output(i).all & ", "   &
                          "Found: " &
                          Image_IO.Image(Decimal_Data(i),Picture_1));
         end if;

         if Image_IO.Image(Decimal_Data(i), Pic => Picture_2) /=
            Picture_2_Output(i).all
         then
            Report.Failed("Incorrect result from function Image with "    &
                          "decimal data item #" & Integer'Image(i) & ", " &
                          "combined with Picture_2 picture string."       &
                          "Expected: " & Picture_2_Output(i).all & ", "   &
                          "Found: " &
                          Image_IO.Image(Decimal_Data(i),Picture_2));
         end if;

         if Image_IO.Image(Decimal_Data(i), Picture_3) /=
            Picture_3_Output(i).all
         then
            Report.Failed("Incorrect result from function Image with "    &
                          "decimal data item #" & Integer'Image(i) & ", " &
                          "combined with Picture_3 picture string."       &
                          "Expected: " & Picture_3_Output(i).all & ", "   &
                          "Found: " &
                          Image_IO.Image(Decimal_Data(i),Picture_3));
         end if;

         if Image_IO.Image(Decimal_Data(i), Picture_5) /=
            Picture_5_Output(i).all
         then
            Report.Failed("Incorrect result from function Image with "    &
                          "decimal data item #" & Integer'Image(i) & ", " &
                          "combined with Picture_5 picture string."       &
                          "Expected: " & Picture_5_Output(i).all & ", "   &
                          "Found: " &
                          Image_IO.Image(Decimal_Data(i),Picture_5));
         end if;
      end loop;


      if Image_IO.Image(Item       => 123456.78, 
                        Pic        => Picture_6,
                        Currency   => "$",
                        Fill       => Def_Fill,
                        Separator  => Def_Sep,
                        Radix_Mark => Def_Radix) /= "   $***123,456.78"
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_6");
      end if;

      if Image_IO.Image(123456.78, 
                        Pic        => Picture_7,
                        Currency   => Def_Cur,
                        Fill       => '*',
                        Separator  => Def_Sep,
                        Radix_Mark => Def_Radix) /= " $***123,456.78"
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_7");
      end if;

      if Image_IO.Image(0.0, 
                        Picture_8,
                        Currency   => "$",
                        Fill       => '*',
                        Separator  => Def_Sep,
                        Radix_Mark => Def_Radix) /= "          "
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_8");
      end if;

      if Image_IO.Image(0.20, 
                        Picture_9,
                        Def_Cur,
                        Fill       => Def_Fill,
                        Separator  => Default_Separator,
                        Radix_Mark => Default_Radix_Mark) /= "      $.20"
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_9");
      end if;

      if Image_IO.Image(123456.00,
                        Picture_10,
                        "$",
                        '*',
                        Separator  => Def_Sep,
                        Radix_Mark => Def_Radix) /= "+      123,456.00"
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_10");
      end if;

      if Image_IO.Image(-123456.78,
                        Picture_11,
                        Default_Currency,
                        Default_Fill,
                        Default_Separator,
                        Radix_Mark => Def_Radix) /= "      -123,457"
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_11");
      end if;

      if Image_IO.Image(123456.78, Picture_12, "$", '*', ',', '.') /= 
         "      $123,456.78"
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_12");
      end if;

      if Image_IO.Image(1.23,
                        Picture_14,
                        Currency => Def_Cur,
                        Fill     => Def_Fill) /= "   $1.23"
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_14");
      end if;

      if Image_IO.Image(12.34, Pic => Picture_15) /= "$12.34"
      then
         Report.Failed("Incorrect result from Fn. Image using Picture_15");
      end if;

   exception
      when The_Error : others => 
         Report.Failed("The following exception was raised in the " &
                       "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXF3003;
