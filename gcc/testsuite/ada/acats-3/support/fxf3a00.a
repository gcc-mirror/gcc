-- FXF3A00.A
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
-- FOUNDATION DESCRIPTION:
--      This foundation contains decimal data values, valid and invalid
--      Picture strings, and Edited Output result strings that will be used
--      in tests of Appendix F.3.
--      Note:  In this foundation package, the effect of "Table Driven Data"
--      is achieved using a series of arrays to hold the various data items.
--      Since the data items (Picture strings, Edited Output) are often of
--      different lengths, the arrays are defined to contain pointers to 
--      string values, thereby allowing the "tables" to hold string data of
--      different sizes.
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      15 Feb 95   SAIC    Picture string, decimal data, and edited_output
--                          modifications.
--      23 Feb 95   SAIC    Picture string modification.
--      10 Mar 95   SAIC    Added explanatory comments.
--      15 Nov 95   SAIC    Corrected picture string for ACVC 2.0.1.
--      06 Oct 96   SAIC    Corrected invalid picture strings.
--      13 Feb 97   PWB.CTA Deleted invalid picture string.
--      17 Feb 97   PWB.CTA Added leading blank to two picture strings
--!

with Ada.Text_IO.Editing;

package FXF3A00 is

   Number_Of_NDP_Items            : constant :=  12; -- No Decimal Places.
   Number_Of_2DP_Items            : constant :=  20; -- Two Decimal Places.
   Number_Of_Valid_Strings        : constant :=  40;
   Number_Of_FF_Strings           : constant :=   4; -- French Francs
   Number_Of_DM_Strings           : constant :=   5; -- Deutchemarks
   Number_Of_CHF_Strings          : constant :=   1; -- Swiss Francs
   Number_Of_Foreign_Strings      : constant :=   Number_Of_FF_Strings +
                                                  Number_Of_DM_Strings +
                                                  Number_Of_CHF_Strings; 
   Number_Of_Invalid_Strings       : constant :=  25;
   Number_Of_Erroneous_Conditions  : constant :=   3;
   Number_Of_Edited_Output_Strings : constant :=  32;

   -- The following string is to be used as a picture string with length 
   -- beyond the maximum (Max_Picture_Length) that is supported by the 
   -- implementation.  

   A_Picture_String_Too_Long       : constant 
     String (1..Ada.Text_IO.Editing.Max_Picture_Length + 1) := (others => '9');


   type Str_Ptr is access String;

   type Decimal_Type_NDP is delta 1.0  digits 16;  -- no decimal places
   type Decimal_Type_2DP is delta 0.01 digits 16;  -- two decimal places

   type Data_Array_Type_1 is array (Integer range <>) of Decimal_Type_NDP;
   type Data_Array_Type_2 is array (Integer range <>) of Decimal_Type_2DP;


   type Picture_String_Array_Type        is 
     array (Integer range <>) of Str_Ptr;

   type Edited_Output_Results_Array_Type is 
     array (Integer range <>) of Str_Ptr;



  Data_With_NDP : Data_Array_Type_1 (1..Number_Of_NDP_Items) := 
                     ( 1 => 1234.0,
                       2 => 51234.0,
                       3 => -1234.0,
                       4 => 1234.0,
                       5 => 1.0,
                       6 => 0.0,
                       7 => -10.0,
                       8 => -1.0,
                       9 => 1234.0,
                      10 => 1.0,
                      11 => 36.0,
                      12 => 0.0
                     );


   Data_With_2DP : Data_Array_Type_2 (1..Number_Of_2DP_Items) := 
                     ( 1 => 123456.78,
                       2 => 123456.78,
                       3 => 0.0,
                       4 => 0.20,
                       5 => 123456.00,
                       6 => -123456.78,
                       7 => 123456.78,
                       8 => -12.34,
                       9 => 1.23,
                      10 => 12.34,

        -- Items 11-20 are used with picture strings in evaluating use of
        -- foreign currency symbols.

                      11 => 123456.78,
                      12 => 123456.78,
                      13 => 32.10,
                      14 => -5432.10,
                      15 => -1234.57,
                      16 => 123456.78,
                      17 => 12.34,
                      18 => 12.34,
                      19 => 1.23,
                      20 => 12345.67
                     );



   Valid_Strings   : Picture_String_Array_Type 
                       (1..Number_Of_Valid_Strings) := 

        -- Items 1-10 are used in conjunction with Data_With_2DP values
        -- to produce edited output strings, as well as in tests of
        -- function Valid.

                       ( 1 => new String'("-###**_***_**9.99"),
                         2 => new String'("-$**_***_**9.99"),
                         3 => new String'("-$$$$$$.$$"),
                         4 => new String'("-$$$$$$.$$"),
                         5 => new String'("+BBBZZ_ZZZ_ZZZ.ZZ"),
                         6 => new String'("--_---_---_--9"),
                         7 => new String'("-$_$$$_$$$_$$9.99"),
                         8 => new String'("<$$_$$$9.99>"),
                         9 => new String'("$_$$9.99"),
                        10 => new String'("$$9.99"),

        -- Items 11-22 are used in conjunction with Data_With_NDP values
        -- to produce edited output strings.

                        11 => new String'("ZZZZ9"),
                        12 => new String'("ZZZZ9"),
                        13 => new String'("<#Z_ZZ9>"),
                        14 => new String'("<#Z_ZZ9>"),
                        15 => new String'("ZZZ.ZZ"),
                        16 => new String'("ZZZ.ZZ"),
                        17 => new String'("<###99>"),
                        18 => new String'("ZZZZZ-"),
                        19 => new String'("$$$$9"),
                        20 => new String'("$$$$$"),
                        21 => new String'("<###99>"),
                        22 => new String'("$$$$9"),

        -- Items 23-40 are used in validation of the Valid, To_Picture, and
        -- Pic_String subprograms of package Text_IO.Editing, and are not
        -- used to generate edited output.

                        23 => new String'("zZzZzZzZzZzZzZzZzZ"),
                        24 => new String'("999999999999999999"),
                        25 => new String'("******************"),
                        26 => new String'("$$$$$$$$$$$$$$$$$$"),
                        27 => new String'("9999/9999B9999_999909999"),
                        28 => new String'("+999999999999999999"),
                        29 => new String'("-999999999999999999"),
                        30 => new String'("999999999999999999+"),
                        31 => new String'("999999999999999999-"),
                        32 => new String'("<<<_<<<_<<<_<<<_<<<_<<9>"),
                        33 => new String'("++++++++++++++++++++"),
                        34 => new String'("--------------------"),
                        35 => new String'("zZzZzZzZzZzZzZzZzZ.zZ"),
                        36 => new String'("******************.99"),
                        37 => new String'("$$$$$$$$$$$$$$$$$$.99"),

       -- The following string has length 30, which is the minimum value 
       -- that must be supported for Max_Picture_Length.

                        38 => new String'("9_999_999_999_999_999_999BB.99"),
                        39 => new String'("<<<_<<<_<<<_<<<.99>"),
                        40 => new String'("ZZZZZZZZZZZZZZZZZ+")
                       );



   Foreign_Strings : Picture_String_Array_Type 
                       (1..Number_Of_Foreign_Strings) :=

       -- These strings are going to be used in conjunction with non-default
       -- values for Currency string, Radix mark, and Separator in calls to
       -- Image and Put, as well as in tests of function Valid.

                       ( 1 => new String'("-###**_***_**9.99"),    -- FF
                         2 => new String'("-$**_***_**9.99"),      -- FF
                         3 => new String'("<###z_ZZ9.99>"),        -- FF
                         4 => new String'("<###Z_ZZ9.99>"),        -- FF
                         5 => new String'("<<<<_<<<.<<###>"),      -- DM
                         6 => new String'("-$_$$$_$$$_$$9.99"),    -- DM
                         7 => new String'("$z99.99"),              -- DM
                         8 => new String'("$$$9.99"),              -- DM
                         9 => new String'("$_$$9.99"),             -- DM
                        10 => new String'("###_###_##9.99")        -- CHF
                       );



   Invalid_Strings : Picture_String_Array_Type 
                       (1..Number_Of_Invalid_Strings) := 
       --
       -- The RM references to the right of these invalid picture strings
       -- indicates which of the composition constraints of picture strings
       -- is violated by the particular string (and all following strings
       -- until another reference is presented).  However, certain strings
       -- violate multiple of the constraints.
       --
                       ( 1 => new String'("<<<"),        
                         2 => new String'("<<>>"),       
                         3 => new String'("<<<9_B0/$DB"),
                         4 => new String'("+BB"),        
                         5 => new String'("<-"),
                         6 => new String'("<CR"),
                         7 => new String'("<db"),
                         8 => new String'("<<BBBcr"),    
                         9 => new String'("<<__DB"),
                        10 => new String'("<<<++++_++-"),
                        11 => new String'("-999.99>"),   
                        12 => new String'("+++9.99+"),
                        13 => new String'("++++>>"),     
                        14 => new String'("->"),
                        15 => new String'("++9-"),  
                        16 => new String'("---999999->"),
                        17 => new String'("+++-"),       
                        18 => new String'("+++_+++_+.--"),
                        19 => new String'("--B.BB+>"),
                        20 => new String'("$$#$"),       
                        21 => new String'("#B$$$$"),
                        22 => new String'("**Z"),        
                        23 => new String'("ZZZzzz*"),
                        24 => new String'("9.99DB(2)"), 
                        25 => new String'(A_Picture_String_Too_Long)
                       );


   Edited_Output : Edited_Output_Results_Array_Type
                     (1..Number_Of_Edited_Output_Strings) :=   

        -- The following 10 edited output strings result from the first 10
        -- valid strings when used with the first 10 Data_With_2DP numeric 
        -- values.
                           ( 1 => new String'("   $***123,456.78"),
                             2 => new String'(" $***123,456.78"),
                             3 => new String'("          "),
                             4 => new String'("      $.20"),
                             5 => new String'("+      123,456.00"),
                             6 => new String'("      -123,457"),
                             7 => new String'("      $123,456.78"),
                             8 => new String'("(    $12.34)"),
                             9 => new String'("   $1.23"),
                            10 => new String'("$12.34"),

        -- The following 10 edited output strings correspond to the 10 foreign
        -- currency picture strings (the currency string is supplied at the
        -- time of the call to Editing.Image or Editing.Put), when used in
        -- conjunction with Data_With_2DP items 11-20

                            11 => new String'("  FF***123.456,78"),
                            12 => new String'(" FF***123.456,78"),
                            13 => new String'("  FF   32,10 "),
                            14 => new String'("( FF5.432,10)"), 
                            15 => new String'("  (1,234.57DM )"),
                            16 => new String'("      DM123,456.78"), 
                            17 => new String'("DM 12.34"),
                            18 => new String'(" DM12.34"),
                            19 => new String'("   DM1.23"),
                            20 => new String'("  CHF12,345.67"),

        -- The following 12 edited output strings correspond to the 12 
        -- Data_With_NDP items formatted using Valid_String items 11-22.
        -- This combination shows decimal data with no decimal places 
        -- formatted using picture strings.

                            21 => new String'(" 1234"),
                            22 => new String'("51234"),
                            23 => new String'("($1,234)"),
                            24 => new String'(" $1,234 "),
                            25 => new String'("  1.00"),
                            26 => new String'("      "),
                            27 => new String'("(  $10)"),
                            28 => new String'("    1-"),
                            29 => new String'("$1234"),
                            30 => new String'("   $1"),
                            31 => new String'("   $36 "),
                            32 => new String'("   $0")
                           );



   -- The following data is used to create exception situations in tests of
   -- the Edited Output capabilities of package Ada.Text_IO.Editing.  The data
   -- are not themselves erroneous, but will produce exceptions based on the
   -- data/picture string combination used.

   Erroneous_Data : Data_Array_Type_2 (1..Number_Of_Erroneous_Conditions) :=
                     ( 1 => 12.34,
                       2 => -12.34,
                       3 => 51234.0
                     );

   Erroneous_Strings : Picture_String_Array_Type 
                         (1..Number_Of_Erroneous_Conditions) :=
                           ( 1 => new String'("9.99"),
                             2 => new String'("99.99"),
                             3 => new String'("$$$$9")
                           );

end FXF3A00;
