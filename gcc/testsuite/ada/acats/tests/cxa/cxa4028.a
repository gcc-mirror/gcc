-- CXA4028.A
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
--      Check that Ada.Strings.Bounded procedures Append, Head, Tail, and 
--      Trim, and relational operator functions "=", ">", ">=", "<", "<=" 
--      with parameter combinations of type String and Bounded_String, 
--      produce correct results.
--
-- TEST DESCRIPTION:
--      This test examines the operation of several subprograms from within 
--      the Ada.Strings.Bounded package.  Four different instantiations of
--      Ada.Strings.Bounded.Generic_Bounded_Length provide packages defined
--      to manipulate bounded strings of lengths 1, 20, 40, and 80.
--      Examples of the above mentioned procedures and relational operators
--      from each of these instantiations are tested, with results compared
--      against expected output.
--      
--      Testing of the function versions of many of the subprograms tested
--      here is performed in tests CXA4006-CXA4009.
--      
--       
-- CHANGE HISTORY:
--      16 Feb 95   SAIC    Initial prerelease version
--      10 Mar 95   SAIC    Incorporated reviewer comments.
--      15 Apr 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--
--!

with Ada.Exceptions;
with Ada.Strings.Bounded;
with Report;

procedure CXA4028 is

begin

   Report.Test ("CXA4028", "Check that Ada.Strings.Bounded procedures "    &
                           "Append, Head, Tail, and Trim, and relational " &
                           "operator functions =, >, >=, <, <= with "      &
                           "parameter combinations of type String and "    &
                           "Bounded_String, produce correct results");

   Test_Block:
   declare

      use Ada.Exceptions;
      use Ada.Strings;

      -- Instantiations of Bounded String generic package.

      package BS1  is new Ada.Strings.Bounded.Generic_Bounded_Length(1);
      package BS20 is new Ada.Strings.Bounded.Generic_Bounded_Length(20);
      package BS40 is new Ada.Strings.Bounded.Generic_Bounded_Length(40);
      package BS80 is new Ada.Strings.Bounded.Generic_Bounded_Length(80);

      use type BS1.Bounded_String,  BS20.Bounded_String, 
               BS40.Bounded_String, BS80.Bounded_String;

      String_1   : String(1..1)  := "A";
      String_20  : String(1..20) := "ABCDEFGHIJKLMNOPQRST";
      String_40  : String(1..40) := "abcdefghijklmnopqrst" & String_20;
      String_80  : String(1..80) := String_40 & String_40;

      BString_1  : BS1.Bounded_String  := BS1.Null_Bounded_String;
      BString_20 : BS20.Bounded_String := BS20.Null_Bounded_String;
      BString_40 : BS40.Bounded_String := BS40.Null_Bounded_String;
      BString_80 : BS80.Bounded_String := BS80.Null_Bounded_String;

   begin

      -- Procedure Append.

      declare
         use BS1, BS20;
      begin
         Append(Source => BString_1, New_Item => To_Bounded_String("A"));
         Append(BString_1, "B", Ada.Strings.Left);
         Append(BString_1, 'C', Drop => Ada.Strings.Right); -- Drop appended
                                                            -- character.
         if BString_1 /= To_Bounded_String("B") then
            Report.Failed("Incorrect results from BS1 versions of " &
                          "procedure Append");
         end if;

         Append(BString_20, 'T');                      -- Character.
         Append(BString_20, "his string");             -- String.
         Append(BString_20, 
                To_Bounded_String(" is complete."),    -- Bounded string.
                Drop => Ada.Strings.Right);            -- Drop 4 characters.

         if BString_20 /= To_Bounded_String("This string is compl") then
            Report.Failed("Incorrect results from BS20 versions of " &
                          "procedure Append");
         end if;
      end;


      -- Operator "=".

      BString_40 := BS40.To_Bounded_String(String_40);
      BString_80 := BS80.To_Bounded_String(
                           BS40.To_String(BString_40) &
                           BS40.To_String(BString_40));

      if not (BString_40 = String_40 and         -- (Bounded_String, String)
              BS80."="(String_80, BString_80))   -- (String, Bounded_String)
      then
         Report.Failed("Incorrect results from function ""="" with " &
                       "string - bounded string parameter combinations");
      end if;


      -- Operator "<". 

      BString_1  := BS1.To_Bounded_String("cat",         -- string "c" only.
                                          Drop => Ada.Strings.Right);
      BString_20 := BS20.To_Bounded_String("Santa Claus");

      if BString_1 < "C"        or               -- (Bounded_String, String)
         BS1."<"(BString_1,"c") or               -- (Bounded_String, String)
         "x"  < BString_1       or               -- (String, Bounded_String)
         BString_20 < "Santa "  or               -- (Bounded_String, String)
         "Santa and his Elves" < BString_20      -- (String, Bounded_String)
      then
         Report.Failed("Incorrect results from function ""<"" with " &
                       "string - bounded string parameter combinations");
      end if;


      -- Operator "<=". 

      BString_20 := BS20.To_Bounded_String("Sample string");

      if BString_20          <= "Sample strin"  or -- (Bounded_String, String)
         "sample string"     <= BString_20      or -- (String, Bounded_String)
         not("Sample string" <= BString_20)        -- (String, Bounded_String)
      then
         Report.Failed("Incorrect results from function ""<="" with " &
                       "string - bounded string parameter combinations");
      end if;


      -- Operator ">". 

      BString_40 := BS40.To_Bounded_String("A MUCH LONGER SAMPLE STRING.");

      if BString_40 > "A much longer sample string"     or  -- (Bnd_Str, Str)
         String_20  > BS40.To_Bounded_String(String_40) or  -- (Str, Bnd_Str)
         BS40.To_Bounded_String("ABCDEFGH") > "abcdefgh"    -- (Str, Bnd_Str)
      then
         Report.Failed("Incorrect results from function "">"" with " &
                       "string - bounded string parameter combinations");
      end if;


      -- Operator ">=". 

      BString_80 := BS80.To_Bounded_String(String_80);

      if not (BString_80 >= String_80  and 
              BS80.To_Bounded_String("Programming") >= "PROGRAMMING" and
              "test" >= BS80.To_Bounded_String("tess"))
      then
         Report.Failed("Incorrect results from function "">="" with " &
                       "string - bounded string parameter combinations");
      end if;


      -- Procedure Trim

      BString_20 := BS20.To_Bounded_String("      Left Spaces  ");
      BS20.Trim(Source => BString_20, 
                Side   => Ada.Strings.Left);

      if "Left Spaces  " /= BString_20 then
         Report.Failed("Incorrect results from Procedure Trim with " &
                       "Side = Left");
      end if;

      BString_40 := BS40.To_Bounded_String("    Right Spaces    ");
      BS40.Trim(BString_40, Side => Ada.Strings.Right);

      if BString_40 /= "    Right Spaces" then
         Report.Failed("Incorrect results from Procedure Trim with " &
                       "Side = Right");
      end if;

      BString_20 := BS20.To_Bounded_String("   Both Sides      ");
      BS20.Trim(BString_20, Ada.Strings.Both);

      if BString_20 /= BS20.To_Bounded_String("Both Sides") then
         Report.Failed("Incorrect results from Procedure Trim with " &
                       "Side = Both");
      end if;

      BString_80 := BS80.To_Bounded_String("Centered    Spaces");
      BS80.Trim(BString_80, Ada.Strings.Both);

      if BString_80 /= BS80.To_Bounded_String("Centered    Spaces") then
         Report.Failed("Incorrect results from Procedure Trim with " &
                       "no blank spaces on the ends of the string");
      end if;


      -- Procedure Head

      BString_40 := BS40.To_Bounded_String("Test String");
      BS40.Head(Source => BString_40,
                Count  => 4);                       -- Count < Source'Length

      if BString_40 /= BS40.To_Bounded_String("Test") then
         Report.Failed("Incorrect results from Procedure Head with " &
                       "the Count parameter less than Source'Length");
      end if;

      BString_1 := BS1.To_Bounded_String("X");
      BS1.Head(BString_1, BS1.Length(BString_1));   -- Count = Source'Length

      if BString_1 /= "X" then
         Report.Failed("Incorrect results from Procedure Head with " &
                       "the Count parameter equal to Source'Length");
      end if;

      BString_20 := BS20.To_Bounded_String("Sample string");
      BS20.Head(BString_20, 
                Count => BS20.Max_Length,           -- Count > Source'Length
                Pad => '*');

      if BString_20 /= BS20.To_Bounded_String("Sample string*******") then
         Report.Failed("Incorrect results from Procedure Head with "    &
                       "the Count parameter greater than Source'Length");
      end if;

      BString_20 := BS20.To_Bounded_String("Twenty Characters 20");
      BS20.Head(BString_20, 22, Pad => '*', Drop => Ada.Strings.Left);

      if BString_20 /= "enty Characters 20**" then
         Report.Failed("Incorrect results from Procedure Head with "      &
                       "the Count parameter greater than Source'Length, " &
                       "and the Drop parameter = Left");
      end if;

      BString_20 := BS20.To_Bounded_String("Short String");
      BS20.Head(BString_20, 23, '-', Ada.Strings.Right);

      if ("Short String--------") /= BString_20 then
         Report.Failed("Incorrect results from Procedure Head with " &
                       "the Count parameter greater than Source'Length, " &
                       "and the Drop parameter = Right");
      end if;


      -- Procedure Tail

      BString_40 := BS40.To_Bounded_String("Test String");
      BS40.Tail(Source => BString_40,
                Count  => 6);                       -- Count < Source'Length

      if BString_40 /= BS40.To_Bounded_String("String") then
         Report.Failed("Incorrect results from Procedure Tail with " &
                       "the Count parameter less than Source'Length");
      end if;

      BString_1 := BS1.To_Bounded_String("X");
      BS1.Tail(BString_1, BS1.Length(BString_1));   -- Count = Source'Length

      if BString_1 /= "X" then
         Report.Failed("Incorrect results from Procedure Tail with " &
                       "the Count parameter equal to Source'Length");
      end if;

      BString_20 := BS20.To_Bounded_String("Sample string");
      BS20.Tail(BString_20, 
                Count => BS20.Max_Length,           -- Count > Source'Length
                Pad => '*');

      if BString_20 /= BS20.To_Bounded_String("*******Sample string") then
         Report.Failed("Incorrect results from Procedure Tail with "    &
                       "the Count parameter greater than Source'Length");
      end if;

      BString_20 := BS20.To_Bounded_String("Twenty Characters");  -- Len = 17
      BS20.Tail(BString_20, 22, Pad => '*', Drop => Ada.Strings.Left);

      if BString_20 /= "***Twenty Characters" then
         Report.Failed("Incorrect results from Procedure Tail with "      &
                       "the Count parameter greater than Source'Length, " &
                       "and the Drop parameter = Left");
      end if;

      BString_20 := BS20.To_Bounded_String("Maximum Length Chars");
      BS20.Tail(BString_20, 23, '-', Ada.Strings.Right);

      if ("---Maximum Length Ch") /= BString_20 then
         Report.Failed("Incorrect results from Procedure Tail with " &
                       "the Count parameter greater than Source'Length, " &
                       "and the Drop parameter = Right");
      end if;

   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXA4028;
