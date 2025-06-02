-- CXA4032.A
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
--      Check that procedures defined in package Ada.Strings.Unbounded
--      are available, and that they produce correct results. Specifically, 
--      check the procedures Replace_Slice, Insert, Overwrite, Delete,
--      Trim (2 versions), Head, and Tail.
--      
-- TEST DESCRIPTION:
--      This test demonstrates the uses of many of the procedures defined
--      in package Ada.Strings.Unbounded for use with unbounded strings.
--      The test simulates how unbounded strings could be processed in a
--      user environment, using the procedures provided in this package.
--
--      This test, when taken in conjunction with tests CXA4010, CXA4011,
--      CXA4030, and CXA4031 will constitute a test of all the functionality 
--      contained in package Ada.Strings.Unbounded.  This test uses a variety 
--      of the procedures defined in the unbounded string package in ways 
--      typical of common usage.
--      
--       
-- CHANGE HISTORY:
--      02 Mar 95   SAIC    Initial prerelease version.
--
--!

with Report;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;

procedure CXA4032 is
begin

   Report.Test ("CXA4032", "Check that the subprograms defined in "        &
                           "package Ada.Strings.Unbounded are available, " &
                           "and that they produce correct results");

   Test_Block:
   declare

      package Unb renames Ada.Strings.Unbounded;
      use Unb;
      use Ada.Strings;

      TC_Null_String : constant String := "";
      TC_String_5    : String(1..5)    := "ABCDE";

      TC_Unb_String  : Unb.Unbounded_String := 
                         Unb.To_Unbounded_String("Test String");

   begin

      -- Procedure Replace_Slice

      begin                                           -- Low > Source'Last+1
         Unb.Replace_Slice(Source => TC_Unb_String, 
                           Low    => Unb.Length(TC_Unb_String) + 2,
                           High   => Unb.Length(TC_Unb_String),
                           By     => TC_String_5);
         Report.Failed("Index_Error not raised by Replace_Slice when Low " &
                       "> Source'Last+1");
      exception
         when Index_Error => null;  -- OK, expected exception.
         when others      =>
            Report.Failed("Unexpected exception raised by Replace_Slice" &
                          "when Low > Source'Last+1");
      end;

      -- High >= Low

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      Unb.Replace_Slice(TC_Unb_String, 5, 5, TC_String_5);

      if TC_Unb_String /= Unb.To_Unbounded_String("TestABCDEString") then
         Report.Failed("Incorrect results from Replace_Slice - 1");
      end if;

      Unb.Replace_Slice(TC_Unb_String, 1, 4, TC_String_5);

      if TC_Unb_String /= Unb.To_Unbounded_String("ABCDEABCDEString") then
         Report.Failed("Incorrect results from Replace_Slice - 2");
      end if;

      Unb.Replace_Slice(TC_Unb_String, 
                        11, 
                        Unb.Length(TC_Unb_String),
                        TC_Null_String);

      if TC_Unb_String /= Unb.To_Unbounded_String("ABCDEABCDE") then
         Report.Failed("Incorrect results from Replace_Slice - 3");
      end if;

      -- High < Low   

      Unb.Replace_Slice(TC_Unb_String, Low => 4, High => 1, By => "xxx");

      if TC_Unb_String /= Unb.To_Unbounded_String("ABCxxxDEABCDE") then
         Report.Failed("Incorrect results from Replace_Slice - 4");
      end if;

      Unb.Replace_Slice(TC_Unb_String, Low => 1, High => 0, By => "yyy");

      if TC_Unb_String /= Unb.To_Unbounded_String("yyyABCxxxDEABCDE") then
         Report.Failed("Incorrect results from Replace_Slice - 5");
      end if;

      Unb.Replace_Slice(TC_Unb_String, 
                        Unb.Length(TC_Unb_String) + 1, 
                        Unb.Length(TC_Unb_String), 
                        By => "zzz");

      if TC_Unb_String /= Unb.To_Unbounded_String("yyyABCxxxDEABCDEzzz") then
         Report.Failed("Incorrect results from Replace_Slice - 6");
      end if;


      -- Procedure Insert

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      begin                  -- Before not in Source'First..Source'Last + 1
         Unb.Insert(Source   => TC_Unb_String,
                    Before   => Unb.Length(TC_Unb_String) + 2,
                    New_Item => TC_String_5);
         Report.Failed("Index_Error not raised by Insert when Before " &
                       "not in the range Source'First..Source'Last+1");
      exception
         when Index_Error => null;  -- OK, expected exception.
         when others      =>
            Report.Failed
              ("Unexpected exception raised by Insert when Before not in " &
               "the range Source'First..Source'Last+1");
      end;

      Unb.Insert(TC_Unb_String, 1, "**");

      if TC_Unb_String /= Unb.To_Unbounded_String("**Test String") then
         Report.Failed("Incorrect results from Insert - 1");
      end if;

      Unb.Insert(TC_Unb_String, Unb.Length(TC_Unb_String)+1, "**");

      if TC_Unb_String /= Unb.To_Unbounded_String("**Test String**") then
         Report.Failed("Incorrect results from Insert - 2");
      end if;

      Unb.Insert(TC_Unb_String, 8, "---");

      if TC_Unb_String /= Unb.To_Unbounded_String("**Test ---String**") then
         Report.Failed("Incorrect results from Insert - 3");
      end if;

      Unb.Insert(TC_Unb_String, 3, TC_Null_String);

      if TC_Unb_String /= Unb.To_Unbounded_String("**Test ---String**") then
         Report.Failed("Incorrect results from Insert - 4");
      end if;


      -- Procedure Overwrite

      begin                -- Position not in Source'First..Source'Last + 1
         Unb.Overwrite(Source   => TC_Unb_String,
                       Position => Unb.Length(TC_Unb_String) + 2,
                       New_Item => TC_String_5);
         Report.Failed("Index_Error not raised by Overwrite when Position " &
                       "not in the range Source'First..Source'Last+1");
      exception
         when Index_Error => null;  -- OK, expected exception.
         when others      =>
            Report.Failed
              ("Unexpected exception raised by Overwrite when Position not " &
               "in the range Source'First..Source'Last+1");
      end;

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      Unb.Overwrite(Source   => TC_Unb_String,
                    Position => 1,
                    New_Item => "XXXX");

      if TC_Unb_String /= Unb.To_Unbounded_String("XXXX String") then
         Report.Failed("Incorrect results from Overwrite - 1");
      end if;

      Unb.Overwrite(TC_Unb_String, Unb.Length(TC_Unb_String)+1, "**");

      if TC_Unb_String /= Unb.To_Unbounded_String("XXXX String**") then
         Report.Failed("Incorrect results from Overwrite - 2");
      end if;

      Unb.Overwrite(TC_Unb_String, 3, TC_Null_String);

      if TC_Unb_String /= Unb.To_Unbounded_String("XXXX String**") then
         Report.Failed("Incorrect results from Overwrite - 3");
      end if;

      Unb.Overwrite(TC_Unb_String, 1, "abcdefghijklmn");

      if TC_Unb_String /= Unb.To_Unbounded_String("abcdefghijklmn") then
         Report.Failed("Incorrect results from Overwrite - 4");
      end if;


      -- Procedure Delete

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      -- From > Through  (No change to Source)

      Unb.Delete(Source  => TC_Unb_String, 
                 From    => Unb.Length(TC_Unb_String),
                 Through => Unb.Length(TC_Unb_String)-1);

      if TC_Unb_String /= Unb.To_Unbounded_String("Test String") then
         Report.Failed("Incorrect results from Delete - 1");
      end if;

      Unb.Delete(TC_Unb_String, 1, 0);

      if TC_Unb_String /= Unb.To_Unbounded_String("Test String") then
         Report.Failed("Incorrect results from Delete - 2");
      end if;

      -- From <= Through

      Unb.Delete(TC_Unb_String, 1, 5);

      if TC_Unb_String /= Unb.To_Unbounded_String("String") then
         Report.Failed("Incorrect results from Delete - 3");
      end if;

      Unb.Delete(TC_Unb_String, 3, 3);

      if TC_Unb_String /= Unb.To_Unbounded_String("Sting") then
         Report.Failed("Incorrect results from Delete - 4");
      end if;


      -- Procedure Trim 

      TC_Unb_String := Unb.To_Unbounded_String("No Spaces");

      Unb.Trim(Source => TC_Unb_String, Side => Ada.Strings.Both);

      if TC_Unb_String /= Unb.To_Unbounded_String("No Spaces") then
         Report.Failed("Incorrect results from Trim - 1");
      end if;

      TC_Unb_String := Unb.To_Unbounded_String("   Leading Spaces   ");

      Unb.Trim(TC_Unb_String, Ada.Strings.Left);

      if TC_Unb_String /= Unb.To_Unbounded_String("Leading Spaces   ") then
         Report.Failed("Incorrect results from Trim - 2");
      end if;

      TC_Unb_String := Unb.To_Unbounded_String("   Ending Spaces   ");

      Unb.Trim(TC_Unb_String, Ada.Strings.Right);

      if TC_Unb_String /= Unb.To_Unbounded_String("   Ending Spaces") then
         Report.Failed("Incorrect results from Trim - 3");
      end if;

      TC_Unb_String := 
        Unb.To_Unbounded_String("    Spaces   on  both  ends     ");

      Unb.Trim(TC_Unb_String, Ada.Strings.Both);

      if TC_Unb_String /= 
         Unb.To_Unbounded_String("Spaces   on  both  ends") 
      then
         Report.Failed("Incorrect results from Trim - 4");
      end if;


      -- Procedure Trim (with Character Set parameters)

      TC_Unb_String := Unb.To_Unbounded_String("lowerCASEletters");

      Unb.Trim(Source => TC_Unb_String,
               Left   => Ada.Strings.Maps.Constants.Lower_Set,
               Right  => Ada.Strings.Maps.Constants.Lower_Set);

      if TC_Unb_String /= Unb.To_Unbounded_String("CASE") then
         Report.Failed("Incorrect results from Trim with Sets - 1");
      end if;

      TC_Unb_String := Unb.To_Unbounded_String("lowerCASEletters");

      Unb.Trim(TC_Unb_String, 
               Ada.Strings.Maps.Constants.Upper_Set,
               Ada.Strings.Maps.Constants.Upper_Set);

      if TC_Unb_String /= Unb.To_Unbounded_String("lowerCASEletters") then
         Report.Failed("Incorrect results from Trim with Sets - 2");
      end if;

      TC_Unb_String := Unb.To_Unbounded_String("012abcdefghGFEDCBA789ab");

      Unb.Trim(TC_Unb_String,
               Ada.Strings.Maps.Constants.Hexadecimal_Digit_Set,
               Ada.Strings.Maps.Constants.Hexadecimal_Digit_Set);

      if TC_Unb_String /= Unb.To_Unbounded_String("ghG") then
         Report.Failed("Incorrect results from Trim with Sets - 3");
      end if;


      -- Procedure Head

      -- Count <= Source'Length

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      Unb.Head(Source => TC_Unb_String,
               Count  => 0,
               Pad    => '*');

      if TC_Unb_String /= Unb.Null_Unbounded_String then
         Report.Failed("Incorrect results from Head - 1");
      end if;

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      Unb.Head(Source => TC_Unb_String,
               Count  => 4,
               Pad    => '*');

      if TC_Unb_String /= Unb.To_Unbounded_String("Test") then
         Report.Failed("Incorrect results from Head - 2");
      end if;

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      Unb.Head(Source => TC_Unb_String,
               Count  => Unb.Length(TC_Unb_String),
               Pad    => '*');

      if TC_Unb_String /= Unb.To_Unbounded_String("Test String") then
         Report.Failed("Incorrect results from Head - 3");
      end if;

      -- Count > Source'Length

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      Unb.Head(Source => TC_Unb_String,
               Count  => Unb.Length(TC_Unb_String) + 4,
               Pad    => '*');

      if TC_Unb_String /= Unb.To_Unbounded_String("Test String****") then
         Report.Failed("Incorrect results from Head - 4");
      end if;

      TC_Unb_String := Unb.Null_Unbounded_String;

      Unb.Head(Source => TC_Unb_String,
               Count  => Unb.Length(TC_Unb_String) + 3,
               Pad    => '*');

      if TC_Unb_String /= Unb.To_Unbounded_String("***") then
         Report.Failed("Incorrect results from Head - 5");
      end if;


      -- Procedure Tail

      -- Count <= Source'Length

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      Unb.Tail(Source => TC_Unb_String,
               Count  => 0,
               Pad    => '*');

      if TC_Unb_String /= Unb.Null_Unbounded_String then
         Report.Failed("Incorrect results from Tail - 1");
      end if;

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      Unb.Tail(Source => TC_Unb_String,
               Count  => 6,
               Pad    => '*');

      if TC_Unb_String /= Unb.To_Unbounded_String("String") then
         Report.Failed("Incorrect results from Tail - 2");
      end if;

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      Unb.Tail(Source => TC_Unb_String,
               Count  => Unb.Length(TC_Unb_String),
               Pad    => '*');

      if TC_Unb_String /= Unb.To_Unbounded_String("Test String") then
         Report.Failed("Incorrect results from Tail - 3");
      end if;

      -- Count > Source'Length

      TC_Unb_String := Unb.To_Unbounded_String("Test String");

      Unb.Tail(Source => TC_Unb_String,
               Count  => Unb.Length(TC_Unb_String) + 5,
               Pad    => 'x');

      if TC_Unb_String /= Unb.To_Unbounded_String("xxxxxTest String") then
         Report.Failed("Incorrect results from Tail - 4");
      end if;

      TC_Unb_String := Unb.Null_Unbounded_String;

      Unb.Tail(Source => TC_Unb_String,
               Count  => Unb.Length(TC_Unb_String) + 3,
               Pad    => 'X');

      if TC_Unb_String /= Unb.To_Unbounded_String("XXX") then
         Report.Failed("Incorrect results from Tail - 5");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA4032;
