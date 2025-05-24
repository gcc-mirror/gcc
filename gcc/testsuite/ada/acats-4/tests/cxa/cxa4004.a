-- CXA4004.A
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
--      Check that the subprograms defined in package Ada.Strings.Fixed are
--      available, and that they produce correct results. Specifically, check 
--      the subprograms Count, Find_Token, Index, Index_Non_Blank, and Move.
--      
-- TEST DESCRIPTION:
--      This test, when combined with tests CXA4002,3, and 5 will provide 
--      thorough coverage of the functionality found in Ada.Strings.Fixed.  
--      This test contains many small, specific test cases, situations that 
--      although common in user environments, are often difficult to generate 
--      in large numbers in a application-based test.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      10 Apr 95   SAIC    Corrected subtest for Move, Drop=Right.
--
--!

with Report;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure CXA4004 is
begin

   Report.Test("CXA4004", "Check that the subprograms defined in "    &
                          "package Ada.Strings.Fixed are available, " &
                          "and that they produce correct results");

   Test_Block:
   declare

      package ASF  renames Ada.Strings.Fixed;
      package Maps renames Ada.Strings.Maps;

      Result_String  : String(1..10) := (others => Ada.Strings.Space);

      Source_String1 : String(1..5)  := "abcde";        -- odd length string
      Source_String2 : String(1..6)  := "abcdef";       -- even length string
      Source_String3 : String(1..12) := "abcdefghijkl";   
      Source_String4 : String(1..12) := "abcdefghij  "; -- last two ch pad
      Source_String5 : String(1..12) := "  cdefghijkl"; -- first two ch pad
      Source_String6 : String(1..12) := "abcdefabcdef";   
                                                                              
      Location       : Natural := 0;
      Slice_Start    : Positive;
      Slice_End,
      Slice_Count    : Natural := 0;

      CD_Set         : Maps.Character_Set := Maps.To_Set("cd");  
      ABCD_Set       : Maps.Character_Set := Maps.To_Set("abcd");
      A_to_F_Set     : Maps.Character_Set := Maps.To_Set("abcdef");
                  
      CD_to_XY_Map   : Maps.Character_Mapping :=
                         Maps.To_Mapping(From => "cd",  To => "xy");

   begin

      -- Procedure Move

      -- Evaluate the Procedure Move with various combinations of
      -- parameters.

      -- Justify = Left (default case)
     
      ASF.Move(Source => Source_String1,       -- "abcde"
               Target => Result_String);
      
      if Result_String /= "abcde     " then
         Report.Failed("Incorrect result from Move with Justify = Left");
      end if;
 
      -- Justify = Right
 
      ASF.Move(Source  => Source_String2,      -- "abcdef"
               Target  => Result_String,
               Drop    => Ada.Strings.Error,
               Justify => Ada.Strings.Right);

      if Result_String /= "    abcdef" then
         Report.Failed("Incorrect result from Move with Justify = Right");
      end if;

      -- Justify = Center (two cases, odd and even pad lengths)
 
      ASF.Move(Source_String1,                 -- "abcde"
               Result_String,
               Ada.Strings.Error,
               Ada.Strings.Center,
               'x');                           -- non-default padding.

      if Result_String /= "xxabcdexxx" then  -- Unequal padding added right
         Report.Failed("Incorrect result from Move with Justify = Center-1");
      end if;

      ASF.Move(Source_String2,                 -- "abcdef"
               Result_String,
               Ada.Strings.Error,
               Ada.Strings.Center);

      if Result_String /= "  abcdef  " then  -- Equal padding added on L/R.
         Report.Failed("Incorrect result from Move with Justify = Center-2");
      end if;

      -- When the source string is longer than the target string, several
      -- cases can be examined, with the results depending on the value of
      -- the Drop parameter.

      -- Drop = Left

      ASF.Move(Source => Source_String3,       -- "abcdefghijkl"
               Target => Result_String,
               Drop   => Ada.Strings.Left);

      if Result_String /= "cdefghijkl" then
         Report.Failed("Incorrect result from Move with Drop = Left");
      end if;

      -- Drop = Right
     
      ASF.Move(Source_String3, Result_String, Ada.Strings.Right);

      if Result_String /= "abcdefghij" then
         Report.Failed("Incorrect result from Move with Drop = Right");
      end if;

      -- Drop = Error
      -- The effect in this case depends on the value of the justify
      -- parameter, and on whether any characters in Source other than 
      -- Pad would fail to be copied.

      -- Drop = Error, Justify = Left, right overflow characters are pad.

      ASF.Move(Source  => Source_String4,      -- "abcdefghij  "
               Target  => Result_String,
               Drop    => Ada.Strings.Error,
               Justify => Ada.Strings.Left);

      if not(Result_String = "abcdefghij") then  -- leftmost 10 characters
          Report.Failed("Incorrect result from Move with Drop = Error - 1");
      end if;

      -- Drop = Error, Justify = Right, left overflow characters are pad.

      ASF.Move(Source_String5,                 -- "  cdefghijkl"
               Result_String, 
               Ada.Strings.Error, 
               Ada.Strings.Right);

      if Result_String /= "cdefghijkl" then  -- rightmost 10 characters
         Report.Failed("Incorrect result from Move with Drop = Error - 2");
      end if;

      -- In other cases of Drop=Error, Length_Error is propagated, such as:

      begin

         ASF.Move(Source_String3,     -- 12 characters, no Pad.
                  Result_String,      -- 10 characters
                  Ada.Strings.Error,  
                  Ada.Strings.Left);  

         Report.Failed("Length_Error not raised by Move - 1");

      exception
         when Ada.Strings.Length_Error => null;   -- OK
         when others => 
            Report.Failed("Incorrect exception raised by Move - 1");
      end;



      -- Function Index
      -- (Other usage examples of this function found in CXA4002-3.)
      -- Check when the pattern is not found in the source.

      if ASF.Index("abcdef", "gh")       /= 0 or
         ASF.Index("abcde",  "abcdef")   /= 0 or  -- pattern > source
         ASF.Index("xyz", 
                   "abcde", 
                   Ada.Strings.Backward) /= 0 or
         ASF.Index("",      "ab")        /= 0 or  -- null source string.
         ASF.Index("abcde", "  ")        /= 0     -- blank pattern.
      then
         Report.Failed("Incorrect result from Index, no pattern match");
      end if;

      -- Check that Pattern_Error is raised when the pattern is the 
      -- null string.
      begin
         Location := ASF.Index(Source_String6,    -- "abcdefabcdef"
                               "",                -- null pattern string.
                               Ada.Strings.Forward);
         Report.Failed("Pattern_Error not raised by Index");
      exception
         when Ada.Strings.Pattern_Error => null;  -- OK, expected exception.
         when others                    =>
           Report.Failed("Incorrect exception raised by Index, null pattern");
      end;

      -- Use the search direction "backward" to locate the particular
      -- pattern within the source string.

      Location := ASF.Index(Source_String6,         -- "abcdefabcdef"
                            "de",                   -- slice 4..5, 10..11
                            Ada.Strings.Backward);  -- search from right end.

      if Location /= 10  then
         Report.Failed("Incorrect result from Index going Backward");
      end if;

      -- Using the version of Index testing character set membership, 
      -- check combinations of forward/backward, inside/outside parameter
      -- configurations.

      if ASF.Index(Source => Source_String1,      -- "abcde"
                   Set    => CD_Set,
                   Test   => Ada.Strings.Inside,
                   Going  => Ada.Strings.Forward) /= 3 or -- 'c' at pos 3.
         ASF.Index(Source_String6,                -- "abcdefabcdef"
                   CD_Set,
                   Ada.Strings.Outside,
                   Ada.Strings.Backward)  /=  12  or   -- 'f' at position 12
         ASF.Index(Source_String6,                -- "abcdefabcdef"
                   CD_Set,
                   Ada.Strings.Inside,
                   Ada.Strings.Backward)  /=   10  or  -- 'd' at position 10
         ASF.Index("cdcdcdcdacdcdcdcd",               
                   CD_Set,
                   Ada.Strings.Outside,
                   Ada.Strings.Forward)   /=    9      -- 'a' at position 9
      then
         Report.Failed("Incorrect result from function Index for sets - 1");
      end if;

      -- Additional interesting uses/combinations using Index for sets.

      if ASF.Index("cd",                               -- same size, str-set
                   CD_Set,
                   Ada.Strings.Inside,
                   Ada.Strings.Forward)   /=    1  or  -- 'c' at position 1
         ASF.Index("abcd",                             -- same size, str-set,
                   Maps.To_Set("efgh"),                -- different contents.
                   Ada.Strings.Outside,                
                   Ada.Strings.Forward)   /=    1  or
         ASF.Index("abccd",                            -- set > string
                   Maps.To_Set("acegik"),
                   Ada.Strings.Inside,
                   Ada.Strings.Backward)  /=    4  or  -- 'c' at position 4
         ASF.Index("abcde",
                   Maps.Null_Set)         /=    0  or
         ASF.Index("",                                 -- Null string.
                   CD_Set)                /=    0  or
         ASF.Index("abc ab",                           -- blank included
                   Maps.To_Set("e "),                  -- in string and set.
                   Ada.Strings.Inside,
                   Ada.Strings.Backward)  /=    4      -- blank in string.
      then
         Report.Failed("Incorrect result from function Index for sets - 2");
      end if;



      -- Function Index_Non_Blank.                      
      -- (Other usage examples of this function found in CXA4002-3.)
                                                         

      if ASF.Index_Non_Blank(Source => Source_String4,  -- "abcdefghij  "
                             Going  => Ada.Strings.Backward)  /= 10  or
         ASF.Index_Non_Blank("abc def ghi jkl  ",
                             Ada.Strings.Backward)            /= 15  or
         ASF.Index_Non_Blank("  abcdef")                      /=  3  or
         ASF.Index_Non_Blank("        ")                      /=  0
      then
          Report.Failed("Incorrect result from Index_Non_Blank");
      end if;



      -- Function Count
      -- (Other usage examples of this function found in CXA4002-3.)

      if ASF.Count("abababa",   "aba")            /=  2  or
         ASF.Count("abababa",   "ab" )            /=  3  or
         ASF.Count("babababa",  "ab")             /=  3  or
         ASF.Count("abaabaaba", "aba")            /=  3  or
         ASF.Count("xxxxxxxxxxxxxxxxxxxy", "xy")  /=  1  or
         ASF.Count("xxxxxxxxxxxxxxxxxxxx", "x")   /= 20
      then
         Report.Failed("Incorrect result from Function Count");
      end if;

      -- Determine the number of slices of Source that when mapped to a 
      -- non-identity map, match the pattern string.

      Slice_Count := ASF.Count(Source_String6, -- "abcdefabcdef"
                               "xy",
                               CD_to_XY_Map);  -- maps 'c' to 'x', 'd' to 'y'

      if Slice_Count /= 2 then  -- two slices "xy" in "mapped" Source_String6
         Report.Failed("Incorrect result from Count with non-identity map");
      end if;

      -- If the pattern supplied to Function Count is the null string, then
      -- Pattern_Error is propagated.

      declare
         The_Null_String : constant String := "";
      begin
         Slice_Count := ASF.Count(Source_String6, The_Null_String);
         Report.Failed("Pattern_Error not raised by Function Count");
      exception
         when Ada.Strings.Pattern_Error => null;   -- OK
         when others =>
           Report.Failed("Incorrect exception from Count with null pattern");
      end;


      -- Function Count returning the number of characters in a particular
      -- set that are found in source string.

      if ASF.Count(Source_String6, CD_Set) /= 4 then  -- 2 'c' and 'd' chars.
         Report.Failed("Incorrect result from Count with set");
      end if;


 
      -- Function Find_Token.                            
      -- (Other usage examples of this function found in CXA4002-3.)

      ASF.Find_Token(Source  => Source_String6,      -- First slice with no 
                     Set     => ABCD_Set,            -- 'a', 'b', 'c', or 'd'
                     Test    => Ada.Strings.Outside, -- is "ef" at 5..6.
                     First   => Slice_Start,
                     Last    => Slice_End);

      if Slice_Start /= 5  or Slice_End /= 6 then
         Report.Failed("Incorrect result from Find_Token - 1");
      end if;

      -- If no appropriate slice is contained by the source string, then the 
      -- value returned in Last is zero, and the value in First is 
      -- Source'First.

      ASF.Find_Token(Source_String6,      -- "abcdefabcdef"
                     A_to_F_Set,          -- Set of characters 'a' thru 'f'.
                     Ada.Strings.Outside, -- No characters outside this set.
                     Slice_Start,
                     Slice_End);
      
      if Slice_Start /= Source_String6'First  or Slice_End /= 0 then
         Report.Failed("Incorrect result from Find_Token - 2");
      end if;

      -- Additional testing of Find_Token.

      ASF.Find_Token("eabcdabcddcab", 
                     ABCD_Set, 
                     Ada.Strings.Inside,
                     Slice_Start,
                     Slice_End);

      if Slice_Start /= 2  or Slice_End /= 13 then
         Report.Failed("Incorrect result from Find_Token - 3");
      end if;

      ASF.Find_Token("efghijklabcdabcd",
                     ABCD_Set,
                     Ada.Strings.Outside,
                     Slice_Start,
                     Slice_End);

      if Slice_Start /= 1  or Slice_End /= 8 then
         Report.Failed("Incorrect result from Find_Token - 4");
      end if;

      ASF.Find_Token("abcdefgabcdabcd",
                     ABCD_Set,
                     Ada.Strings.Outside,
                     Slice_Start,
                     Slice_End);

      if Slice_Start /= 5  or Slice_End /= 7 then
         Report.Failed("Incorrect result from Find_Token - 5");
      end if;

      ASF.Find_Token("abcdcbabcdcba",
                     ABCD_Set,
                     Ada.Strings.Inside,
                     Slice_Start,
                     Slice_End);

      if Slice_Start /= 1  or Slice_End /= 13 then
         Report.Failed("Incorrect result from Find_Token - 6");
      end if;


   exception
      when others => Report.Failed("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA4004;
