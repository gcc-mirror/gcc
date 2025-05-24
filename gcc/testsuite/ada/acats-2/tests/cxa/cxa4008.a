-- CXA4008.A
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
--      Check that the subprograms defined in package Ada.Strings.Bounded are
--      available, and that they produce correct results, especially under
--      conditions where truncation of the result is required.  Specifically, 
--      check the subprograms Append, Count with non-Identity maps, Index with 
--      non-Identity maps, Index with Set parameters, Insert (function and
--      procedure), Replace_Slice (function and procedure), To_Bounded_String, 
--      and Translate.
--
-- TEST DESCRIPTION:
--      This test, in conjunction with tests CXA4006, CXA4007, and CXA4009, 
--      will provide coverage of the most common usages of the functionality
--      found in the Ada.Strings.Bounded package.  It deals in large part
--      with truncation effects and options.  This test contains many small, 
--      specific test cases, situations that are often difficult to generate 
--      in large numbers in an application-based test.  These cases represent 
--      specific usage paradigms in-the-small.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      10 Apr 95   SAIC    Corrected acceptance condition of subtest for
--                          Function Append with Truncation = Left.
--      31 Oct 95   SAIC    Update and repair for ACVC 2.0.1.
--
--!

with Report;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Bounded;
with Ada.Strings.Maps;

procedure CXA4008 is

begin

   Report.Test("CXA4008", "Check that the subprograms defined in "      &
                          "package Ada.Strings.Bounded are available, " &
                          "and that they produce correct results, "     &
                          "especially under conditions where "          &
                          "truncation of the result is required");

   Test_Block:
   declare

      package AS   renames Ada.Strings;
      package ASB  renames Ada.Strings.Bounded;
      package ASC  renames Ada.Strings.Maps.Constants;
      package Maps renames Ada.Strings.Maps;

      package B10 is new ASB.Generic_Bounded_Length(Max => 10);
      use type B10.Bounded_String;

      Result_String : B10.Bounded_String;
      Test_String   : B10.Bounded_String;
      AtoE_Bnd_Str  : B10.Bounded_String := B10.To_Bounded_String("abcde");
      FtoJ_Bnd_Str  : B10.Bounded_String := B10.To_Bounded_String("fghij");
      AtoJ_Bnd_Str  : B10.Bounded_String := 
                        B10.To_Bounded_String("abcdefghij");

      Location     : Natural := 0;
      Total_Count  : Natural := 0;

      CD_Set       : Maps.Character_Set := Maps.To_Set("cd"); 

      AB_to_YZ_Map : Maps.Character_Mapping :=
                       Maps.To_Mapping(From =>  "ab",  To => "yz");

      CD_to_XY_Map : Maps.Character_Mapping :=
                       Maps.To_Mapping(From =>  "cd",  To => "xy");


   begin
      -- Function To_Bounded_String with Truncation
      -- Evaluate the function Append with parameters that will 
      -- cause the truncation of the result.

      -- Drop = Error (default case, Length_Error will be raised)

      begin
         Test_String := 
           B10.To_Bounded_String("Much too long for this bounded string");
         Report.Failed("Length Error not raised by To_Bounded_String");
      exception
         when AS.Length_Error => null;  -- Expected exception raised.
         when others          =>
           Report.Failed("Incorrect exception raised by To_Bounded_String");
      end;

      -- Drop = Left 

      Test_String := B10.To_Bounded_String(Source => "abcdefghijklmn",
                                           Drop   => Ada.Strings.Left);

      if Test_String /= B10.To_Bounded_String("efghijklmn") then
         Report.Failed
           ("Incorrect result from To_Bounded_String, Drop = Left");
      end if;

      -- Drop = Right 

      Test_String := B10.To_Bounded_String(Source => "abcdefghijklmn",
                                           Drop   => Ada.Strings.Right);

      if not(Test_String = AtoJ_Bnd_Str) then
         Report.Failed
           ("Incorrect result from To_Bounded_String, Drop = Right");
      end if;




      -- Function Append with Truncation
      -- Evaluate the function Append with parameters that will 
      -- cause the truncation of the result.

      -- Drop = Error (default case, Length_Error will be raised)

      begin
         -- Append (Bnd Str, Bnd Str);
         Result_String := 
           B10.Append(B10.To_Bounded_String("abcde"),
                      B10.To_Bounded_String("fghijk")); -- 11 char
         Report.Failed("Length_Error not raised by Append - 1");
      exception
         when AS.Length_Error => null;   -- OK, correct exception raised.
         when others          =>
            Report.Failed("Incorrect exception raised by Append - 1");
      end;

      begin
         -- Append (Str, Bnd Str);
         Result_String := B10.Append(B10.To_String(AtoE_Bnd_Str),
                                     B10.To_Bounded_String("fghijk"),
                                     AS.Error); 
         Report.Failed("Length_Error not raised by Append - 2");
      exception
         when AS.Length_Error => null;   -- OK, correct exception raised.
         when others          =>
            Report.Failed("Incorrect exception raised by Append - 2");
      end;

      begin
         -- Append (Bnd Str, Char);
         Result_String := 
           B10.Append(B10.To_Bounded_String("abcdefghij"), 'k');
         Report.Failed("Length_Error not raised by Append - 3");
      exception
         when AS.Length_Error => null;   -- OK, correct exception raised.
         when others          =>
            Report.Failed("Incorrect exception raised by Append - 3");
      end;

      -- Drop = Left 

      -- Append (Bnd Str, Bnd Str)
      Result_String := B10.Append(B10.To_Bounded_String("abcdefgh"), -- 8 chs
                                  B10.To_Bounded_String("ijklmn"),   -- 6 chs
                                  Ada.Strings.Left);

      if Result_String /= B10.To_Bounded_String("efghijklmn") then -- 10 chars
         Report.Failed("Incorrect truncation performed by Append - 4");
      end if;

      -- Append (Bnd Str, Str)
      Result_String := 
        B10.Append(B10.To_Bounded_String("abcdefghij"), 
                   "xyz",
                   Ada.Strings.Left);

      if Result_String /= B10.To_Bounded_String("defghijxyz") then
         Report.Failed("Incorrect truncation performed by Append - 5");
      end if;

      -- Append (Char, Bnd Str)

      Result_String := B10.Append('A', 
                                  B10.To_Bounded_String("abcdefghij"),
                                  Ada.Strings.Left);

      if Result_String /= B10.To_Bounded_String("abcdefghij") then
         Report.Failed("Incorrect truncation performed by Append - 6");
      end if;

      -- Drop = Right 

      -- Append (Bnd Str, Bnd Str)
      Result_String := B10.Append(FtoJ_Bnd_Str, 
                                  AtoJ_Bnd_Str,
                                  Ada.Strings.Right);

      if Result_String /= B10.To_Bounded_String("fghijabcde") then
         Report.Failed("Incorrect truncation performed by Append - 7");
      end if;

      -- Append (Str, Bnd Str)
      Result_String := B10.Append(B10.To_String(AtoE_Bnd_Str), 
                                  AtoJ_Bnd_Str,
                                  Ada.Strings.Right);

      if Result_String /= B10.To_Bounded_String("abcdeabcde") then
         Report.Failed("Incorrect truncation performed by Append - 8");
      end if;

      -- Append (Char, Bnd Str)
      Result_String := B10.Append('A', AtoJ_Bnd_Str, Ada.Strings.Right);

      if Result_String /= B10.To_Bounded_String("Aabcdefghi") then
         Report.Failed("Incorrect truncation performed by Append - 9");
      end if;


      -- Function Index with non-Identity map.
      -- Evaluate the function Index with a non-identity map 
      -- parameter which will cause mapping of the source parameter 
      -- prior to the evaluation of the index position search.

      Location := B10.Index(Source  => AtoJ_Bnd_Str,   -- "abcdefghij"
                            Pattern => "xy",
                            Going   => Ada.Strings.Forward,
                            Mapping => CD_to_XY_Map);  -- change "cd" to "xy"

      if Location /= 3 then
         Report.Failed("Incorrect result from Index, non-Identity map - 1");
      end if;

      Location := B10.Index(B10.To_Bounded_String("AND IF MAN"),
                            "an",
                            Ada.Strings.Backward,
                            ASC.Lower_Case_Map); 

      if Location /= 9 then
         Report.Failed("Incorrect result from Index, non-Identity map - 2");
      end if;

      Location := B10.Index(Source  => B10.To_Bounded_String("The the"),
                            Pattern => "the",
                            Going   => Ada.Strings.Forward,
                            Mapping => ASC.Lower_Case_Map); 

      if Location /= 1 then
         Report.Failed("Incorrect result from Index, non-Identity map - 3");
      end if;


      if B10.Index(B10.To_Bounded_String("abcd"),        -- Pattern = Source
                   "abcd")                       /= 1 or
         B10.Index(B10.To_Bounded_String("abc"),         -- Pattern < Source
                   "abcd")                       /= 0 or
         B10.Index(B10.Null_Bounded_String,              -- Source = Null
                   "abc")                        /= 0 
      then
         Report.Failed("Incorrect result from Index with string patterns");
      end if;


      -- Function Index (for Sets).
      -- This version of Index uses Sets as the basis of the search.

      -- Test = Inside, Going = Forward  (Default case).
      Location := 
        B10.Index(Source => B10.To_Bounded_String("abcdeabcde"),
                  Set    => CD_Set,  -- set containing 'c' and 'd'
                  Test   => Ada.Strings.Inside,
                  Going  => Ada.Strings.Forward);

      if not (Location = 3) then     -- position of first 'c' in source.
         Report.Failed("Incorrect result from Index using Sets - 1");
      end if;

      -- Test = Inside, Going = Backward.
      Location := 
        B10.Index(Source => B10."&"(AtoE_Bnd_Str, AtoE_Bnd_Str), 
                  Set    => CD_Set,  -- set containing 'c' and 'd'
                  Test   => Ada.Strings.Inside,
                  Going  => Ada.Strings.Backward);

      if not (Location = 9) then   -- position of last 'd' in source.
         Report.Failed("Incorrect result from Index using Sets - 2");
      end if;

      -- Test = Outside, Going = Forward.
      Location := B10.Index(B10.To_Bounded_String("deddacd"),  
                            CD_Set,
                            Test  => Ada.Strings.Outside,
                            Going => Ada.Strings.Forward);

      if Location /= 2  then  -- position of 'e' in source.
         Report.Failed("Incorrect result from Index using Sets - 3");
      end if;

      -- Test = Outside, Going = Backward.
      Location := B10.Index(B10.To_Bounded_String("deddacd"),
                            CD_Set,
                            Ada.Strings.Outside,
                            Ada.Strings.Backward);

      if Location /= 5 then                      -- correct position of 'a'.
         Report.Failed("Incorrect result from Index using Sets - 4");
      end if;

      if B10.Index(B10.To_Bounded_String("cd"),        -- Source = Set
                   CD_Set)                     /= 1 or
         B10.Index(B10.To_Bounded_String("c"),         -- Source < Set
                   CD_Set)                     /= 1 or
         B10.Index(B10.Null_Bounded_String,            -- Source = Null
                   CD_Set)                     /= 0 or
         B10.Index(AtoE_Bnd_Str,                       -- "abcde"
                   Maps.Null_Set)              /= 0 or -- Null set
         B10.Index(AtoE_Bnd_Str,
                   Maps.To_Set('x'))           /= 0    -- No match.
      then
         Report.Failed("Incorrect result from Index using Sets - 5");
      end if;


      -- Function Count with non-Identity mapping.
      -- Evaluate the function Count with a non-identity map 
      -- parameter which will cause mapping of the source parameter 
      -- prior to the evaluation of the number of matching patterns.

      Total_Count := 
        B10.Count(Source  => B10.To_Bounded_String("abbabaabab"),
                  Pattern => "yz",
                  Mapping => AB_to_YZ_Map);

      if Total_Count /= 4 then                                                 
         Report.Failed
           ("Incorrect result from function Count, non-Identity map - 1");
      end if;

      -- And a few with identity maps as well.

      if B10.Count(B10.To_Bounded_String("ABABABABAB"),
                   "ABA",
                   Maps.Identity)                       /= 2 or
         B10.Count(B10.To_Bounded_String("ADCBADABCD"),
                   "AB",
                   Maps.To_Mapping("CD", "AB"))         /= 5 or
         B10.Count(B10.To_Bounded_String("aaaaaaaaaa"),
                   "aaa")                               /= 3 or
         B10.Count(B10.To_Bounded_String("XX"),         -- Source < Pattern
                   "XXX",
                   Maps.Identity)                       /= 0 or
         B10.Count(AtoE_Bnd_Str,                        -- Source = Pattern
                   "abcde")                             /= 1 or
         B10.Count(B10.Null_Bounded_String,             -- Source = Null
                   " ")                                 /= 0
      then
         Report.Failed
           ("Incorrect result from function Count, w,w/o mapping");
      end if;
                                         

      -- Procedure Translate

      -- Partial mapping of source.

      Test_String := B10.To_Bounded_String("abcdeabcab");

      B10.Translate(Source => Test_String, Mapping => AB_to_YZ_Map);

      if Test_String /= B10.To_Bounded_String("yzcdeyzcyz") then
         Report.Failed("Incorrect result from procedure Translate - 1");
      end if;

      -- Total mapping of source.

      Test_String := B10.To_Bounded_String("abbaaababb");

      B10.Translate(Source => Test_String, Mapping => ASC.Upper_Case_Map);

      if Test_String /= B10.To_Bounded_String("ABBAAABABB") then
         Report.Failed("Incorrect result from procedure Translate - 2");
      end if;

      -- No mapping of source.

      Test_String := B10.To_Bounded_String("xyzsypcc");

      B10.Translate(Source => Test_String, Mapping => AB_to_YZ_Map);

      if Test_String /= B10.To_Bounded_String("xyzsypcc") then
         Report.Failed("Incorrect result from procedure Translate - 3");
      end if;

      -- Map > 2 characters, partial mapping.

      Test_String := B10.To_Bounded_String("have faith");

      B10.Translate(Test_String, 
                    Maps.To_Mapping("aeiou", "AEIOU"));

      if Test_String /= B10.To_Bounded_String("hAvE fAIth") then
         Report.Failed("Incorrect result from procedure Translate - 4");
      end if;


      -- Function Replace_Slice
      -- Evaluate function Replace_Slice with 
      -- a variety of Truncation options.

      -- Drop = Error (Default)

      begin
         Test_String   := AtoJ_Bnd_Str;
         Result_String := 
           B10.Replace_Slice(Source => Test_String,  -- "abcdefghij"
                             Low    => 3,
                             High   => 5,            -- 3-5, 3 chars.
                             By     => "xxxxxx");    -- more than 3.
         Report.Failed("Length_Error not raised by Function Replace_Slice");
      exception
         when AS.Length_Error => null;   -- Correct exception raised.
         when others          =>
            Report.Failed
              ("Incorrect exception raised by Function Replace_Slice");
      end;

      -- Drop = Left                                                          

      Result_String := 
        B10.Replace_Slice(Source => Test_String,  -- "abcdefghij"
                          Low    =>  7,
                          High   => 10,           -- 7-10, 4 chars.
                          By     => "xxxxxx",     --  6 chars.
                          Drop   => Ada.Strings.Left);

      if Result_String /= B10.To_Bounded_String("cdefxxxxxx") then -- drop a,b
         Report.Failed
           ("Incorrect result from Function Replace Slice, Drop = Left");
      end if;

      -- Drop = Right

      Result_String := 
        B10.Replace_Slice(Source => Test_String, -- "abcdefghij"
                          Low    =>  2,
                          High   =>  5,          -- 2-5, 4 chars.
                          By     => "xxxxxx",    --  6 chars.
                          Drop   => Ada.Strings.Right);

      if Result_String /= B10.To_Bounded_String("axxxxxxfgh") then -- drop i,j
         Report.Failed
           ("Incorrect result from Function Replace Slice, Drop = Right");
      end if;

      -- Low = High = Source'Last, "By" length = 1.

      if B10.Replace_Slice(AtoE_Bnd_Str,
                           B10.To_String(AtoE_Bnd_Str)'Last,
                           B10.To_String(AtoE_Bnd_Str)'Last,
                           "X",
                           Ada.Strings.Error)  /=
         B10.To_Bounded_String("abcdX")
      then
         Report.Failed("Incorrect result from Function Replace_Slice");
      end if;



      -- Procedure Replace_Slice
      -- Evaluate procedure Replace_Slice with 
      -- a variety of Truncation options.

      -- Drop = Error (Default)

      begin
         Test_String := AtoJ_Bnd_Str;
         B10.Replace_Slice(Source => Test_String,  -- "abcdefghij"
                           Low    => 3,
                           High   => 5,            -- 3-5, 3 chars.
                           By     => "xxxxxx");    -- more than 3.
         Report.Failed("Length_Error not raised by Procedure Replace_Slice");
      exception
         when AS.Length_Error => null;   -- Correct exception raised.
         when others          =>
            Report.Failed
              ("Incorrect exception raised by Procedure Replace_Slice");
      end;

      -- Drop = Left                                                          

      Test_String := AtoJ_Bnd_Str;
      B10.Replace_Slice(Source => Test_String,  -- "abcdefghij"
                        Low    =>  7,
                        High   =>  9,           -- 7-9, 3 chars.
                        By     => "xxxxx",      --  5 chars.
                        Drop   => Ada.Strings.Left);

      if Test_String /= B10.To_Bounded_String("cdefxxxxxj") then -- drop a,b
         Report.Failed
           ("Incorrect result from Procedure Replace Slice, Drop = Left");
      end if;

      -- Drop = Right

      Test_String := AtoJ_Bnd_Str;
      B10.Replace_Slice(Source => Test_String,  -- "abcdefghij"
                        Low    =>  1,
                        High   =>  3,           -- 1-3, 3chars.
                        By     => "xxxx",       --  4 chars.
                        Drop   => Ada.Strings.Right);

      if Test_String /= B10.To_Bounded_String("xxxxdefghi") then  -- drop j
         Report.Failed
           ("Incorrect result from Procedure Replace Slice, Drop = Right");
      end if;

      -- High = Source'First, Low > High (Insert before Low).

      Test_String := AtoE_Bnd_Str;
      B10.Replace_Slice(Source => Test_String,  -- "abcde"
                        Low    => B10.To_String(Test_String)'Last,
                        High   => B10.To_String(Test_String)'First,
                        By     => "XXXX",       --  4 chars.
                        Drop   => Ada.Strings.Right);

      if Test_String /= B10.To_Bounded_String("abcdXXXXe") then 
         Report.Failed
           ("Incorrect result from Procedure Replace Slice");
      end if;



      -- Function Insert with Truncation
      -- Drop = Error (Default).

      begin
         Result_String := 
           B10.Insert(Source   => AtoJ_Bnd_Str,    -- "abcdefghij"
                      Before   => 2,
                      New_Item => "xyz");
         Report.Failed("Length_Error not raised by Function Insert");
      exception
         when AS.Length_Error => null;  -- Correct exception raised.
         when others          => 
            Report.Failed("Incorrect exception raised by Function Insert");
      end;

      -- Drop = Left

      Result_String :=
        B10.Insert(Source   => AtoJ_Bnd_Str,      -- "abcdefghij"
                   Before   => 5,
                   New_Item => "xyz",             -- 3 additional chars.
                   Drop     => Ada.Strings.Left);

      if B10.To_String(Result_String) /= "dxyzefghij" then   -- drop a, b, c
         Report.Failed("Incorrect result from Function Insert, Drop = Left");
      end if;

      -- Drop = Right

      Result_String := 
        B10.Insert(Source   => B10.To_Bounded_String("abcdef"),
                   Before   => 2,
                   New_Item => "vwxyz",             -- 5 additional chars.
                   Drop     => Ada.Strings.Right);

      if B10.To_String(Result_String) /= "avwxyzbcde" then     -- drop f.
         Report.Failed("Incorrect result from Function Insert, Drop = Right");
      end if;

      -- Additional cases.

      if B10.Insert(B10.To_Bounded_String("a"), 1, " B") /=   
         B10.To_Bounded_String(" Ba")                       or
         B10.Insert(B10.Null_Bounded_String, 1, "abcde") /=
         AtoE_Bnd_Str                                       or
         B10.Insert(B10.To_Bounded_String("ab"), 2, "")  /=
         B10.To_Bounded_String("ab")
      then
         Report.Failed("Incorrect result from Function Insert");
      end if;


      -- Procedure Insert

      -- Drop = Error (Default).
      begin
         Test_String := AtoJ_Bnd_Str;
         B10.Insert(Source   => Test_String,    -- "abcdefghij"
                    Before   => 9,
                    New_Item => "wxyz",
                    Drop     => Ada.Strings.Error);
         Report.Failed("Length_Error not raised by Procedure Insert");
      exception
         when AS.Length_Error => null;  -- Correct exception raised.
         when others          => 
            Report.Failed("Incorrect exception raised by Procedure Insert");
      end;

      -- Drop = Left
      
      Test_String := AtoJ_Bnd_Str;
      B10.Insert(Source   => Test_String,             -- "abcdefghij"
                 Before   => B10.Length(Test_String), -- before last char
                 New_Item => "xyz",                   -- 3 additional chars.
                 Drop     => Ada.Strings.Left);

      if B10.To_String(Test_String) /= "defghixyzj" then     -- drop a, b, c
         Report.Failed("Incorrect result from Procedure Insert, Drop = Left");
      end if;

      -- Drop = Right

      Test_String := AtoJ_Bnd_Str;
      B10.Insert(Source   => Test_String,
                 Before   => 4,
                 New_Item => "yz",             -- 2 additional chars.
                 Drop     => Ada.Strings.Right);

      if B10.To_String(Test_String) /= "abcyzdefgh" then     -- drop i,j
         Report.Failed
           ("Incorrect result from Procedure Insert, Drop = Right");
      end if;

      -- Before = Source'First, New_Item length = 1.

      Test_String := B10.To_Bounded_String(" abc ");
      B10.Insert(Test_String, 
                 B10.To_String(Test_String)'First,
                 "Z");

      if Test_String /= B10.To_Bounded_String("Z abc ") then
         Report.Failed("Incorrect result from Procedure Insert");
      end if;


   exception
      when others => Report.Failed("Exception raised in  Test_Block");
   end Test_Block;

   Report.Result;

end CXA4008;
