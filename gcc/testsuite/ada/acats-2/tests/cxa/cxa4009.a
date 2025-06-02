-- CXA4009.A
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
--      check the subprograms Overwrite (function and procedure), Delete,
--      Function Trim (blanks), Trim (Set characters, function and procedure),
--      Head, Tail, and Replicate (characters and strings).
--
-- TEST DESCRIPTION:
--      This test, in conjunction with tests CXA4006, CXA4007, and CXA4008, 
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
--      10 Apr 95   SAIC    Corrected errors in Procedure Overwrite subtests.
--      01 Nov 95   SAIC    Fixed bugs for ACVC 2.0.1.
--
--!

with Report;
with Ada.Strings.Bounded;
with Ada.Strings.Maps;

procedure CXA4009 is

begin

   Report.Test("CXA4009", "Check that the subprograms defined in "      &
                          "package Ada.Strings.Bounded are available, " &
                          "and that they produce correct results, "     &
                          "especially under conditions where "          &
                          "truncation of the result is required");

   Test_Block:
   declare

      package AS   renames Ada.Strings;
      package ASB  renames Ada.Strings.Bounded;
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
      XY_Set       : Maps.Character_Set := Maps.To_Set("xy"); 


   begin

      -- Function Overwrite with Truncation                 
      -- Drop = Error (Default).

      begin
         Test_String   := AtoJ_Bnd_Str;
         Result_String := 
           B10.Overwrite(Source   => Test_String, -- "abcdefghij"
                         Position => 9,
                         New_Item => "xyz",
                         Drop     => AS.Error);
         Report.Failed("Exception not raised by Function Overwrite");
      exception
         when AS.Length_Error => null;  -- Expected exception raised.
         when others          => 
           Report.Failed("Incorrect exception raised by Function Overwrite");
      end;

      -- Drop = Left
 
      Result_String :=
        B10.Overwrite(Source   => Test_String,  -- "abcdefghij"
                      Position => B10.Length(Test_String), -- 10
                      New_Item => "xyz",
                      Drop     => Ada.Strings.Left);

      if B10.To_String(Result_String) /= "cdefghixyz" then   -- drop a,b
         Report.Failed
           ("Incorrect result from Function Overwrite, Drop = Left");
      end if;

      -- Drop = Right

      Result_String := B10.Overwrite(Test_String,  -- "abcdefghij"
                                     3,
                                     "xxxyyyzzz",
                                     Ada.Strings.Right);

      if B10.To_String(Result_String) /= "abxxxyyyzz" then -- one 'z' dropped
         Report.Failed
           ("Incorrect result from Function Overwrite, Drop = Right");
      end if;

      -- Additional cases of function Overwrite.

      if B10.Overwrite(B10.To_Bounded_String("a"),      -- Source length = 1
                       1,
                       "  abc  ")                 /= 
         B10.To_Bounded_String("  abc  ")            or
         B10.Overwrite(B10.Null_Bounded_String,         -- Null source
                       1, 
                       "abcdefghij")              /=
         AtoJ_Bnd_Str                                or
         B10.Overwrite(AtoE_Bnd_Str, 
                       B10.To_String(AtoE_Bnd_Str)'First,
                       " ")                       /=      -- New_Item = 1
         B10.To_Bounded_String(" bcde")
      then
         Report.Failed("Incorrect result from Function Overwrite");
      end if;



      -- Procedure Overwrite                                 
      -- Correct usage, no truncation.

      Test_String := AtoE_Bnd_Str;   -- "abcde"
      B10.Overwrite(Test_String, 2, "xyz");

      if Test_String /= B10.To_Bounded_String("axyze") then
         Report.Failed("Incorrect result from Procedure Overwrite - 1");
      end if;

      Test_String := B10.To_Bounded_String("abc");
      B10.Overwrite(Test_String, 2, "");   -- New_Item is null string.

      if Test_String /= B10.To_Bounded_String("abc") then
         Report.Failed("Incorrect result from Procedure Overwrite - 2");
      end if;

      -- Drop = Error (Default).

      begin
         Test_String   := AtoJ_Bnd_Str;
         B10.Overwrite(Source   => Test_String, -- "abcdefghij"
                       Position => 8,
                       New_Item => "uvwxyz");
         Report.Failed("Exception not raised by Procedure Overwrite");
      exception
         when AS.Length_Error => null;  -- Expected exception raised.
         when others          => 
           Report.Failed("Incorrect exception raised by Procedure Overwrite");
      end;

      -- Drop = Left
 
      Test_String   := AtoJ_Bnd_Str;
      B10.Overwrite(Source   => Test_String,  -- "abcdefghij"
                    Position => B10.Length(Test_String) - 2, -- 8
                    New_Item => "uvwxyz",
                    Drop     => Ada.Strings.Left);

      if B10.To_String(Test_String) /= "defguvwxyz" then   -- drop a-c
         Report.Failed
           ("Incorrect result from Procedure Overwrite, Drop = Left");
      end if;

      -- Drop = Right

      Test_String   := AtoJ_Bnd_Str;
      B10.Overwrite(Test_String,  -- "abcdefghij"
                    3,
                    "xxxyyyzzz",
                    Ada.Strings.Right);

      if B10.To_String(Test_String) /= "abxxxyyyzz" then -- one 'z' dropped
         Report.Failed
           ("Incorrect result from Procedure Overwrite, Drop = Right");
      end if;



      -- Function Delete                                     

      if B10.Delete(Source  => AtoJ_Bnd_Str,   -- "abcdefghij"
                    From    => 3,
                    Through => 8)                               /=
         B10."&"(B10.Head(AtoJ_Bnd_Str, 2), 
                 B10.Tail(AtoJ_Bnd_Str, 2))                         or
         B10.Delete(AtoJ_Bnd_Str, 6, B10.Length(AtoJ_Bnd_Str))  /=
         AtoE_Bnd_Str                                               or
         B10.Delete(AtoJ_Bnd_Str, 1, 5)                         /= 
         FtoJ_Bnd_Str                                               or
         B10.Delete(AtoE_Bnd_Str, 4, 5)                         /=
         B10.Delete(AtoJ_Bnd_Str, 4, B10.Length(AtoJ_Bnd_Str)) 
      then
         Report.Failed("Incorrect result from Function Delete - 1");
      end if;

      if B10.Delete(B10.To_Bounded_String("a"), 1, 1)  /= 
         B10.Null_Bounded_String                           or
         B10.Delete(AtoE_Bnd_Str, 
                    5, 
                    B10.To_String(AtoE_Bnd_Str)'First) /=
         AtoE_Bnd_Str                                      or
         B10.Delete(AtoE_Bnd_Str,
                    B10.To_String(AtoE_Bnd_Str)'Last,
                    B10.To_String(AtoE_Bnd_Str)'Last)  /=
         B10.To_Bounded_String("abcd")
      then
         Report.Failed("Incorrect result from Function Delete - 2");
      end if;
      


      -- Function Trim                                       

      declare

         Text : B10.Bounded_String := B10.To_Bounded_String("Text");
         type Bnd_Array_Type is array (1..5) of B10.Bounded_String;
         Bnd_Array : Bnd_Array_Type :=
           (B10.To_Bounded_String("  Text"),
            B10.To_Bounded_String("Text    "),
            B10.To_Bounded_String("   Text   "),
            B10.To_Bounded_String("Text  Text"),   -- Ensure no inter-string
            B10.To_Bounded_String(" Text Text"));  -- trimming of blanks.

      begin

         for i in Bnd_Array_Type'Range loop
            case i is
               when 4 =>
                  if B10.Trim(Bnd_Array(i), AS.Both) /= 
                     Bnd_Array(i) then  -- no change
                     Report.Failed("Incorrect result from Function Trim - 4");
                  end if;
               when 5 =>
                  if B10.Trim(Bnd_Array(i), AS.Both) /= 
                     B10."&"(Text, B10."&"(' ', Text)) then
                     Report.Failed("Incorrect result from Function Trim - 5");
                  end if;
               when others =>
                  if B10.Trim(Bnd_Array(i), AS.Both) /= Text then
                     Report.Failed("Incorrect result from Function Trim - " &
                                    Integer'Image(i));
                  end if;
            end case;
         end loop;

      end;



      -- Function Trim using Sets                            

      -- Trim characters in sets from both sides of the bounded string.
      if B10.Trim(Source => B10.To_Bounded_String("ddabbaxx"),
                  Left   => CD_Set,
                  Right  => XY_Set)  /=
         B10.To_Bounded_String("abba")
      then
         Report.Failed
           ("Incorrect result from Fn Trim - Sets, Left & Right side - 1");
      end if;

      -- Ensure that the characters in the set provided as the actual to
      -- parameter Right are not trimmed from the left side of the bounded
      -- string; likewise for the opposite side.  Only "cd" trimmed from left
      -- side, and only "xy" trimmed from right side.

      if B10.Trim(B10.To_Bounded_String("cdxyabcdxy"), CD_Set, XY_Set) /=
         B10.To_Bounded_String("xyabcd")
      then
         Report.Failed
           ("Incorrect result from Fn Trim - Sets, Left & Right side - 2");
      end if;

      -- Ensure that characters contained in the sets are not trimmed from
      -- the "interior" of the bounded string, just the appropriate ends.

      if B10.Trim(B10.To_Bounded_String("cdabdxabxy"), CD_Set, XY_Set) /=
         B10.To_Bounded_String("abdxab")
      then
         Report.Failed
           ("Incorrect result from Fn Trim - Sets, Left & Right side - 3");
      end if;

      -- Trim characters in set from right side only.  No change to Left side.

      if B10.Trim(B10.To_Bounded_String("abxyzddcd"), XY_Set, CD_Set) /=
         B10.To_Bounded_String("abxyz")
      then
         Report.Failed
           ("Incorrect result from Fn Trim - Sets, Right side");
      end if;

      -- Trim no characters on either side of the bounded string.

      Result_String := B10.Trim(AtoJ_Bnd_Str, CD_Set, XY_Set);
      if Result_String /= AtoJ_Bnd_Str then
         Report.Failed("Incorrect result from Fn Trim - Sets, Neither side");
      end if;

      if B10.Trim(AtoE_Bnd_Str, Maps.Null_Set, Maps.Null_Set) /=
         AtoE_Bnd_Str                                            or
         B10.Trim(B10.To_Bounded_String("dcddcxyyxx"),
                  CD_Set,
                  XY_Set)                                     /=
         B10.Null_Bounded_String
      then
         Report.Failed("Incorrect result from Function Trim");
      end if;



      -- Procedure Trim using Sets                           

      -- Trim characters in sets from both sides of the bounded string.

      Test_String := B10.To_Bounded_String("dcabbayx");
      B10.Trim(Source => Test_String,
               Left   => CD_Set,
               Right  => XY_Set);

      if Test_String /= B10.To_Bounded_String("abba") then
         Report.Failed
           ("Incorrect result from Proc Trim - Sets, Left & Right side - 1");
      end if;

      -- Ensure that the characters in the set provided as the actual to
      -- parameter Right are not trimmed from the left side of the bounded
      -- string; likewise for the opposite side.  Only "cd" trimmed from left
      -- side, and only "xy" trimmed from right side.

      Test_String := B10.To_Bounded_String("cdxyabcdxy");
      B10.Trim(Test_String, CD_Set, XY_Set);

      if Test_String /= B10.To_Bounded_String("xyabcd") then
         Report.Failed
           ("Incorrect result from Proc Trim - Sets, Left & Right side - 2");
      end if;

      -- Ensure that characters contained in the sets are not trimmed from
      -- the "interior" of the bounded string, just the appropriate ends.

      Test_String := B10.To_Bounded_String("cdabdxabxy");
      B10.Trim(Test_String, CD_Set, XY_Set);

      if not (Test_String = B10.To_Bounded_String("abdxab")) then
         Report.Failed
           ("Incorrect result from Proc Trim - Sets, Left & Right side - 3");
      end if;

      -- Trim characters in set from Left side only.  No change to Right side.

      Test_String := B10.To_Bounded_String("cccdabxyz");
      B10.Trim(Test_String, CD_Set, XY_Set);

      if Test_String /= B10.To_Bounded_String("abxyz") then
         Report.Failed
           ("Incorrect result from Proc Trim for Sets, Left side only");
      end if;

      -- Trim no characters on either side of the bounded string.

      Test_String := AtoJ_Bnd_Str;
      B10.Trim(Test_String, CD_Set, CD_Set);

      if Test_String /= AtoJ_Bnd_Str then
         Report.Failed("Incorrect result from Proc Trim-Sets, Neither side");
      end if;



      -- Function Head with Truncation                       
      -- Drop = Error (Default).

      begin
         Result_String := B10.Head(Source => AtoJ_Bnd_Str,   -- max length
                                   Count  => B10.Length(AtoJ_Bnd_Str) + 1,
                                   Pad    => 'X');
         Report.Failed("Length_Error not raised by Function Head");
      exception
         when AS.Length_Error => null;  -- Expected exception raised.
         when others          => 
            Report.Failed("Incorrect exception raised by Function Head");
      end;

      -- Drop = Left

      -- Pad characters (5) are appended to the right end of the string 
      -- (which is initially at its maximum length), then the first five
      -- characters of the intermediate result are dropped to conform to
      -- the maximum size limit of the bounded string (10).

      Result_String := B10.Head(B10.To_Bounded_String("ABCDEFGHIJ"),
                                15,
                                'x',
                                Ada.Strings.Left);

      if Result_String /= B10.To_Bounded_String("FGHIJxxxxx") then
         Report.Failed("Incorrect result from Function Head, Drop = Left");
      end if;

      -- Drop = Right

      -- Pad characters (6) are appended to the left end of the string 
      -- (which is initially at one less than its maximum length), then the 
      -- last five characters of the intermediate result are dropped 
      -- (which in this case are the pad characters) to conform to the 
      -- maximum size limit of the bounded string (10).

      Result_String := B10.Head(B10.To_Bounded_String("ABCDEFGHI"),
                                15,
                                'x',
                                Ada.Strings.Right);

      if Result_String /= B10.To_Bounded_String("ABCDEFGHIx") then
         Report.Failed("Incorrect result from Function Head, Drop = Right");
      end if;

      -- Additional cases.

      if B10.Head(B10.Null_Bounded_String, 5) /=
         B10.To_Bounded_String("     ")          or
         B10.Head(AtoE_Bnd_Str,
                  B10.Length(AtoE_Bnd_Str))   /=
         AtoE_Bnd_Str
      then
         Report.Failed("Incorrect result from Function Head");
      end if;



      -- Function Tail with Truncation                       
      -- Drop = Error (Default Case)

      begin
         Result_String := B10.Tail(Source => AtoJ_Bnd_Str,   -- max length
                                   Count  => B10.Length(AtoJ_Bnd_Str) + 1,
                                   Pad    => Ada.Strings.Space,
                                   Drop   => Ada.Strings.Error);
         Report.Failed("Length_Error not raised by Function Tail");
      exception
         when AS.Length_Error => null;  -- Expected exception raised.
         when others          => 
            Report.Failed("Incorrect exception raised by Function Tail");
      end;

      -- Drop = Left

      -- Pad characters (5) are appended to the left end of the string 
      -- (which is initially at two less than its maximum length), then 
      -- the first three characters of the intermediate result (in this 
      -- case, 3 pad characters) are dropped.

      Result_String := B10.Tail(B10.To_Bounded_String("ABCDEFGH"), -- 8 ch
                                13,
                                'x',
                                Ada.Strings.Left);

      if Result_String /= B10.To_Bounded_String("xxABCDEFGH") then
         Report.Failed("Incorrect result from Function Tail, Drop = Left");
      end if;

      -- Drop = Right

      -- Pad characters (3) are appended to the left end of the string 
      -- (which is initially at its maximum length), then the last three
      -- characters of the intermediate result are dropped.

      Result_String := B10.Tail(B10.To_Bounded_String("ABCDEFGHIJ"),
                                13,
                                'x',
                                Ada.Strings.Right);

      if Result_String /= B10.To_Bounded_String("xxxABCDEFG") then
         Report.Failed("Incorrect result from Function Tail, Drop = Right");
      end if;

      -- Additional cases.

      if B10.Tail(B10.Null_Bounded_String, 3, ' ')    /=
         B10.To_Bounded_String("   ")                    or
         B10.Tail(AtoE_Bnd_Str, 
                  B10.To_String(AtoE_Bnd_Str)'First)  /=
         B10.To_Bounded_String("e")                      
      then
         Report.Failed("Incorrect result from Function Tail");
      end if;



      -- Function Replicate (#, Char) with Truncation        
      -- Drop = Error (Default).

      begin
         Result_String := B10.Replicate(Count => B10.Max_Length + 5,
                                        Item  => 'A',
                                        Drop  => AS.Error);
         Report.Failed
           ("Length_Error not raised by Replicate for characters");
      exception
         when AS.Length_Error => null;  -- Expected exception raised.
         when others          => 
           Report.Failed
             ("Incorrect exception raised by Replicate for characters");
      end;

      -- Drop = Left, Right
      -- Since this version of Replicate uses character parameters, the 
      -- result after truncation from left or right will appear the same.
      -- The result will be a 10 character bounded string, composed of 10
      -- "Item" characters.

      if B10.Replicate(Count => 20, Item => 'A', Drop => Ada.Strings.Left) /=
         B10.Replicate(15, 'A', Ada.Strings.Right)
      then
         Report.Failed("Incorrect result from Replicate for characters - 1");
      end if;

      -- Blank-filled 10 character bounded strings.

      if B10.Replicate(B10.Max_Length + 1, ' ', Drop => Ada.Strings.Left) /=
         B10.Replicate(B10.Max_Length, Ada.Strings.Space)
      then
         Report.Failed("Incorrect result from Replicate for characters - 2");
      end if;

      -- Additional cases.

      if B10.Replicate(0, 'a')  /= B10.Null_Bounded_String    or
         B10.Replicate(1, 'a')  /= B10.To_Bounded_String("a")    
      then
         Report.Failed("Incorrect result from Replicate for characters - 3");
      end if;



      -- Function Replicate (#, String) with Truncation      
      -- Drop = Error (Default).

      begin
         Result_String := B10.Replicate(Count => 5,  -- result would be 15.
                                        Item  => "abc");
         Report.Failed
           ("Length_Error not raised by Replicate for strings");
      exception
         when AS.Length_Error => null;  -- Expected exception raised.
         when others          => 
           Report.Failed
             ("Incorrect exception raised by Replicate for strings");
      end;

      -- Drop = Left

      Result_String := B10.Replicate(3, "abcd", Drop => Ada.Strings.Left);

      if Result_String /= B10.To_Bounded_String("cdabcdabcd") then
         Report.Failed
           ("Incorrect result from Replicate for strings, Drop = Left");
      end if;

      -- Drop = Right

      Result_String := B10.Replicate(3, "abcd", Drop => Ada.Strings.Right);

      if Result_String /= B10.To_Bounded_String("abcdabcdab") then
         Report.Failed
           ("Incorrect result from Replicate for strings, Drop = Right");
      end if;

      -- Additional cases.
      
      if B10.Replicate(10, "X")  /= B10.To_Bounded_String("XXXXXXXXXX") or
         B10.Replicate(10, "")   /= B10.Null_Bounded_String             or
         B10.Replicate( 0, "ab") /= B10.Null_Bounded_String
      then
         Report.Failed("Incorrect result from Replicate for strings");
      end if;


   exception
      when others => Report.Failed("Exception raised in  Test_Block");
   end Test_Block;

   Report.Result;

end CXA4009;
