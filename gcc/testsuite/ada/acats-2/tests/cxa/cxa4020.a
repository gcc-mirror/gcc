-- CXA4020.A
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
--      Check that the subprograms defined in package Ada.Strings.Wide_Bounded
--      are available, and that they produce correct results, especially under
--      conditions where truncation of the result is required.  Specifically, 
--      check the subprograms Overwrite (function and procedure), Delete,
--      Function Trim (blanks), Trim (Set wide characters, function and
--      procedure), Head, Tail, and Replicate (wide characters and wide 
--      strings).
--
-- TEST DESCRIPTION:
--      This test, in conjunction with tests CXA4017, CXA4018, CXA4019, 
--      will provide coverage of the most common usages of the functionality
--      found in the Ada.Strings.Wide_Bounded package.  It deals in large part
--      with truncation effects and options.  This test contains many small, 
--      specific test cases, situations that are often difficult to generate 
--      in large numbers in an application-based test.  These cases represent 
--      specific usage paradigms in-the-small.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      22 Dec 94   SAIC    Changed obsolete constant to Strings.Wide_Space.
--      13 Apr 95   SAIC    Corrected certain subtest acceptance conditions.
--
--!

with Report;
with Ada.Characters.Handling;
with Ada.Strings.Wide_Bounded;
with Ada.Strings.Wide_Maps;

procedure CXA4020 is

   -- The following two functions are used to translate character and string
   -- values to "Wide" values.  They will be applied to all the Wide_Bounded
   -- subprogram parameters to simulate the use of Wide_Characters and 
   -- Wide_Strings in actual practice. Blanks are translated to Wide_Character
   -- blanks and all other characters are translated into Wide_Characters with
   -- position values 256 greater than their (narrow) character position
   -- values.

   function Translate (Ch : Character) return Wide_Character is
      C : Character := Ch;
   begin
      if Ch = ' ' then
         return Ada.Characters.Handling.To_Wide_Character(C);
      else
         return Wide_Character'Val(Character'Pos(Ch) + 
                Character'Pos(Character'Last) + 1);
      end if;
   end Translate;


   function Translate (Str : String) return Wide_String is
      WS : Wide_String(Str'First..Str'Last);
   begin
      for i in Str'First..Str'Last loop
         WS(i) := Translate(Str(i));
      end loop;
      return WS;
   end Translate;


begin

   Report.Test("CXA4020", "Check that the subprograms defined in "      &
                          "package Ada.Strings.Wide_Bounded are "       &
                          "available, and that they produce correct "   &
                          "results, especially under conditions where " &
                          "truncation of the result is required");

   Test_Block:
   declare

      package AS   renames Ada.Strings;
      package ASW  renames Ada.Strings.Wide_Bounded;
      package Maps renames Ada.Strings.Wide_Maps;

      package B10 is new ASW.Generic_Bounded_Length(Max => 10);
      use type B10.Bounded_Wide_String;

      Result_String : B10.Bounded_Wide_String;
      Test_String   : B10.Bounded_Wide_String;
      AtoE_Bnd_Str  : B10.Bounded_Wide_String := 
                        B10.To_Bounded_Wide_String(Translate("abcde"));
      FtoJ_Bnd_Str  : B10.Bounded_Wide_String := 
                        B10.To_Bounded_Wide_String(Translate("fghij"));
      AtoJ_Bnd_Str  : B10.Bounded_Wide_String := 
                        B10.To_Bounded_Wide_String(Translate("abcdefghij"));

      Location     : Natural := 0;
      Total_Count  : Natural := 0;

      CD_Set       : Maps.Wide_Character_Set := Maps.To_Set(Translate("cd")); 
      XY_Set       : Maps.Wide_Character_Set := Maps.To_Set(Translate("xy")); 


   begin

      -- Function Overwrite with Truncation                  
      -- Drop = Error (Default).

      begin
         Test_String   := AtoJ_Bnd_Str;
         Result_String := 
           B10.Overwrite(Source   => Test_String, -- "abcdefghij"
                         Position => 9,
                         New_Item => Translate("xyz"),
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
                      New_Item => Translate("xyz"),
                      Drop     => Ada.Strings.Left);

      if B10.To_Wide_String(Result_String) /= 
         Translate("cdefghixyz") then   -- drop a,b
         Report.Failed
           ("Incorrect result from Function Overwrite, Drop = Left");
      end if;

      -- Drop = Right

      Result_String := B10.Overwrite(Test_String,  -- "abcdefghij"
                                     3,
                                     Translate("xxxyyyzzz"),
                                     Ada.Strings.Right);

      if B10.To_Wide_String(Result_String) /= 
         Translate("abxxxyyyzz") 
      then
         Report.Failed
           ("Incorrect result from Function Overwrite, Drop = Right");
      end if;

      -- Additional cases of function Overwrite.

      if B10.Overwrite(B10.To_Bounded_Wide_String(Translate("a")),      
                       1,                                 -- Source length = 1
                       Translate("  abc  "))              /= 
         B10.To_Bounded_Wide_String(Translate("  abc  "))       or
         B10.Overwrite(B10.Null_Bounded_Wide_String,      -- Null source
                       1, 
                       Translate("abcdefghij"))           /=
         AtoJ_Bnd_Str                                or
         B10.Overwrite(AtoE_Bnd_Str, 
                       B10.To_Wide_String(AtoE_Bnd_Str)'First,
                       Translate(" "))                    /=  -- New_Item = 1
         B10.To_Bounded_Wide_String(Translate(" bcde"))
      then
         Report.Failed("Incorrect result from Function Overwrite");
      end if;



      -- Procedure Overwrite                                 
      -- Correct usage, no truncation.

      Test_String := AtoE_Bnd_Str;   -- "abcde"
      B10.Overwrite(Test_String, 2, Translate("xyz"));

      if Test_String /= B10.To_Bounded_Wide_String(Translate("axyze")) then
         Report.Failed("Incorrect result from Procedure Overwrite - 1");
      end if;

      Test_String := B10.To_Bounded_Wide_String(Translate("abc"));
      B10.Overwrite(Test_String, 2, "");   -- New_Item is null string.

      if Test_String /= B10.To_Bounded_Wide_String(Translate("abc")) then
         Report.Failed("Incorrect result from Procedure Overwrite - 2");
      end if;

      -- Drop = Error (Default).

      begin
         Test_String   := AtoJ_Bnd_Str;
         B10.Overwrite(Source   => Test_String, -- "abcdefghij"
                       Position => 8,
                       New_Item => Translate("uvwxyz"));
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
                    New_Item => Translate("uvwxyz"),
                    Drop     => Ada.Strings.Left);

      if B10.To_Wide_String(Test_String) /= 
         Translate("defguvwxyz")
      then  
         Report.Failed
           ("Incorrect result from Procedure Overwrite, Drop = Left");
      end if;

      -- Drop = Right

      Test_String   := AtoJ_Bnd_Str;
      B10.Overwrite(Test_String,  -- "abcdefghij"
                    3,
                    Translate("xxxyyyzzz"),
                    Ada.Strings.Right);

      if B10.To_Wide_String(Test_String) /= Translate("abxxxyyyzz") then
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
         FtoJ_Bnd_Str                                               
      then
         Report.Failed("Incorrect result from Function Delete - 1");
      end if;

      if B10.Delete(B10.To_Bounded_Wide_String(Translate("a")), 1, 1)  /= 
         B10.Null_Bounded_Wide_String                                    or
         B10.Delete(AtoE_Bnd_Str, 
                    5, 
                    B10.To_Wide_String(AtoE_Bnd_Str)'First) /=
         AtoE_Bnd_Str                                           or
         B10.Delete(AtoE_Bnd_Str,
                    B10.To_Wide_String(AtoE_Bnd_Str)'Last,
                    B10.To_Wide_String(AtoE_Bnd_Str)'Last)  /=
         B10.To_Bounded_Wide_String(Translate("abcd"))
      then
         Report.Failed("Incorrect result from Function Delete - 2");
      end if;
      


      -- Function Trim

      declare

         Text : B10.Bounded_Wide_String := 
                  B10.To_Bounded_Wide_String(Translate("Text"));
         type Bnd_Array_Type is array (1..5) of B10.Bounded_Wide_String;
         Bnd_Array : Bnd_Array_Type :=
           (B10.To_Bounded_Wide_String(Translate("  Text")),
            B10.To_Bounded_Wide_String(Translate("Text    ")),
            B10.To_Bounded_Wide_String(Translate("   Text   ")),
            B10.To_Bounded_Wide_String(Translate("Text  Text")),
            B10.To_Bounded_Wide_String(Translate(" Text Text")));

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
                     B10."&"(Text, B10."&"(Translate(' '), Text)) 
                  then
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

      -- Trim characters in sets from both sides of the bounded wide string.
      if B10.Trim(Source => B10.To_Bounded_Wide_String(Translate("ddabbaxx")),
                  Left   => CD_Set,
                  Right  => XY_Set)  /=
         B10.To_Bounded_Wide_String(Translate("abba"))
      then
         Report.Failed
           ("Incorrect result from Fn Trim - Sets, Left & Right side - 1");
      end if;

      -- Ensure that the characters in the set provided as the actual to
      -- parameter Right are not trimmed from the left side of the bounded
      -- wide string; likewise for the opposite side.  Only "cd" trimmed 
      -- from left side, and only "xy" trimmed from right side.

      if B10.Trim(B10.To_Bounded_Wide_String(Translate("cdxyabcdxy")), 
                  CD_Set, 
                  XY_Set) /=
         B10.To_Bounded_Wide_String(Translate("xyabcd"))
      then
         Report.Failed
           ("Incorrect result from Fn Trim - Sets, Left & Right side - 2");
      end if;

      -- Ensure that characters contained in the sets are not trimmed from
      -- the "interior" of the bounded wide string, just the appropriate ends.

      if B10.Trim(B10.To_Bounded_Wide_String(Translate("cdabdxabxy")), 
                  CD_Set, 
                  XY_Set) /=
         B10.To_Bounded_Wide_String(Translate("abdxab"))
      then
         Report.Failed
           ("Incorrect result from Fn Trim - Sets, Left & Right side - 3");
      end if;

      -- Trim characters in set from right side only.  No change to Left side.

      if B10.Trim(B10.To_Bounded_Wide_String(Translate("abxyzddcd")), 
                  XY_Set, 
                  CD_Set) /=
         B10.To_Bounded_Wide_String(Translate("abxyz"))
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
         B10.Trim(B10.To_Bounded_Wide_String(Translate("dcddcxyyxx")),
                  CD_Set,
                  XY_Set)                                     /=
         B10.Null_Bounded_Wide_String
      then
         Report.Failed("Incorrect result from Function Trim");
      end if;



      -- Procedure Trim using Sets                           

      -- Trim characters in sets from both sides of the bounded wide string.

      Test_String := B10.To_Bounded_Wide_String(Translate("dcabbayx"));
      B10.Trim(Source => Test_String,
               Left   => CD_Set,
               Right  => XY_Set);

      if Test_String /= B10.To_Bounded_Wide_String(Translate("abba")) then
         Report.Failed
           ("Incorrect result from Proc Trim - Sets, Left & Right side - 1");
      end if;

      -- Ensure that the characters in the set provided as the actual to
      -- parameter Right are not trimmed from the left side of the bounded
      -- wide string; likewise for the opposite side.  Only "cd" trimmed 
      -- from left side, and only "xy" trimmed from right side.

      Test_String := B10.To_Bounded_Wide_String(Translate("cdxyabcdxy"));
      B10.Trim(Test_String, CD_Set, XY_Set);

      if Test_String /= B10.To_Bounded_Wide_String(Translate("xyabcd")) then
         Report.Failed
           ("Incorrect result from Proc Trim - Sets, Left & Right side - 2");
      end if;

      -- Ensure that characters contained in the sets are not trimmed from
      -- the "interior" of the bounded wide string, just the appropriate ends.

      Test_String := B10.To_Bounded_Wide_String(Translate("cdabdxabxy"));
      B10.Trim(Test_String, CD_Set, XY_Set);

      if not 
        (Test_String = B10.To_Bounded_Wide_String(Translate("abdxab"))) then
         Report.Failed
           ("Incorrect result from Proc Trim - Sets, Left & Right side - 3");
      end if;

      -- Trim characters in set from Left side only.  No change to Right side.

      Test_String := B10.To_Bounded_Wide_String(Translate("cccdabxyz"));
      B10.Trim(Test_String, CD_Set, XY_Set);

      if Test_String /= B10.To_Bounded_Wide_String(Translate("abxyz")) then
         Report.Failed
           ("Incorrect result from Proc Trim for Sets, Left side only");
      end if;

      -- Trim no characters on either side of the bounded wide string.

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
                                   Pad    => Translate('X'));
         Report.Failed("Length_Error not raised by Function Head");
      exception
         when AS.Length_Error => null;  -- Expected exception raised.
         when others          => 
            Report.Failed("Incorrect exception raised by Function Head");
      end;

      -- Drop = Left

      -- Pad characters (5) are appended to the right end of the bounded
      -- wide string (which is initially at its maximum length), then the 
      -- first five characters of the intermediate result are dropped to 
      -- conform to the maximum size limit of the bounded wide string (10).

      Result_String := 
        B10.Head(B10.To_Bounded_Wide_String(Translate("ABCDEFGHIJ")),
                 15,
                 Translate('x'),
                 Ada.Strings.Left);

      if Result_String /= 
         B10.To_Bounded_Wide_String(Translate("FGHIJxxxxx"))
      then
         Report.Failed("Incorrect result from Function Head, Drop = Left");
      end if;

      -- Drop = Right

      -- Pad characters (6) are appended to the left end of the bounded
      -- wide string (which is initially at one less than its maximum length), 
      -- then the last five characters of the intermediate result are dropped 
      -- (which in this case are the pad characters) to conform to the 
      -- maximum size limit of the bounded wide string (10).

      Result_String := 
        B10.Head(B10.To_Bounded_Wide_String(Translate("ABCDEFGHI")),
                 15,
                 Translate('x'),
                 Ada.Strings.Right);

      if Result_String /= 
         B10.To_Bounded_Wide_String(Translate("ABCDEFGHIx")) 
      then
         Report.Failed("Incorrect result from Function Head, Drop = Right");
      end if;

      -- Additional cases.

      if B10.Head(B10.Null_Bounded_Wide_String, 5, Translate('a')) /=
         B10.To_Bounded_Wide_String(Translate("aaaaa"))          or
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
                                   Pad    => Ada.Strings.Wide_Space,
                                   Drop   => Ada.Strings.Error);
         Report.Failed("Length_Error not raised by Function Tail");
      exception
         when AS.Length_Error => null;  -- Expected exception raised.
         when others          => 
            Report.Failed("Incorrect exception raised by Function Tail");
      end;

      -- Drop = Left

      -- Pad characters (5) are appended to the left end of the bounded wide
      -- string (which is initially at two less than its maximum length), 
      -- then the first three characters of the intermediate result (in this 
      -- case, 3 pad characters) are dropped.

      Result_String := 
        B10.Tail(B10.To_Bounded_Wide_String(Translate("ABCDEFGH")),
                 13,
                 Translate('x'),
                 Ada.Strings.Left);

      if Result_String /= 
         B10.To_Bounded_Wide_String(Translate("xxABCDEFGH")) 
      then
         Report.Failed("Incorrect result from Function Tail, Drop = Left");
      end if;

      -- Drop = Right

      -- Pad characters (3) are appended to the left end of the bounded wide 
      -- string (which is initially at its maximum length), then the last
      -- three characters of the intermediate result are dropped.

      Result_String := 
        B10.Tail(B10.To_Bounded_Wide_String(Translate("ABCDEFGHIJ")),
                 13,
                 Translate('x'),
                 Ada.Strings.Right);

      if Result_String /= 
         B10.To_Bounded_Wide_String(Translate("xxxABCDEFG"))
      then
         Report.Failed("Incorrect result from Function Tail, Drop = Right");
      end if;

      -- Additional cases.

      if B10.Tail(B10.Null_Bounded_Wide_String, 3, Translate(' ')) /=
         B10.To_Bounded_Wide_String(Translate("   "))                or
         B10.Tail(AtoE_Bnd_Str, 
                  B10.To_Wide_String(AtoE_Bnd_Str)'First)  /=
         B10.To_Bounded_Wide_String(Translate("e"))
      then
         Report.Failed("Incorrect result from Function Tail");
      end if;



      -- Function Replicate (#, Char) with Truncation        
      -- Drop = Error (Default).

      begin
         Result_String := B10.Replicate(Count => B10.Max_Length + 5,
                                        Item  => Translate('A'),
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
      -- Since this version of Replicate uses wide character parameters, the 
      -- result after truncation from left or right will appear the same.
      -- The result will be a 10 character bounded wide string, composed of
      -- 10 "Item" wide characters.

      if B10.Replicate(Count => 20, 
                       Item => Translate('A'), 
                       Drop => Ada.Strings.Left) /=
         B10.Replicate(15, Translate('A'), Ada.Strings.Right)
      then
         Report.Failed("Incorrect result from Replicate for characters - 1");
      end if;

      -- Blank-filled, 10 character bounded wide strings.

      if B10.Replicate(B10.Max_Length + 1, 
                       Translate(' '), 
                       Drop => Ada.Strings.Left) /=
         B10.Replicate(B10.Max_Length, Ada.Strings.Wide_Space)
      then
         Report.Failed("Incorrect result from Replicate for characters - 2");
      end if;

      -- Additional cases.

      if B10.Replicate(0, Translate('a')) /= B10.Null_Bounded_Wide_String or
         B10.Replicate(1, Translate('a')) /= 
         B10.To_Bounded_Wide_String(Translate("a"))
      then
         Report.Failed("Incorrect result from Replicate for characters - 3");
      end if;



      -- Function Replicate (#, String) with Truncation      
      -- Drop = Error (Default).

      begin
         Result_String := B10.Replicate(Count => 5,  -- result would be 15.
                                        Item  => Translate("abc"));
         Report.Failed
           ("Length_Error not raised by Replicate for wide strings");
      exception
         when AS.Length_Error => null;  -- Expected exception raised.
         when others          => 
           Report.Failed
             ("Incorrect exception raised by Replicate for wide strings");
      end;

      -- Drop = Left

      Result_String := B10.Replicate(3, Translate("abcd"), Ada.Strings.Left);

      if Result_String /= 
         B10.To_Bounded_Wide_String(Translate("cdabcdabcd"))
      then
         Report.Failed
           ("Incorrect result from Replicate for wide strings, Drop = Left");
      end if;

      -- Drop = Right

      Result_String := B10.Replicate(3, Translate("abcd"), Ada.Strings.Right);

      if Result_String /= 
         B10.To_Bounded_Wide_String(Translate("abcdabcdab")) then
         Report.Failed
           ("Incorrect result from Replicate for wide strings, Drop = Right");
      end if;

      -- Additional cases.
      
      if B10.Replicate(5, Translate("X"))    /= 
         B10.To_Bounded_Wide_String(Translate("XXXXX"))   or
         B10.Replicate(10, "")               /= 
         B10.Null_Bounded_Wide_String                     or
         B10.Replicate(0, Translate("ab"))   /= 
         B10.Null_Bounded_Wide_String
      then
         Report.Failed("Incorrect result from Replicate for wide strings");
      end if;


   exception
      when others => Report.Failed("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA4020;
