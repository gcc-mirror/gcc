-- CXA4005.A
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
--      available, and that they produce correct results.  Specifically,
--      check the subprograms Delete, Head, Insert, Overwrite, Replace_Slice,
--      Tail, Trim, and "*".
--
-- TEST DESCRIPTION:
--      This test, when combined with tests CXA4002-4 will provide coverage
--      of the functionality found in Ada.Strings.Fixed.
--      This test contains many small, specific test cases, situations that
--      although common in user environments, are often difficult to generate
--      in large numbers in a application-based test.  They represent
--      individual usage paradigms in-the-small.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      11 Apr 95   SAIC    Corrected acceptance conditions of certain
--                          subtests.
--	06 Nov 95   SAIC    Fixed bugs for ACVC 2.0.1.
--      22 Feb 01   PHL     Check that the lower bound of the result is 1.
--      13 Mar 01   RLB     Fixed a couple of ACATS style violations;
--                          removed pointless checks of procedures.
--                          Added checks of other functions. These changes
--                          were made to test Defect Report 8652/0049, as
--                          reflected in Technical Corrigendum 1.
--
--!

with Report;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure CXA4005 is

    type TC_Name_Holder is access String;
    Name : TC_Name_Holder;

    function TC_Check (S : String) return String is
    begin
        if S'First /= 1 then
            Report.Failed ("Lower bound of result of function " & Name.all &
                           " is" & Integer'Image (S'First));
        end if;
        return S;
    end TC_Check;

    procedure TC_Set_Name (N : String) is
    begin
        Name := new String'(N);
    end TC_Set_Name;

begin

   Report.Test("CXA4005", "Check that the subprograms defined in "    &
                          "package Ada.Strings.Fixed are available, " &
                          "and that they produce correct results");

   Test_Block:
   declare

      package ASF  renames Ada.Strings.Fixed;
      package Maps renames Ada.Strings.Maps;

      Result_String,
      Delete_String,
      Insert_String,
      Trim_String,
      Overwrite_String  : String(1..10) := (others => Ada.Strings.Space);

      Source_String1    : String(1..5)  := "abcde";     -- odd length string
      Source_String2    : String(1..6)  := "abcdef";    -- even length string
      Source_String3    : String(1..12) := "abcdefghijkl";
      Source_String4    : String(1..12) := "abcdefghij  "; -- last two ch pad
      Source_String5    : String(1..12) := "  cdefghijkl"; -- first two ch pad
      Source_String6    : String(1..12) := "abcdefabcdef";

      Location     : Natural := 0;
      Slice_Start  : Positive;
      Slice_End,
      Slice_Count  : Natural := 0;

      CD_Set       : Maps.Character_Set := Maps.To_Set("cd");
      X_Set        : Maps.Character_Set := Maps.To_Set('x');
      ABCD_Set     : Maps.Character_Set := Maps.To_Set("abcd");
      A_to_F_Set   : Maps.Character_Set := Maps.To_Set("abcdef");

      CD_to_XY_Map : Maps.Character_Mapping :=
                                 Maps.To_Mapping(From => "cd",  To => "xy");

   begin

      -- Procedure Replace_Slice
      -- The functionality of this procedure
      -- is similar to procedure Move, and
      -- is tested here in the same manner, evaluated
      -- with various combinations of parameters.

      -- Index_Error propagation when Low > Source'Last + 1

      begin
         ASF.Replace_Slice(Result_String,
                           Result_String'Last + 2, -- should raise exception
                           Result_String'Last,
                           "xxxxxxx");
         Report.Failed("Index_Error not raised by Replace_Slice - 1");
      exception
         when Ada.Strings.Index_Error => null; -- OK, expected exception.
         when others                  =>
            Report.Failed("Incorrect exception from Replace_Slice - 1");
      end;

      -- Index_Error propagation when High < Source'First - 1

      begin
         ASF.Replace_Slice(Result_String(5..10),
                           5,
                           3, -- should raise exception since < 'First - 1.
                           "xxxxxxx");
         Report.Failed("Index_Error not raised by Replace_Slice - 2");
      exception
         when Ada.Strings.Index_Error => null; -- OK, expected exception.
         when others                  =>
            Report.Failed("Incorrect exception from Replace_Slice - 2");
      end;

      -- Justify = Left (default case)

      Result_String := "XXXXXXXXXX";

      ASF.Replace_Slice(Source => Result_String,
                        Low    =>  1,
                        High   => 10,
                        By     => Source_String1);   -- "abcde"

      if Result_String /= "abcde     " then
         Report.Failed("Incorrect result from Replace_Slice - Justify = Left");
      end if;

      -- Justify = Right

      ASF.Replace_Slice(Source  => Result_String,
                        Low     => 1,
                        High    => Result_String'Last,
                        By      => Source_String2,      -- "abcdef"
                        Drop    => Ada.Strings.Error,
                        Justify => Ada.Strings.Right);

      if Result_String /= "    abcdef" then
         Report.Failed("Incorrect result from Replace_Slice - Justify=Right");
      end if;

      -- Justify = Center (two cases, odd and even pad lengths)

      ASF.Replace_Slice(Result_String,
                        1,
                        Result_String'Last,
                        Source_String1,      -- "abcde"
                        Ada.Strings.Error,
                        Ada.Strings.Center,
                        'x');                -- non-default padding.

      if Result_String /= "xxabcdexxx" then  -- Unequal padding added right
         Report.Failed("Incorrect result, Replace_Slice - Justify=Center - 1");
      end if;

      ASF.Replace_Slice(Result_String,
                        1,
                        Result_String'Last,
                        Source_String2,        -- "abcdef"
                        Ada.Strings.Error,
                        Ada.Strings.Center);

      if Result_String /= "  abcdef  " then    -- Equal padding added on L/R.
         Report.Failed("Incorrect result from Replace_Slice with " &
                       "Justify = Center - 2");
      end if;

      -- When the source string is longer than the target string, several
      -- cases can be examined, with the results depending on the value of
      -- the Drop parameter.

      -- Drop = Left

      ASF.Replace_Slice(Result_String,
                        1,
                        Result_String'Last,
                        Source_String3,            -- "abcdefghijkl"
                        Drop => Ada.Strings.Left);

      if Result_String /= "cdefghijkl" then
         Report.Failed("Incorrect result from Replace_Slice - Drop=Left");
      end if;

      -- Drop = Right

      ASF.Replace_Slice(Result_String,
                        1,
                        Result_String'Last,
                        Source_String3,        -- "abcdefghijkl"
                        Ada.Strings.Right);

      if Result_String /= "abcdefghij" then
         Report.Failed("Incorrect result, Replace_Slice with Drop=Right");
      end if;

      -- Drop = Error

      -- The effect in this case depends on the value of the justify
      -- parameter, and on whether any characters in Source other than
      -- Pad would fail to be copied.

      -- Drop = Error, Justify = Left, right overflow characters are pad.

      ASF.Replace_Slice(Result_String,
                        1,
                        Result_String'Last,
                        Source_String4,               -- "abcdefghij  "
                        Drop    => Ada.Strings.Error,
                        Justify => Ada.Strings.Left);

      if not(Result_String = "abcdefghij") then  -- leftmost 10 characters
          Report.Failed("Incorrect result, Replace_Slice - Drop = Error - 1");
      end if;

      -- Drop = Error, Justify = Right, left overflow characters are pad.

      ASF.Replace_Slice(Source   => Result_String,
                        Low      => 1,
                        High     => Result_String'Last,
                        By       => Source_String5,     -- "  cdefghijkl"
                        Drop     => Ada.Strings.Error,
                        Justify  => Ada.Strings.Right);

      if Result_String /= "cdefghijkl" then  -- rightmost 10 characters
         Report.Failed("Incorrect result, Replace_Slice - Drop = Error - 2");
      end if;

      -- In other cases of Drop=Error, Length_Error is propagated, such as:

      begin

         ASF.Replace_Slice(Source   => Result_String,
                           Low      => 1,
                           High     => Result_String'Last,
                           By       => Source_String3,    -- "abcdefghijkl"
                           Drop     => Ada.Strings.Error);

          Report.Failed("Length_Error not raised by Replace_Slice - 1");

      exception
         when Ada.Strings.Length_Error => null;   -- OK
         when others =>
            Report.Failed("Incorrect exception from Replace_Slice - 3");
      end;


      -- Function Replace_Slice

      TC_Set_Name ("Replace_Slice");

      if TC_Check (ASF.Replace_Slice("abcde", 3, 3, "x"))
                                           /= "abxde"  or -- High = Low
         TC_Check (ASF.Replace_Slice("abc",   2, 3, "xyz")) /= "axyz"   or
         TC_Check (ASF.Replace_Slice("abcd",  4, 1, "xy"))
                                           /= "abcxyd" or -- High < Low
         TC_Check (ASF.Replace_Slice("abc",   2, 3, "x"))   /= "ax"     or
         TC_Check (ASF.Replace_Slice("a",     1, 1, "z"))   /= "z"
      then
         Report.Failed("Incorrect result from Function Replace_Slice - 1");
      end if;

      if TC_Check (ASF.Replace_Slice("abcde", 5, 5, "z"))
                                           /= "abcdz" or  -- By length 1
         TC_Check (ASF.Replace_Slice("abc",   1, 3, "xyz"))
                                           /= "xyz"   or  -- High > Low
         TC_Check (ASF.Replace_Slice("abc",   3, 2, "xy"))
                                           /= "abxyc" or  -- insert
         TC_Check (ASF.Replace_Slice("a",     1, 1, "xyz")) /= "xyz"
      then
         Report.Failed("Incorrect result from Function Replace_Slice - 2");
      end if;



      -- Function Insert.

      TC_Set_Name ("Insert");

      declare
         New_String : constant String :=
           TC_Check (
              ASF.Insert(Source   => Source_String1(2..5),    -- "bcde"
                         Before   => 3,
                         New_Item => Source_String2));         -- "abcdef"
      begin
         if New_String /= "babcdefcde" then
            Report.Failed("Incorrect result from Function Insert - 1");
         end if;
      end;

      if TC_Check (ASF.Insert("a",   1, "z")) /= "za"   or
         TC_Check (ASF.Insert("abc", 3, ""))  /= "abc"  or
         TC_Check (ASF.Insert("abc", 1, "z")) /= "zabc"
      then
         Report.Failed("Incorrect result from Function Insert - 2");
      end if;

      begin
         if TC_Check (ASF.Insert(Source   => Source_String1(2..5), -- "bcde"
                         Before   => Report.Ident_Int(7),
                         New_Item => Source_String2))         -- "abcdef"
                /= "babcdefcde" then
            Report.Failed("Index_Error not raised by Insert - 3A");
         else
            Report.Failed("Index_Error not raised by Insert - 3B");
         end if;
      exception
         when Ada.Strings.Index_Error => null; -- OK, expected exception.
         when others                  =>
            Report.Failed("Incorrect exception from Insert - 3");
      end;


      -- Procedure Insert

      -- Drop = Right

      ASF.Insert(Source   => Insert_String,
                 Before   => 6,
                 New_Item => Source_String2,       -- "abcdef"
                 Drop     => Ada.Strings.Right);

      if Insert_String /= "     abcde" then  -- last char of New_Item dropped.
         Report.Failed("Incorrect result from Insert with Drop = Right");
      end if;

      -- Drop = Left

      ASF.Insert(Source   => Insert_String,     -- 10 char string
                 Before   => 2,                 -- 9 chars, 2..10 available
                 New_Item => Source_String3,    -- 12 characters long.
                 Drop     => Ada.Strings.Left); -- truncate from Left.

      if Insert_String /= "l    abcde" then     -- 10 chars, leading blank.
          Report.Failed("Incorrect result from Insert with Drop=Left");
      end if;

      -- Drop = Error

      begin
         ASF.Insert(Source   => Result_String,       -- 10 chars
                    Before   => Result_String'Last,
                    New_Item => "abcdefghijk",
                    Drop     => Ada.Strings.Error);
         Report.Failed("Exception not raised by Procedure Insert");
      exception
         when Ada.Strings.Length_Error => null; -- OK, expected exception
         when others                   =>
           Report.Failed("Incorrect exception raised by Procedure Insert");
      end;



      -- Function Overwrite

      TC_Set_Name ("Overwrite");

      Overwrite_String := TC_Check (
          ASF.Overwrite(Result_String,  -- 10 chars
                        1,              -- starting at pos=1
                        Source_String3(1..10)));

      if Overwrite_String /= Source_String3(1..10) then
         Report.Failed("Incorrect result from Function Overwrite - 1");
      end if;


      if TC_Check (ASF.Overwrite("abcdef", 4, "xyz")) /= "abcxyz" or
         TC_Check (ASF.Overwrite("a",      1, "xyz"))
                                           /= "xyz"    or  -- chars appended
         TC_Check (ASF.Overwrite("abc",    3, "   "))
                                           /= "ab   "  or  -- blanks appended
         TC_Check (ASF.Overwrite("abcde",  1, "z"  )) /= "zbcde"
      then
         Report.Failed("Incorrect result from Function Overwrite - 2");
      end if;



      -- Procedure Overwrite, with truncation.

      ASF.Overwrite(Source   => Overwrite_String,    -- 10 characters.
                    Position => 1,
                    New_Item => Source_String3,      -- 12 characters.
                    Drop     => Ada.Strings.Left);

      if Overwrite_String /= "cdefghijkl" then
         Report.Failed("Incorrect result from Overwrite with Drop=Left");
      end if;

      -- The default drop value is Right, used here.

      ASF.Overwrite(Source   => Overwrite_String,    -- 10 characters.
                    Position => 1,
                    New_Item => Source_String3);     -- 12 characters.

      if Overwrite_String /= "abcdefghij" then
         Report.Failed("Incorrect result from Overwrite with Drop=Right");
      end if;

      -- Drop = Error

      begin
         ASF.Overwrite(Source   => Overwrite_String,    -- 10 characters.
                       Position => 1,
                       New_Item => Source_String3,      -- 12 characters.
                       Drop     => Ada.Strings.Error);
         Report.Failed("Exception not raised by Procedure Overwrite");
      exception
         when Ada.Strings.Length_Error => null; -- OK, expected exception.
         when others                   =>
            Report.Failed
              ("Incorrect exception raised by Procedure Overwrite");
      end;

      Overwrite_String := "ababababab";
      ASF.Overwrite(Overwrite_String, Overwrite_String'Last, "z");
      ASF.Overwrite(Overwrite_String, Overwrite_String'First,"z");
      ASF.Overwrite(Overwrite_String, 5, "zz");

      if Overwrite_String /= "zbabzzabaz" then
         Report.Failed("Incorrect result from Procedure Overwrite");
      end if;



      -- Function Delete

      TC_Set_Name ("Delete");

      declare
         New_String1 : constant String :=    -- This returns a 4 char string.
           TC_Check (ASF.Delete(Source  => Source_String3,
                                From    => 3,
                                Through => 10));
         New_String2 : constant String :=    -- This returns Source.
           TC_Check (ASF.Delete(Source_String3, 10, 3));
      begin
           if New_String1 /= "abkl"  or
              New_String2 /= Source_String3
           then
               Report.Failed("Incorrect result from Function Delete - 1");
           end if;
      end;

      if TC_Check (ASF.Delete("a",   1, 1))
                              /= ""    or     -- Source length = 1
         TC_Check (ASF.Delete("abc", 1, 2))
                              /= "c"   or     -- From = Source'First
         TC_Check (ASF.Delete("abc", 3, 3))
                              /= "ab"  or     -- From = Source'Last
         TC_Check (ASF.Delete("abc", 3, 1))
                              /= "abc"        -- From > Through
      then
         Report.Failed("Incorrect result from Function Delete - 2");
      end if;



      -- Procedure Delete

      -- Justify = Left

      Delete_String := Source_String3(1..10);  -- Initialize to "abcdefghij"

      ASF.Delete(Source  => Delete_String,
                 From    => 6,
                 Through => Delete_String'Last,
                 Justify => Ada.Strings.Left,
                 Pad     => 'x');              -- pad with char 'x'

      if Delete_String /= "abcdexxxxx" then
          Report.Failed("Incorrect result from Delete - Justify = Left");
      end if;

      -- Justify = Right

      ASF.Delete(Source  => Delete_String,      -- Remove x"s from end and
                 From    => 6,                  -- shift right.
                 Through => Delete_String'Last,
                 Justify => Ada.Strings.Right,
                 Pad     => 'x');               -- pad with char 'x' on left.

      if Delete_String /= "xxxxxabcde" then
          Report.Failed("Incorrect result from Delete - Justify = Right");
      end if;

      -- Justify = Center

      ASF.Delete(Source  => Delete_String,
                 From    => 1,
                 Through => 5,
                 Justify => Ada.Strings.Center,
                 Pad     => 'z');

      if Delete_String /= "zzabcdezzz" then  -- extra pad char on right side.
          Report.Failed("Incorrect result from Delete - Justify = Center");
      end if;



      -- Function Trim
      -- Use non-identity character sets to perform the trim operation.

      TC_Set_Name ("Trim");

      Trim_String := "cdabcdefcd";

      -- Remove the "cd" from each end of the string.  This will not effect
      -- the "cd" slice at 5..6.

      declare
         New_String : constant String :=
           TC_Check (ASF.Trim(Source => Trim_String,
                              Left => CD_Set, Right => CD_Set));
      begin
         if New_String /= Source_String2 then    -- string "abcdef"
            Report.Failed("Incorrect result from Trim with character sets");
         end if;
      end;

      if TC_Check (ASF.Trim("abcdef", Maps.Null_Set, Maps.Null_Set))
             /= "abcdef" then
         Report.Failed("Incorrect result from Trim with Null sets");
      end if;

      if TC_Check (ASF.Trim("cdxx", CD_Set, X_Set)) /= "" then
         Report.Failed("Incorrect result from Trim, string removal");
      end if;


      -- Procedure Trim

      -- Justify = Right

      ASF.Trim(Source  => Trim_String,
               Left    => CD_Set,
               Right   => CD_Set,
               Justify => Ada.Strings.Right,
               Pad     => 'x');

      if Trim_String /= "xxxxabcdef" then
         Report.Failed("Incorrect result from Trim with Justify = Right");
      end if;

      -- Justify = Left

      ASF.Trim(Source  => Trim_String,
               Left    => X_Set,
               Right   => Maps.Null_Set,
               Justify => Ada.Strings.Left,
               Pad     => Ada.Strings.Space);

      if Trim_String /= "abcdef    " then  -- Padded with 4 blanks on right.
         Report.Failed("Incorrect result from Trim with Justify = Left");
      end if;

      -- Justify = Center

      ASF.Trim(Source  => Trim_String,
               Left    => ABCD_Set,
               Right   => CD_Set,
               Justify => Ada.Strings.Center,
               Pad     => 'x');

      if Trim_String /= "xxef    xx" then  -- Padded with 2 pad chars on L/R
         Report.Failed("Incorrect result from Trim with Justify = Center");
      end if;



      -- Function Head, demonstrating use of padding.

      TC_Set_Name ("Head");

      -- Use the characters of Source_String1 ("abcde") and pad the
      -- last five characters of Result_String with 'x' characters.


      Result_String := TC_CHeck (ASF.Head(Source_String1, 10, 'x'));

      if Result_String /= "abcdexxxxx" then
         Report.Failed("Incorrect result from Function Head with padding");
      end if;

      if TC_Check (ASF.Head("  ab  ", 2))        /= "  "     or
         TC_Check (ASF.Head("a", 6, 'A'))        /= "aAAAAA" or
         TC_Check (ASF.Head("abcdefgh", 3, 'x')) /= "abc"    or
         TC_Check (ASF.Head(ASF.Head("abc  ", 7, 'x'), 10, 'X'))
                   /= "abc  xxXXX"
      then
         Report.Failed("Incorrect result from Function Head");
      end if;



      -- Function Tail, demonstrating use of padding.

      TC_Set_Name ("Tail");

      -- Use the characters of Source_String1 ("abcde") and pad the
      -- first five characters of Result_String with 'x' characters.

      Result_String := TC_Check (ASF.Tail(Source_String1, 10, 'x'));

      if Result_String /= "xxxxxabcde" then
         Report.Failed("Incorrect result from Function Tail with padding");
      end if;

      if TC_Check (ASF.Tail("abcde  ", 5))
                            /= "cde  "    or  -- blanks, back
         TC_Check (ASF.Tail("  abc ",  8, ' '))
                            /= "    abc " or  -- blanks, front/back
         TC_Check (ASF.Tail("",        5, 'Z'))
                            /= "ZZZZZ"    or  -- pad characters only
         TC_Check (ASF.Tail("abc",     0))
                            /= ""         or  -- null result
         TC_Check (ASF.Tail("abcdefgh", 3))
                            /= "fgh"      or
         TC_Check (ASF.Tail(ASF.Tail(" abc ", 6, 'x'),
                  10,
                  'X'))              /= "XXXXx abc "
      then
         Report.Failed("Incorrect result from Function Tail");
      end if;


      -- Function "*"  - with (Natural, String) parameters

      TC_Set_Name ("""*""");

      if TC_Check (ASF."*"(3, Source_String1))       /= "abcdeabcdeabcde"  or
         TC_Check (ASF."*"(2, Source_String2))       /= Source_String6     or
         TC_Check (ASF."*"(4, Source_String1(1..2))) /= "abababab"         or
         TC_Check (ASF."*"(0, Source_String1))       /= ""
      then
         Report.Failed("Incorrect result from Function ""*"" with strings");
      end if;

   exception
      when others => Report.Failed("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA4005;
