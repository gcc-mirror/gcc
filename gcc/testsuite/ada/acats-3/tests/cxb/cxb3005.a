-- CXB3005.A
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
--      Check that the procedure To_C converts the character elements of
--      a string parameter into char elements of the char_array parameter
--      Target, with nul termination if parameter Append_Nul is true.
--
--      Check that the out parameter Count of procedure To_C is set to the
--      appropriate value for both the nul/no nul terminated cases.
--
--      Check that Constraint_Error is propagated by procedure To_C if the
--      length of the char_array parameter Target is not sufficient to
--      hold the converted string value.
--
--      Check that the Procedure To_Ada converts char elements of the
--      char_array parameter Item to the corresponding character elements
--      of string out parameter Target.
--
--      Check that Constraint_Error is propagated by Procedure To_Ada if the
--      length of string parameter Target is not long enough to hold the
--      converted char_array value.
--
--      Check that Terminator_Error is propagated by Procedure To_Ada if the
--      parameter Trim_Nul is set to True, but the actual Item parameter
--      contains no nul char.
--
-- TEST DESCRIPTION:
--      This test uses a variety of String, and char_array objects to test
--      versions of the To_C and To_Ada procedures.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', 'a'..'z', 'A'..'Z', '0'..'9', and '-'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      package Interfaces.C.  If an implementation provides
--      package Interfaces.C, this test must compile, execute, and
--      report "PASSED".
--
-- CHANGE HISTORY:
--      01 Sep 95   SAIC    Initial prerelease version.
--      09 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Incorporated reviewer comments.
--      14 Sep 99   RLB     Removed incorrect and unnecessary
--                          Unchecked_Conversion.
--
--!

with Report;
with Interfaces.C;                                            -- N/A => ERROR
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Fixed;

procedure CXB3005 is
begin

   Report.Test ("CXB3005", "Check that the procedures To_C and To_Ada " &
                           "produce correct results");
   Test_Block:
   declare

      use Interfaces, Interfaces.C;
      use Ada.Characters;
      use Ada.Exceptions;
      use Ada.Strings.Fixed;

      TC_Short_String  : String(1..4)     := (others => 'x');
      TC_String        : String(1..8)     := (others => 'y');
      TC_char_array    : char_array(0..7) := (others => char'Last);
      TC_size_t_Count  : size_t           := size_t'First;
      TC_Natural_Count : Natural          := Natural'First;


      -- We can use the character forms of To_Ada and To_C here to check
      -- the results; they were tested in CXB3004. We give them different
      -- names to avoid confusion below.

      function Character_to_char (Source : in Character) return char
          renames To_C;
      function char_to_Character (Source : in char) return Character
          renames To_Ada;

   begin

      -- Check that the procedure To_C converts the character elements of
      -- a string parameter into char elements of char_array out parameter
      -- Target.
      --
      -- Case of nul termination.

      TC_String(1..6) := "abcdef";

      To_C (Item       => TC_String(1..6),  -- Source slice of length 6.
            Target     => TC_char_array,    -- Length 8 will accommodate nul.
            Count      => TC_size_t_Count,
            Append_Nul => True);

      -- Check that the out parameter Count is set to the appropriate value
      -- for the nul terminated case.

      if TC_size_t_Count /= 7 then
         Report.Failed("Incorrect setting of out parameter Count by " &
                       "Procedure To_C when Append_Nul => True");
      end if;

      for i in 1..TC_size_t_Count-1 loop
         if char_to_Character(TC_char_array(i-1)) /= TC_String(Integer(i))
         then
            Report.Failed("Incorrect result from Procedure To_C when " &
                          "checking individual char values, case of "  &
                          "Append_Nul => True; "                       &
                          "char position = " & Integer'Image(Integer(i)));
         end if;
      end loop;

      if not Is_Nul_Terminated(TC_char_array) then
         Report.Failed("No nul char appended to the char_array result " &
                       "from Procedure To_C when Append_Nul => True");
      end if;

      if TC_char_array(0..6) /= To_C("abcdef", True) then
         Report.Failed("Incorrect result from Procedure To_C when "   &
                       "directly comparing char_array results, case " &
                       "of Append_Nul => True");
      end if;


      -- Check Procedure To_C with no nul termination.

      TC_char_array   := (others => Character_to_char('M')); -- Reinitialize.
      TC_String(1..4) := "WXYZ";

      To_C (Item       => TC_String(1..4),  -- Source slice of length 4.
            Target     => TC_char_array,
            Count      => TC_size_t_Count,
            Append_Nul => False);

      -- Check that the out parameter Count is set to the appropriate value
      -- for the non-nul terminated case.

      if TC_size_t_Count /= 4 then
         Report.Failed("Incorrect setting of out parameter Count by " &
                       "Procedure To_C when Append_Nul => False");
      end if;

      for i in 1..TC_size_t_Count loop
         if char_to_Character(TC_char_array(i-1)) /= TC_String(Integer(i))
         then
            Report.Failed("Incorrect result from Procedure To_C when " &
                          "checking individual char values, case of "  &
                          "Append_Nul => False; "                      &
                          "char position = " & Integer'Image(Integer(i)));
         end if;
      end loop;

      if Is_Nul_Terminated(TC_char_array) then
         Report.Failed("The nul char was appended to the char_array " &
                       "result of Procedure To_C when Append_Nul => False");
      end if;

      if TC_char_array(0..3) /= To_C("WXYZ", False) then
         Report.Failed("Incorrect result from Procedure To_C when "   &
                       "directly comparing char_array results, case " &
                       "of Append_Nul => False");
      end if;



      -- Check that Constraint_Error is raised by procedure To_C if the
      -- length of the target char_array parameter is not sufficient to
      -- hold the converted string value (plus nul if Append_Nul is True).

      begin
         To_C("A string too long",
              TC_char_array,
              TC_size_t_Count,
              Append_Nul => True);

         Report.Failed("Constraint_Error not raised when the Target " &
                       "parameter of Procedure To_C is not long enough " &
                       "to hold the converted string");
         Report.Comment(char_to_Character(TC_char_array(0)) &
                        " printed to defeat optimization");
      exception
         when Constraint_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Incorrect exception raised by Procedure "    &
                          "To_C when the Target parameter is not long " &
                          "enough to contain the char_array result");
      end;



      -- Check that the procedure To_Ada converts char elements of the
      -- char_array parameter Item to the corresponding character elements
      -- of string out parameter Target, with result string length based on
      -- the Trim_Nul parameter.
      --
      -- Case of appended nul char on the char_array In parameter.

      TC_char_array := To_C ("ACVC-95", Append_Nul => True); -- 8 total chars.
      TC_String     := (others => '*');                      -- Reinitialize.

      To_Ada (Item     => TC_char_array,
              Target   => TC_String,
              Count    => TC_Natural_Count,
              Trim_Nul => False);

      if TC_Natural_Count /= 8 then
         Report.Failed("Incorrect value returned in out parameter Count " &
                       "by Procedure To_Ada, case of Trim_Nul => False");
      end if;

      for i in 1..TC_Natural_Count loop
         if Character_to_char(TC_String(i)) /= TC_char_array(size_t(i-1))
         then
            Report.Failed("Incorrect result from Procedure To_Ada when " &
                          "checking individual char values, case of "    &
                          "Trim_Nul => False, when a nul is present in " &
                          "the char_array input parameter; "             &
                          "position = " & Integer'Image(Integer(i)));
         end if;
      end loop;

      if TC_String(TC_Natural_Count) /= Latin_1.Nul then
         Report.Failed("Last character of String result of Procedure "     &
                       "To_Ada is not Nul, even though a nul was present " &
                       "in the char_array argument, and the Trim_Nul "     &
                       "parameter was set to False");
      end if;


      TC_char_array(0..3) := To_C ("XYz", Append_Nul => True); -- 4 chars.
      TC_String           := (others => '*');                  -- Reinit.

      To_Ada (Item     => TC_char_array,
              Target   => TC_String,
              Count    => TC_Natural_Count,
              Trim_Nul => True);

      if TC_Natural_Count /= 3 then
         Report.Failed("Incorrect value returned in out parameter Count " &
                       "by Procedure To_Ada, case of Trim_Nul => True");
      end if;

      for i in 1..TC_Natural_Count loop
         if Character_to_char(TC_String(i)) /= TC_char_array(size_t(i-1))
         then
            Report.Failed("Incorrect result from Procedure To_Ada when " &
                          "checking individual char values, case of "    &
                          "Trim_Nul => True, when a nul is present in "  &
                          "the char_array input parameter; "             &
                          "position = " & Integer'Image(Integer(i)));
         end if;
      end loop;

      if TC_String(TC_Natural_Count) = Latin_1.Nul then
         Report.Failed("Last character of String result of Procedure " &
                       "To_Ada is  Nul, even though the Trim_Nul "     &
                       "parameter was set to True");
      end if;

      -- Check that TC_String(TC_Natural_Count+1) is unchanged by procedure
      -- To_Ada.

      if TC_String(TC_Natural_Count+1) /= '*' then
         Report.Failed("Incorrect modification to TC_String at position " &
                       Integer'Image(TC_Natural_Count+1) & " expected = " &
                       "*, found = " & TC_String(TC_Natural_Count+1));
      end if;


      -- Case of no nul char being present in the char_array argument.

      TC_char_array := To_C ("ABCDWXYZ", Append_Nul => False);
      TC_String     := (others => '*');                  -- Reinitialize.

      To_Ada (Item     => TC_char_array,
              Target   => TC_String,
              Count    => TC_Natural_Count,
              Trim_Nul => False);

      if TC_Natural_Count /= 8 then
         Report.Failed("Incorrect value returned in out parameter Count " &
                       "by Procedure To_Ada, case of Trim_Nul => False, " &
                       "with no nul char present in the parameter Item");
      end if;

      for i in 1..TC_Natural_Count loop
         if Character_to_char(TC_String(i)) /= TC_char_array(size_t(i-1))
         then
            Report.Failed("Incorrect result from Procedure To_Ada when "  &
                          "checking individual char values, case of "     &
                          "Trim_Nul => False, when a nul is not present " &
                          "in the char_array input parameter; "           &
                          "position = " & Integer'Image(Integer(i)));
         end if;
      end loop;

      if TC_String(TC_Natural_Count) = Latin_1.Nul then
         Report.Failed("Last character of String result of Procedure " &
                       "To_Ada is Nul, even though the nul char was "  &
                       "not present in the parameter Item, with the "  &
                       "parameter Trim_Nul => False");
      end if;



      -- Check that the Procedure To_Ada raises Terminator_Error if the
      -- parameter Trim_Nul is set to True, but the actual Item parameter
      -- does not contain the nul char.

      begin
         TC_char_array := To_C ("ABCDWXYZ", Append_Nul => False);
         TC_String     := (others => '*');

         To_Ada(TC_char_array,
                TC_String,
                Count    => TC_Natural_Count,
                Trim_Nul => True);

         Report.Failed("Terminator_Error not raised when Item "    &
                       "parameter of To_Ada does not contain the " &
                       "nul char, but parameter Trim_Nul => True");
         Report.Comment(TC_String & " printed to defeat optimization");
      exception
         when Terminator_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Incorrect exception raised by Procedure " &
                          "To_Ada when the Item parameter does not " &
                          "contain the nul char, but parameter "     &
                          "Trim_Nul => True");
      end;



      -- Check that Constraint_Error is propagated by procedure To_Ada if the
      -- length of string parameter Target is not long enough to hold the
      -- converted char_array value (plus nul if Trim_Nul is False).

      begin
         TC_char_array(0..4) := To_C ("ABCD", Append_Nul => True);

         To_Ada(TC_char_array(0..4),   -- 4 chars plus nul char.
                TC_Short_String,       -- Length of 4.
                Count    => TC_Natural_Count,
                Trim_Nul => False);

         Report.Failed("Constraint_Error not raised when string "     &
                       "parameter Target of Procedure To_Ada is not " &
                       "long enough to hold the converted chars");
         Report.Comment(TC_Short_String & " printed to defeat optimization");
      exception
         when Constraint_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Incorrect exception raised by Procedure " &
                          "To_Ada when string parameter Target is "  &
                          "not long enough to hold the converted chars");
      end;



   exception
      when The_Error : others =>
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB3005;
