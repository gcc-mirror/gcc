-- CXB30041.AM
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
--      Check that the functions To_C and To_Ada map between the Ada type
--      Character and the C type char.
--
--      Check that the function Is_Nul_Terminated returns True if the
--      char_array parameter contains nul, and otherwise False.
--
--      Check that the function To_C produces a correct char_array result,
--      with lower bound of 0, and length dependent upon the Item and
--      Append_Nul parameters.
--
--      Check that the function To_Ada produces a correct string result, with
--      lower bound of 1, and length dependent upon the Item and Trim_Nul
--      parameters.
--
--      Check that the function To_Ada raises Terminator_Error if the
--      parameter Trim_Nul is set to True, but the actual Item parameter
--      does not contain the nul char.
--
-- TEST DESCRIPTION:
--      This test uses a variety of Character, char, String, and char_array
--      objects to test versions of the To_C, To_Ada, and Is_Nul_Terminated
--      functions.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', ',', '.', '0'..'9', 'a'..'z' and 'A'..'Z'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      package Interfaces.C.  If an implementation provides
--      package Interfaces.C, this test must compile, execute, and
--      report "PASSED".
--
-- SPECIAL REQUIREMENTS:
--      The file CXB30040.C must be compiled with a C compiler.
--      Implementation dialects of C may require alteration of
--      the C program syntax (see individual C files).
--
--      Note that the compiled C code must be bound with the compiled Ada
--      code to create an executable image.  An implementation must provide
--      the necessary commands to accomplish this.
--
--      Note that the C code included in CXB30040.C conforms
--      to ANSI-C.  Modifications to these files may be required for other
--      C compilers.  An implementation must provide the necessary
--      modifications to satisfy the function requirements.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         CXB30040.C
--         CXB30041.AM
--
-- CHANGE HISTORY:
--      30 Aug 95   SAIC    Initial prerelease version.
--      09 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Incorporated reviewer comments.
--      13 Sep 99   RLB     Replaced (bogus) Unchecked_Conversions with a
--                          C function character generator.
--
--!

with Report;
with Interfaces.C;                                            -- N/A => ERROR
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Impdef;

procedure CXB30041 is
begin

   Report.Test ("CXB3004", "Check that the functions To_C and To_Ada " &
                           "produce correct results");

   Test_Block:
   declare

      use Interfaces, Interfaces.C;
      use Ada.Characters, Ada.Characters.Latin_1;
      use Ada.Exceptions;
      use Ada.Strings.Fixed;

      Start_Character,
      Stop_Character,
      TC_Character    : Character         := Character'First;
      TC_char,
      TC_Low_char,
      TC_High_char    : char              := char'First;
      TC_String       : String(1..8)      := (others => Latin_1.NUL);
      TC_char_array   : char_array(0..7)  := (others => C.nul);

      -- The function Char_Gen returns a character corresponding to its
      -- argument.
      --     Value   0 ..  9 ==> '0' .. '9'
      --     Value  10 .. 19 ==> 'A' .. 'J'
      --     Value  20 .. 29 ==> 'k' .. 't'
      --     Value  30       ==> ' '
      --     Value  31       ==> '.'
      --     Value  32       ==> ','

      function Char_Gen (Value   : in int) return char;

      -- Use the user-defined C function char_gen as a completion to the
      -- function specification above.

      pragma Import (Convention    => C,
                     Entity        => Char_Gen,
                     External_Name => Impdef.CXB30040_External_Name);

   begin

      -- Check that the functions To_C and To_Ada map between the Ada type
      -- Character and the C type char.

      if To_C(Ada.Characters.Latin_1.NUL) /= Interfaces.C.nul then
         Report.Failed("Incorrect result from To_C with NUL character input");
      end if;

      Start_Character := Report.Ident_Char('k');
      Stop_Character  := Report.Ident_Char('t');
      for TC_Character in Start_Character..Stop_Character loop
         if To_C(Item => TC_Character) /=
            Char_Gen(Character'Pos(TC_Character) - Character'Pos('k') + 20) then
            Report.Failed("Incorrect result from To_C with lower case " &
                          "alphabetic character input");
         end if;
      end loop;

      Start_Character := Report.Ident_Char('A');
      Stop_Character  := Report.Ident_Char('J');
      for TC_Character in Start_Character..Stop_Character loop
         if To_C(Item => TC_Character) /=
            Char_Gen(Character'Pos(TC_Character) - Character'Pos('A') + 10) then
            Report.Failed("Incorrect result from To_C with upper case " &
                          "alphabetic character input");
         end if;
      end loop;

      Start_Character := Report.Ident_Char('0');
      Stop_Character  := Report.Ident_Char('9');
      for TC_Character in Start_Character..Stop_Character loop
         if To_C(Item => TC_Character) /=
            Char_Gen(Character'Pos(TC_Character) - Character'Pos('0')) then
            Report.Failed("Incorrect result from To_C with digit " &
                          "character input");
         end if;
      end loop;
      if To_C(Item => ' ') /= Char_Gen(30) then
         Report.Failed("Incorrect result from To_C with space " &
                       "character input");
      end if;
      if To_C(Item => '.') /= Char_Gen(31) then
         Report.Failed("Incorrect result from To_C with dot " &
                       "character input");
      end if;
      if To_C(Item => ',') /= Char_Gen(32) then
         Report.Failed("Incorrect result from To_C with comma " &
                       "character input");
      end if;

      if To_Ada(Interfaces.C.nul) /= Ada.Characters.Latin_1.NUL then
         Report.Failed("Incorrect result from To_Ada with nul char input");
      end if;

      for Code in int range
         int(Report.Ident_Int(20)) .. int(Report.Ident_Int(29)) loop
            -- 'k' .. 't'
         if To_Ada(Item => Char_Gen(Code)) /=
	    Character'Val (Character'Pos('k') + (Code - 20)) then
            Report.Failed("Incorrect result from To_Ada with lower case " &
                          "alphabetic char input");
         end if;
      end loop;

      for Code in int range
         int(Report.Ident_Int(10)) .. int(Report.Ident_Int(19)) loop
            -- 'A' .. 'J'
         if To_Ada(Item => Char_Gen(Code)) /=
	    Character'Val (Character'Pos('A') + (Code - 10)) then
            Report.Failed("Incorrect result from To_Ada with upper case " &
                          "alphabetic char input");
         end if;
      end loop;

      for Code in int range
         int(Report.Ident_Int(0)) .. int(Report.Ident_Int(9)) loop
            -- '0' .. '9'
         if To_Ada(Item => Char_Gen(Code)) /=
	    Character'Val (Character'Pos('0') + (Code)) then
            Report.Failed("Incorrect result from To_Ada with digit " &
                          "char input");
         end if;
      end loop;

      if To_Ada(Item => Char_Gen(30)) /= ' ' then
         Report.Failed("Incorrect result from To_Ada with space " &
                       "char input");
      end if;
      if To_Ada(Item => Char_Gen(31)) /= '.' then
         Report.Failed("Incorrect result from To_Ada with dot " &
                       "char input");
      end if;
      if To_Ada(Item => Char_Gen(32)) /= ',' then
         Report.Failed("Incorrect result from To_Ada with comma " &
                       "char input");
      end if;

      -- Check that the function Is_Nul_Terminated produces correct results
      -- whether or not the char_array argument contains the
      -- Ada.Interfaces.C.nul character.

      TC_String := "abcdefgh";
      if Is_Nul_Terminated(Item => To_C(TC_String, Append_Nul => False)) then
         Report.Failed("Incorrect result from Is_Nul_Terminated when no " &
                       "nul char is present");
      end if;

      if not Is_Nul_Terminated(To_C(TC_String, Append_Nul => True)) then
         Report.Failed("Incorrect result from Is_Nul_Terminated when the " &
                       "nul char is present");
      end if;


      -- Now that we've tested the character/char versions of To_Ada and To_C,
      -- use them to test the string versions.

      declare
         i                    : size_t  := 0;
         j                    : integer := 1;
         Incorrect_Conversion : Boolean := False;

         TC_No_nul       : constant char_array := To_C(TC_String, False);
         TC_nul_Appended : constant char_array := To_C(TC_String, True);
      begin

         -- Check that the function To_C produces a char_array result with
         -- lower bound of 0, and length dependent upon the Item and
         -- Append_Nul parameters (if Append_Nul is True, length is
         -- Item'Length + 1; if False, length is Item'Length).

         if TC_No_nul'First /= 0 or TC_nul_Appended'First /= 0 then
            Report.Failed("Incorrect lower bound from Function To_C");
         end if;

         if TC_No_nul'Length /= TC_String'Length then
            Report.Failed("Incorrect length returned from Function To_C " &
                          "when Append_Nul => False");
         end if;

         for TC_char in Report.Ident_Char('a')..Report.Ident_Char('h') loop
            if TC_No_nul(i)       /= To_C(TC_char) or -- Single character To_C.
               TC_nul_Appended(i) /= To_C(TC_char) then
               Incorrect_Conversion := True;
            end if;
            i := i + 1;
         end loop;

         if Incorrect_Conversion then
            Report.Failed("Incorrect result from To_C with string input " &
                          "and char_array result");
         end if;


         if TC_nul_Appended'Length /= TC_String'Length + 1 then
            Report.Failed("Incorrect length returned from Function To_C " &
                          "when Append_Nul => True");
         end if;

         if not Is_Nul_Terminated(TC_nul_Appended) then
            Report.Failed("No nul appended to the string parameter during " &
                          "conversion to char_array by function To_C");
         end if;


         -- Check that the function To_Ada produces a string result with
         -- lower bound of 1, and length dependent upon the Item and
         -- Trim_Nul parameters (if Trim_Nul is False, length is Item'Length;
         -- if True, length will be the length of the slice of Item prior to
         -- the first nul).

         declare
            TC_No_NUL_String       : constant String :=
                                       To_Ada(Item     => TC_nul_Appended,
                                              Trim_Nul => True);
            TC_NUL_Appended_String : constant String :=
                                       To_Ada(TC_nul_Appended, False);
         begin

            if TC_No_NUL_String'First       /= 1 or
               TC_NUL_Appended_String'First /= 1
            then
               Report.Failed("Incorrect lower bound from Function To_Ada");
            end if;

            if TC_No_NUL_String'Length /= TC_String'Length then
               Report.Failed("Incorrect length returned from Function " &
                             "To_Ada when Trim_Nul => True");
            end if;

            if TC_NUL_Appended_String'Length /= TC_String'Length + 1 then
               Report.Failed("Incorrect length returned from Function " &
                             "To_Ada when Trim_Nul => False");
            end if;

            Start_Character := Report.Ident_Char('a');
            Stop_Character  := Report.Ident_Char('h');
            for TC_Character in Start_Character..Stop_Character loop
               if TC_No_NUL_String(j)       /= TC_Character or
                  TC_NUL_Appended_String(j) /= TC_Character
               then
                  Report.Failed("Incorrect result from To_Ada with " &
                                "char_array input, index = "         &
                                Integer'Image(j));
               end if;
               j := j + 1;
            end loop;

         end;


         -- Check that the function To_Ada raises Terminator_Error if the
         -- parameter Trim_Nul is set to True, but the actual Item parameter
         -- does not contain the nul char.

         begin
            TC_String := To_Ada(TC_No_nul, Trim_Nul => True);
            Report.Failed("Terminator_Error not raised when Item "    &
                          "parameter of To_Ada does not contain the " &
                          "nul char, but parameter Trim_Nul => True");
            Report.Comment(TC_String & " printed to defeat optimization");
         exception
            when Terminator_Error => null;  -- OK, expected exception.
            when others           =>
               Report.Failed("Incorrect exception raised by function "  &
                             "To_Ada when the Item parameter does not " &
                             "contain the nul char, but parameter "     &
                             "Trim_Nul => True");
         end;

      end;

   exception
      when The_Error : others =>
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB30041;
