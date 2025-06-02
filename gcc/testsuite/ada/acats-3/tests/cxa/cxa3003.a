-- CXA3003.A
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
--      Check that the functions defined in package Ada.Characters.Handling
--      for use in classifying and converting characters between the ISO 646
--      and type Character sets produce the correct results with both
--      Character and String input values.
--
-- TEST DESCRIPTION:
--      This test is designed to exercise the classification and conversion
--      functions (between Character and ISO_646 types) found in package 
--      Ada.Characters.Handling.  Two subprograms are defined, a procedure for
--      characters, a function for strings, that will utilize these functions
--      to validate and change characters in variables.  In the procedure, if
--      a character argument is found to be outside the subtype ISO_646, this 
--      character is evaluated to determine whether it is also a letter.  
--      If it is a letter, the character is converted to a basic character and 
--      returned.  If it is not a letter, the character is exchanged with an 
--      asterisk.  In the case of the function subprogram designed for strings, 
--      if a character component of a string argument is outside the subtype
--      ISO_646, that character is substituted with an asterisk.
--      
--      Arguments for the defined subprograms consist of ISO_646 characters,
--      non-ISO_646 characters, strings with only ISO_646 characters, and
--      strings with non-ISO_646 characters. The character and string values 
--      are then validated to determine that the expected results were 
--      obtained.
--      
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      29 Apr 95   SAIC    Modified identifier string lengths.
--      31 Oct 95   SAIC    Update and repair for ACVC 2.0.1.
--
--!

with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Report;

procedure CXA3003 is

begin

   Report.Test ("CXA3003", "Check that the functions defined in package " &
                           "Ada.Characters.Handling for use in "          &
                           "classifying and converting characters "       &
                           "between the ISO 646 and type Character sets " &
                           "produce the correct results with both "       &
                           "Character and String input values" );

   Test_Block:
   declare

      -- ISO_646 Characters

      Char_1,
      TC_Char_1  : Character := Ada.Characters.Latin_1.NUL;    -- Control Char
      Char_2,
      TC_Char_2  : Character := Ada.Characters.Latin_1.Colon;  -- Graphic Char
      Char_3,
      TC_Char_3  : Character := '4';
      Char_4,
      TC_Char_4  : Character := 'Z';
      Char_5,
      TC_Char_5  : Character := Ada.Characters.Latin_1.LC_W;   -- w

      New_ISO_646_Char : Character := '*';


      -- Non-ISO_646 Characters

      Char_Array : array (6..10) of Character := 
                                       (Ada.Characters.Latin_1.SSA,
                                        Ada.Characters.Latin_1.Cent_Sign,
                                        Ada.Characters.Latin_1.Cedilla,
                                        Ada.Characters.Latin_1.UC_A_Ring,
                                        Ada.Characters.Latin_1.LC_A_Ring);

      TC_Char : constant Character := '*';

      -- ISO_646 Strings

      Str_1, 
      TC_Str_1 : String (1..5) := "ABCDE";

      Str_2,
      TC_Str_2 : String (1..5) := "#$%^&";


      -- Non-ISO_646 Strings

      Str_3    : String (1..8) := "$123.45" & 
                                  Ada.Characters.Latin_1.Cent_Sign;
      TC_Str_3 : String (1..8) := "$123.45*";

      Str_4    : String (1..7) := "abc" & 
                                  Ada.Characters.Latin_1.Cedilla &
                                  "efg";
      TC_Str_4 : String (1..7) := "abc*efg";

      Str_5    : String (1..3) := Ada.Characters.Latin_1.LC_E_Grave & 
                                  Ada.Characters.Latin_1.LC_T       &
                                  Ada.Characters.Latin_1.LC_E_Acute;
      TC_Str_5 : String (1..3) := "*t*";

      ---

      procedure Validate_Character (Char : in out Character) is
         -- If parameter Char is an ISO_646 character, Char will be returned,
         -- otherwise the following constant will be returned.
         Star : constant Ada.Characters.Handling.ISO_646 := 
           Ada.Characters.Latin_1.Asterisk;                    
      begin
         if Ada.Characters.Handling.Is_ISO_646(Char) then
            -- Check that the Is_ISO_646 function provide a correct result.
            if Character'Pos(Char) > 127 then
               Report.Failed("Is_ISO_646 returns a false positive result");
            end if;
         else
            if Character'Pos(Char) < 128 then
               Report.Failed("Is_ISO_646 returns a false negative result");
            end if;
         end if;
         -- Cross-check Is_ISO_646 with To_ISO_646.  '*' will be returned
         -- if Char is not in the ISO_646 set.
         Char := Ada.Characters.Handling.To_ISO_646(Char, Star);
      exception
         when others => Report.Failed ("Exception in Validate_Character");
      end Validate_Character;

      ---

      function Validate_String (Str : String) return String is
         New_ISO_646_Char : constant Ada.Characters.Handling.ISO_646 := 
           Ada.Characters.Latin_1.Asterisk;                    
      begin
         -- Checking that the string contains non-ISO_646 characters at this
         -- point is not strictly necessary, since the function To_ISO_646
         -- will perform that check as part of its processing, and would 
         -- return the original string if no modification were necessary.  
         -- However, this format allows for the testing of both functions.

         if not Ada.Characters.Handling.Is_ISO_646(Str) then
            return Ada.Characters.Handling.To_ISO_646
                     (Item => Str, Substitute => New_ISO_646_Char);
         else
            return Str;
         end if;
      exception
         when others => Report.Failed ("Exception in Validate_String");
                        return Str;
      end Validate_String;


   begin

      -- Check each character in turn, and if the character does not belong
      -- to the ISO_646 subset of type Character, replace it with an
      -- asterisk.  If the character is a member of the subset, the character
      -- should be returned unchanged.

      Validate_Character (Char_1);
      Validate_Character (Char_2);
      Validate_Character (Char_3);
      Validate_Character (Char_4);
      Validate_Character (Char_5);

      if Char_1 /= TC_Char_1 or Char_2 /= TC_Char_2 or
         Char_3 /= TC_Char_3 or Char_4 /= TC_Char_4 or
         Char_5 /= TC_Char_5
      then
         Report.Failed ("Incorrect ISO_646 character substitution");
      end if;

      -- Non-ISO_646 characters

      for i in 6..10 loop
         Validate_Character (Char_Array(i));
      end loop;

      for i in 6..10 loop
         if Char_Array(i) /= TC_Char then
            Report.Failed ("Character position " & Integer'Image(i) &
                           " not replaced correctly");
         end if;
      end loop;



      -- Check each string, and if the string contains characters that do not
      -- belong to the ISO_646 subset of type Character, replace that character
      -- in the string with an asterisk.  If the string is comprised of only
      -- ISO_646 characters, the string should be returned unchanged.


      Str_1 := Validate_String (Str_1);
      Str_2 := Validate_String (Str_2);
      Str_3 := Validate_String (Str_3);
      Str_4 := Validate_String (Str_4);
      Str_5 := Validate_String (Str_5);


      if Str_1 /= TC_Str_1 or
         Str_2 /= TC_Str_2 or
         Str_3 /= TC_Str_3 or
         Str_4 /= TC_Str_4 or
         Str_5 /= TC_Str_5
      then
         Report.Failed ("Incorrect ISO_646 character substitution in string");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA3003;
