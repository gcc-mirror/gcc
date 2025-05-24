-- CXA3006.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
-- OBJECTIVE:
--      Check that the conversion functions for Wide_Characters and Wide_Strings
--      defined in package Ada.Wide_Characters.Handling provide correct results
--      when given wide character/string input parameters.
--
-- TEST DESCRIPTION:
--      This test checks the output of the To_Lower and To_Upper functions for
--      both Wide_Characters and Wide_Strings.  Each function is called with
--      input parameters that are within the appropriate range of values, and
--      also with values outside the specified range (i.e. lower case 'a' to
--      To_Lower).  The functions are also used in combination with one another,
--      with the result of one function providing the actual input parameter
--      value to another.
--
-- CHANGE HISTORY:
--      21 May 13   JAC     Initial pre-release version.
--      10 Jul 13   JAC     Second pre-release version.
--      20 Mar 14   RLB     Readied to issue; renamed.
--!
with Ada.Wide_Characters.Handling;
with Report;

procedure CXA3006 is

   package AWCH renames Ada.Wide_Characters.Handling;

   procedure Test_Wide_Strings
     (LC_Wide_String_1 : in Wide_String;
      UC_Wide_String_1 : in Wide_String;
      LC_Wide_String_2 : in Wide_String;
      UC_Wide_String_2 : in Wide_String) is
   begin

      -- Function To_Lower for Wide_Strings

      if AWCH.To_Lower (UC_Wide_String_1) /= LC_Wide_String_1  or
         AWCH.To_Lower (LC_Wide_String_1) /= LC_Wide_String_1
      then

         Report.Failed ("Incorrect result from To_Lower for wide strings - 1");

      end if;

      if AWCH.To_Lower (UC_Wide_String_2) /= LC_Wide_String_2 then

         Report.Failed ("Incorrect result from To_Lower for wide strings - 2");

      end if;

      -- Function To_Upper for Wide_Strings

      if not (AWCH.To_Upper (LC_Wide_String_1) = UC_Wide_String_1) then

         Report.Failed ("Incorrect result from To_Upper for wide strings - 1");

      end if;

      if AWCH.To_Upper (LC_Wide_String_2) /= UC_Wide_String_2 or
         AWCH.To_Upper (UC_Wide_String_1) /= UC_Wide_String_1
      then

         Report.Failed ("Incorrect result from To_Upper for wide strings - 2");

      end if;

      -- Using Functions (for Wide_Strings) in Combination

      if AWCH.To_Upper (AWCH.To_Lower (UC_Wide_String_2)) /= UC_Wide_String_2 or
         AWCH.To_Lower (AWCH.To_Upper (LC_Wide_String_2)) /= LC_Wide_String_2
      then

         Report.Failed
           ("Incorrect operation of combined functions for wide strings");

      end if;

   end Test_Wide_Strings;

begin

   Report.Test
     ("CXA3006",
      "Check that the conversion functions for Wide_Characters and " &
      "Wide_Strings defined in package Ada.Wide_Characters.Handling provide " &
      "correct results when given wide character/string input parameters");


   Wide_Character_Block:
   declare
      Offset : constant Integer := Wide_Character'Pos ('a') -
                                   Wide_Character'Pos ('A');
   begin

      -- Function To_Lower for Wide_Characters

      if AWCH.To_Lower ('A') /= 'a' or AWCH.To_Lower ('Z') /= 'z' then

         Report.Failed ("Incorrect operation of function To_Lower - 1");

      end if;

      for I in Wide_Character'Pos ('A') .. Wide_Character'Pos ('Z') loop

         if AWCH.To_Lower (Wide_Character'Val (I)) /=
                           Wide_Character'Val (I + Offset) then

            Report.Failed ("Incorrect operation of function To_Lower - 2");

         end if;

      end loop;

      if AWCH.To_Lower ('c') /= 'c' or
         AWCH.To_Lower ('w') /= 'w' or
         AWCH.To_Lower ('0') /= '0' or
         AWCH.To_Lower ('9') /= '9'
      then

         Report.Failed ("Incorrect operation of function To_Lower - 3");

      end if;

      --- Function To_Upper for Wide_Characters

      if not (AWCH.To_Upper ('b') = 'B') and (AWCH.To_Upper ('y') = 'Y') then

         Report.Failed ("Incorrect operation of function To_Upper - 1");

      end if;

      if not (AWCH.To_Upper ('F') = 'F' and
              AWCH.To_Upper ('U') = 'U')
      then

         Report.Failed ("Incorrect operation of function To_Upper - 2");

      end if;

      -- Using Functions (for Wide_Characters) in Combination

      if AWCH.To_Upper (AWCH.To_Lower ('A')) /= 'A'
      then

         Report.Failed ("Incorrect operation of combined functions - 1");

      end if;

   exception

      when others => Report.Failed ("Exception raised in Wide_Character_Block");

   end Wide_Character_Block;


   Greek_Wide_Character_Block:
   declare
      -- Small Alpha - Capital Alpha
      Offset : constant Integer := 16#3B1# - 16#0391#;
   begin

      -- Function To_Lower for Wide_Characters

      for I in 16#0391# .. 16#3A1# loop -- Capital Alpha to Capital Rho

         if AWCH.To_Lower (Wide_Character'Val (I)) /=
                           Wide_Character'Val (I + Offset) then

            Report.Failed ("Incorrect operation of function To_Lower - 4");

         end if;

      end loop;

      -- Note that Final Sigma is small only

      for I in 16#03A3# .. 16#3A9# loop -- Capital Sigma to Capital Omega

         if AWCH.To_Lower (Wide_Character'Val (I)) /=
                           Wide_Character'Val (I + Offset) then

            Report.Failed ("Incorrect operation of function To_Lower - 5");

         end if;

      end loop;

      if AWCH.To_Lower (Wide_Character'Val (16#0391#)) /= -- Capital Alpha
          Wide_Character'Val (16#03B1#) or
         AWCH.To_Lower (Wide_Character'Val (16#03A9#)) /= -- Capital Omega
          Wide_Character'Val (16#03C9#)
      then

         Report.Failed ("Incorrect operation of function To_Lower - 6");

      end if;

      --- Function To_Upper for Wide_Characters

      for I in 16#03B1# .. 16#3C1# loop -- Small Alpha to Small Rho

         if AWCH.To_Upper (Wide_Character'Val (I)) /=
                           Wide_Character'Val (I - Offset) then

            Report.Failed ("Incorrect operation of function To_Upper - 3");

         end if;

      end loop;

      -- Note that Final Sigma is small only

      for I in 16#03C3# .. 16#3C9# loop -- Small Sigma to Small Omega

         if AWCH.To_Upper (Wide_Character'Val (I)) /=
                           Wide_Character'Val (I - Offset) then

            Report.Failed ("Incorrect operation of function To_Upper - 4");

         end if;

      end loop;

      if AWCH.To_Lower (Wide_Character'Val (16#03B1#)) /= -- Small Alpha
          Wide_Character'Val (16#03B1#) or
         AWCH.To_Lower (Wide_Character'Val (16#03B9#)) /= -- Snmall Omega
          Wide_Character'Val (16#03B9#)
      then

         Report.Failed ("Incorrect operation of function To_Lower - 7");

      end if;

      -- Using Functions (for Wide_Characters) in Combination

      if AWCH.To_Upper (AWCH.To_Lower (Wide_Character'Val (16#0391#))) /=
        Wide_Character'Val (16#0391#) -- Capital Alpha
      then

         Report.Failed ("Incorrect operation of combined functions - 2");

      end if;

   exception

      when others => Report.Failed
                      ("Exception raised in Greek_Wide_Character_Block");

   end Greek_Wide_Character_Block;


   Wide_String_Block:
   declare

      LC_Wide_String_1 : constant Wide_String := "az";

      UC_Wide_String_1 : constant Wide_String := "AZ";

      LC_Wide_String_2 : constant Wide_String := "aei" & 'o' & 'u';

      UC_Wide_String_2 : constant Wide_String := "AEIOU";

   begin

      Test_Wide_Strings
        (LC_Wide_String_1 => LC_Wide_String_1,
         UC_Wide_String_1 => UC_Wide_String_1,
         LC_Wide_String_2 => LC_Wide_String_2,
         UC_Wide_String_2 => UC_Wide_String_2);

   exception

      when others => Report.Failed ("Exception raised in Wide_String_Block");

   end Wide_String_Block;


   Greek_Wide_String_Block:
   declare

      LC_Wide_String_1 : constant Wide_String :=
        (Wide_Character'Val (16#03B1#),  -- Small Alpha
         Wide_Character'Val (16#03C9#)); -- Small Omega

      UC_Wide_String_1 : constant Wide_String :=
        (Wide_Character'Val (16#0391#),  -- Capital Alpha
         Wide_Character'Val (16#03A9#)); -- Capital Omega

      LC_Wide_String_2 : constant Wide_String :=
        (Wide_Character'Val (16#03B1#),  -- Small Alpha
         Wide_Character'Val (16#03B5#),  -- Small Epsilon
         Wide_Character'Val (16#03B9#),  -- Small Iota
         Wide_Character'Val (16#03BF#),  -- Small Omicron
         Wide_Character'Val (16#03C5#)); -- Small Upsilon

      UC_Wide_String_2 : constant Wide_String :=
        (Wide_Character'Val (16#0391#),  -- Capital Alpha
         Wide_Character'Val (16#0395#),  -- Capital Epsilon
         Wide_Character'Val (16#0399#),  -- Capital Iota
         Wide_Character'Val (16#039F#),  -- Capital Omicron
         Wide_Character'Val (16#03A5#)); -- Capital Upsilon

   begin

      Test_Wide_Strings
        (LC_Wide_String_1 => LC_Wide_String_1,
         UC_Wide_String_1 => UC_Wide_String_1,
         LC_Wide_String_2 => LC_Wide_String_2,
         UC_Wide_String_2 => UC_Wide_String_2);

   exception

      when others => Report.Failed
                      ("Exception raised in Greek_Wide_String_Block");

   end Greek_Wide_String_Block;


   Report.Result;

end CXA3006;
