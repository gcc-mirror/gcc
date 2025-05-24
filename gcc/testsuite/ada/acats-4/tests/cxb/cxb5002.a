-- CXB5002.A
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
--      Check that the Function To_Fortran with a Character parameter will
--      return the corresponding Fortran Character_Set value.
--
--      Check that the Function To_Ada with a Character_Set parameter will 
--      return the corresponding Ada Character value.
--
--      Check that the Function To_Fortran with a String parameter will
--      return the corresponding Fortran_Character value.
--
--      Check that the Function To_Ada with a Fortran_Character parameter
--      will return the corresponding Ada String value.
--
-- TEST DESCRIPTION:
--      This test checks that the functions To_Fortran and To_Ada produce
--      the correct results, based on a variety of parameter input values.
--      
--      In the first series of subtests, the results of the function
--      To_Fortran are compared against expected Character_Set type results.
--      In the second series of subtests, the results of the function To_Ada
--      are compared against expected String type results, and the length of
--      the String result is also verified against the Fortran_Character type
--      parameter.
--      
--      This test uses Fixed, Bounded, and Unbounded_Strings in combination
--      with the functions under validation.
--      
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.Fortran.Character_Set:
--      ' ', 'a'..'z', 'A'..'Z', '1'..'9', '-', '_', '$', '#', and '*'.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.Fortran.  If an implementation provides
--      package Interfaces.Fortran, this test must compile, execute, and 
--      report "PASSED".
--
--      This test does not apply to an implementation in which the Fortran
--      character set ranges are not contiguous (e.g., EBCDIC).
--      
--
--       
-- CHANGE HISTORY:
--      11 Mar 96   SAIC    Initial release for 2.1.
--      10 Jun 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Interfaces.Fortran;                                      -- N/A => ERROR
with Report;

procedure CXB5002 is
begin

   Report.Test ("CXB5002", "Check that functions To_Fortran and To_Ada " &
                           "produce correct results");

   Test_Block:
   declare

      package ACL renames Ada.Characters.Latin_1;
      package Bnd is new Ada.Strings.Bounded.Generic_Bounded_Length(10);
      package Unb renames Ada.Strings.Unbounded;

      use Bnd, Unb;
      use Interfaces.Fortran;
      use Ada.Exceptions;

      Null_Fortran_Character  : constant Fortran_Character := "";
      Fortran_Character_1     : Fortran_Character(1..1)    := " ";
      Fortran_Character_5     : Fortran_Character(1..5)    := "     ";
      Fortran_Character_10    : Fortran_Character(1..10)   := "          ";
      Fortran_Character_20    : Fortran_Character(1..20)   := 
                                  "                    ";
      TC_Fortran_Character_1  : Fortran_Character(1..1)    := "A";
      TC_Fortran_Character_5  : Fortran_Character(1..5)    := "ab*de";
      TC_Fortran_Character_10 : Fortran_Character(1..10)   := "$1a2b3C4D5";
      TC_Fortran_Character_20 : Fortran_Character(1..20)   := 
                                  "1234-ABCD_6789#fghij";

      Bnd_String              : Bnd.Bounded_String   :=
                                  Bnd.To_Bounded_String("          ");
      TC_Bnd_String           : Bounded_String       :=
                                  To_Bounded_String("$1a2b3C4D5");

      Unb_String              : Unb.Unbounded_String :=
                                  Unb.To_Unbounded_String("     ");
      TC_Unb_String           : Unbounded_String     :=
                                  To_Unbounded_String("ab*de");

      String_1                : String(1..1)    := " ";
      String_5                : String(1..5)    := "     ";
      String_10               : String(1..10)   := "          ";
      String_20               : String(1..20)   := "                    ";
      TC_String_1             : String(1..1)    := "A";
      TC_String_20            : String(1..20)   := "1234-ABCD_6789#fghij";
      Null_String             : constant String := "";

      Null_Character          : constant Character := ACL.Nul;
      Character_A             : constant Character := Character'Val(65);
      Character_Z             : constant Character := Character'Val(90);
      TC_Character            : Character          := Character'First;

      Null_Character_Set      : Character_Set      := To_Fortran(ACL.Nul);
      TC_Character_Set,
      TC_Low_Character_Set,
      TC_High_Character_Set   : Character_Set      := Character_Set'First;


      -- The following procedure checks the results of function To_Ada.

      procedure Check_Length (Str : in String;
                              Ftn : in Fortran_Character;
                              Num : in Natural) is
      begin
         if Str'Length /= Ftn'Length or
            Str'Length /= Num
         then
            Report.Failed("Incorrect result from Function To_Ada " &
                          "with string length " & Integer'Image(Num));
         end if;
      end Check_Length;

      -- To facilitate the conversion of Character-Character_Set data, the 
      -- following functions have been instantiated.  

      function Character_to_Character_Set is
        new Ada.Unchecked_Conversion(Character, Character_Set);

      function Character_Set_to_Character is
        new Ada.Unchecked_Conversion(Character_Set, Character);

   begin

      -- Check that the Function To_Fortran with a Character parameter
      -- will return the corresponding Fortran Character_Set value.

      for TC_Character in ACL.LC_A..ACL.LC_Z loop
         if To_Fortran(Item => TC_Character) /= 
            Character_to_Character_Set(TC_Character) 
         then
            Report.Failed("Incorrect result from To_Fortran with lower " &
                          "case alphabetic character input");
         end if;
      end loop;

      for TC_Character in Character_A..Character_Z loop
         if To_Fortran(TC_Character) /= 
            Character_to_Character_Set(TC_Character) 
         then
            Report.Failed("Incorrect result from To_Fortran with upper " &
                          "case alphabetic character input");
         end if;
      end loop;

      if To_Fortran(Null_Character) /= 
         Character_to_Character_Set(Null_Character)
      then
         Report.Failed
           ("Incorrect result from To_Fortran with null character input");
      end if;


      -- Check that the Function To_Ada with a Character_Set parameter 
      -- will return the corresponding Ada Character value.

      TC_Low_Character_Set  := Character_to_Character_Set('a');
      TC_High_Character_Set := Character_to_Character_Set('z');
      for TC_Character_Set in TC_Low_Character_Set..TC_High_Character_Set loop
         if To_Ada(Item => TC_Character_Set) /= 
            Character_Set_to_Character(TC_Character_Set) 
         then
            Report.Failed("Incorrect result from To_Ada with lower case " &
                          "alphabetic Character_Set input");
         end if;
      end loop;

      TC_Low_Character_Set  := Character_to_Character_Set('A');
      TC_High_Character_Set := Character_to_Character_Set('Z');
      for TC_Character_Set in TC_Low_Character_Set..TC_High_Character_Set loop
         if To_Ada(TC_Character_Set) /= 
            Character_Set_to_Character(TC_Character_Set) 
         then
            Report.Failed("Incorrect result from To_Ada with upper case " &
                          "alphabetic Character_Set input");
         end if;
      end loop;

      if To_Ada(Character_to_Character_Set(Null_Character)) /=
         Null_Character
      then
         Report.Failed("Incorrect result from To_Ada with a null " &
                       "Character_Set input");
      end if;


      -- Check that the Function To_Fortran with a String parameter 
      -- will return the corresponding Fortran_Character value.
      -- Note: The type Fortran_Character is a character array type that
      --       corresponds to Ada type String.

      Fortran_Character_1 := To_Fortran(Item => TC_String_1);

      if Fortran_Character_1 /= TC_Fortran_Character_1 then
         Report.Failed("Incorrect result from procedure To_Fortran - 1");
      end if;

      Fortran_Character_5 := To_Fortran(To_String(TC_Unb_String));

      if Fortran_Character_5 /= TC_Fortran_Character_5 then
         Report.Failed("Incorrect result from procedure To_Fortran - 2");
      end if;

      Fortran_Character_10 := To_Fortran(To_String(TC_Bnd_String));

      if Fortran_Character_10 /= TC_Fortran_Character_10 then
         Report.Failed("Incorrect result from procedure To_Fortran - 3");
      end if;

      Fortran_Character_20 := To_Fortran(Item => TC_String_20);

      if Fortran_Character_20 /= TC_Fortran_Character_20 then
         Report.Failed("Incorrect result from procedure To_Fortran - 4");
      end if;

      if To_Fortran(Null_String) /= Null_Fortran_Character then
         Report.Failed("Incorrect result from procedure To_Fortran - 5");
      end if;


      -- Check that the Function To_Ada with a Fortran_Character parameter
      -- will return the corresponding Ada String value.

      String_1 := To_Ada(TC_Fortran_Character_1);

      if String_1 /= TC_String_1 then
         Report.Failed("Incorrect value returned from function To_Ada - 1");
      end if;

      Check_Length(To_Ada(TC_Fortran_Character_1),
                   TC_Fortran_Character_1,
                   Num => 1);


      Unb_String := Unb.To_Unbounded_String(To_Ada(TC_Fortran_Character_5));

      if Unb_String /= TC_Unb_String then
         Report.Failed("Incorrect value returned from function To_Ada - 2");
      end if;

      Check_Length(To_Ada(TC_Fortran_Character_5),
                   TC_Fortran_Character_5,
                   Num => 5);


      Bnd_String := Bnd.To_Bounded_String 
                         (To_Ada(TC_Fortran_Character_10));

      if Bnd_String /= TC_Bnd_String then
         Report.Failed("Incorrect value returned from function To_Ada - 3");
      end if;

      Check_Length(To_Ada(TC_Fortran_Character_10),
                   TC_Fortran_Character_10,
                   Num => 10);


      String_20 := To_Ada(TC_Fortran_Character_20);

      if String_20 /= TC_String_20 then
         Report.Failed("Incorrect value returned from function To_Ada - 4");
      end if;

      Check_Length(To_Ada(TC_Fortran_Character_20),
                   TC_Fortran_Character_20,
                   Num => 20);

      if To_Ada(Null_Character_Set) /= Null_Character then
         Report.Failed("Incorrect value returned from function To_Ada - 5");
      end if;


      -- Check the two functions when used in combination.

      if To_Ada(Item => To_Fortran("This is a test")) /=
         "This is a test"                                or
         To_Ada(To_Fortran("1234567890abcdeFGHIJ"))   /=
         Report.Ident_Str("1234567890abcdeFGHIJ")
      then
         Report.Failed("Incorrect result returned when using the " &
                       "functions To_Ada and To_Fortran in combination");
      end if;


   exception
      when The_Error : others => 
         Report.Failed("The following exception was raised in the " &
                       "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB5002;
