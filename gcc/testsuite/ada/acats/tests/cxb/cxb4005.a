-- CXB4005.A
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
--      Check that the function To_COBOL will convert a String 
--      parameter value into a type Alphanumeric array of 
--      COBOL_Characters, with lower bound of one, and length 
--      equal to length of the String parameter, based on the 
--      mapping Ada_to_COBOL.
--
--      Check that the function To_Ada will convert a type 
--      Alphanumeric parameter value into a String type result, 
--      with lower bound of one, and length equal to the length 
--      of the Alphanumeric parameter, based on the mapping 
--      COBOL_to_Ada.
--
--      Check that the Ada_to_COBOL and COBOL_to_Ada mapping 
--      arrays provide a mapping capability between Ada's type 
--      Character and COBOL run-time character sets.
--
-- TEST DESCRIPTION:
--      This test checks that the functions To_COBOL and To_Ada produce
--      the correct results, based on a variety of parameter input values.
--      
--      In the first series of subtests, the results of the function
--      To_COBOL are compared against expected Alphanumeric type results,
--      and the length and lower bound of the alphanumeric result are
--      also verified.  In the second series of subtests, the results of
--      the function To_Ada are compared against expected String type
--      results, and the length of the String result is also verified 
--      against the Alphanumeric type parameter.
--      
--      This test also verifies that two mapping array variables defined 
--      in package Interfaces.COBOL, Ada_To_COBOL and COBOL_To_Ada, are 
--      available, and that they can be modified by a user at runtime.  
--      Finally, the effects of user modifications on these mapping 
--      variables is checked in the test.
--      
--      This test uses Fixed, Bounded, and Unbounded_Strings in combination
--      with the functions under validation.
--      
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.COBOL.COBOL_Character:
--      ' ', 'a'..'z', 'A'..'Z', '0'..'9', '*', ',', '.', and '$'.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.COBOL.  If an implementation provides
--      package Interfaces.COBOL, this test must compile, execute, and 
--      report "PASSED".
--
--       
-- CHANGE HISTORY:
--      11 Jan 96   SAIC    Initial prerelease version for ACVC 2.1
--      30 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Ada.Exceptions;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Interfaces.COBOL;                                          -- N/A => ERROR

procedure CXB4005 is
begin

   Report.Test ("CXB4005", "Check that the functions To_COBOL and " &
                           "To_Ada produce correct results");

   Test_Block:
   declare

      package Bnd is new Ada.Strings.Bounded.Generic_Bounded_Length(5);
      package Unb renames Ada.Strings.Unbounded;

      use Ada.Exceptions;
      use Interfaces;
      use Bnd;
      use type Unb.Unbounded_String;
      use type Interfaces.COBOL.Alphanumeric;

      TC_Alphanumeric_1  : Interfaces.COBOL.Alphanumeric(1..1);
      TC_Alphanumeric_5  : Interfaces.COBOL.Alphanumeric(1..5);
      TC_Alphanumeric_10 : Interfaces.COBOL.Alphanumeric(1..10);
      TC_Alphanumeric_20 : Interfaces.COBOL.Alphanumeric(1..20);

      Bnd_String,
      TC_Bnd_String      : Bnd.Bounded_String   :=
                             Bnd.To_Bounded_String("     ");
      Unb_String,
      TC_Unb_String      : Unb.Unbounded_String :=
                             Unb.To_Unbounded_String("                    ");

      The_String,
      TC_String          : String(1..20) := ("                    ");

   begin

      -- Check that the function To_COBOL will convert a String 
      -- parameter value into a type Alphanumeric array of 
      -- COBOL_Characters, with lower bound of one, and length 
      -- equal to length of the String parameter, based on the 
      -- mapping Ada_to_COBOL.

      Unb_String         := Unb.To_Unbounded_String("A");
      TC_Alphanumeric_1  := COBOL.To_COBOL(Unb.To_String(Unb_String));

      if TC_Alphanumeric_1        /= "A"                    or
         TC_Alphanumeric_1'Length /= Unb.Length(Unb_String) or
         TC_Alphanumeric_1'Length /= 1                      or
         COBOL.To_COBOL(Unb.To_String(Unb_String))'First  /= 1
      then
         Report.Failed("Incorrect result from function To_COBOL - 1");
      end if;

      Bnd_String         := Bnd.To_Bounded_String("abcde");
      TC_Alphanumeric_5  := COBOL.To_COBOL(Bnd.To_String(Bnd_String));

      if TC_Alphanumeric_5        /= "abcde"                or
         TC_Alphanumeric_5'Length /= Bnd.Length(Bnd_String) or
         TC_Alphanumeric_5'Length /= 5                      or
         COBOL.To_COBOL(Bnd.To_String(Bnd_String))'First  /= 1
      then
         Report.Failed("Incorrect result from function To_COBOL - 2");
      end if;

      Unb_String         := Unb.To_Unbounded_String("1A2B3c4d5F");
      TC_Alphanumeric_10 := COBOL.To_COBOL(Unb.To_String(Unb_String));

      if TC_Alphanumeric_10        /= "1A2B3c4d5F"           or
         TC_Alphanumeric_10'Length /= Unb.Length(Unb_String) or
         TC_Alphanumeric_10'Length /= 10                     or
         COBOL.To_COBOL(Unb.To_String(Unb_String))'First  /= 1
      then
         Report.Failed("Incorrect result from function To_COBOL - 3");
      end if;

      The_String         := "abcd  ghij" & "1234  7890";
      TC_Alphanumeric_20 := COBOL.To_COBOL(The_String);

      if TC_Alphanumeric_20                /= "abcd  ghij1234  7890" or
         TC_Alphanumeric_20'Length         /= The_String'Length      or
         TC_Alphanumeric_20'Length         /= 20                     or
         COBOL.To_COBOL(The_String)'First  /= 1
      then
         Report.Failed("Incorrect result from function To_COBOL - 4");
      end if;



      -- Check that the function To_Ada will convert a type 
      -- Alphanumeric parameter value into a String type result, 
      -- with lower bound of one, and length equal to the length 
      -- of the Alphanumeric parameter, based on the mapping 
      -- COBOL_to_Ada.

      TC_Unb_String := Unb.To_Unbounded_String 
                         (COBOL.To_Ada(TC_Alphanumeric_1));

      if TC_Unb_String             /= "A"                       or
         TC_Alphanumeric_1'Length  /= Unb.Length(TC_Unb_String) or
         Unb.Length(TC_Unb_String) /= 1                         or
         COBOL.To_Ada(TC_Alphanumeric_1)'First /= 1
      then
         Report.Failed("Incorrect value returned from function To_Ada - 1");
      end if;

      TC_Bnd_String := Bnd.To_Bounded_String 
                         (COBOL.To_Ada(TC_Alphanumeric_5));

      if TC_Bnd_String             /= "abcde"                   or
         TC_Alphanumeric_5'Length  /= Bnd.Length(TC_Bnd_String) or
         Bnd.Length(TC_Bnd_String) /= 5                         or
         COBOL.To_Ada(TC_Alphanumeric_5)'First /= 1
      then
         Report.Failed("Incorrect value returned from function To_Ada - 2");
      end if;

      TC_Unb_String := Unb.To_Unbounded_String 
                         (COBOL.To_Ada(TC_Alphanumeric_10));

      if TC_Unb_String             /= "1A2B3c4d5F"              or
         TC_Alphanumeric_10'Length /= Unb.Length(TC_Unb_String) or
         Unb.Length(TC_Unb_String) /= 10                        or
         COBOL.To_Ada(TC_Alphanumeric_10)'First /= 1
      then
         Report.Failed("Incorrect value returned from function To_Ada - 3");
      end if;

      TC_String := COBOL.To_Ada(TC_Alphanumeric_20);

      if TC_String                 /= "abcd  ghij1234  7890" or
         TC_Alphanumeric_20'Length /= TC_String'Length       or
         TC_String'Length          /= 20                     or
         COBOL.To_Ada(TC_Alphanumeric_20)'First /= 1
      then
         Report.Failed("Incorrect value returned from function To_Ada - 4");
      end if;


      -- Check the two functions when used in combination.

      if COBOL.To_COBOL(Item => COBOL.To_Ada("This is a test")) /=
         "This is a test"                                         or
         COBOL.To_COBOL(COBOL.To_Ada("1234567890abcdeFGHIJ"))   /=
         "1234567890abcdeFGHIJ"
      then
         Report.Failed("Incorrect result returned when using the " &
                       "functions To_Ada and To_COBOL in combination");
      end if;



      -- Check that the Ada_to_COBOL and COBOL_to_Ada mapping 
      -- arrays provide a mapping capability between Ada's type 
      -- Character and COBOL run-time character sets.

      Interfaces.COBOL.Ada_To_COBOL('a') := 'A';
      Interfaces.COBOL.Ada_To_COBOL('b') := 'B';
      Interfaces.COBOL.Ada_To_COBOL('c') := 'C';
      Interfaces.COBOL.Ada_To_COBOL('d') := '1';
      Interfaces.COBOL.Ada_To_COBOL('e') := '2';
      Interfaces.COBOL.Ada_To_COBOL('f') := '3';
      Interfaces.COBOL.Ada_To_COBOL(' ') := '*';

      Unb_String         := Unb.To_Unbounded_String("b");
      TC_Alphanumeric_1  := COBOL.To_COBOL(Unb.To_String(Unb_String));

      if TC_Alphanumeric_1 /= "B" then
         Report.Failed("Incorrect result from function To_COBOL after " &
                       "modification to Ada_To_COBOL mapping array - 1");
      end if;

      Bnd_String         := Bnd.To_Bounded_String("abcde");
      TC_Alphanumeric_5  := COBOL.To_COBOL(Bnd.To_String(Bnd_String));

      if TC_Alphanumeric_5 /= "ABC12" then
         Report.Failed("Incorrect result from function To_COBOL after " &
                       "modification to Ada_To_COBOL mapping array - 2");
      end if;

      Unb_String         := Unb.To_Unbounded_String("1a2B3c4d5e");
      TC_Alphanumeric_10 := COBOL.To_COBOL(Unb.To_String(Unb_String));

      if TC_Alphanumeric_10 /= "1A2B3C4152" then
         Report.Failed("Incorrect result from function To_COBOL after " &
                       "modification to Ada_To_COBOL mapping array - 3");
      end if;

      The_String         := "abcd  ghij" & "1234  7890";
      TC_Alphanumeric_20 := COBOL.To_COBOL(The_String);

      if TC_Alphanumeric_20 /= "ABC1**ghij1234**7890" then
         Report.Failed("Incorrect result from function To_COBOL after " &
                       "modification to Ada_To_COBOL mapping array - 4");
      end if;


      -- Reset the Ada_To_COBOL mapping array to its original state.

      Interfaces.COBOL.Ada_To_COBOL('a') := 'a';
      Interfaces.COBOL.Ada_To_COBOL('b') := 'b';
      Interfaces.COBOL.Ada_To_COBOL('c') := 'c';
      Interfaces.COBOL.Ada_To_COBOL('d') := 'd';
      Interfaces.COBOL.Ada_To_COBOL('e') := 'e';
      Interfaces.COBOL.Ada_To_COBOL('f') := 'f';
      Interfaces.COBOL.Ada_To_COBOL(' ') := ' ';

      -- Modify the COBOL_To_Ada mapping array to check its effect on
      -- the function To_Ada.

      Interfaces.COBOL.COBOL_To_Ada(' ') := '*';
      Interfaces.COBOL.COBOL_To_Ada('$') := 'F';
      Interfaces.COBOL.COBOL_To_Ada('1') := '7';
      Interfaces.COBOL.COBOL_To_Ada('.') := ',';

      Unb_String         := Unb.To_Unbounded_String("  $$100.00");
      TC_Alphanumeric_10 := COBOL.To_COBOL(Unb.To_String(Unb_String));
      TC_Unb_String      := Unb.To_Unbounded_String(
                              COBOL.To_Ada(TC_Alphanumeric_10));

      if Unb.To_String(TC_Unb_String) /= "**FF700,00" then
         Report.Failed("Incorrect result from function To_Ada after " &
                       "modification of COBOL_To_Ada mapping array - 1");
      end if;

      Interfaces.COBOL.COBOL_To_Ada('*') := ' ';
      Interfaces.COBOL.COBOL_To_Ada('F') := '$';
      Interfaces.COBOL.COBOL_To_Ada('7') := '1';
      Interfaces.COBOL.COBOL_To_Ada(',') := '.';

      if COBOL.To_Ada(COBOL.To_COBOL(Unb.To_String(TC_Unb_String))) /= 
         Unb_String 
      then
         Report.Failed("Incorrect result from function To_Ada after " &
                       "modification of COBOL_To_Ada mapping array - 2");
      end if;


   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB4005;
