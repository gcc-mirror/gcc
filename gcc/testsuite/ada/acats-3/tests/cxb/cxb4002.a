-- CXB4002.A
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
--      Check that the procedure To_COBOL converts the character elements 
--      of the String parameter Item into COBOL_Character elements of the 
--      Alphanumeric type parameter Target, using the Ada_to_COBOL mapping 
--      as the basis of conversion.  
--      Check that the parameter Last contains the index of the last element 
--      of parameter Target that was assigned by To_COBOL.
--
--      Check that Constraint_Error is propagated by procedure To_COBOL 
--      when the length of String parameter Item exceeds the length of 
--      Alphanumeric parameter Target.
--
--      Check that the procedure To_Ada converts the COBOL_Character 
--      elements of the Alphanumeric parameter Item into Character elements 
--      of the String parameter Target, using the COBOL_to_Ada mapping array 
--      as the basis of conversion.  
--      Check that the parameter Last contains the index of the last element 
--      of parameter Target that was assigned by To_Ada.
--
--      Check that Constraint_Error is propagated by procedure To_Ada when 
--      the length of Alphanumeric parameter Item exceeds the length of 
--      String parameter Target.
--
-- TEST DESCRIPTION:
--      This test checks that the procedures To_COBOL and To_Ada produce
--      the correct results, based on a variety of parameter input values.
--      
--      In the first series of subtests, the Out parameter results of 
--      procedure To_COBOL are compared against expected results,
--      which includes (in the parameter Last) the index in Target of the 
--      last element assigned.  The situation where procedure To_COBOL raises
--      Constraint_Error (when Item'Length exceeds Target'Length) is also
--      verified.
--
--      In the second series of subtests, the Out parameter results of 
--      procedure To_Ada are verified, in a similar manner as is done for
--      procedure To_COBOL.  The case of procedure To_Ada raising 
--      Constraint_Error is also verified.
--      
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.COBOL.COBOL_Character:
--      ' ', 'a'..'z', 'A'..'Z', '0'..'9', '*', '$', '-', '_', and '#'.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.COBOL.  If an implementation provides
--      package Interfaces.COBOL, this test must compile, execute, and 
--      report "PASSED".
--
--       
-- CHANGE HISTORY:
--      12 Jan 96   SAIC    Initial prerelease version.
--      30 May 96   SAIC    Added applicability criteria for ACVC 2.1.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Interfaces.COBOL;                                          -- N/A => ERROR

procedure CXB4002 is
begin

   Report.Test ("CXB4002", "Check that the procedures To_COBOL and " &
                           "To_Ada produce correct results");

   Test_Block:
   declare

      package Bnd is new Ada.Strings.Bounded.Generic_Bounded_Length(10);
      package Unb renames Ada.Strings.Unbounded;

      use Interfaces;
      use Bnd, Unb;
      use type Interfaces.COBOL.Alphanumeric;


      Alphanumeric_1     : COBOL.Alphanumeric(1..1)  := " ";
      Alphanumeric_5     : COBOL.Alphanumeric(1..5)  := "     ";
      Alphanumeric_10    : COBOL.Alphanumeric(1..10) := "          ";
      Alphanumeric_20    : COBOL.Alphanumeric(1..20) := "                    ";
      TC_Alphanumeric_1  : COBOL.Alphanumeric(1..1)  := "A";
      TC_Alphanumeric_5  : COBOL.Alphanumeric(1..5)  := "ab*de";
      TC_Alphanumeric_10 : COBOL.Alphanumeric(1..10) := "$1a2b3C4D5";
      TC_Alphanumeric_20 : COBOL.Alphanumeric(1..20) := "1234-ABCD_6789#fghij";

      Bnd_String         : Bnd.Bounded_String   :=
                             Bnd.To_Bounded_String("          ");
      TC_Bnd_String      : Bounded_String       :=
                             To_Bounded_String("$1a2b3C4D5");

      Unb_String         : Unb.Unbounded_String :=
                             Unb.To_Unbounded_String("     ");
      TC_Unb_String      : Unbounded_String     :=
                             To_Unbounded_String("ab*de");

      String_1           : String(1..1)   := " ";
      String_5           : String(1..5)   := "     ";
      String_10          : String(1..10)  := "          ";
      String_20          : String(1..20)  := "                    ";
      TC_String_1        : String(1..1)   := "A";
      TC_String_20       : String(1..20)  := "1234-ABCD_6789#fghij";

      TC_Alphanumeric    : constant COBOL.Alphanumeric := ""; -- null array.
      TC_String          : constant String := "";             -- null string.
      TC_Natural         : Natural         := 0;


   begin

      -- Check that the procedure To_COBOL converts the character elements 
      -- of the String parameter Item into COBOL_Character elements of the 
      -- Alphanumeric type parameter Target, using the Ada_to_COBOL mapping 
      -- as the basis of conversion.  
      -- Check that the parameter Last contains the index of the last element 
      -- of parameter Target that was assigned by To_COBOL.
      
      COBOL.To_COBOL(Item   => TC_String_1,
                     Target => Alphanumeric_1,
                     Last   => TC_Natural);

      if Alphanumeric_1 /= TC_Alphanumeric_1        or
         TC_Natural     /= TC_Alphanumeric_1'Length or
         TC_Natural     /= 1
      then
         Report.Failed("Incorrect result from procedure To_COBOL - 1");
      end if;

      COBOL.To_COBOL(To_String(TC_Unb_String),
                     Target => Alphanumeric_5,
                     Last   => TC_Natural);

      if Alphanumeric_5 /= TC_Alphanumeric_5        or
         TC_Natural     /= TC_Alphanumeric_5'Length or
         TC_Natural     /= 5
      then
         Report.Failed("Incorrect result from procedure To_COBOL - 2");
      end if;

      COBOL.To_COBOL(To_String(TC_Bnd_String),
                     Alphanumeric_10,
                     Last   => TC_Natural);

      if Alphanumeric_10 /= TC_Alphanumeric_10        or
         TC_Natural      /= TC_Alphanumeric_10'Length or
         TC_Natural      /= 10
      then
         Report.Failed("Incorrect result from procedure To_COBOL - 3");
      end if;

      COBOL.To_COBOL(TC_String_20,
                     Alphanumeric_20,
                     TC_Natural);

      if Alphanumeric_20 /= TC_Alphanumeric_20        or
         TC_Natural      /= TC_Alphanumeric_20'Length or
         TC_Natural      /= 20
      then
         Report.Failed("Incorrect result from procedure To_COBOL - 4");
      end if;

      COBOL.To_COBOL(Item   => TC_String,     -- null string
                     Target => Alphanumeric_1,
                     Last   => TC_Natural);

      if TC_Natural /= 0 then
         Report.Failed("Incorrect result from procedure To_COBOL, value "  &
                       "returned in parameter Last should be zero, since " &
                       "parameter Item is null array");
      end if;



      -- Check that Constraint_Error is propagated by procedure To_COBOL 
      -- when the length of String parameter Item exceeds the length of 
      -- Alphanumeric parameter Target.

      begin

         COBOL.To_COBOL(Item   => TC_String_20,
                        Target => Alphanumeric_10,
                        Last   => TC_Natural);
         Report.Failed("Constraint_Error not raised by procedure To_COBOL " &
                       "when Item'Length exceeds Target'Length");
      exception
         when Constraint_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Incorrect exception raised by procedure To_COBOL " &
                          "when Item'Length exceeds Target'Length");
      end;

      
      -- Check that the procedure To_Ada converts the COBOL_Character 
      -- elements of the Alphanumeric parameter Item into Character elements 
      -- of the String parameter Target, using the COBOL_to_Ada mapping array 
      -- as the basis of conversion.  
      -- Check that the parameter Last contains the index of the last element 
      -- of parameter Target that was assigned by To_Ada.
      
      COBOL.To_Ada(Item   => TC_Alphanumeric_1,
                   Target => String_1,
                   Last   => TC_Natural);

      if String_1   /= TC_String_1        or
         TC_Natural /= TC_String_1'Length or
         TC_Natural /= 1
      then
         Report.Failed("Incorrect result from procedure To_Ada - 1");
      end if;

      COBOL.To_Ada(TC_Alphanumeric_5,
                   Target => String_5,
                   Last   => TC_Natural);

      if String_5   /= To_String(TC_Unb_String) or
         TC_Natural /= Length(TC_Unb_String)    or
         TC_Natural /= 5
      then
         Report.Failed("Incorrect result from procedure To_Ada - 2");
      end if;

      COBOL.To_Ada(TC_Alphanumeric_10,
                   String_10,
                   Last   => TC_Natural);

      if String_10   /= To_String(TC_Bnd_String) or
         TC_Natural  /= Length(TC_Bnd_String)    or
         TC_Natural  /= 10
      then
         Report.Failed("Incorrect result from procedure To_Ada - 3");
      end if;

      COBOL.To_Ada(TC_Alphanumeric_20,
                   String_20,
                   TC_Natural);

      if String_20   /= TC_String_20        or
         TC_Natural  /= TC_String_20'Length or
         TC_Natural  /= 20
      then
         Report.Failed("Incorrect result from procedure To_Ada - 4");
      end if;

      COBOL.To_Ada(Item   => TC_Alphanumeric,  -- null array.
                   Target => String_20,
                   Last   => TC_Natural);

      if TC_Natural /= 0 then
         Report.Failed("Incorrect result from procedure To_Ada, value "    &
                       "returned in parameter Last should be zero, since " &
                       "parameter Item is null array");
      end if;



      -- Check that Constraint_Error is propagated by procedure To_Ada when 
      -- the length of Alphanumeric parameter Item exceeds the length of 
      -- String parameter Target.

      begin

         COBOL.To_Ada(Item   => TC_Alphanumeric_10,
                      Target => String_5,
                      Last   => TC_Natural);
         Report.Failed("Constraint_Error not raised by procedure To_Ada " &
                       "when Item'Length exceeds Target'Length");
      exception
         when Constraint_Error => null;  -- OK, expected exception.
         when others           =>
            Report.Failed("Incorrect exception raised by procedure To_Ada " &
                          "when Item'Length exceeds Target'Length");
      end;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXB4002;
