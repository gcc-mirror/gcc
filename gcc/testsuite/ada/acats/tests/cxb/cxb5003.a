-- CXB5003.A
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
--      Check that the procedure To_Fortran converts the character elements
--      of the String parameter Item into Character_Set elements of the
--      Fortran_Character type parameter Target.  Check that the parameter 
--      Last contains the index of the last element of parameter Target 
--      that was assigned by To_Fortran.
--
--      Check that Constraint_Error is propagated by procedure To_Fortran
--      when the length of String parameter Item exceeds the length of 
--      Fortran_Character parameter Target.
--
--      Check that the procedure To_Ada converts the Character_Set
--      elements of the Fortran_Character parameter Item into Character
--      elements of the String parameter Target.  Check that the parameter
--      Last contains the index of the last element of parameter Target 
--      that was assigned by To_Ada.
--
--      Check that Constraint_Error is propagated by procedure To_Ada when
--      the length of Fortran_Character parameter Item exceeds the length of 
--      String parameter Target.
--
-- TEST DESCRIPTION:
--      This test checks that the procedures To_Fortran and To_Ada produce
--      the correct results, based on a variety of parameter input values.
--      
--      In the first series of subtests, the Out parameter results of 
--      procedure To_Fortran are compared against expected results,
--      which includes (in the parameter Last) the index in Target of the 
--      last element assigned.  The situation where procedure To_Fortran
--      raises Constraint_Error (when Item'Length exceeds Target'Length) 
--      is also verified.
--
--      In the second series of subtests, the Out parameter results of 
--      procedure To_Ada are verified, in a similar manner as is done for
--      procedure To_Fortran.  The case of procedure To_Ada raising 
--      Constraint_Error is also verified.
--      
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.Fortran.Character_Set:
--      ' ', 'a'..'j', 'A'..'D', '1'..'9', '-', '_', '$', '#', and '*'.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.Fortran.  If an implementation provides
--      package Interfaces.Fortran, this test must compile, execute, and 
--      report "PASSED".
--
--       
-- CHANGE HISTORY:
--      14 Mar 96   SAIC    Initial release for 2.1.
--      10 Jun 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Ada.Exceptions;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with Interfaces.Fortran;                                      -- N/A => ERROR
with Report;

procedure CXB5003 is
begin

   Report.Test ("CXB5003", "Check that procedures To_Fortran and To_Ada " &
                           "produce correct results");

   Test_Block:
   declare

      package Bnd is new Ada.Strings.Bounded.Generic_Bounded_Length(10);
      package Unb renames Ada.Strings.Unbounded;

      use Bnd, Unb;
      use Interfaces.Fortran;
      use Ada.Exceptions;

      Fortran_Character_1     : Fortran_Character(1..1)  := " ";
      Fortran_Character_5     : Fortran_Character(1..5)  := "     ";
      Fortran_Character_10    : Fortran_Character(1..10) := "          ";
      Fortran_Character_20    : Fortran_Character(1..20) := 
                                  "                    ";
      TC_Fortran_Character_1  : Fortran_Character(1..1)  := "A";
      TC_Fortran_Character_5  : Fortran_Character(1..5)  := "ab*de";
      TC_Fortran_Character_10 : Fortran_Character(1..10) := "$1a2b3C4D5";
      TC_Fortran_Character_20 : Fortran_Character(1..20) := 
                                  "1234-ABCD_6789#fghij";

      Bnd_String              : Bnd.Bounded_String   :=
                                  Bnd.To_Bounded_String("          ");
      TC_Bnd_String           : Bounded_String       :=
                                  To_Bounded_String("$1a2b3C4D5");

      Unb_String              : Unb.Unbounded_String :=
                                  Unb.To_Unbounded_String("     ");
      TC_Unb_String           : Unbounded_String     :=
                                  To_Unbounded_String("ab*de");

      String_1                : String(1..1)   := " ";
      String_5                : String(1..5)   := "     ";
      String_10               : String(1..10)  := "          ";
      String_20               : String(1..20)  := "                    ";
      TC_String_1             : String(1..1)   := "A";
      TC_String_20            : String(1..20)  := "1234-ABCD_6789#fghij";

      TC_Fortran_Character    : constant Fortran_Character := ""; 
      TC_String               : constant String            := ""; 
      TC_Natural              : Natural                    := 0;


   begin

      -- Check that the procedure To_Fortran converts the character elements
      -- of the String parameter Item into Character_Set elements of the 
      -- Fortran_Character type parameter Target.  
      -- Check that the parameter Last contains the index of the last element
      -- of parameter Target that was assigned by To_Fortran.

      To_Fortran(Item   => TC_String_1,
                 Target => Fortran_Character_1,
                 Last   => TC_Natural);

      if Fortran_Character_1 /= TC_Fortran_Character_1        or
         TC_Natural          /= TC_Fortran_Character_1'Length 
      then
         Report.Failed("Incorrect result from procedure To_Fortran - 1");
      end if;

      To_Fortran(To_String(TC_Unb_String),
                 Target => Fortran_Character_5,
                 Last   => TC_Natural);

      if Fortran_Character_5 /= TC_Fortran_Character_5        or
         TC_Natural          /= TC_Fortran_Character_5'Length 
      then
         Report.Failed("Incorrect result from procedure To_Fortran - 2");
      end if;

      To_Fortran(To_String(TC_Bnd_String),
                 Fortran_Character_10,
                 Last   => TC_Natural);

      if Fortran_Character_10 /= TC_Fortran_Character_10        or
         TC_Natural           /= TC_Fortran_Character_10'Length
      then
         Report.Failed("Incorrect result from procedure To_Fortran - 3");
      end if;

      To_Fortran(TC_String_20, Fortran_Character_20, TC_Natural);

      if Fortran_Character_20 /= TC_Fortran_Character_20        or
         TC_Natural           /= TC_Fortran_Character_20'Length 
      then
         Report.Failed("Incorrect result from procedure To_Fortran - 4");
      end if;

      To_Fortran(Item   => TC_String,     -- null string
                 Target => Fortran_Character_1,
                 Last   => TC_Natural);

      if TC_Natural /= 0 then
         Report.Failed("Incorrect result from procedure To_Fortran, value "  &
                       "returned in parameter Last should be zero, since " &
                       "parameter Item is null array");
      end if;


      -- Check that Constraint_Error is propagated by procedure To_Fortran
      -- when the length of String parameter Item exceeds the length of 
      -- Fortran_Character parameter Target.

      begin

         To_Fortran(Item   => TC_String_20,
                    Target => Fortran_Character_10,
                    Last   => TC_Natural);
         Report.Failed("Constraint_Error not raised by procedure " &
                       "To_Fortran when Item'Length exceeds Target'Length");
      exception
         when Constraint_Error   => null;  -- OK, expected exception.
         when The_Error : others =>
            Report.Failed("The following exception was raised by procedure " &
                          "To_Fortran when Item'Length exceeds "             &
                          "Target'Length: " & Exception_Name(The_Error));
      end;


      -- Check that the procedure To_Ada converts the Character_Set 
      -- elements of the Fortran_Character parameter Item into Character
      -- elements of the String parameter Target.  
      -- Check that the parameter Last contains the index of the last 
      -- element of parameter Target that was assigned by To_Ada.

      To_Ada(Item   => TC_Fortran_Character_1,
             Target => String_1,
             Last   => TC_Natural);

      if String_1   /= TC_String_1        or
         TC_Natural /= TC_String_1'Length 
      then
         Report.Failed("Incorrect result from procedure To_Ada - 1");
      end if;

      To_Ada(TC_Fortran_Character_5,
             Target => String_5,
             Last   => TC_Natural);

      if String_5   /= To_String(TC_Unb_String) or
         TC_Natural /= Length(TC_Unb_String)    
      then
         Report.Failed("Incorrect result from procedure To_Ada - 2");
      end if;

      To_Ada(TC_Fortran_Character_10,
             String_10,
             Last   => TC_Natural);

      if String_10   /= To_String(TC_Bnd_String) or
         TC_Natural  /= Length(TC_Bnd_String)   
      then
         Report.Failed("Incorrect result from procedure To_Ada - 3");
      end if;

      To_Ada(TC_Fortran_Character_20, String_20, TC_Natural);

      if String_20   /= TC_String_20        or
         TC_Natural  /= TC_String_20'Length 
      then
         Report.Failed("Incorrect result from procedure To_Ada - 4");
      end if;

      To_Ada(Item   => TC_Fortran_Character,  -- null array.
             Target => String_20,
             Last   => TC_Natural);

      if TC_Natural /= 0 then
         Report.Failed("Incorrect result from procedure To_Ada, value "    &
                       "returned in parameter Last should be zero, since " &
                       "parameter Item is null array");
      end if;


      -- Check that Constraint_Error is propagated by procedure To_Ada 
      -- when the length of Fortran_Character parameter Item exceeds the 
      -- length of String parameter Target.

      begin

         To_Ada(Item   => TC_Fortran_Character_10,
                Target => String_5,
                Last   => TC_Natural);
         Report.Failed("Constraint_Error not raised by procedure To_Ada " &
                       "when Item'Length exceeds Target'Length");
      exception
         when Constraint_Error   => null;  -- OK, expected exception.
         when The_Error : others =>
            Report.Failed("Incorrect exception raised by procedure To_Ada " &
                          "when Item'Length exceeds Target'Length");
      end;


   exception
      when The_Error : others => 
         Report.Failed("The following exception was raised in the " &
                       "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB5003;
