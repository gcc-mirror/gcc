-- CXA3004.A
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
--      for classification of and conversion between Wide_Character and
--      Character values produce correct results when given the appropriate
--      Character and String inputs.
--
-- TEST DESCRIPTION:
--      This test demonstrates the functions defined in package 
--      Ada.Characters.Handling which provide for the classification of and 
--      conversion between Wide_Characters and Characters, in character 
--      variables and strings.
--      Each of the functions is provided with input values that are of the 
--      appropriate range. The results of the function processing are 
--      subsequently evaluated.
--      
-- APPLICABILITY CRITERIA: 
--      Applicable to all implementations using the Latin_1 set as the
--      definition of Character.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      27 Dec 94   SAIC    Corrected variable names.
--
--!

with Report;
with Ada.Characters.Handling;

procedure CXA3004 is
begin

   Report.Test ("CXA3004", "Check that the functions defined in package "  &
                           "Ada.Characters.Handling for classification "   &
                           "of and conversion between Wide_Character and " &
                           "Character values produce correct results "     &
                           "when given the appropriate Character "         &
                           "and String inputs");

   Test_Block:
   declare

      package ACH renames Ada.Characters.Handling;

      Char_End      : Integer        := 255;
      WC_Start      : Integer        := 256;
      Sub_Char      : Character      := '*';

      Blank         : Character      := ' ';
      First_Char    : Character      := Character'First;
      Last_Char     : Character      := Character'Last;
      F_Char        : Character      := 'F';


      First_Wide_Char        : Wide_Character := Wide_Character'First;
      Last_Non_Wide_Char     : Wide_Character := Wide_Character'Val(Char_End);
      First_Unique_Wide_Char : Wide_Character := Wide_Character'Val(WC_Start);
      Last_Wide_Char         : Wide_Character := Wide_Character'Last;

      A_String      : String (1..3)      := First_Char & 'X' & Last_Char;
      A_Wide_String : Wide_String (1..3) := First_Wide_Char            & 
                                            ACH.To_Wide_Character('X') & 
                                            ACH.To_Wide_Character(Last_Char);

      Unique_Wide_String : Wide_String (1..2) := First_Unique_Wide_Char &
                                                 Last_Wide_Char;

      Mixed_Wide_String  : Wide_String (1..6) := ACH.To_Wide_Character('A') & 
                                                 First_Wide_Char            &
                                                 Last_Non_Wide_Char         & 
                                                 First_Unique_Wide_Char     & 
                                                 Last_Wide_Char             &
                                                 ACH.To_Wide_Character('Z');


      Basic_Char         : Character          := 'A';
      Basic_Wide_Char    : Wide_Character     := 'A';
      Basic_String       : String (1..6)      := "ABCXYZ";
      Basic_Wide_String  : Wide_String (1..6) := "ABCXYZ";

   begin


      -- Function Is_Character


      if not ACH.Is_Character(First_Wide_Char) then
         Report.Failed ("Incorrect result from Is_Character - 1");
      end if;


      if ACH.Is_Character(First_Unique_Wide_Char) or        
         ACH.Is_Character(Last_Wide_Char)
      then
         Report.Failed ("Incorrect result from Is_Character - 2");
      end if;
       

      -- Function Is_String


      if not ACH.Is_String(A_Wide_String) then
         Report.Failed ("Incorrect result from Is_String - 1");
      end if;


      if ACH.Is_String(Unique_Wide_String) or
         ACH.Is_String(Mixed_Wide_String)
      then
         Report.Failed ("Incorrect result from Is_String - 2");
      end if;
      

      -- Function To_Character


      -- Use default substitution character in call of To_Character.

      if ACH.To_Character(First_Wide_Char)    /= First_Char or 
         ACH.To_Character(Last_Non_Wide_Char) /= Last_Char
      then
         Report.Failed ("Incorrect result from To_Character - 1");
      end if;


      -- Provide a substitution character for use with To_Character.

      if ACH.To_Character(First_Unique_Wide_Char, Blank)    /= Blank    or
         ACH.To_Character(First_Unique_Wide_Char, Sub_Char) /= Sub_Char or
         ACH.To_Character(Last_Wide_Char)                   /= ' '  -- default
      then
         Report.Failed ("Incorrect result from To_Character - 2");
      end if;


      -- Function To_String


      if ACH.To_String(A_Wide_String) /=  A_String then
         Report.Failed ("Incorrect result from To_String - 1");
      end if;

     
      if ACH.To_String(Unique_Wide_String, Sub_Char) /= "**" then
         Report.Failed ("Incorrect result from To_String - 2");
      end if;



      if ACH.To_String(Mixed_Wide_String,  Sub_Char) /= 
         ('A' & First_Char & Last_Char & "**" & 'Z')   or
         ACH.To_String(Mixed_Wide_String,  Sub_Char) /= 
         (ACH.To_Character(Mixed_Wide_String(1), Sub_Char) &
          ACH.To_Character(Mixed_Wide_String(2), Sub_Char) &
          ACH.To_Character(Mixed_Wide_String(3), Sub_Char) &
          ACH.To_Character(Mixed_Wide_String(4), Sub_Char) &
          ACH.To_Character(Mixed_Wide_String(5), Sub_Char) &
          ACH.To_Character(Mixed_Wide_String(6), Sub_Char)) 
      then
         Report.Failed ("Incorrect result from To_String - 3");
      end if;


      -- Function To_Wide_Character


      if ACH.To_Wide_Character(Basic_Char) /= Basic_Wide_Char then
         Report.Failed ("Incorrect result from To_Wide_Character");
      end if;

    
      -- Function To_Wide_String


      if not (ACH.To_Wide_String(Basic_String) = Basic_Wide_String) then
         Report.Failed ("Incorrect result from To_Wide_String");
      end if;


      -- Functions Used In Combination

      if not ACH.Is_Character (ACH.To_Wide_Character (
        ACH.To_Character(First_Wide_Char))) 
      then
         Report.Failed ("Incorrect result from functions in combination - 1");
      end if;


      if not ACH.Is_String(ACH.To_Wide_String(ACH.To_String(A_Wide_String)))
      then
         Report.Failed ("Incorrect result from functions in combination - 2");
      end if;


      if ACH.To_String(ACH.To_Wide_Character('A')    &
                       ACH.To_Wide_Character(F_Char) &
                       ACH.To_Wide_Character('Z'))     /=  "AFZ"
      then
         Report.Failed ("Incorrect result from functions in combination - 3");
      end if;
         

   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;


   Report.Result;

end CXA3004;
