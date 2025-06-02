-- CXA3002.A
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
--      Check that the conversion functions for Characters and Strings
--      defined in package Ada.Characters.Handling provide correct results
--      when given character/string input parameters.
--
-- TEST DESCRIPTION:
--      This test checks the output of the To_Lower, To_Upper, and
--      To_Basic functions for both Characters and Strings.  Each function
--      is called with input parameters that are within the appropriate
--      range of values, and also with values outside the specified 
--      range (i.e., lower case 'a' to To_Lower).  The functions are also
--      used in combination with one another, with the result of one function
--      providing the actual input parameter value to another.
--      
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      22 Dec 94   SAIC    Corrected evaluations of Functions In Combination.
--
--!

with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Report;

procedure CXA3002 is

   package AC  renames Ada.Characters;
   package ACH renames Ada.Characters.Handling;

begin

   Report.Test ("CXA3002", "Check that the conversion functions for "   &
                           "Characters and Strings defined in package " &
                           "Ada.Characters.Handling provide correct "   &
                           "results when given character/string input " &
                           "parameters");


   Character_Block:
   declare
      Offset : constant Integer := Character'Pos('a') - Character'Pos('A');
   begin 

      -- Function To_Lower for Characters

      if ACH.To_Lower('A') /= 'a' or  ACH.To_Lower('Z') /= 'z' then
         Report.Failed ("Incorrect operation of function To_Lower - 1");
      end if;


      for i in Character'Pos('A') .. Character'Pos('Z') loop
         if ACH.To_Lower(Character'Val(i)) /= Character'Val(i + Offset) then
            Report.Failed ("Incorrect operation of function To_Lower - 2");
         end if;
      end loop;


      if (ACH.To_Lower(AC.Latin_1.UC_A_Grave)         /= 
          AC.Latin_1.LC_A_Grave)                     or
         (ACH.To_Lower(AC.Latin_1.UC_Icelandic_Thorn) /=
          AC.Latin_1.LC_Icelandic_Thorn)
      then
         Report.Failed ("Incorrect operation of function To_Lower - 3");
      end if;


      if ACH.To_Lower('c')                 /= 'c'                 or  
         ACH.To_Lower('w')                 /= 'w'                 or
         ACH.To_Lower(AC.Latin_1.CR)       /= AC.Latin_1.CR       or
         ACH.To_Lower(AC.Latin_1.LF)       /= AC.Latin_1.LF       or
         ACH.To_Lower(AC.Latin_1.Comma)    /= AC.Latin_1.Comma    or
         ACH.To_Lower(AC.Latin_1.Question) /= AC.Latin_1.Question or
         ACH.To_Lower('0')                 /= '0'                 or
         ACH.To_Lower('9')                 /= '9'                
      then
         Report.Failed ("Incorrect operation of function To_Lower - 4");
      end if;

    
      --- Function To_Upper for Characters
      

      if not (ACH.To_Upper('b') = 'B') and (ACH.To_Upper('y') = 'Y') then
         Report.Failed ("Incorrect operation of function To_Upper - 1");
      end if;


      for i in Character'Pos(AC.Latin_1.LC_A) .. 
               Character'Pos(AC.Latin_1.LC_Z) loop
         if ACH.To_Upper(Character'Val(i)) /= Character'Val(i - Offset) then
            Report.Failed ("Incorrect operation of function To_Upper - 2");
         end if;
      end loop;


      if (ACH.To_Upper(AC.Latin_1.LC_U_Diaeresis) /= 
          AC.Latin_1.UC_U_Diaeresis)                 or
         (ACH.To_Upper(AC.Latin_1.LC_A_Ring)      /=
          AC.Latin_1.UC_A_Ring) 
      then
         Report.Failed ("Incorrect operation of function To_Upper - 3");
      end if;
     

      if not (ACH.To_Upper('F') = 'F'                       and 
              ACH.To_Upper('U') = 'U'                       and
              ACH.To_Upper(AC.Latin_1.LC_German_Sharp_S) =
                AC.Latin_1.LC_German_Sharp_S                and
              ACH.To_Upper(AC.Latin_1.LC_Y_Diaeresis) =
                AC.Latin_1.LC_Y_Diaeresis) 
      then
         Report.Failed ("Incorrect operation of function To_Upper - 4");
      end if;


      --- Function To_Basic for Characters
      

      if ACH.To_Basic(AC.Latin_1.LC_A_Circumflex) /=       
         ACH.To_Basic(AC.Latin_1.LC_A_Tilde)         or       
         ACH.To_Basic(AC.Latin_1.LC_E_Grave)      /=       
         ACH.To_Basic(AC.Latin_1.LC_E_Acute)         or      
         ACH.To_Basic(AC.Latin_1.LC_I_Circumflex) /=       
         ACH.To_Basic(AC.Latin_1.LC_I_Diaeresis)     or          
         ACH.To_Basic(AC.Latin_1.UC_O_Tilde)      /=       
         ACH.To_Basic(AC.Latin_1.UC_O_Acute)         or      
         ACH.To_Basic(AC.Latin_1.UC_U_Grave)      /=       
         ACH.To_Basic(AC.Latin_1.UC_U_Acute)         or      
         ACH.To_Basic(AC.Latin_1.LC_Y_Acute)      /=       
         ACH.To_Basic(AC.Latin_1.LC_Y_Diaeresis)
      then
         Report.Failed ("Incorrect operation of function To_Basic - 1");
      end if;


      if ACH.To_Basic('Y') /= 'Y'                   or 
         ACH.To_Basic(AC.Latin_1.LC_E_Acute) /= 'e' or
         ACH.To_Basic('6') /= '6'                   or
         ACH.To_Basic(AC.Latin_1.LC_R) /= 'r' 
      then
         Report.Failed ("Incorrect operation of function To_Basic - 2");
      end if;
 

      -- Using Functions (for Characters) in Combination


      if (ACH.To_Upper(ACH.To_Lower('A')) /= 'A' ) or
         (ACH.To_Upper(ACH.To_Lower(AC.Latin_1.UC_A_Acute)) /=
          AC.Latin_1.UC_A_Acute )
      then 
         Report.Failed("Incorrect operation of functions in combination - 1");
      end if;


      if ACH.To_Basic(ACH.To_Lower(ACH.To_Upper(AC.Latin_1.LC_U_Grave))) /= 
         'u'
      then
         Report.Failed("Incorrect operation of functions in combination - 2");
      end if;
       

      if ACH.To_Lower (ACH.To_Basic 
                         (ACH.To_Upper(AC.Latin_1.LC_O_Diaeresis))) /= 'o'
      then
         Report.Failed("Incorrect operation of functions in combination - 3");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Character_Block");
   end Character_Block;


   String_Block:
   declare

      LC_String : constant String := "az" & 
                                      AC.Latin_1.LC_A_Grave &
                                      AC.Latin_1.LC_C_Cedilla;

      UC_String : constant String := "AZ" & 
                                      AC.Latin_1.UC_A_Grave &
                                      AC.Latin_1.UC_C_Cedilla;

      LC_Basic_String    : constant String := "aei" & 'o' & 'u';

      LC_NonBasic_String : constant String := AC.Latin_1.LC_A_Diaeresis  &
                                              AC.Latin_1.LC_E_Circumflex &
                                              AC.Latin_1.LC_I_Acute      &
                                              AC.Latin_1.LC_O_Tilde      &
                                              AC.Latin_1.LC_U_Grave;

      UC_Basic_String    : constant String := "AEIOU";

      UC_NonBasic_String : constant String := AC.Latin_1.UC_A_Tilde      &
                                              AC.Latin_1.UC_E_Acute      &
                                              AC.Latin_1.UC_I_Grave      &
                                              AC.Latin_1.UC_O_Diaeresis  &
                                              AC.Latin_1.UC_U_Circumflex;

      LC_Special_String  : constant String := "ab"                         &
                                              AC.Latin_1.LC_German_Sharp_S &
                                              AC.Latin_1.LC_Y_Diaeresis;

      UC_Special_String  : constant String := "AB"                         &
                                              AC.Latin_1.LC_German_Sharp_S &
                                              AC.Latin_1.LC_Y_Diaeresis;

   begin

      -- Function To_Lower for Strings
      

      if ACH.To_Lower (UC_String) /= LC_String  or
         ACH.To_Lower (LC_String) /= LC_String  
      then
         Report.Failed ("Incorrect result from To_Lower for strings - 1");
      end if;


      if ACH.To_Lower (UC_Basic_String) /= LC_Basic_String then
         Report.Failed ("Incorrect result from To_Lower for strings - 2");
      end if;


      -- Function To_Upper for Strings
      

      if not (ACH.To_Upper (LC_String) = UC_String) then
         Report.Failed ("Incorrect result from To_Upper for strings - 1");
      end if;


      if ACH.To_Upper (LC_Basic_String) /= UC_Basic_String or
         ACH.To_Upper (UC_String)       /= UC_String
      then
         Report.Failed ("Incorrect result from To_Upper for strings - 2");
      end if;


      if ACH.To_Upper (LC_Special_String) /= UC_Special_String then
         Report.Failed ("Incorrect result from To_Upper for strings - 3");
      end if;
      


      -- Function To_Basic for Strings
      

      if (ACH.To_Basic (LC_String) /= "azac") or
         (ACH.To_Basic (UC_String) /= "AZAC")
      then
         Report.Failed ("Incorrect result from To_Basic for Strings - 1");
      end if;


      if ACH.To_Basic (LC_NonBasic_String) /= LC_Basic_String then
         Report.Failed ("Incorrect result from To_Basic for Strings - 2");
      end if;


      if ACH.To_Basic (UC_NonBasic_String) /= UC_Basic_String then
         Report.Failed ("Incorrect result from To_Basic for Strings - 3");
      end if;


      -- Using Functions (for Strings) in Combination


      if ACH.To_Upper(ACH.To_Lower(UC_Basic_String)) /= UC_Basic_String or
         ACH.To_Lower(ACH.To_Upper(LC_Basic_String)) /= LC_Basic_String
      then
         Report.Failed ("Incorrect operation of functions in combination - 4");
      end if;


      if (ACH.To_Basic(ACH.To_Lower(UC_NonBasic_String)) /= LC_Basic_String) or
         (ACH.To_Basic(ACH.To_Upper(LC_NonBasic_String)) /= UC_Basic_String)
      then
         Report.Failed ("Incorrect operation of functions in combination - 5");
      end if;
        

   exception
      when others => Report.Failed ("Exception raised in String_Block");
   end String_Block;


   Report.Result;

end CXA3002;
