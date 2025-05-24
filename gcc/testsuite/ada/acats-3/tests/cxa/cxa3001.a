-- CXA3001.A
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
--      Check that the character classification functions defined in 
--      package Ada.Characters.Handling produce correct results when provided 
--      constant arguments from package Ada.Characters.Latin_1.
--
-- TEST DESCRIPTION:
--      This test checks the character classification functions of package
--      Ada.Characters.Handling.  In the evaluation of each function, loops
--      are constructed to examine the function with as many values of type
--      Character (Ada.Characters.Latin_1 constants) as possible in an 
--      amount of code that is about equal to the amount of code required
--      to examine the function with a few representative input values and
--      endpoint values.
--      The usage paradigm being demonstrated by this test is that of the
--      functions being used to assign to boolean variables, as well as
--      serving as boolean conditions.
--      
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      29 Apr 95   SAIC    Fixed subtest checking Is_Graphic function.
--
--!

with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Report;

procedure CXA3001 is

begin

   Report.Test ("CXA3001", "Check that the character classification "      &
                           "functions defined in package "                 &
                           "Ada.Characters.Handling produce "              &
                           "correct results when provided constant "       &
                           "arguments from package Ada.Characters.Latin_1");

   Test_Block:
   declare

      package AC  renames Ada.Characters;
      package ACH renames Ada.Characters.Handling;

      TC_Boolean : Boolean := False;

   begin 

      -- Over the next six statements/blocks of code, evaluate functions 
      -- Is_Control and Is_Graphic with control character and non-control 
      -- character values.

      for i in Character'Pos(AC.Latin_1.NUL) .. 
               Character'Pos(AC.Latin_1.US)  loop
         if not ACH.Is_Control(Character'Val(i)) then
            Report.Failed ("Incorrect result from function Is_Control - 1");
         end if;
         if ACH.Is_Graphic(Character'Val(i)) then
            Report.Failed ("Incorrect result from function Is_Graphic - 1");
         end if;
      end loop;


      for i in Character'Pos(AC.Latin_1.Space) ..
               Character'Pos(AC.Latin_1.Tilde) loop
         if not ACH.Is_Graphic(Character'Val(i)) then    
            Report.Failed ("Incorrect result from function Is_Graphic - 2");
         end if;
         if ACH.Is_Control(Character'Val(i)) then
            Report.Failed ("Incorrect result from function Is_Control - 2");
         end if;
      end loop;


      for i in Character'Pos(AC.Latin_1.Reserved_128) .. 
               Character'Pos(AC.Latin_1.APC)  loop
         if not ACH.Is_Control(Character'Val(i)) then
            Report.Failed ("Incorrect result from function Is_Control - 3");
         end if;
         TC_Boolean := ACH.Is_Graphic(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect result from function Is_Graphic - 3");
            TC_Boolean := False;
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.No_Break_Space) ..
               Character'Pos(AC.Latin_1.LC_Y_Diaeresis) loop
         TC_Boolean := ACH.Is_Control(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect result from function Is_Control - 4");
            TC_Boolean := False;
         end if;
         if not ACH.Is_Graphic(Character'Val(i)) then
            Report.Failed ("Incorrect result from function Is_Graphic - 4");
         end if;
      end loop;

      -- Check renamed constants.

      if not (ACH.Is_Control(AC.Latin_1.IS4) and
              ACH.Is_Control(AC.Latin_1.IS3) and 
              ACH.Is_Control(AC.Latin_1.IS2) and 
              ACH.Is_Control(AC.Latin_1.IS1))       or
         (ACH.Is_Control(AC.Latin_1.NBSP)           or
          ACH.Is_Control(AC.Latin_1.Paragraph_Sign) or
          ACH.Is_Control(AC.Latin_1.Minus_Sign)     or
          ACH.Is_Control(AC.Latin_1.Ring_Above))
      then
         Report.Failed ("Incorrect result from function Is_Control - 5");
      end if;

      if (ACH.Is_Graphic(AC.Latin_1.IS4)  or
          ACH.Is_Graphic(AC.Latin_1.IS3)  or
          ACH.Is_Graphic(AC.Latin_1.IS2)  or
          ACH.Is_Graphic(AC.Latin_1.IS1)) or
         not (ACH.Is_Graphic(AC.Latin_1.NBSP)           and
              ACH.Is_Graphic(AC.Latin_1.Paragraph_Sign) and
              ACH.Is_Graphic(AC.Latin_1.Minus_Sign)     and
              ACH.Is_Graphic(AC.Latin_1.Ring_Above))
      then
         Report.Failed ("Incorrect result from function Is_Graphic - 5");
      end if;


      -- Evaluate function Is_Letter with letter/non-letter inputs.
      
      for i in Character'Pos('A') .. Character'Pos('Z') loop
         if not ACH.Is_Letter(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Letter result - 1");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.LC_A) .. 
               Character'Pos(AC.Latin_1.LC_Z) loop
         if not ACH.Is_Letter(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Letter result - 2");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.UC_A_Grave) .. 
               Character'Pos(AC.Latin_1.UC_O_Diaeresis) loop
         if not ACH.Is_Letter(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Letter result - 3");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.UC_O_Oblique_Stroke) .. 
               Character'Pos(AC.Latin_1.LC_O_Diaeresis) loop
         if not ACH.Is_Letter(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Letter result - 4");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.LC_O_Oblique_Stroke) .. 
               Character'Pos(AC.Latin_1.LC_Y_Diaeresis) loop
         if not ACH.Is_Letter(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Letter result - 5");
         end if;
      end loop;

      -- Check for rejection of non-letters.
      for i in Character'Pos(AC.Latin_1.NUL) .. 
               Character'Pos(AC.Latin_1.Commercial_At) loop
         if ACH.Is_Letter(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Letter result - 6");
         end if;
      end loop;


      -- Evaluate function Is_Lower with lower case/non-lower case inputs.

      for i in Character'Pos(AC.Latin_1.LC_A) .. 
               Character'Pos(AC.Latin_1.LC_Z) loop
         if not ACH.Is_Lower(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Lower result - 1");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.LC_A_Grave) .. 
               Character'Pos(AC.Latin_1.LC_O_Diaeresis) loop
         if not ACH.Is_Lower(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Lower result - 2");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.LC_O_Oblique_Stroke) .. 
               Character'Pos(AC.Latin_1.LC_Y_Diaeresis) loop
         if not ACH.Is_Lower(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Lower result - 3");
         end if;
      end loop;

      if ACH.Is_Lower('A')                         or
         ACH.Is_Lower(AC.Latin_1.UC_Icelandic_Eth) or
         ACH.Is_Lower(AC.Latin_1.Number_Sign)      or
         ACH.Is_Lower(AC.Latin_1.Cedilla)          or
         ACH.Is_Lower(AC.Latin_1.SYN)              or
         ACH.Is_Lower(AC.Latin_1.ESA) 
      then
         Report.Failed ("Incorrect Is_Lower result - 4");
      end if;


      -- Evaluate function Is_Upper with upper case/non-upper case inputs.

      for i in Character'Pos('A') .. Character'Pos('Z') loop
         if not ACH.Is_Upper(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Upper result - 1");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.UC_A_Grave) .. 
               Character'Pos(AC.Latin_1.UC_O_Diaeresis) loop
         if not ACH.Is_Upper(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Upper result - 2");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.UC_O_Oblique_Stroke) .. 
               Character'Pos(AC.Latin_1.UC_Icelandic_Thorn) loop
         if not ACH.Is_Upper(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Upper result - 3");
         end if;
      end loop;

      if ACH.Is_Upper('8')                    or
         ACH.Is_Upper(AC.Latin_1.LC_A_Ring  ) or
         ACH.Is_Upper(AC.Latin_1.Dollar_Sign) or
         ACH.Is_Upper(AC.Latin_1.Broken_Bar)  or
         ACH.Is_Upper(AC.Latin_1.ETB)         or
         ACH.Is_Upper(AC.Latin_1.VTS) 
      then
         Report.Failed ("Incorrect Is_Upper result - 4");
      end if;


      for i in Character'Pos('a') .. Character'Pos('z') loop
         if ACH.Is_Upper(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Upper result - 5");
         end if;
      end loop;


      -- Evaluate function Is_Basic with basic/non-basic inputs.
      -- (Note: Basic letters are those without diacritical marks.)

      for i in Character'Pos('A') .. Character'Pos('Z') loop
         if not ACH.Is_Basic(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Basic result - 1");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.LC_A) .. 
               Character'Pos(AC.Latin_1.LC_Z) loop
         if not ACH.Is_Basic(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Basic result - 2");
         end if;
      end loop;


      if not (ACH.Is_Basic(AC.Latin_1.UC_AE_Diphthong)     and  
              ACH.Is_Basic(AC.Latin_1.LC_AE_Diphthong)     and
              ACH.Is_Basic(AC.Latin_1.LC_German_Sharp_S)   and
              ACH.Is_Basic(AC.Latin_1.LC_Icelandic_Eth)    and
              ACH.Is_Basic(AC.Latin_1.LC_Icelandic_Thorn)  and
              ACH.Is_Basic(AC.Latin_1.UC_Icelandic_Eth)    and
              ACH.Is_Basic(AC.Latin_1.UC_Icelandic_Thorn))
      then
         Report.Failed ("Incorrect Is_Basic result - 3");
      end if;

      -- Check for rejection of non-basics.
      if ACH.Is_Basic(AC.Latin_1.UC_A_Tilde) or
         ACH.Is_Basic(AC.Latin_1.LC_A_Grave) or
         ACH.Is_Basic(AC.Latin_1.Ampersand)  or
         ACH.Is_Basic(AC.Latin_1.Yen_Sign)   or
         ACH.Is_Basic(AC.Latin_1.NAK)        or
         ACH.Is_Basic(AC.Latin_1.SS2) 
      then
         Report.Failed ("Incorrect Is_Basic result - 4");
      end if;



      for i in Character'Pos(AC.Latin_1.NUL) .. 
               Character'Pos(AC.Latin_1.Commercial_At) loop
         if ACH.Is_Basic(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Basic result - 5");
         end if;
      end loop;


      -- Evaluate functions Is_Digit and Is_Decimal_Digit (a rename of
      -- Is_Digit) with decimal digit/non-digit inputs.


      if not (ACH.Is_Digit('0')          and
              ACH.Is_Decimal_Digit('9')) or
         ACH.Is_Digit ('a')              or     -- Hex digits.
         ACH.Is_Decimal_Digit ('f')      or
         ACH.Is_Decimal_Digit ('A')      or
         ACH.Is_Digit ('F')
      then
         Report.Failed ("Incorrect Is_Digit/Is_Decimal_Digit result - 1");
      end if;

      if ACH.Is_Digit         (AC.Latin_1.Full_Stop)         or
         ACH.Is_Decimal_Digit (AC.Latin_1.Dollar_Sign)       or
         ACH.Is_Digit         (AC.Latin_1.Number_Sign)       or
         ACH.Is_Decimal_Digit (AC.Latin_1.Left_Parenthesis)  or
         ACH.Is_Digit         (AC.Latin_1.Right_Parenthesis) 
      then
         Report.Failed ("Incorrect Is_Digit/Is_Decimal_Digit result - 2");
      end if;


      -- Evaluate functions Is_Hexadecimal_Digit with hexadecimal digit and 
      -- non-hexadecimal digit inputs.

      for i in Character'Pos('0') .. Character'Pos('9') loop
         if not ACH.Is_Hexadecimal_Digit(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Hexadecimal_Digit result - 1");
         end if;
      end loop;

      for i in Character'Pos('A') .. Character'Pos('F') loop
         if not ACH.Is_Hexadecimal_Digit(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Hexadecimal_Digit result - 2");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.LC_A) .. 
               Character'Pos(AC.Latin_1.LC_F) loop
         if not ACH.Is_Hexadecimal_Digit(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Hexadecimal_Digit result - 3");
         end if;
      end loop;


      if ACH.Is_Hexadecimal_Digit (AC.Latin_1.Minus_Sign)        or
         ACH.Is_Hexadecimal_Digit (AC.Latin_1.Hyphen)            or
         ACH.Is_Hexadecimal_Digit (AC.Latin_1.LC_G)              or
         ACH.Is_Hexadecimal_Digit (AC.Latin_1.LC_Z)              or
         ACH.Is_Hexadecimal_Digit ('G')                          or
         ACH.Is_Hexadecimal_Digit (AC.Latin_1.Cent_Sign)         or
         ACH.Is_Hexadecimal_Digit (AC.Latin_1.Pound_Sign)
      then
         Report.Failed ("Incorrect Is_HexaDecimal_Digit result - 4");
      end if;


      -- Evaluate functions Is_Alphanumeric and Is_Special with 
      -- letters, digits, and non-alphanumeric inputs.

      for i in Character'Pos(AC.Latin_1.NUL) .. 
               Character'Pos(AC.Latin_1.US)  loop
         if ACH.Is_Alphanumeric(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 1");
         end if;
         TC_Boolean := ACH.Is_Special(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect Is_Special result - 1");
            TC_Boolean := False;
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.Reserved_128) .. 
               Character'Pos(AC.Latin_1.APC)  loop
         TC_Boolean := ACH.Is_Alphanumeric(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect Is_Alphanumeric result - 2");
            TC_Boolean := False;
         end if;
         if ACH.Is_Special(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Special result - 2");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.Space) ..
               Character'Pos(AC.Latin_1.Solidus) loop
         TC_Boolean := ACH.Is_Alphanumeric(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect Is_Alphanumeric result - 3");
            TC_Boolean := False;
         end if;
         if not ACH.Is_Special(Character'Val(i)) then  
            Report.Failed ("Incorrect Is_Special result - 3");
         end if;
      end loop;

      for i in Character'Pos('A') .. Character'Pos('Z') loop
         if not ACH.Is_Alphanumeric(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 4");
         end if;
         TC_Boolean := ACH.Is_Special(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect Is_Special result - 4");
            TC_Boolean := False;
         end if;
      end loop;

      for i in Character'Pos('0') .. Character'Pos('9') loop
         if not ACH.Is_Alphanumeric(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 5");
         end if;
         TC_Boolean := ACH.Is_Special(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect Is_Special result - 5");
            TC_Boolean := False;
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.LC_A) ..
               Character'Pos(AC.Latin_1.LC_Z) loop
         if not ACH.Is_Alphanumeric(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 6");
         end if;
         TC_Boolean := ACH.Is_Special(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect Is_Special result - 6");
            TC_Boolean := False;
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.No_Break_Space) ..
               Character'Pos(AC.Latin_1.Inverted_Question) loop
         TC_Boolean := ACH.Is_Alphanumeric(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect Is_Alphanumeric result - 7");
            TC_Boolean := False;
         end if;
         if not ACH.Is_Special(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Special result - 7");
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.UC_A_Grave) ..
               Character'Pos(AC.Latin_1.UC_O_Diaeresis) loop
         if not ACH.Is_Alphanumeric(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 8");
         end if;
         TC_Boolean := ACH.Is_Special(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect Is_Special result - 8");
            TC_Boolean := False;
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.UC_O_Oblique_Stroke) ..
               Character'Pos(AC.Latin_1.LC_O_Diaeresis) loop
         if not ACH.Is_Alphanumeric(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 9");
         end if;
         TC_Boolean := ACH.Is_Special(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect Is_Special result - 9");
            TC_Boolean := False;
         end if;
      end loop;

      for i in Character'Pos(AC.Latin_1.LC_O_Oblique_Stroke) ..
               Character'Pos(AC.Latin_1.LC_Y_Diaeresis) loop
         if not ACH.Is_Alphanumeric(Character'Val(i)) then
            Report.Failed ("Incorrect Is_Alphanumeric result - 10");
         end if;
         TC_Boolean := ACH.Is_Special(Character'Val(i)); 
         if TC_Boolean then
            Report.Failed ("Incorrect Is_Special result - 10");
            TC_Boolean := False;
         end if;
      end loop;


   exception
      when others => Report.Failed ("Exception raised during processing");
   end Test_Block;


   Report.Result;

end CXA3001;
