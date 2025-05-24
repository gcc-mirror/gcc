-- C250001.AW
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
--      Check that wide character literals are supported.
--      Check that wide character string literals are supported.
--
-- TEST DESCRIPTION:
--      This test utilizes the brackets scheme for representing wide character
--      values in transportable 7 bit ASCII as proposed by Robert Dewar;
--      this test defines Wide_Character and Wide_String objects, and assigns
--      and tests several sample values.
--
-- SPECIAL REQUIREMENTS:
--
--      This file must be preprocessed before it can be executed as a test.
--
--      This test requires that all occurrences of the bracket escape
--      representation for wide characters be replaced, as appropriate, with
--      the corresponding wide character as represented by the implementation.
--
--      Characters above ASCII.Del are represented by an 8 character sequence:
--
--          ["xxxx"]
--
--      where the character code represented is specified by four hexadecimal
--      digits, (<xxxx>) upper case. For example the wide character with the
--      code 16#ABCD# is represented by the eight character sequence:
--
--          ["ABCD"]
--
--      The following function documents the translation algorithm:
--
--     function To_Wide( S:String ) return Wide_character is
--       Numerical : Natural := 0;
--       type Xlate is array(Character range '0'..'F') of Natural;
--       Xlation : Xlate
--               := ('0' =>  0, '1' =>  1, '2' =>  2, '3' =>  3, '4' =>  4,
--                   '5' =>  5, '6' =>  6, '7' =>  7, '8' =>  8, '9' =>  9,
--                   'A' => 10, 'B' => 11, 'C' => 12, 'D' => 13, 'E' => 14,
--                   'F' => 15,  others => 0 );
--     begin
--       for I in S'Range loop
--         Numerical := Numerical * 16 + Xlation(S(I));
--       end loop;
--       return Wide_Character'Val(Numerical);  -- the returned value is
--                                   implementation dependent
--     exception
--       when Constraint_Error => raise;
--     end To_Wide;
--
--
-- CHANGE HISTORY:
--      26 OCT 95   SAIC   Initial .Aversion
--      11 APR 96   SAIC   Minor robustness changes for 2.1
--      12 NOV 96   SAIC   Changed file extension to .AW
--
--!

----------------------------------------------------------------- C250001_0

package C250001_0 is

  -- The wide characters used in this test are sequential starting with
  -- the character '["4F42"]' 16#0F42#

  Four_Eff_Four_Two : constant Wide_Character := '["4F42"]';

  Four_Eff_4_3_Through_9 : constant Wide_String := 
               "["4F43"]["4F44"]["4F45"]["4F46"]["4F47"]["4F48"]["4F49"]";

  Four_Eff_A_B : constant Wide_String := "["4F4A"]["4F4B"]";

end C250001_0;

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- no package body C250001_0 is required or allowed

------------------------------------------------------------------- C250001

with Report;
with C250001_0;
with Ada.Tags;

procedure C250001 is
  use C250001_0; 

  function Hex( N: Natural ) return String is
    S : String := "xxxx";
    T : String := "0123456789ABCDEF";
    V : Natural := N;
  begin
    for I in reverse 1..4 loop
      S(I) := T(V rem 16 +1);
      V := V / 16;
    end loop;
    return S;
  end Hex;

  procedure Match( Check : Wide_Character; Matching : Natural ) is
  begin
    if Wide_Character'Pos( Check ) /= Matching then
      Report.Failed( "Didn't match for " & Hex(Matching) );
    end if;
  end Match;

  type Value_List is array(Positive range <>) of Natural;

  procedure Match( Check : Wide_String; Matching : Value_List ) is
  begin
    if Check'Length /= Matching'Length then
      Report.Failed( "Check'Length /= Matching'Length" );
    else
      for I in Check'Range loop
        Match( Check(I), Matching(I) );
      end loop;
    end if;
  end Match;

begin  -- Main test procedure.

  Report.Test ("C250001", "Check that wide character literals " &
                          "are supported.  Check that wide character " &
                          "string literals are supported." );

  Match( Four_Eff_Four_Two, 16#4F42# );

  Match(Four_Eff_4_3_Through_9,
         (16#4F43#,16#4F44#,16#4F45#,16#4F46#,16#4F47#,16#4F48#,16#4F49#) );

 -- check catenations

  Match( Four_Eff_Four_Two & Four_Eff_Four_Two, (16#4F42#,16#4F42#) );

  Match( Four_Eff_Four_Two & Four_Eff_A_B, (16#4F42#,16#4F4A#,16#4F4B#) );

  Match( Four_Eff_A_B & Four_Eff_Four_Two, (16#4F4A#,16#4F4B#,16#4F42#) );

  Match( Four_Eff_A_B & Four_Eff_A_B,
         (16#4F4A#,16#4F4B#,16#4F4A#,16#4F4B#) );

  Report.Result;

end C250001;
