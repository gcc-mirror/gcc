-- C250002.AW
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
--      Check that characters in Latin-1 above ASCII.Del can be used in
--      identifiers, character literals and strings.
-- 
-- TEST DESCRIPTION:
--      This test utilizes the brackets scheme for representing Latin-1
--      character values in transportable 7 bit ASCII as proposed by
--      Robert Dewar; this test defines Character and String objects,
--      assigns and tests several sample values.  Several Identifiers
--      used in this test also include Characters via the bracket escape
--      sequence scheme.
--
--      Note that C250001 checks Wide_Characters and Wide_Strings.
--
-- SPECIAL REQUIREMENTS:
--
--      This file must be preprocessed before it can be executed as a test.
--
--      This test requires that all occurrences of the bracket escaped
--      characters be replaced with the corresponding 8 bit character.
--
--      Characters above ASCII.Del are represented by a 6 character sequence:
--
--          ["xx"]
--
--      where the character code represented is specified by two hexadecimal
--      digits (<xx>) upper case. For example the Latin-1 character with the
--      code 16#AB# is represented by the six character sequence:
--
--          ["AB"]
--
--      None of the values used in this test should be interpreted as
--      a control character.
--
--      The following function documents the translation algorithm:
--
--     function To_Char( S:String ) return Character is
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
--       return Character'Val(Numerical);
--     end To_Char;
--
--
-- CHANGE HISTORY:
--      10 JAN 96   SAIC   Initial version
--      12 NOV 96   SAIC   Changed file extension to .AW
--
--!

----------------------------------------------------------------- C250002_0

package C250002_0 is

  -- The extended characters used in this test start with
  -- the character '["A1"]' 16#A1# and increase from there

  type Tagged_["C0"]_Id is tagged record
    Length, Width: Natural;
  end record;

  X_Char_A2 : constant Character := '["A2"]';

  X_Char_A3_Through_A9 : constant String := 
               "["A3"]["A4"]["A5"]["A6"]["A7"]["A8"]["A9"]";

  X_Char_AA_AB : constant String := "["AA"]["AB"]";

end C250002_0;

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- no package body C250002_0 is required or allowed

----------------------------------------------------------------- C250002_X

with Ada.Characters.Latin_1;
package C250002_["C1"] is

  type Enum is ( Item, 'A', '["AD"]', AE_["C6"]["E6"]_ae,
                 '["2D"]', '["FF"]' );

  task type C2_["C2"] is
    entry C2_["C3"];
  end C2_["C2"];

end C250002_["C1"];

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

package body C250002_["C1"] is

  task body C2_["C2"] is
  begin
    accept C2_["C3"];
  end C2_["C2"];

end C250002_["C1"];

------------------------------------------------------------------- C250002

with Report;
with C250002_0;
with C250002_["C1"];

with Ada.Tags;

procedure C250002 is
  use C250002_0; 

  My_Task: C250002_["C1"].C2_["C2"];

  function Hex( N: Natural ) return String is
    S : String := "xx";
    T : String := "0123456789ABCDEF";
  begin
    S(1) := T(N  /  16 +1);
    S(2) := T(N mod 16 +1);
    return S;
  end Hex;

  procedure Match( Check : Character; Matching : Natural ) is
  begin
    if Check /= Character'Val( Matching ) then
      Report.Failed( "Didn't match for " & Hex(Matching) );
    end if;
  end Match;

  type Value_List is array(Positive range <>) of Natural;

  procedure Match( Check : String; Matching : Value_List ) is
  begin
    if Check'Length /= Matching'Length then
      Report.Failed( "Check'Length /= Matching'Length" );
    else
      for I in Check'Range loop
        Match( Check(I), Matching(I - Check'First + Matching'First) );
      end loop;
    end if;
  end Match;

  TC_Count : Natural := 0;

begin  -- Main test procedure.

  Report.Test ("C250002", "Check that characters above ASCII.Del can be " &
                          "used in identifiers, character literals and " &
                          "strings" );

  Report.Comment( Ada.Tags.Expanded_Name(Tagged_["C0"]_Id'Tag) );

  for Specials in C250002_["C1"].Enum loop
    TC_Count := TC_Count +1;
  end loop;

  if TC_Count /= 6 then
    Report.Failed("Expected 6 literals in Enum");
  end if;

  Match( X_Char_A2, 16#A2# );

  Match(X_Char_A3_Through_A9,
         (16#A3#,16#A4#,16#A5#,16#A6#,16#A7#,16#A8#,16#A9#) );

  -- check catenations

  Match( X_Char_A2 & X_Char_A2, (16#A2#,16#A2#) );

  Match( X_Char_A2 & X_Char_AA_AB, (16#A2#,16#AA#,16#AB#) );

  Match( X_Char_AA_AB & X_Char_A2, (16#AA#,16#AB#,16#A2#) );

  Match( X_Char_AA_AB & X_Char_AA_AB,
         (16#AA#,16#AB#,16#AA#,16#AB#) );

  My_Task.C2_["C3"];

  Report.Result;

end C250002;
