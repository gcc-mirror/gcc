--  C458001.A
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
--  OBJECTIVE:
--     Check that the predicate of a quantified expression is evaluated in
--     the order specified by the loop_parameter_specification.
--
--     Check that result of a quantified expression with a
--     loop_parameter_specification is True if the predicate is True for
--     all values if the quantifier is all and any value if the quantifier
--     is some, and is False otherwise.
--
--     Check that evaluation of predicates of a quantified expression with
--     a loop_parameter_specification stops when the result is determined,
--     and no extra evaluations occur.
--
--  HISTORY:
--     BJM 12/28/11  Created original test.
--     RLB  3/24/14  Changed name, additional objectives, and four test cases.
--!

with Report; use Report;

procedure C458001 is

   Last_Check : Natural := Natural'First;
   Test_Array : array (1 .. 10) of Integer := (others => -1);

   function Succession_Check
     (X       : Integer;
      Result  : Boolean;
      Forward : Boolean)
      return    Boolean
   is
   begin

      if Forward then
         if Natural'Succ (Last_Check) /= X then
            Failed ("Expression evaluated out of order");
         end if;
      else
         if Natural'Pred (Last_Check) /= X then
            Failed ("Expression evaluated out of order");
         end if;
      end if;

      if Test_Array (X) /= X then
         Failed ("Unexptected array value");
      end if;

      Last_Check := X;
      return Result;
   end Succession_Check;

begin
   Test
     ("C458001",
      "Check that the predicate of a quantified expression " &
      "is evaluated in the order specified by the " &
      "loop_parameter_specification");

   for I in Test_Array'Range loop
      Test_Array (I) := I;
   end loop;

   Last_Check := Test_Array'First - 1;

   declare
      Expression_Value : Boolean := False;
   begin

      --  Forward iteration through all elements
      Expression_Value :=
        (for all I in Test_Array'Range =>
           Succession_Check (I, True, True));

      if not Expression_Value then
         Failed ("Wrong value for quantified expression - 1");
      end if;

      if Last_Check /= Test_Array'Last then
         Failed ("Unexpected value for last evaluated expression - 1");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 1");
   end;

   Last_Check := Test_Array'Last + 1;

   declare
      Expression_Value : Boolean := False;
   begin

      --  Reverse iteration through all elements
      Expression_Value :=
        (for all I in reverse Test_Array'Range =>
           Succession_Check (I, True, False));

      if not Expression_Value then
         Failed ("Wrong value for quantified expression - 2");
      end if;

      if Last_Check /= Test_Array'First then
         Failed ("Unexpected value for last evaluated expression - 2");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 2");
   end;

   Last_Check := Test_Array'First - 1;

   declare
      Expression_Value : Boolean := False;
   begin

      --  Forward iteration to check one element exists
      Expression_Value :=
        (for some I in Test_Array'Range =>
           Succession_Check (I, (I = Test_Array'Last), True));

      if not Expression_Value then
         Failed ("Wrong value for quantified expression - 3");
      end if;

      if Last_Check /= Test_Array'Last then
         Failed ("Unexpected value for last evaluated expression - 3");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 3");
   end;

   Last_Check := Test_Array'Last + 1;

   declare
      Expression_Value : Boolean := False;
   begin

      --  Reverse iteration to check if one element exists
      Expression_Value :=
        (for some I in reverse Test_Array'Range =>
           Succession_Check (I, (I = Test_Array'First), False));

      if not Expression_Value then
         Failed ("Wrong value for Quantified Expression - 4");
      end if;

      if Last_Check /= Test_Array'First then
         Failed ("Unexpected value for last evaluated expression - 4");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 4");
   end;

   Last_Check := Test_Array'First - 1;

   declare
      Expression_Value : Boolean := False;
   begin

      --  Forward iteration stopping on the second element
      Expression_Value :=
        (for all I in Test_Array'Range =>
           Succession_Check (I, Result => I <= 1, Forward => True));

      if Expression_Value then
         Failed ("Wrong value for quantified expression - 5");
      end if;

      if Last_Check < 2 then
         Failed ("Did not evaluate enough elements for False quantified expression - 5");
      elsif Last_Check > 2 then
         Failed ("Evaluated too many elements for False quantified expression - 5");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 5");
   end;

   Last_Check := Test_Array'Last + 1;

   declare
      Expression_Value : Boolean := False;
   begin

      --  Reverse iteration stopping on the 7th element.
      Expression_Value :=
        (for all I in reverse Test_Array'Range =>
           Succession_Check (I, Result => I > 7, Forward => False));

      if Expression_Value then
         Failed ("Wrong value for quantified expression - 6");
      end if;

      if Last_Check < 7 then
         Failed ("Did not evaluate enough elements for False quantified expression - 6");
      elsif Last_Check > 7 then
         Failed ("Evaluated too many elements for False quantified expression - 6");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 6");
   end;

   Last_Check := Test_Array'First - 1;

   declare
      Expression_Value : Boolean := False;
   begin

      --  Forward iteration to check one element exists, in this case
      --  the third element.
      Expression_Value :=
        (for some I in Test_Array'Range =>
           Succession_Check (I, Result => (I = 3), Forward => True));

      if not Expression_Value then
         Failed ("Wrong value for quantified expression - 7");
      end if;

      if Last_Check < 3 then
         Failed ("Did not evaluate enough elements for True quantified expression - 7");
      elsif Last_Check > 3 then
         Failed ("Evaluated too many elements for True quantified expression - 7");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 7");
   end;

   Last_Check := Test_Array'Last + 1;

   declare
      Expression_Value : Boolean := False;
   begin

      --  Reverse iteration to check if one element exists, in this case
      --  the sixth element.
      Expression_Value :=
        (for some I in reverse Test_Array'Range =>
           Succession_Check (I, Result => (I = 6), Forward => False));

      if not Expression_Value then
         Failed ("Wrong value for Quantified Expression - 8");
      end if;

      if Last_Check < 6 then
         Failed ("Did not evaluate enough elements for True quantified expression - 8");
      elsif Last_Check > 6 then
         Failed ("Evaluated too many elements for True quantified expression - 8");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 8");
   end;

   Last_Check := Test_Array'First - 1;

   declare
      Expression_Value : Boolean := False;
   begin

      --  Forward iteration to check one element exists, but none do.
      Expression_Value :=
        (for some I in Test_Array'Range =>
           Succession_Check (I, Result => False, Forward => True));

      if Expression_Value then
         Failed ("Wrong value for quantified expression - 9");
      end if;

      if Last_Check /= Test_Array'Last then
         Failed ("Unexpected value for last evaluated expression - 9");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 9");
   end;

   Last_Check := Test_Array'Last + 1;

   declare
      Expression_Value : Boolean := False;
   begin

      --  Reverse iteration to check if one element exists, but none do.
      Expression_Value :=
        (for some I in reverse Test_Array'Range =>
           Succession_Check (I, Result => False, Forward => False));

      if Expression_Value then
         Failed ("Wrong value for Quantified Expression - 10");
      end if;

      if Last_Check /= Test_Array'First then
         Failed ("Unexpected value for last evaluated expression - 10");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 10");
   end;

   Result;
end C458001;
