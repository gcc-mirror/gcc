--  C457001.A
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
--     Check that for the evaluation of a non-boolean if expression,
--     the condition specified after if, and any conditions specified
--     after elsif, are evaluated in succession, until one evaluates to true.
--
--     Check that result of a non-boolean if expression is the result of
--     evaluating the dependent expression corresponding to the condition that
--     evaluates to True.
--
--  HISTORY:
--     BJM 12/28/11  Created original test.
--     RLB  3/24/14  Changed name and added missing objective.
--!

with Report; use Report;

procedure C457001 is

   Last_Check           : Natural := Natural'First;
   Expression_Evaluated : Boolean := False;

   function Evaluation_Check (X : Integer) return Natural is
   begin
      if Expression_Evaluated then
         Failed ("More than one dependent expression evaluated");
      end if;

      Expression_Evaluated := True;
      return X;
   end Evaluation_Check;

   function Succession_Check (X : Integer; Result : Boolean) return Boolean is
   begin
      if Natural'Succ (Last_Check) /= X then
         Failed ("Expression evaluated out of order");
      end if;

      Last_Check := X;
      return Result;
   end Succession_Check;

begin
   Test
     ("C457001",
      "Check that for the evaluation of a non-Boolean " &
      "if expression, the condition specified after if, " &
      "and any conditions specified after elsif, are " &
      "evaluated in succession, until one evalues to True");

   declare
      Expression_Value : Natural := 0;
   begin

      --  All conditions are evaluated, including the else
      Expression_Value :=
              (if Succession_Check (1, False) then Evaluation_Check (1)
               elsif Succession_Check (2, False) then Evaluation_Check (2)
               elsif Succession_Check (3, False) then Evaluation_Check (3)
               else Evaluation_Check (4));

      if Expression_Value /= 4 then
         Failed ("Unexpected value for conditional expression - 1");
      end if;

      if Last_Check /= 3 then
         Failed ("Unexpected value for last evaluated expression - 1");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 1");
   end;

   Last_Check           := Natural'First;
   Expression_Evaluated := False;

   declare
      Expression_Value : Natural := 0;
   begin

      --  Conditions up to the last elsif are evaluated.
      Expression_Value :=
              (if Succession_Check (1, False) then Evaluation_Check (1)
               elsif Succession_Check (2, False) then Evaluation_Check (2)
               elsif Succession_Check (3, True) then Evaluation_Check (3)
               else Evaluation_Check (4));

      if Expression_Value /= 3 then
         Failed ("Unexpected value for conditional expression - 2");
      end if;

      if Last_Check /= 3 then
         Failed ("Unexpected value for last evaluated expression - 2");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 2");
   end;

   Last_Check           := Natural'First;
   Expression_Evaluated := False;

   declare
      Expression_Value : Natural := 0;
   begin

      --  first conditional and first elsif are evaluated
      Expression_Value :=
              (if Succession_Check (1, False) then Evaluation_Check (1)
               elsif Succession_Check (2, True) then Evaluation_Check (2)
               elsif Succession_Check (3, False) then Evaluation_Check (3)
               else Evaluation_Check (4));

      if Expression_Value /= 2 then
         Failed ("Unexpected value for conditional expression - 3");
      end if;

      if Last_Check /= 2 then
         Failed ("Unexpected value for last evaluated expression - 3");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 3");
   end;

   Last_Check           := Natural'First;
   Expression_Evaluated := False;

   declare
      Expression_Value : Natural := 0;
   begin

      --  Only the first conditional is evaluated
      Expression_Value :=
              (if Succession_Check (1, True) then Evaluation_Check (1)
               elsif Succession_Check (2, True) then Evaluation_Check (2)
               elsif Succession_Check (3, True) then Evaluation_Check (3)
               else Evaluation_Check (4));

      if Expression_Value /= 1 then
         Failed ("Unexpected value for conditional expression - 4");
      end if;

      if Last_Check /= 1 then
         Failed ("Unexpected value for last evaluated expression - 4");
      end if;

   exception
      when others =>
         Failed ("Unexpected exception raised - 4");
   end;

   Result;
end C457001;
