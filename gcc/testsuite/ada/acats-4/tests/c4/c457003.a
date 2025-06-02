-- C457003.A
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
--     Check that result of a case expression is the result of
--     evaluating the dependent expression of the case expression alternative
--     whose discrete choice list covers the selecting expression.
--
--     Check that only one dependent expression is evaluated for each
--     evaluation of a case expression.
--
--     Check that if the value of the selecting expression is not covered by
--     the discrete choice list of any case expression alternative, then
--     Constraint_Error is raised.
--
--  HISTORY:
--     JAC 2014-04-24  Created original test.
--     RLB 2014-04-24  Revised Constraint_Error check to take the possibility
--                     of the bounded error being detected into account, and
--                     to avoid triggering 13.9.1(12/3) which would make the
--                     test erroneous (and unusable).
--!

with Report;

procedure C457003 is

   subtype Small_Base_Type is Natural range 0 .. 7;
   subtype Small_Type is Small_Base_Type range 1 .. 4;

   Expression_Evaluated : Boolean := False;

   function Evaluation_Check (X : Natural) return Natural is
   begin
      if Expression_Evaluated then
         Report.Failed ("More than one dependent expression evaluated");
      end if;
      Expression_Evaluated := True;
      return X;
   end Evaluation_Check;


   function Do_Case (Selector : Small_Type;
                     Invalid_Selector : Boolean := False)
          return Natural is
      Expression_Value : Natural := 0;
   begin
      Expression_Value :=
        (case Selector is
            when 1 =>
               Evaluation_Check (99),
            when 2 | 3 =>
               Evaluation_Check (87),
            when 4 =>
               Evaluation_Check (66)
        );
      if Invalid_Selector then
         Report.Failed
            ("Invalid selecting expression should have raised " &
             "Constraint_Error or Program_Error");
      --else no check needed.
      end if;
      return Expression_Value;
   exception
      when Constraint_Error =>
         if Invalid_Selector then
            Report.Comment ("Constraint_Error raised by case expression " &
                            "with invalid selector");
            return Expression_Value;
         else
            Report.Failed ("Constraint_Error raised by case expression");
            raise;
         end if;
      when Program_Error =>
         if Invalid_Selector then
            Report.Comment ("Program_Error raised by case expression " &
                            "with invalid selector");
            -- We have to allow this, as evaluating the invalid parameter again
            -- triggers the bounded error (see below), which again could be
            -- detected and raise Program_Error.
            return Expression_Value;
         else
            Report.Failed ("Program_Error raised by case expression");
            raise;
         end if;
   end Do_Case;


begin

   Report.Test
     ("C457003",
      "Check the evaluation of a case expression");

   declare
      Expression_Value : Natural := 0;
   begin

      Expression_Value := Do_Case (1);

      if Expression_Value /= 99 then
         Report.Failed ("Unexpected value for case expression - 1");
      end if;

   exception
      when others =>
         Report.Failed ("Unexpected exception raised - 1");
   end;

   Expression_Evaluated := False;

   declare
      Expression_Value : Natural := 0;
   begin

      Expression_Value := Do_Case (2);

      if Expression_Value /= 87 then
         Report.Failed ("Unexpected value for case expression - 2");
      end if;

   exception
      when others =>
         Report.Failed ("Unexpected exception raised - 2");
   end;


   Expression_Evaluated := False;

   declare
      Expression_Value : Natural := 0;
   begin

      Expression_Value := Do_Case (3);

      if Expression_Value /= 87 then
         Report.Failed ("Unexpected value for case expression - 3");
      end if;

   exception
      when others =>
         Report.Failed ("Unexpected exception raised - 3");
   end;


   Expression_Evaluated := False;

   declare
      Expression_Value : Natural := 0;
   begin

      Expression_Value := Do_Case (4);

      if Expression_Value /= 66 then
         Report.Failed ("Unexpected value for case expression - 4");
      end if;

   exception
      when others =>
         Report.Failed ("Unexpected exception raised - 4");
   end;


   Expression_Evaluated := False;

   -- Attempt to put known invalid values into memory (this will often work,
   -- but not always):
   declare
      Junk_Value1 : Integer := Report.Ident_Int(0);
      Junk_Value2 : Integer := Report.Ident_Int(10);
   begin
      if Junk_Value1 /= 0 then
         Report.Failed ("Wrong junk value");
      elsif Junk_Value2 /= 10 then
         Report.Failed ("Wrong junk value");
      end if;
   end;

   declare

      Parameter_Value : Small_Type; -- Uninitialized, hopefully will get
                                    -- an invalid 0 or 10 value.

      Expression_Value : Natural;

   begin

      if Parameter_Value'Valid then
         Report.Comment ("Uninitialized value is valid, skipping " &
                         "case expression Constraint_Error check");
      else
         begin

            Expression_Value := Do_Case (Parameter_Value,
                 Invalid_Selector => True);
               -- Passing an invalid value; this is a bounded error by
               -- 13.9.1(9). We have to be prepared for either Constraint_Error
               -- or Program_Error to be raised here if the bounded error is
               -- detected.

            -- Failure checked inside of Do_Case.

         exception

            when Constraint_Error =>
               Report.Comment ("Constraint_Error raised by passing " &
                               "invalid value, unable to test case expression " &
                               "with invalid value");

            when Program_Error =>
               Report.Comment ("Program_Error raised by passing " &
                               "invalid value, unable to test case expression " &
                               "with invalid value");

            when others =>

               Report.Failed ("Unexpected exception raised - 5");

         end;
      end if;

   end;

   Report.Result;

end C457003;
