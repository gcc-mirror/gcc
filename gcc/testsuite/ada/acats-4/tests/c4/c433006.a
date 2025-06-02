-- C433006.A
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
--
--*
-- OBJECTIVE
--     Check that the constraint of the constrained array subtype of a function
--     return is used to determine the bounds of an array aggregate with an
--     others choice in the return expression of an expression function.
--
-- TEST DESCRIPTION
--     In this test, we declare several unconstrained array types, and
--     several dynamic subtypes. We then test a variety of cases of using
--     appropriate aggregates. Some of these cases raise Constraint_Error.
--
--     We test a few cases of parenthesized aggregates; these should test
--     the combination of 4.3.3(11/4) and 4.3.3(15/3).
--
--     The rule in question was added by AI12-0157-1, from the 2015
--     Technical Corrigendum, although most people would have expected it
--     to be true.
--
-- CHANGE HISTORY:
--      16 Apr 2015   RLB   Created from C433005.

with Report;
procedure C433006 is

    type Color_Type is (Red, Orange, Yellow, Green, Blue, Indigo, Violet);

    type Default_to_Zero is range -10000 .. 10000
       with Default_Value => 0;

    function Ident_Int (Val : in Default_to_Zero) return Default_to_Zero is
        (Default_to_Zero(Report.Ident_Int(Integer(Val))));

    type Array_1 is array (Positive range <>) of Default_to_Zero;

    subtype Sub_1_1 is Array_1 (Report.Ident_Int(1) .. Report.Ident_Int(3));
    subtype Sub_1_2 is Array_1 (Report.Ident_Int(3) .. Report.Ident_Int(5));
    subtype Sub_1_3 is Array_1 (Report.Ident_Int(5) .. Report.Ident_Int(9));

    type Array_2 is array (Color_Type range <>) of Default_to_Zero;

    subtype Sub_2_1 is Array_2 (Color_Type'Val(Report.Ident_Int(0)) ..
                                Color_Type'Val(Report.Ident_Int(2)));
                                                 -- Red .. Yellow
    subtype Sub_2_2 is Array_2 (Color_Type'Val(Report.Ident_Int(3)) ..
                                Color_Type'Val(Report.Ident_Int(6)));
                                                 -- Green .. Violet
    type Array_3 is array (Color_Type range <>, Positive range <>)
                       of Default_to_Zero;

    subtype Sub_3_1 is Array_3 (Color_Type'Val(Report.Ident_Int(0)) ..
                                Color_Type'Val(Report.Ident_Int(2)),
                                Report.Ident_Int(3) .. Report.Ident_Int(5));
                                                 -- Red .. Yellow, 3 .. 5
    subtype Sub_3_2 is Array_3 (Color_Type'Val(Report.Ident_Int(1)) ..
                                Color_Type'Val(Report.Ident_Int(3)),
                                Report.Ident_Int(6) .. Report.Ident_Int(8));
                                                 -- Orange .. Green, 6 .. 8

    procedure Check_1 (Obj : Array_1; Low, High : Integer;
                       First_Component, Second_Component,
                           Last_Component : Default_to_Zero;
                       Test_Case : Character) is
    begin
        if Obj'First /= Low then
           Report.Failed ("Low bound incorrect (" & Test_Case & ")");
        end if;
        if Obj'Last /= High then
           Report.Failed ("High bound incorrect (" & Test_Case & ")");
        end if;
        if Obj(Low) /= First_Component then
           Report.Failed ("First Component incorrect (" & Test_Case & ")");
        end if;
        if Obj(Low+1) /= Second_Component then
           Report.Failed ("Second Component incorrect (" & Test_Case & ")");
        end if;
        if Obj(High) /= Last_Component then
           Report.Failed ("Last Component incorrect (" & Test_Case & ")");
        end if;
    end Check_1;

    procedure Check_2 (Obj : Array_2; Low, High : Color_Type;
                       First_Component, Second_Component,
                           Last_Component : Default_to_Zero;
                       Test_Case : Character) is
    begin
        if Obj'First /= Low then
           Report.Failed ("Low bound incorrect (" & Test_Case & ")");
        end if;
        if Obj'Last /= High then
           Report.Failed ("High bound incorrect (" & Test_Case & ")");
        end if;
        if Obj(Low) /= First_Component then
           Report.Failed ("First Component incorrect (" & Test_Case & ")");
        end if;
        if Obj(Color_Type'Succ(Low)) /= Second_Component then
           Report.Failed ("Second Component incorrect (" & Test_Case & ")");
        end if;
        if Obj(High) /= Last_Component then
           Report.Failed ("Last Component incorrect (" & Test_Case & ")");
        end if;
    end Check_2;

    procedure Check_3 (Test_Obj, Check_Obj : Array_3;
                       Low_1, High_1 : Color_Type;
                       Low_2, High_2 : Integer;
                       Test_Case : Character) is
    begin
        if Test_Obj'First(1) /= Low_1 then
           Report.Failed ("Low bound for dimension 1 incorrect (" &
                Test_Case & ")");
        end if;
        if Test_Obj'Last(1) /= High_1 then
           Report.Failed ("High bound for dimension 1 incorrect (" &
                Test_Case & ")");
        end if;
        if Test_Obj'First(2) /= Low_2 then
           Report.Failed ("Low bound for dimension 2 incorrect (" &
                Test_Case & ")");
        end if;
        if Test_Obj'Last(2) /= High_2 then
           Report.Failed ("High bound for dimension 2 incorrect (" &
                Test_Case & ")");
        end if;
        if Test_Obj /= Check_Obj then
           Report.Failed ("Components incorrect (" & Test_Case & ")");
        end if;
    end Check_3;

    function Test_A return Sub_1_1 is (2, 3, others => <>);

    function Test_B return Sub_2_1 is ((1, others => Ident_Int(6)));
       -- Extra parentheses.

    function Test_C return Sub_1_2 is (5, 6, 8, others => <>);

    function Test_D return Sub_1_1 is (2 => <>, others => 8);

    function Test_F return Sub_1_3 is (((6 => <>, 8 => <>, others => 1)));
       -- Two sets of extra parentheses.

    function Test_G return Sub_2_2 is
       (Ident_Int(88), 89, 90, 91, 92, others => <>);

    function Test_H return Sub_1_3 is
        ((5 => Ident_Int(88), 8 => <>,
          10 => <>, -- 10 not in applicable index constraint
          others => <>));
       -- Extra parentheses.

    function Test_J return Sub_2_2 is
        (Yellow => <>, -- Yellow not in applicable index constraint.
           Blue => <>, others => 77);

begin

    Report.Test ("C433006",
                 "Check that the constraint of the constrained array " &
                 "subtype of a function return is used to determine the " &
                 "bounds of an array aggregate with an others choice in the " &
                 "return expression of an expression function");

    Check_1 (Test_A, Low => 1, High => 3,
             First_Component => 2, Second_Component => 3, Last_Component => 0,
             Test_Case => 'A');

    Check_2 (Test_B,
             Low => Red, High => Yellow,
             First_Component => 1, Second_Component => 6, Last_Component => 6,
             Test_Case => 'B');

    -- Check that the others clause does not need to represent any components:
    Check_1 (Test_C, Low => 3, High => 5,
             First_Component => 5, Second_Component => 6, Last_Component => 8,
             Test_Case => 'C');

    -- Check named choices are allowed:
    Check_1 (Test_D,
             Low => 1, High => 3,
             First_Component => 8, Second_Component => 0, Last_Component => 8,
             Test_Case => 'D');

    Check_1 (Test_F,
             Low => 5, High => 9,
             First_Component => 1, Second_Component => 0, Last_Component => 1,
             Test_Case => 'F');

    -- Check positional aggregates that are too long:
    begin
        Check_2 (Test_G,
             Low => Green, High => Violet,
             First_Component => 88, Second_Component => 89,
             Last_Component => 91,
             Test_Case => 'G');
        Report.Failed ("Constraint_Error not raised by positional " &
                       "aggregate with too many choices (G)");
    exception
        when Constraint_Error => null; -- Expected exception.
    end;

    -- Check named aggregates with choices in the index subtype but not in the
    -- applicable index constraint:

    begin
        Check_1 (Test_H,
             Low => 5, High => 9,
             First_Component => 88, Second_Component => 0,
             Last_Component => 0,
             Test_Case => 'H');
        Report.Failed ("Constraint_Error not raised by aggregate choice " &
                       "index outside of applicable index constraint (H)");
    exception
        when Constraint_Error => null; -- Expected exception.
    end;

    begin
        Check_2 (Test_J,
             Low => Green, High => Violet,
             First_Component => 77, Second_Component => 0,
             Last_Component => 77,
             Test_Case => 'J');
        Report.Failed ("Constraint_Error not raised by aggregate choice " &
                       "index outside of applicable index constraint (J)");
    exception
        when Constraint_Error => null; -- Expected exception.
    end;

    Report.Result;

end C433006;
