-- C452A01.A
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
-- OBJECTIVES:
--     Check that the "=" operator of a language-defined nonlimited private
--     type is used in the equality for a record type containing a component of
--     the type. Part 1: Core types.
--
-- TEST DESCRIPTION:
--     This test also checks for reemergence of some other operation in a
--     generic, and for use in the equality for an array type.
--
--     The foundation is used to do the actual checks; this unit just has
--     to set up appropriate values.
--
--     Note that this test could pass even if the type is incorrectly
--     implemented if the predefined equality and the user-defined equality
--     happen to get the same result for the values used in this test.
--
--     This test is less likely to fail for a correct Ada 2012 implementation
--     than for an implementation for earlier versions of Ada, as Ada 2012
--     requires all record types to compose properly (this was only true of
--     tagged types in earlier versions).
--
-- CHANGE HISTORY:
--     25 JAN 2001   PHL   Initial version.
--     19 Dec 2018   RLB   Created test from submitted version; added
--                         missing cases for Exception_Id and Time.

with System;
with Ada.Exceptions;
with Ada.Calendar;

with Report;
use Report;
with F452A00;
procedure C452A01 is
begin
    Test ("C452A01",
          "Check that ""="" for all language-defined nonlimited types " &
          "behaves as if it is predefined for composition and for " &
          "formal types. Part 1: Core types");

    Address:
    declare
        X : aliased Float;

        C1 : constant System.Address := X'Address;
        C2 : System.Address;
        C3 : System.Address;

        package Inst is new F452A00
                               ("System.Address", System.Address, C1, C2, C3);
    begin
        if Equal (1, 1) then -- Optimization blocker.
            C2 := X'Address;
            C3 := C2'Address;
        else
            C2 := C2'Address;
            C3 := X'Address;
        end if;
        Inst.Check;
    end Address;

    Id:
    declare
        Some_Error : exception;
        Some_Other_Error : exception;

        C1 : constant Ada.Exceptions.Exception_Id := Some_Error'Identity;
        C2 : Ada.Exceptions.Exception_Id;
        C3 : Ada.Exceptions.Exception_Id;

        package Inst1 is new F452A00
                               ("Exception_Id_1",
                                Ada.Exceptions.Exception_Id,
                                Ada.Exceptions.Null_Id, C2, C3);

        package Inst2 is new F452A00
                               ("Exception_Id_2",
                                Ada.Exceptions.Exception_Id,
                                C1, C2, C3);
    begin
        for Subtest in 1 .. 2 loop
            if Equal (Subtest, 1) then -- Optimization blocker.
                C2 := Ada.Exceptions.Null_Id;
                C3 := Some_Other_Error'Identity;
            else
                C2 := Some_Error'Identity;
                C3 := Some_Other_Error'Identity;
            end if;
            if Equal (Subtest, 2) then -- Optimization blocker.
                Inst2.Check;
            else
                Inst1.Check;
            end if;
        end loop;
    end Id;

    Time:
    declare

        use type Ada.Calendar.Time;

        C1 : constant Ada.Calendar.Time := Ada.Calendar.Clock;
        C2 : Ada.Calendar.Time;
        C3 : Ada.Calendar.Time;

        package Inst1 is new F452A00
                               ("Time_1",
                                Ada.Calendar.Time,
                                C1, C2, C3);

        package Inst2 is new F452A00
                               ("Time_2",
                                Ada.Calendar.Time,
                                (C1 - 86400.0) - 86400.0, -- Two days.
                                C2, C3);

        package Inst3 is new F452A00
                               ("Time_3",
                                Ada.Calendar.Time,
                                C1 + 40000.0,
                                C2, C3);
    begin
        for Subtest in 1 .. 3 loop
            if Equal (Subtest, 1) then -- Optimization blocker.
                C2 := C1;
                C3 := Ada.Calendar.Time_Of (Month => 8, Day => 31,
                                            Seconds => 35640.0,
                                            Year => Ada.Calendar.Year(C1));
            elsif Equal (Subtest, 2) then -- Optimization blocker.
                C2 := (C1 - 86400.0) - 86400.0;
                C3 := C1;
            else
                C2 := C1 + 40000.0;
                C3 := C1;
            end if;
            case Subtest is
                when 1 => Inst1.Check;
                when 2 => Inst2.Check;
                when 3 => Inst3.Check;
                when others => Failed ("Out of test range");
            end case;
        end loop;
    end Time;

    Result;
end C452A01;

