-- C420001.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687 and
--     F08630-91-C-0015, the U.S. Government obtained unlimited rights in the
--     software and documentation contained herein.  Unlimited rights are
--     defined in DFAR 252.227-7013(a)(19).  By making this public release,
--     the Government intends to confer upon all recipients unlimited rights
--     equal to those held by the Government.  These rights include rights to
--     use, duplicate, release or disclose the released technical data and
--     computer software in whole or in part, in any manner and for any purpose
--     whatsoever, and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE
--     Check that if the index subtype of a string type is a modular subtype
--     whose lower bound is zero, then the evaluation of a null string_literal
--     raises Constraint_Error. This was confirmed by AI95-00138.
--
-- TEST DESCRIPTION
--     In this test, we have a generic formal modular type, and we have
--     several null string literals of that type.  Because the type is
--     generic formal, the string literals are not static, and therefore
--     the Constraint_Error should be detected at run time.
--
-- CHANGE HISTORY:
--      29 JUN 1999   RAD   Initial Version
--      23 SEP 1999   RLB   Improved comments and messages, renamed, issued.
--
--!
with Report; use Report; pragma Elaborate_All(Report);
with System;
procedure C420001 is
    generic
        type Modular is mod <>;
    package Mod_Test is
        type Str is array(Modular range <>) of Character;
        procedure Test_String_Literal;
    end Mod_Test;

    package body Mod_Test is
        procedure Test_String_Literal is
        begin
            begin
                declare
                    Null_String: Str := ""; -- Should raise C_E.
                begin
                    Comment(String(Null_String)); -- Avoid 11.6 issues.
                end;
                Failed("Null string didn't raise Constraint_Error");
            exception
                when Exc: Constraint_Error =>
                    null; -- Comment("Constraint_Error -- OK");
                when Exc2: others =>
                    Failed("Null string raised wrong exception");
            end;
            begin
                Failed(String(Str'(""))); -- Should raise C_E, not do Failed.
                Failed("Null string didn't raise Constraint_Error");
            exception
                when Exc: Constraint_Error =>
                    null; -- Comment("Constraint_Error -- OK");
                when Exc2: others =>
                    Failed("Null string raised wrong exception");
            end;
        end Test_String_Literal;
    begin
        Test_String_Literal;
    end Mod_Test;
begin
    Test("C420001", "Check that if the index subtype of a string type is a " &
                    "modular subtype whose lower bound is zero, then the " &
                    "evaluation of a null string_literal raises " &
                    "Constraint_Error. ");
    declare
        type M1 is mod 1;
        package Test_M1 is new Mod_Test(M1);
        type M2 is mod 2;
        package Test_M2 is new Mod_Test(M2);
        type M3 is mod 3;
        package Test_M3 is new Mod_Test(M3);
        type M4 is mod 4;
        package Test_M4 is new Mod_Test(M4);
        type M5 is mod 5;
        package Test_M5 is new Mod_Test(M5);
        type M6 is mod 6;
        package Test_M6 is new Mod_Test(M6);
        type M7 is mod 7;
        package Test_M7 is new Mod_Test(M7);
        type M8 is mod 8;
        package Test_M8 is new Mod_Test(M8);
        type M_Max_Binary_Modulus is mod System.Max_Binary_Modulus;
        package Test_M_Max_Binary_Modulus is new Mod_Test(M_Max_Binary_Modulus);
        type M_Max_Nonbinary_Modulus is mod System.Max_Nonbinary_Modulus;
        package Test_M_Max_Nonbinary_Modulus is new Mod_Test(M_Max_Nonbinary_Modulus);
    begin
        null;
    end;
    Result;
end C420001;
