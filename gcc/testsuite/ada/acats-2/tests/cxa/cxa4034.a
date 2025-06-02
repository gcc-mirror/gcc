-- CXA4034.A
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
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--    Check that Ada.Strings.Bounded.Slice raises Index_Error if
--    High > Length (Source) or Low > Length (Source) + 1.
--    (Defect Report 8652/0049).
--
--    Check that Ada.Strings.Wide_Bounded.Slice raises Index_Error if
--    High > Length (Source) or Low > Length (Source) + 1.
--
-- CHANGE HISTORY:
--    12 FEB 2001   PHL   Initial version
--    14 MAR 2001   RLB   Added Wide_Bounded subtest.
--
--!
with Ada.Exceptions;
use Ada.Exceptions;
with Ada.Strings.Bounded;
with Ada.Strings.Wide_Bounded;
use Ada.Strings;
with Report;
use Report;
procedure CXA4034 is

    package Bs is new Ada.Strings.Bounded.Generic_Bounded_Length (40);

    package WBs is new Ada.Strings.Wide_Bounded.Generic_Bounded_Length (32);

    Source : String (Ident_Int (1) .. Ident_Int (30));

    Wide_Source : Wide_String (Ident_Int (1) .. Ident_Int (24));

    X : Bs.Bounded_String;

    WX : WBs.Bounded_Wide_String;

begin
    Test ("CXA4034",
          "Check that Slice raises Index_Error if either Low or High is " &
             "greater than the Length(Source) for Ada.Strings.Bounded and " &
             "Ada.Strings.Wide_Bounded");

    -- Fill Source with "ABC..."
    for I in Source'Range loop
        Source (I) := Ident_Char (Character'Val (I +
                        Character'Pos ('A') - Source'First));
    end loop;
    -- and W with "ABC..."
    for I in Wide_Source'Range loop
        Wide_Source (I) := Ident_Wide_Char (Wide_Character'Val (I +
                         Wide_Character'Pos ('A') - Wide_Source'First));
    end loop;

    X := Bs.To_Bounded_String (Source);

    begin
        declare
            S : constant String :=
               Bs.Slice (X, Low => Ident_Int (28), High => Ident_Int (41));
        begin
            Failed ("No exception raised by Slice - 1");
            if S = Source then
                Comment ("Don't optimize S");
            end if;
        end;
    exception
        when Index_Error =>
            null; -- Expected exception.
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 1");
    end;

    begin
        declare
            S : constant String :=
               Bs.Slice (X, Low => Ident_Int (8), High => Ident_Int (31));
        begin
            Failed ("No exception raised by Slice - 2");
            if S = Source then
                Comment ("Don't optimize S");
            end if;
        end;
    exception
        when Index_Error =>
            null; -- Expected exception.
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 2");
    end;

    begin
        declare
            S : constant String :=
               Bs.Slice (X, Low => Ident_Int (15), High => Ident_Int (30));
        begin
            if S /= Source(15..30) then
                Failed ("Wrong result - 3");
            end if;
        end;
    exception
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 3");
    end;

    begin
        declare
            S : constant String :=
               Bs.Slice (X, Low => Ident_Int (42), High => Ident_Int (28));
        begin
            Failed ("No exception raised by Slice - 4");
            if S = Source then
                Comment ("Don't optimize S");
            end if;
        end;
    exception
        when Index_Error =>
            null; -- Expected exception.
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 4");
    end;

    begin
        declare
            S : constant String :=
               Bs.Slice (X, Low => Ident_Int (31), High => Ident_Int (28));
        begin
            if S /= "" then
                Failed ("Wrong result - 5");
            end if;
        end;
    exception
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 5");
    end;

    begin
        declare
            S : constant String :=
               Bs.Slice (X, Low => Ident_Int (30), High => Ident_Int (30));
        begin
            if S /= Source(30..30) then
                Failed ("Wrong result - 6");
            end if;
        end;
    exception
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 6");
    end;

    WX := WBs.To_Bounded_Wide_String (Wide_Source);

    begin
        declare
            W : constant Wide_String :=
               WBs.Slice (WX, Low => Ident_Int (21), High => Ident_Int (33));
        begin
            Failed ("No exception raised by Slice - 7");
            if W = Wide_Source then
                Comment ("Don't optimize W");
            end if;
        end;
    exception
        when Index_Error =>
            null; -- Expected exception.
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 7");
    end;

    begin
        declare
            W : constant Wide_String :=
               WBs.Slice (WX, Low => Ident_Int (8), High => Ident_Int (25));
        begin
            Failed ("No exception raised by Slice - 8");
            if W = Wide_Source then
                Comment ("Don't optimize W");
            end if;
        end;
    exception
        when Index_Error =>
            null; -- Expected exception.
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 8");
    end;

    begin
        declare
            W : constant Wide_String :=
               WBs.Slice (WX, Low => Ident_Int (15), High => Ident_Int (24));
        begin
            if W /= Wide_Source(15..24) then
                Failed ("Wrong result - 8");
            end if;
        end;
    exception
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 9");
    end;

    begin
        declare
            W : constant Wide_String :=
               WBs.Slice (WX, Low => Ident_Int (36), High => Ident_Int (20));
        begin
            Failed ("No exception raised by Slice - 10");
            if W = Wide_Source then
                Comment ("Don't optimize W");
            end if;
        end;
    exception
        when Index_Error =>
            null; -- Expected exception.
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 10");
    end;

    begin
        declare
            W : constant Wide_String :=
               WBs.Slice (WX, Low => Ident_Int (25), High => Ident_Int (21));
        begin
            if W /= "" then
                Failed ("Wrong result - 11");
            end if;
        end;
    exception
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 11");
    end;

    begin
        declare
            W : constant Wide_String :=
               WBs.Slice (WX, Low => Ident_Int (24), High => Ident_Int (24));
        begin
            if W /= Wide_Source(24..24) then
                Failed ("Wrong result - 12");
            end if;
        end;
    exception
        when E: others =>
            Failed ("Exception raised - " & Exception_Name (E) &
                    " - " & Exception_Message (E) & " - 12");
    end;

    Result;
end CXA4034;

