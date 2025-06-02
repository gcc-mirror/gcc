-- C380001.A
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
--*
--
-- OBJECTIVE:
--    Check that checks are made properly when a per-object expression contains
--    an attribute whose prefix denotes the current instance of the type.
--    (Defect Report 8652/0002, as reflected in Technical Corrigendum 1,
--    RM95 3.8(18/1)).
--
-- CHANGE HISTORY:
--     9 FEB 2001   PHL   Initial version.
--    29 JUN 2002   RLB   Readied for release.
--
--!
with Ada.Exceptions;
use Ada.Exceptions;
with Report;
use Report;
procedure C380001 is

    type Negative is range Integer'First .. -1;

    type R1 is
        record
            C : Negative := Negative (Ident_Int (R1'Size));
        end record;


    type R2;

    type R3 (D1 : access R2; D2 : Natural) is limited null record;

    type R2 is limited
        record
            C : R3 (R2'Access, Ident_Int (-1));
        end record;

begin
    Test ("C380001", "Check that checks are made properly when a " &
                        "per-object expression contains an attribute whose " &
                        "prefix denotes the current instance of the type");
    begin
        declare
            X : R1;
        begin
            Failed
               ("No exception raised when evaluating a per-object expression " &
                "containing an attribute - 1");
        end;
    exception
        when Constraint_Error =>
            null;
        when E: others =>
            Failed ("Exception " & Exception_Name (E) &
                    " raised - " & Exception_Information (E) & " - 1");
    end;

    declare
        type A is access R1;
        X : A;
    begin
        X := new R1;
        Failed ("No exception raised when evaluating a per-object expression " &
                "containing an attribute - 2");
    exception
        when Constraint_Error =>
            null;
        when E: others =>
            Failed ("Exception " & Exception_Name (E) &
                    " raised - " & Exception_Information (E) & " - 2");
    end;

    begin
        declare
            X : R2;
        begin
            Failed
               ("No exception raised when elaborating a per-object constraint " &
                "containing an attribute - 3");
        end;
    exception
        when Constraint_Error =>
            null;
        when E: others =>
            Failed ("Exception " & Exception_Name (E) &
                    " raised - " & Exception_Information (E) & " - 3");
    end;

    declare
        type A is access R2;
        X : A;
    begin
        X := new R2;
        Failed
           ("No exception raised when evaluating a per-object constraint " &
            "containing an attribute - 4");
    exception
        when Constraint_Error =>
            null;
        when E: others =>
            Failed ("Exception " & Exception_Name (E) &
                    " raised - " & Exception_Information (E) & " - 4");
    end;

    Result;
end C380001;
