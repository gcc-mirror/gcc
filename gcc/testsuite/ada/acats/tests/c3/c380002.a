-- C380002.A
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
--    Check that an expression in a per-object discriminant constraint which is
--    part of a named association is evaluated once for each association.
--    (Defect Report 8652/0002, as reflected in Technical Corrigendum 1,
--    RM95 3.8(18.1/1)).
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
procedure C380002 is

    F_Val : Integer := Ident_Int (0);

    function F return Integer is
    begin
        F_Val := F_Val + Ident_Int (1);
        return F_Val;
    end F;

    type R1;

    type R2 (D0 : Integer; D1 : access R1; D2 : Integer; D3 : Integer) is
       limited null record;

    type R1 is limited
        record
            C : R2 (D1 => R1'Access, D0 | D2 | D3 => F);
        end record;

begin
    Test ("C380002", "Check that an expression in a per-object discriminant " &
                        "constraint which is part of a named association is " &
                        "evaluated once for each association");

    if not Equal (F_Val, 3) then
        Failed ("Expression not evaluated the proper number of times");
    end if;

    Result;
end C380002;

