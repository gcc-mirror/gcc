-- C854003.A
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
--    Check that a renaming-as-body used before the subprogram is frozen only
--    requires mode conformance.  (Defect Report 8652/0028, as reflected in
--    Technical Corrigendum 1, RM95 8.5.4(5/1)).
--
-- CHANGE HISTORY:
--    29 JAN 2001   PHL   Initial version.
--     5 DEC 2001   RLB   Reformatted for ACATS.
--
--!
with Report;
use Report;
procedure C854003 is

    package P is
        type T is private;
        C1 : constant T;
        C2 : constant T;
    private
        type T is new Integer'Base;
        C1 : constant T := T (Ident_Int (1));
        C2 : constant T := T (Ident_Int (1));
    end P;

    function Equals (X, Y : P.T) return Boolean;
    function Equals (X, Y : P.T) return Boolean renames P."=";

begin
    Test ("C854003",
          "Check that a renaming-as-body used before the subprogram " &
             "is frozen only requires mode conformance");

    if not Equals (P.C1, P.C2) then
        Failed ("Equality returned an unexpected result");
    end if;

    Result;
end C854003;

