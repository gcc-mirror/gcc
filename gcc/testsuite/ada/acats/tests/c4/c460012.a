-- C460012.A
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
--    Check that the view created by a view conversion is constrained if the
--    target subtype is indefinite.  (Defect Report 8652/0017, Technical
--    Corrigendum 4.6(54/1)).
--
-- CHANGE HISTORY:
--    25 JAN 2001   PHL   Initial version.
--    29 JUN 2001   RLB   Reformatted for ACATS. Added optimization blocking.
--    02 JUL 2001   RLB   Fixed discriminant reference.
--
--!
with Ada.Exceptions;
use Ada.Exceptions;
with Report;
use Report;
procedure C460012 is

    subtype Index is Positive range 1 .. 10;

    type Definite_Parent (D1 : Index := 6) is
	record
	    F : String (1 .. D1) := (others => 'a');
	end record;

    type Indefinite_Child (D2 : Index) is new Definite_Parent (D1 => D2);

    Y : Definite_Parent;

    procedure P (X : in out Indefinite_Child) is
	C : Character renames X.F (3);
    begin
	X := (1, "a");
        if C /= 'a' then
            Failed ("No exception raised when changing the " &
		    "discriminant of a view conversion, value of C changed");
        elsif X.D2 /= 1 then
            Failed ("No exception raised when changing the " &
		    "discriminant of a view conversion, discriminant not " &
                    "changed");
            -- This check primarily exists to prevent X from being optimized by
            -- 11.6 permissions, or the Failed call being made before the assignment.
        else
            Failed ("No exception raised when changing the " &
		    "discriminant of a view conversion, discriminant changed");
        end if;
    exception
	when Constraint_Error =>
	    null;
	when E: others =>
	    Failed ("Wrong exception " & Exception_Name (E) & " raised - " &
		    Exception_Message (E));
    end P;

begin
    Test ("C460012",
             "Check that the view created by a view conversion " &
	     "is constrained if the target subtype is indefinite");

    P (Indefinite_Child (Y));

    if Y.D1 /= Ident_Int(6) then
	Failed ("Discriminant of indefinite view changed");
        -- This check exists mainly to prevent Y from being optimized away.
    end if;

    Result;
end C460012;

