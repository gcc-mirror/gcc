-- CDD1001.A
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
--    Check that components of Stream_Element_Array are aliased.  (Defect
--    Report 8652/0044).
--
-- APPLICABILITY CRITERIA:
--    All implementations must attempt to compile this test.
--
--    For implementations for which Stream_Element'Size is a multiple of
--    System.Storage_Unit, this test must execute.
--
--    For other implementations, if this test compiles without error messages
--    at compilation, it must bind and execute.
--
-- PASS/FAIL CRITERIA:
--    For implementations for which Stream_Element'Size is a multiple of
--      System.Storage_Unit, this test must execute, report PASSED, and
--      complete normally, otherwise the test FAILS.
--
--    For other implementations:
--      PASSING behavior is:
--        this test executes, reports PASSED, and completes normally
--      or
--        this test produces at least one error message at compilation, and
--        the error message is associated with one of the items marked:
--           -- N/A => ERROR.
--
--      All other behaviors are FAILING.
--
--
-- CHANGE HISTORY:
--    12 FEB 2001   PHL   Initial version
--    15 MAR 2001   RLB   Readied for release.

--!
with Ada.Streams;
use Ada.Streams;
with Report;
use Report;
procedure CDD1001 is

    type Acc is access all Stream_Element;

    A : Stream_Element_Array
	   (Stream_Element_Offset (Ident_Int (1)) ..
	       Stream_Element_Offset (Ident_Int (10)));
    B : array (A'Range) of Acc;
begin
    Test ("CDD1001",
	  "Check that components of Stream_Element_Array are aliased");

    for I in A'Range loop
	A (I) := Stream_Element (Ident_Int (Integer (I)) * Ident_Int (3));
    end loop;

    for I in B'Range loop
	B (I) := A (I)'Access;                                -- N/A => ERROR.
    end loop;

    for I in B'Range loop
	if B (I).all /= Stream_Element
			   (Ident_Int (Integer (I)) * Ident_Int (3)) then
	    Failed ("Unable to build access values desginating elements " &
		    "of a Stream_Element_Array");
	end if;
    end loop;

    Result;
end CDD1001;

