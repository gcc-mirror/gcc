-- CA200022.AM
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
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      See CA200020.A.
--
-- TEST DESCRIPTION:
--      See CA200020.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         CA200020.A
--         CA200021.A
--      -> CA200022.AM
--
-- PASS/FAIL CRITERIA:
--      See CA200020.A.
--
-- CHANGE HISTORY:
--     25 JAN 99   RLB       Initial version.
--     08 JUL 99   RLB       Repaired comments.
--     20 MAR 00   RLB       Removed special requirements, because there
--                           aren't any.
--!

with Report;
use Report;
with CA20002_0; -- Child unit not included in the partition.
procedure CA200022 is
     Value : Integer := 0;
begin
     Test ("CA20002","Check that compiling multiple units with the same " &
                     "name does not prevent the creation of a partition " &
                     "using only one of the units.");
     CA20002_0.Do_a_Little (Value);
     if Report.Equal (Value, 5) then
        null; -- OK.
     else
        Failed ("Wrong result from subunit");
     end if;

     Result;
end CA200022;
