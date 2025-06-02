-- CA140231.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
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
--      See CA140232.AM.
--
-- TEST DESCRIPTION:
--      See CA140232.AM.
--
-- TEST FILES:
--      This test consists of the following files:
--         CA140230.A
--      -> CA140231.A
--         CA140232.AM
--         CA140233.A
--
-- PASS/FAIL CRITERIA:
--      See CA140232.AM.
--
-- CHANGE HISTORY:
--     07 DEC 96   SAIC        ACVC 2.1: Initial version.
--     13 SEP 99   RLB         Changed to C-test (by AI-00077).
--     20 MAR 00   RLB         Removed special requirements, because there
--                             aren't any.
--
--!

function CA14023_1 (P1, P2 : Data_type) return Data_type is
begin
     if Floor > P1 and Floor > P2 then
          return Floor;
     elsif P2 > P1 then
          return P2;
     else
          return P1;
     end if;
end CA14023_1;
