-- CA3011A4M.ADA

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
-- OBJECTIVE:
--     CHECK THAT AN IMPLEMENTATION DOES NOT REQUIRE GENERIC UNIT BODIES AND
--     SUBUNITS TO BE COMPILED TOGETHER IN THE SAME FILE.

-- SEPARATE FILES ARE:
--     CA3011A0 - A GENERIC UNIT.
--     CA3011A1, CA3011A2, CA3011A3 - SUBUNITS OF GENERIC UNIT.
--     CA3011A4M - THE MAIN PROCEDURE.

-- APPLICABILITY CRITERIA:
--     THIS TEST MUST RUN AND REPORT "PASSED" FOR ALL ADA 95 IMPLEMENTATIONS.
--     THIS WAS NOT REQUIRED FOR ADA 83.

-- HISTORY:
--     RJW 09/22/86  CREATED ORIGINAL TEST.
--     BCB 01/05/88  MODIFIED HEADER.
--     RLB 09/13/99  UPDATED APPLICABILITY CRITERIA FOR ADA 95.
--     RLB 09/15/99  REPAIRED OBJECTIVE FOR ADA 95.

WITH REPORT; USE REPORT;
WITH CA3011A0;
PROCEDURE CA3011A4M IS
     I : INTEGER;
     PROCEDURE P IS NEW CA3011A0 (INTEGER, 22);

BEGIN
     TEST ( "CA3011A", "CHECK THAT AN IMPLEMENTATION DOES NOT REQUIRE " &
                       "GENERIC UNIT BODIES AND SUBUNITS TO BE " &
                       "COMPILED TOGETHER IN THE SAME FILE" );

     P (I);
     IF I /= 22 THEN
          FAILED ( "INCORRECT INSTANTIATION" );
     END IF;

     RESULT;
END CA3011A4M;
