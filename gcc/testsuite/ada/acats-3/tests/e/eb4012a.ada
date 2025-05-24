-- EB4012A.ADA

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
--     CHECK THAT WHEN AN UNHANDLED EXCEPTION IS RAISED IN THE MAIN
--     PROGRAM, THE MAIN PROGRAM IS ABANDONED.

-- PASS/FAIL CRITERIA:
--     THIS TEST MUST EXECUTE AND PRINT "TENTATIVELY PASSED". IN
--     ADDITION, THE OUTPUT/LOG FILE MUST SHOW THAT THE PROGRAM
--     WAS ABANDONED DUE TO AN UNHANDLED EXCEPTION.

-- HISTORY:
--     DHH 03/29/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE EB4012A IS

BEGIN
     TEST("EB4012A", "CHECK THAT WHEN AN UNHANDLED EXCEPTION IS " &
                     "RAISED IN THE MAIN PROGRAM, THE MAIN PROGRAM " &
                     "IS ABANDONED");
     SPECIAL_ACTION("CHECK THE OUTPUT/LOG FILE TO SEE THAT THIS " &
                    "PROGRAM WAS ABANDONED BECAUSE OF AN UNHANDLED " &
                    "EXCEPTION");

     RESULT;

     IF EQUAL(3,3) THEN
          RAISE CONSTRAINT_ERROR;
     END IF;

     TEST("EB4012A", "SHOULD NOT PRINT OUT");
     FAILED("CONSTRAINT_ERROR NOT RAISED");

     RESULT;

END EB4012A;
