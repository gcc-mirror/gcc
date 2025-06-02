-- CB4013A.ADA

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
--     CHECK THAT AN UNHANDLED EXCEPTION RAISED IN A TASK BODY, BUT
--     OUTSIDE AN ACCEPT STATEMENT, RAISES NO EXCEPTION OUTSIDE THE
--     TASK.

-- HISTORY:
--     DHH 03/29/88 CREATED ORIGINAL TEST.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE CB4013A IS

     TASK TYPE CHOICE IS
          ENTRY E1;
          ENTRY STOP;
     END CHOICE;

     T : CHOICE;

     TASK BODY CHOICE IS
     BEGIN
          ACCEPT E1;
          IF EQUAL(3,3) THEN
               RAISE CONSTRAINT_ERROR;
          END IF;
          ACCEPT STOP;
     END CHOICE;

BEGIN

     TEST("CB4013A", "CHECK THAT AN UNHANDLED EXCEPTION RAISED IN " &
                     "A TASK BODY, BUT OUTSIDE AN ACCEPT STATEMENT, " &
                     "RAISES NO EXCEPTION OUTSIDE THE TASK");

     T.E1;
     DELAY 1.0;
     IF T'CALLABLE THEN
          FAILED("TASK NOT COMPLETED ON RAISING CONSTRAINT_ERROR");
          T.STOP;
     END IF;

     RESULT;

EXCEPTION
     WHEN TASKING_ERROR =>
          FAILED("TASKING_ERROR RAISED OUTSIDE TASK");
          RESULT;

     WHEN CONSTRAINT_ERROR =>
          FAILED("CONSTRAINT_ERROR PROPAGATED OUTSIDE TASK");
          RESULT;

     WHEN OTHERS =>
          FAILED("UNEXPECTED EXCEPTION RAISED");
          RESULT;
END CB4013A;
