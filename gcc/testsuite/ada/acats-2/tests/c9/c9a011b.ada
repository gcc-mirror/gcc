-- C9A011B.ADA

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
--     CHECK THAT "TASKING_ERROR" IS RAISED BY A TIMED ENTRY CALL IF
--     THE CALLED TASK IS ABORTED BEFORE THE DELAY EXPIRES BUT NOT
--     WHEN THE CALL IS FIRST EXECUTED.

-- HISTORY:
--     DHH 06/14/88 CREATED ORIGINAL TEST.

with Impdef;
WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE C9A011B IS

     TASK TIMED_ENTRY IS
          ENTRY WAIT_AROUND;
     END TIMED_ENTRY;

     TASK OWNER IS
          ENTRY START;
          ENTRY SELF_ABORT;
     END OWNER;

     TASK BODY TIMED_ENTRY IS
     BEGIN
          SELECT
               OWNER.SELF_ABORT;
          OR
               DELAY 60.0 * Impdef.One_Second;
          END SELECT;
          FAILED("NO EXCEPTION RAISED");

          ACCEPT WAIT_AROUND;
     EXCEPTION
          WHEN TASKING_ERROR =>
               ACCEPT WAIT_AROUND;
          WHEN OTHERS =>
               FAILED("WRONG EXCEPTION RAISED");
               ACCEPT WAIT_AROUND;
     END TIMED_ENTRY;

     TASK BODY OWNER IS
     BEGIN
          ACCEPT START DO
               WHILE SELF_ABORT'COUNT = 0 LOOP
                    DELAY 1.0 * Impdef.One_Second;
               END LOOP;
          END START;

          ABORT OWNER;

          ACCEPT SELF_ABORT;

     END OWNER;

BEGIN

     TEST("C9A011B", "CHECK THAT ""TASKING_ERROR"" IS RAISED BY A " &
                     "TIMED ENTRY CALL IF THE CALLED TASK IS " &
                     "ABORTED BEFORE THE DELAY EXPIRES BUT NOT " &
                     "WHEN THE CALL IS FIRST EXECUTED");

     OWNER.START;
     DELAY 5.0 * Impdef.One_Second;

     IF TIMED_ENTRY'CALLABLE THEN
          TIMED_ENTRY.WAIT_AROUND;
     ELSE
          FAILED("TASK ABORTED WHEN TASKING ERROR IS RAISED");
     END IF;

     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED("EXCEPTION RAISED OUTSIDE OF TASK");
          RESULT;

END C9A011B;
