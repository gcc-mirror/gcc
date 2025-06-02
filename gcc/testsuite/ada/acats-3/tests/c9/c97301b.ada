-- C97301B.ADA

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
--     CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT LEAST THE SPECIFIED
--     AMOUNT OF TIME IF A RENDEZVOUS IS NOT POSSIBLE.

--     CASE  B:  THE QUEUE FOR THE CALLED ENTRY ALREADY CONTAINS
--           ANOTHER  TASK WHOSE RENDEZVOUS CANNOT BE COMPLETED WITHIN
--           THE SPECIFIED DELAY.

--HISTORY:
--     RJW 03/31/86 CREATED ORIGINAL TEST.
--     DHH 10/20/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Impdef;
WITH REPORT; USE REPORT;
WITH CALENDAR; USE CALENDAR;
PROCEDURE C97301B IS

     OR_BRANCH_TAKEN : BOOLEAN := FALSE;

BEGIN

     TEST ("C97301B", "CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT " &
                      "LEAST THE SPECIFIED AMOUNT OF TIME WHEN THE " &
                      "QUEUE FOR THE CALLED ENTRY ALREADY CONTAINS " &
                      "ANOTHER TASK WHOSE RENDEZVOUS CANNOT BE " &
                      "COMPLETED WITHIN THE SPECIFIED DELAY" );


     DECLARE
          WAIT_TIME  : DURATION :=  3.0 * Impdef.One_Second;

          TASK T1;

          TASK T2 IS
               ENTRY AWAKEN_T2;
          END T2;

          TASK T3 IS
               ENTRY AWAKEN_T3;
               ENTRY RELEASE_T;
          END T3;

          TASK  T  IS
               ENTRY  DO_IT_NOW_OR_WAIT (X : INTEGER);
          END  T;

          TASK BODY  T  IS
          BEGIN
               ACCEPT DO_IT_NOW_OR_WAIT (X : INTEGER) DO
                    IF X = 1 THEN
                         T2.AWAKEN_T2;
                         WHILE DO_IT_NOW_OR_WAIT'COUNT = 0 LOOP
                              DELAY 1.0 * Impdef.One_Second;
                         END LOOP;
                         T3.AWAKEN_T3;
                         T3.RELEASE_T;
                    ELSE
                         FAILED ("WRONG TASK IN RENDEZVOUS - 1");
                    END IF;
               END DO_IT_NOW_OR_WAIT;
               ACCEPT DO_IT_NOW_OR_WAIT (X : INTEGER) DO
                    IF X /= 2 THEN
                         FAILED ("WRONG TASK IN RENDEZVOUS - 2");
                    END IF;
               END DO_IT_NOW_OR_WAIT;
          END  T;

          TASK BODY T1 IS
          BEGIN
               T.DO_IT_NOW_OR_WAIT (1);
          END T1;

          TASK BODY T2 IS
          BEGIN
               ACCEPT AWAKEN_T2;
               T.DO_IT_NOW_OR_WAIT (2);
          END T2;

          TASK BODY T3 IS
               START_TIME : TIME;
               STOP_TIME : TIME;
          BEGIN
               BEGIN
                    ACCEPT AWAKEN_T3;
                    START_TIME := CLOCK;
                    SELECT
                         T.DO_IT_NOW_OR_WAIT (3);
                    OR
                                         -- THIS BRANCH MUST BE CHOSEN.
                         DELAY WAIT_TIME;
                         STOP_TIME := CLOCK;
                         IF STOP_TIME >= (WAIT_TIME + START_TIME) THEN
                              NULL;
                         ELSE
                              FAILED ( "INSUFFICIENT DELAY" );
                         END IF;
                         OR_BRANCH_TAKEN := TRUE;
                         COMMENT( "OR_BRANCH TAKEN" );
                         ACCEPT RELEASE_T;
                    END SELECT;
               EXCEPTION
                    WHEN TASKING_ERROR =>
                         FAILED ( "TASKING ERROR" );
               END;
                             -- END OF BLOCK CONTAINING TIMED
                             -- ENTRY CALL.

                             -- BY NOW, THE TASK T IS EFFECTIVELY
                             -- TERMINATED (AND THE NONLOCALS UPDATED).

               IF OR_BRANCH_TAKEN THEN
                    NULL;
               ELSE
                    FAILED( "RENDEZVOUS ATTEMPTED" );
               END IF;
          END T3;
     BEGIN
          NULL;
     END;

     RESULT;

END C97301B;
