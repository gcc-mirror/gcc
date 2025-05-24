-- C97301C.ADA

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
-- CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT LEAST THE SPECIFIED 
-- AMOUNT OF TIME IF A RENDEVOUS IS NOT POSSIBLE.

-- CASE  C:  AN ACCEPT STATEMENT FOR THE CALLED ENTRY HAS NOT BEEN
--           REACHED.

-- RJW 3/31/86

with Impdef;
WITH REPORT; USE REPORT;
WITH CALENDAR; USE CALENDAR;
PROCEDURE C97301C IS
     
     OR_BRANCH_TAKEN : BOOLEAN := FALSE;
     
BEGIN

     TEST ("C97301C", "CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT " &
                      "LEAST THE SPECIFIED AMOUNT OF TIME WHEN AN " &
                      "ACCEPT STATEMENT FOR THE CALLED ENTRY HAS " &
                      "NOT BEEN REACHED" );


     DECLARE
          START_TIME : TIME;
          STOP_TIME : TIME;
          WAIT_TIME  : DURATION :=  3.0 * Impdef.One_Second;

          TASK  T  IS
               ENTRY NO_SPIN;
               ENTRY DO_IT_NOW_OR_WAIT;
          END  T;

          TASK BODY  T  IS
          BEGIN
               ACCEPT NO_SPIN;
               ACCEPT DO_IT_NOW_OR_WAIT;
          END  T;

     BEGIN
          START_TIME := CLOCK;
          SELECT
               T.DO_IT_NOW_OR_WAIT;
               FAILED("RENDEZVOUS OCCURRED");
               ABORT T;
          OR
                                      -- THIS BRANCH MUST BE CHOSEN.
               DELAY WAIT_TIME;
               STOP_TIME := CLOCK;
               IF STOP_TIME >= (WAIT_TIME + START_TIME) THEN
                    NULL;
               ELSE
                    FAILED ( "INSUFFICIENT DELAY" );
               END IF;
               T.NO_SPIN;
               OR_BRANCH_TAKEN := TRUE;
               COMMENT( "OR_BRANCH TAKEN" );
               T.DO_IT_NOW_OR_WAIT;
          END SELECT;
     EXCEPTION
          WHEN TASKING_ERROR =>
               FAILED ( "TASKING ERROR" );
     END;
                                     -- END OF BLOCK CONTAINING TIMED 
                                     -- ENTRY CALL.

     -- BY NOW, TASK T IS TERMINATED (AND THE NONLOCALS UPDATED).
          
     IF OR_BRANCH_TAKEN THEN
          NULL;
     ELSE
          FAILED( "RENDEZVOUS ATTEMPTED" );
     END IF;

     RESULT;

END C97301C; 
