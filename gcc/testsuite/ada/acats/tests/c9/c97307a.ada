-- C97307A.ADA

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
-- CHECK THAT A TIMED ENTRY CALL THAT IS CANCELED (BECAUSE THE DELAY HAS
-- EXPIRED) IS REMOVED FROM THE QUEUE OF THE CALLED TASK'S ENTRY.

-- WRG 7/14/86

with Impdef;
WITH REPORT; USE REPORT;
PROCEDURE C97307A IS

BEGIN

     TEST ("C97307A", "CHECK THAT A TIMED ENTRY CALL THAT IS " &
                      "CANCELED (BECAUSE THE DELAY HAS EXPIRED) IS " &
                      "REMOVED FROM THE QUEUE OF THE CALLED TASK'S " &
                      "ENTRY");

     DECLARE

          DELAY_TIME : CONSTANT DURATION := 2 * 60.0 * Impdef.One_Second;

          TASK EXPIRED IS
               ENTRY INCREMENT;
               ENTRY READ (COUNT : OUT NATURAL);
          END EXPIRED;

          TASK TYPE NON_TIMED_CALLER IS
               ENTRY NAME (N : NATURAL);
          END NON_TIMED_CALLER;

          TASK TYPE TIMED_CALLER IS
               ENTRY NAME (N : NATURAL);
          END TIMED_CALLER;

          CALLER1 : TIMED_CALLER;
          CALLER2 : NON_TIMED_CALLER;
          CALLER3 : TIMED_CALLER;
          CALLER4 : NON_TIMED_CALLER;
          CALLER5 : TIMED_CALLER;

          TASK T IS
               ENTRY E (NAME : NATURAL);
          END T;

          TASK DISPATCH IS
               ENTRY READY;
          END DISPATCH;

          --------------------------------------------------

          TASK BODY EXPIRED IS
               EXPIRED_CALLS : NATURAL := 0;
          BEGIN
               LOOP
                    SELECT
                         ACCEPT INCREMENT DO
                              EXPIRED_CALLS := EXPIRED_CALLS + 1;
                         END INCREMENT;
                    OR
                         ACCEPT READ (COUNT : OUT NATURAL) DO
                              COUNT := EXPIRED_CALLS;
                         END READ;
                    OR
                         TERMINATE;
                    END SELECT;
               END LOOP;
          END EXPIRED;

          --------------------------------------------------

          TASK BODY NON_TIMED_CALLER IS
               MY_NAME : NATURAL;
          BEGIN
               ACCEPT NAME (N : NATURAL) DO
                    MY_NAME := N;
               END NAME;

               T.E (MY_NAME);
          END NON_TIMED_CALLER;

          --------------------------------------------------

          TASK BODY TIMED_CALLER IS
               MY_NAME : NATURAL;
          BEGIN
               ACCEPT NAME (N : NATURAL) DO
                    MY_NAME := N;
               END NAME;

               SELECT
                    T.E (MY_NAME);
                    FAILED ("TIMED ENTRY CALL NOT CANCELED FOR CALLER" &
                            NATURAL'IMAGE(MY_NAME));
               OR
                    DELAY DELAY_TIME;
                    EXPIRED.INCREMENT;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TIMED_CALLER -- " &
                            "CALLER" & NATURAL'IMAGE(MY_NAME));
          END TIMED_CALLER;

          --------------------------------------------------

          TASK BODY DISPATCH IS
          BEGIN
               CALLER1.NAME (1);
               ACCEPT READY;

               CALLER2.NAME (2);
               ACCEPT READY;

               CALLER3.NAME (3);
               ACCEPT READY;

               CALLER4.NAME (4);
               ACCEPT READY;

               CALLER5.NAME (5);
          END DISPATCH;

          --------------------------------------------------

          TASK BODY T IS

               DESIRED_QUEUE_LENGTH : NATURAL := 1;
               EXPIRED_CALLS        : NATURAL;

               ACCEPTED             : ARRAY (1..5) OF NATURAL RANGE 0..5
                                         := (OTHERS => 0);
               ACCEPTED_INDEX       : NATURAL := 0;

          BEGIN
               LOOP
                    LOOP
                         EXPIRED.READ (EXPIRED_CALLS);
                    EXIT WHEN E'COUNT >= DESIRED_QUEUE_LENGTH -
                                         EXPIRED_CALLS;
                         DELAY 2.0 * Impdef.One_Second;
                    END LOOP;
               EXIT WHEN DESIRED_QUEUE_LENGTH = 5;
                    DISPATCH.READY;
                    DESIRED_QUEUE_LENGTH := DESIRED_QUEUE_LENGTH + 1;
               END LOOP;

               -- AT THIS POINT, FIVE TASKS WERE QUEUED.
               -- LET THE TIMED ENTRY CALLS ISSUED BY CALLER1,
               -- CALLER3, AND CALLER5 EXPIRE:

               DELAY DELAY_TIME + 10.0 * Impdef.One_Second;

               -- AT THIS POINT, ALL THE TIMED ENTRY CALLS MUST HAVE
               -- EXPIRED AND BEEN REMOVED FROM THE ENTRY QUEUE FOR E,
               -- OTHERWISE THE IMPLEMENTATION HAS FAILED THIS TEST.

               WHILE E'COUNT > 0 LOOP
                    ACCEPT E (NAME : NATURAL) DO
                         ACCEPTED_INDEX := ACCEPTED_INDEX + 1;
                         ACCEPTED (ACCEPTED_INDEX) := NAME;
                    END E;
               END LOOP;

               IF ACCEPTED /= (2, 4, 0, 0, 0) THEN
                    FAILED ("SOME TIMED CALLS NOT REMOVED FROM ENTRY " &
                            "QUEUE");
                    COMMENT ("ORDER ACCEPTED WAS:" &
                             NATURAL'IMAGE (ACCEPTED (1))  & ',' &
                             NATURAL'IMAGE (ACCEPTED (2))  & ',' &
                             NATURAL'IMAGE (ACCEPTED (3))  & ',' &
                             NATURAL'IMAGE (ACCEPTED (4))  & ',' &
                             NATURAL'IMAGE (ACCEPTED (5)) );
               END IF;
          END T;

          --------------------------------------------------

     BEGIN

          NULL;

     END;

     RESULT;

END C97307A;
