-- C95021A.ADA

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
-- CHECK THAT CALLS TO AN ENTRY ARE PLACED IN A FIFO QUEUE.

-- JBG 2/22/84
-- DAS 10/8/90  ADDED PRAGMA PRIORITY TO ENSURE THAT THE FIFO
--              DISCIPLINE MUST BE FOLLOWED (OTHERWISE THE
--              IMPLEMENTATION MIGHT PROHIBIT QUEUES FROM
--              FORMING SO THAT E'COUNT IS ALWAYS ZERO FOR
--              AN ENTRY E).
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

-- THE TASK QUEUE IS THE TASK THAT CHECKS THE QUEUEING DISCIPLINE.
--
-- THIS TEST PLACES TWO CALLS ON AN ENTRY, WAITS UNTIL ONE OF THE CALLS
-- IS ACCEPTED, AND THEN PLACES A THIRD CALL ON THE ENTRY.  THE TEST
-- CHECKS THAT THE SECOND CALL IS HANDLED BEFORE THE THIRD.  (IT IS
-- NONDETERMINISTIC WHICH CALL WILL BE THE FIRST ONE ON THE QUEUE, SO
-- THIS MORE COMPLICATED APPROACH IS NECESSARY.)
--
-- THE TASK DISPATCH FIRES UP THE TWO TASKS THAT MAKE THE FIRST TWO
-- CALLS AND THEN WAITS UNTIL QUEUE SAYS IT IS READY FOR THE THIRD CALL.
--
-- THE TASK TYPE CALLERS IS USED TO CREATE TASKS THAT WILL CALL THE
-- ENTRY IN THE TASK QUEUE.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM;
PROCEDURE C95021A IS
BEGIN

     TEST ("C95021A", "CHECK THAT ENTRY CALLS ARE PUT IN FIFO QUEUES");

-- DO THIS TEST 3 TIMES TO ALLOW FOR RANDOM VARIATIONS IN TIMING.
     FOR I IN 1..3 LOOP  
          COMMENT ("ITERATION" & INTEGER'IMAGE(I));

     DECLARE

          TASK TYPE CALLERS IS
               ENTRY NAME (N : NATURAL);
          END CALLERS;

          TASK QUEUE IS
               ENTRY GO;
               ENTRY E1 (NAME : NATURAL);
          END QUEUE;

          TASK DISPATCH IS
               ENTRY READY;
          END DISPATCH;

          TASK BODY CALLERS IS
               MY_NAME : NATURAL;
          BEGIN

-- GET NAME OF THIS TASK OBJECT
               ACCEPT NAME (N : NATURAL) DO
                    MY_NAME := N;
               END NAME;

-- PUT THIS TASK ON QUEUE FOR QUEUE.E1
               QUEUE.E1 (MY_NAME);
          END CALLERS;

          TASK BODY DISPATCH IS
               TYPE ACC_CALLERS IS ACCESS CALLERS;
               OBJ : ACC_CALLERS;
          BEGIN

-- FIRE UP TWO CALLERS FOR QUEUE.E1
               OBJ := NEW CALLERS;
               OBJ.NAME(1);
               OBJ := NEW CALLERS;
               OBJ.NAME(2);

-- ALLOW THESE CALLS TO BE PROCESSED (ONLY ONE WILL BE ACCEPTED).
               QUEUE.GO;

-- WAIT TILL ONE CALL HAS BEEN PROCESSED.
               ACCEPT READY;       -- CALLED FROM QUEUE

-- FIRE UP THIRD CALLER
               OBJ := NEW CALLERS;
               OBJ.NAME(3);

          END DISPATCH;

          TASK BODY QUEUE IS
               NEXT : NATURAL;     -- NUMBER OF SECOND CALLER IN QUEUE.
          BEGIN

-- WAIT UNTIL TWO TASKS CALLING E1 HAVE BEEN ACTIVATED.
               ACCEPT GO;

-- WAIT FOR TWO CALLS TO BE AVAILABLE.  THIS WAIT ASSUMES THAT THE
-- CALLER TASKS WILL PROCEED IF THIS TASK IS EXECUTING A DELAY
-- STATEMENT, ALTHOUGH THIS IS NOT STRICTLY REQUIRED BY THE STANDARD.
               FOR I IN 1..6       -- WILL WAIT FOR ONE MINUTE
               LOOP
                    EXIT WHEN E1'COUNT = 2;
                    DELAY 10.0 * Impdef.One_Second;    -- WAIT FOR CALLS TO ARRIVE
               END LOOP;

               IF E1'COUNT /= 2 THEN
                    FAILED ("CALLER TASKS NOT QUEUED AFTER ONE " &
                            "MINUTE - 1");
               END IF;

-- ASSUMING NO FAILURE, PROCESS ONE OF THE QUEUED CALLS.
               ACCEPT E1 (NAME : NATURAL) DO

-- GET NAME OF NEXT CALLER
                    CASE NAME IS
                         WHEN 1 => 
                              NEXT := 2;
                         WHEN 2 => 
                              NEXT := 1;
                         WHEN OTHERS => 
                              FAILED ("UNEXPECTED ERROR");
                    END CASE;
               END E1;

-- TELL DISPATCH TO FIRE UP NEXT CALLER (ONE IS STILL IN QUEUE).
               DISPATCH.READY;

-- WAIT FOR CALL TO ARRIVE.
               FOR I IN 1..6       -- WILL WAIT FOR ONE MINUTE
               LOOP
                    EXIT WHEN E1'COUNT = 2;
                    DELAY 10.0 * Impdef.One_Second;    -- WAIT FOR CALLS TO ARRIVE
               END LOOP;

               IF E1'COUNT /= 2 THEN
                    FAILED ("CALLER TASKS NOT QUEUED AFTER ONE " &
                            "MINUTE - 2");
               END IF;

-- ASSUMING NO FAILURE, ACCEPT SECOND CALL AND CHECK THAT IT IS FROM THE
-- CORRECT TASK.
               ACCEPT E1 (NAME : NATURAL) DO
                    IF NAME /= NEXT THEN
                         FAILED ("FIFO DISCIPLINE NOT OBEYED");
                    END IF;
               END E1;

-- ACCEPT THE LAST CALLER
               ACCEPT E1 (NAME : NATURAL);

          END QUEUE;

     BEGIN
          NULL;
     END;           -- ALL TASKS NOW TERMINATED.
     END LOOP;

     RESULT;

END C95021A;
