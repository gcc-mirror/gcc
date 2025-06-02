-- C9A009G.ADA

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
-- CHECK THAT A MASTER ABORTED WITH SUBTASKS IN AN ENTRY CALL BECOMES
-- COMPLETED, BUT NOT TERMINATED, BEFORE THE END OF THE RENDEZVOUS.

-- JEAN-PIERRE ROSEN 16-MAR-1984
-- JBG 6/1/84
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

WITH REPORT,SYSTEM; 
USE REPORT,SYSTEM;
PROCEDURE C9A009G IS


     TASK BLOCKING IS
          ENTRY START;
          ENTRY STOP;
          ENTRY RESTART;
          ENTRY NO_CALL;
     END BLOCKING;

     TASK BODY BLOCKING IS
     BEGIN
          SELECT
               ACCEPT STOP DO
                    ACCEPT START;
                    ACCEPT RESTART;
               END;
          OR TERMINATE;
          END SELECT;
     END;

BEGIN

     TEST("C9A009G", "MASTER COMPLETED BUT NOT TERMINATED");

     DECLARE         -- T1 ABORTED WHILE DEPENDENT TASK IN RENDEVOUS 9C?

          TASK T1 IS
               ENTRY LOCK;
          END T1;                              
     
          TASK BODY T1 IS
               TASK T2;
     
               TASK BODY T2 IS
               BEGIN
                    BLOCKING.STOP;
                    FAILED ("T2 NOT ABORTED");
               END;
          BEGIN
               BLOCKING.NO_CALL;          -- WILL DEADLOCK UNTIL ABORT
          END T1;
     
     BEGIN
          BLOCKING.START;
          ABORT T1;

          IF T1'CALLABLE THEN
               FAILED("T1 STILL CALLABLE - 2");
          END IF;

          IF T1'TERMINATED THEN    -- T1'S DEPENDENT TASK, T2, STILL IN
                                   -- RENDEVOUS 
               FAILED("T1 PREMATURELY TERMINATED - 2");
          END IF;

          BLOCKING.RESTART;
     END;

     RESULT;

END C9A009G;
