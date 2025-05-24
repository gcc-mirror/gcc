-- C9A010A.ADA

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
-- TEST ABORT DURING RENDEZVOUS

-- ABORTING AN ABNORMAL (NOT YET TERMINATED) TASK.

-- JEAN-PIERRE ROSEN 09 MARCH 1984
-- JBG 6/1/84
-- JWC 6/28/85   RENAMED FROM C9A009E-B.ADA
-- PWN 01/31/95  REMOVED PRAGMA PRIORITY FOR ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE C9A010A IS

BEGIN

     TEST("C9A010A", "ABORTING AN ABNORMAL TASK");

     DECLARE
     -- T1 CALLS T2.  WHILE IN RENDEVOUS, T2 ABORTS T1 AND WAITS FOR A
     -- CALL FROM THE MAIN PROGRAM.  WHEN THE CALL IS ACCEPTED, THE MAIN
     -- PROGRAM AGAIN ABORTS T1, WHICH IS NOW ABNORMAL, SINCE T1 HAS NOT
     -- YET COMPLETED ITS RENDEVOUS WITH T2.

          TASK T1 IS
          END T1;

          TASK T2 IS
               ENTRY E1;
               ENTRY E2;
          END T2;

          TASK BODY T1 IS
          BEGIN
               T2.E1;
               FAILED("T1 NOT ABORTED");
          EXCEPTION
               WHEN TASKING_ERROR =>
                    FAILED ("TASKING_ERROR IN T1");
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION IN T1");
          END T1;

          TASK BODY T2 IS
          BEGIN
               ACCEPT E1 DO
                    ABORT  T1;
                    ACCEPT E2;     -- NOTE CALLER REMAINS IN RENDEVOUS
                    ACCEPT E2;     -- UNTIL TWO ENTRY CALLS ACCEPTED
               END E1;
          END T2;
     BEGIN
          T2.E2;    -- ONLY ACCEPTED AFTER T1 HAS BEEN ABORTED.
          ABORT T1; -- T1 IS ABNORMAL BECAUSE IT IS STILL IN RENDEVOUS.
          IF T1'CALLABLE THEN
               FAILED ("T1 CALLABLE AFTER BEING ABORTED");
          END IF;
          IF T1'TERMINATED THEN
               FAILED ("T1 TERMINATED ALTHOUGH IN RENDEVOUS");
          END IF;
          T2.E2; -- T1'S RENDEVOUS CAN NOW COMPLETE; T1 CAN TERMINATE.
     END;

     RESULT;

END C9A010A;
