-- C9A009C.ADA

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

-- THE CALLING TASK IN THE RENDEVOUS IS DEPENDENT ON THE ABORTED TASK,
-- SO THE DEPENDENT TASK IS INDIRECTLY ABORTED WHILE IN A RENDEVOUS;
-- NEITHER THE CALLING TASK NOR ITS MASTER CAN BE TERMINATED WHILE THE
-- RENDEVOUS CONTINUES.

-- JEAN-PIERRE ROSEN 09 MARCH 1984
-- JBG 6/1/84

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE C9A009C IS

BEGIN 

     TEST("C9A009C", "DEPENDENT TASK IN RENDEVOUS WHEN MASTER IS " &
                     "ABORTED");

     DECLARE
     -- T2 CONTAINS DEPENDENT TASK T3 WHICH CALLS T1.
     -- T1 ABORTS T2 WHILE IN RENDEVOUS WITH T3.

          TASK T1 IS
               ENTRY E1;
          END T1;

          TASK BODY T1 IS
      
               TASK T2;
     
               TASK BODY T2 IS
                    TASK T3;
                    TASK BODY T3 IS
                    BEGIN
                         T1.E1;
                         FAILED ("T3 NOT ABORTED");
                    EXCEPTION
                         WHEN TASKING_ERROR =>
                              FAILED ("TASKING_ERROR IN T3");
                         WHEN OTHERS =>
                              FAILED ("OTHER EXCEPTION IN T3");
                    END;
               BEGIN     -- T3 ACTIVATED NOW
                    NULL;
               END T2;
     
          BEGIN     -- T1
               ACCEPT E1 DO
                    ABORT T2;
                    ABORT T2;
                    ABORT T2; -- WHY NOT?
                    IF T2'TERMINATED THEN
                         FAILED ("T2 TERMINATED PREMATURELY");
                    END IF;
               END E1;
          EXCEPTION
               WHEN TASKING_ERROR =>
                    FAILED ("TASKING_ERROR IN T1 BECAUSE CALLING TASK "&
                            "WAS ABORTED");
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION - T1");
          END T1;

     BEGIN
          NULL;
     END;

     RESULT;

END C9A009C;
