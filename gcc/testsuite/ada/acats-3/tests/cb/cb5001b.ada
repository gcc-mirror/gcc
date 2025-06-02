-- CB5001B.ADA

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
-- CHECK THAT AN EXCEPTION RAISED IN A RENDEVOUS IS PROPAGATED BOTH TO
-- THE CALLER AND TO THE CALLED TASK.

-- THIS VERSION CHECKS THAT THE EXCEPTION IS PROPAGATED THROUGH TWO
-- LEVELS OF RENDEVOUS.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- JEAN-PIERRE ROSEN 09 MARCH 1984
-- JBG 6/1/84
-- MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE CB5001B IS

BEGIN

     TEST("CB5001B", "CHECK THAT AN EXCEPTION IN A RENDEVOUS IS " &
                     "PROPAGATED TO CALLER AND CALLED TASKS -- TWO " &
                     "LEVELS");

     DECLARE
          TASK T1 IS
               ENTRY E1;
          END T1;
     
          TASK T2 IS
               ENTRY E2;
          END T2;
     
          TASK BODY T1 IS
          BEGIN
               ACCEPT E1 DO 
                    T2.E2; 
               END E1;
               FAILED ("T1: EXCEPTION NOT RAISED");
          EXCEPTION
               WHEN CONSTRAINT_ERROR | PROGRAM_ERROR =>
                    FAILED ("PREDEFINED EXCEPTION RAISED IN T1");
               WHEN TASKING_ERROR =>
                    FAILED ("TASKING_ERROR RAISED IN T1");
               WHEN OTHERS => 
                    NULL;
          END T1;
     
          TASK BODY T2 IS
               MY_EXCEPTION: EXCEPTION;
          BEGIN
               ACCEPT E2 DO 
                    IF EQUAL (1,1) THEN
                         RAISE MY_EXCEPTION; 
                    END IF;
               END E2;
               FAILED ("T2: EXCEPTION NOT RAISED");
          EXCEPTION
               WHEN MY_EXCEPTION => 
                    NULL;
               WHEN TASKING_ERROR =>
                    FAILED ("TASKING_ERROR RAISED IN T2");
               WHEN OTHERS =>
                    FAILED ("T2 RECEIVED ABNORMAL EXCEPTION");
          END T2;
     
     BEGIN 
          T1.E1;
          FAILED ("MAIN: EXCEPTION NOT RAISED");
     EXCEPTION
          WHEN CONSTRAINT_ERROR | PROGRAM_ERROR =>
               FAILED ("PREDEFINED ERROR RAISED IN MAIN");
          WHEN TASKING_ERROR =>
               FAILED ("TASKING_ERROR RAISED IN MAIN");
          WHEN OTHERS => 
               NULL;
     END;

     RESULT;

END CB5001B;
