-- C87B26B.ADA

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
-- CHECK THAT 'ADDRESS, 'CONSTRAINED, 'SIZE, AND 'STORAGE_SIZE MAY BE
-- USED WITH THE DESIGNATED OBJECTS OF ACCESS VALUES RETURNED FROM
-- OVERLOADED FUNCTIONS, AND THAT EXPLICIT DEREFERENCING IS USED BY
-- OVERLOADING RESOLUTION TO RESOLVE THE PREFIXES OF THE ATTRIBUTES.

-- DSJ 22 JUN 83
-- JBG 11/22/83
-- JBG 4/23/84
-- JBG 5/25/85

WITH REPORT;  WITH SYSTEM;
USE  REPORT;  USE  SYSTEM;

PROCEDURE C87B26B IS

     TYPE REC (D : INTEGER) IS
          RECORD
               C1, C2 : INTEGER;
          END RECORD;
     TYPE P_REC IS ACCESS REC;

     P_REC_OBJECT : P_REC := NEW REC'(1,1,1);

     TYPE BIG_INT IS RANGE 0..SYSTEM.MAX_INT;
     TASK TYPE TASK_TYPE IS
          -- NOTHING AT ALL
     END TASK_TYPE;

     TYPE P_TASK IS ACCESS TASK_TYPE;

     P_TASK_OBJECT : P_TASK;

     TASK BODY TASK_TYPE IS
     BEGIN
          NULL;
     END TASK_TYPE;

     ------------------------------------------------------------

     FUNCTION F RETURN REC IS
     BEGIN
          RETURN (0,0,0);
     END F;

     FUNCTION F RETURN P_REC IS
     BEGIN
          RETURN P_REC_OBJECT;
     END F;

     ------------------------------------------------------------

     FUNCTION G RETURN TASK_TYPE IS
          NEW_TASK : TASK_TYPE;
     BEGIN
          RETURN NEW_TASK;
     END G;

     FUNCTION G RETURN P_TASK IS
     BEGIN
          RETURN P_TASK_OBJECT;
     END G;

     ------------------------------------------------------------

BEGIN

     TEST("C87B26B","CHECK THAT EXPLICIT DEREFERENCING IN AN " &
          "ATTRIBUTE PREFIX IS USED IN OVERLOADING RESOLUTION " &
          "WITH 'ADDRESS, 'CONSTRAINED, 'SIZE, AND 'STORAGE_SIZE");

     DECLARE

          A : ADDRESS;   -- FOR 'ADDRESS OF RECORD
          B : BOOLEAN;   -- FOR 'CONSTRAINED OF RECORD
          C : INTEGER;   -- FOR 'SIZE OF RECORD
          D : ADDRESS;   -- FOR 'ADDRESS OF TASK
          E : BIG_INT;   -- FOR 'STORAGE_SIZE OF TASK

     BEGIN

          P_TASK_OBJECT := NEW TASK_TYPE;
          A := F.ALL'ADDRESS;
          B := F.ALL'CONSTRAINED;
          C := F.ALL'SIZE;
          D := G.ALL'ADDRESS;
          E := G.ALL'STORAGE_SIZE;

          IF A /= P_REC_OBJECT.ALL'ADDRESS THEN
               FAILED("INCORRECT RESOLUTION FOR 'ADDRESS - REC");
          END IF;

          IF B /= P_REC_OBJECT.ALL'CONSTRAINED THEN
               FAILED("INCORRECT RESOLUTION FOR 'CONSTRAINED");
          END IF;

          IF C /= P_REC_OBJECT.ALL'SIZE THEN
               FAILED("INCORRECT RESOLUTION FOR 'SIZE");
          END IF;

          IF D /= P_TASK_OBJECT.ALL'ADDRESS THEN
               FAILED("INCORRECT RESOLUTION FOR 'ADDRESS - TASK");
          END IF;

          IF E /= P_TASK_OBJECT.ALL'STORAGE_SIZE THEN
               FAILED("INCORRECT RESOLUTION FOR 'STORAGE_SIZE");
          END IF;

          IF A = P_REC_OBJECT'ADDRESS THEN
               FAILED("INCORRECT DEREFERENCING FOR 'ADDRESS - REC");
          END IF;

          IF C = P_REC_OBJECT'SIZE AND C /= P_REC_OBJECT.ALL'SIZE THEN
               FAILED("INCORRECT DEREFERENCING FOR 'SIZE");
          END IF;

          IF D = P_TASK_OBJECT'ADDRESS THEN
               FAILED("INCORRECT DEREFERENCING FOR 'ADDRESS - TASK");
          END IF;


     END;

     RESULT;

END C87B26B; 
