-- C91004B.ADA

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
--     CHECK THAT A TASK (TYPE) IDENTIFIER, WHEN USED WITHIN ITS OWN
--     BODY, REFERS TO THE EXECUTING TASK.

--      TEST USING IDENTIFIER IN ABORT STATEMENT, AS AN EXPRESSION IN
--      A MEMBERSHIP TEST, AND THE PREFIX OF 'CALLABLE AND
--      'TERMINATED.

-- HISTORY:
--     WEI  3/ 4/82  CREATED ORIGINAL TEST.
--     RJW 11/13/87  RENAMED TEST FROM C910BDA.ADA.  ADDED CHECKS FOR
--                   MEMBERSHIP TEST, AND 'CALLABLE AND 'TERMINATED
--                   ATTRIBUTES.

WITH REPORT;  USE REPORT;
PROCEDURE C91004B IS

     TYPE I0 IS RANGE 0..1;
     SUBTYPE ARG IS NATURAL RANGE 0..9;
     SPYNUMB : NATURAL := 0;

     TASK TYPE TT1 IS
          ENTRY E1 (P1 : IN I0; P2 : ARG);
          ENTRY BYE;
     END TT1;

     SUBTYPE SUB_TT1 IS TT1;

     OBJ_TT1 : ARRAY (NATURAL RANGE 1..2) OF TT1;

     PROCEDURE PSPY_NUMB (DIGT: IN ARG) IS
     BEGIN
          SPYNUMB := 10*SPYNUMB+DIGT;
     END PSPY_NUMB;

     TASK BODY TT1 IS
     BEGIN
          IF TT1 NOT IN SUB_TT1 THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP TEST");
          END IF;

          IF NOT TT1'CALLABLE THEN
               FAILED ("INCORRECT RESULTS FOR 'CALLABLE");
          END IF;

          IF TT1'TERMINATED THEN
               FAILED ("INCORRECT RESULTS FOR 'TERMINATED");
          END IF;

          ACCEPT E1 (P1 : IN I0; P2 : ARG) DO
               IF P1 = 1 THEN
                    ABORT TT1;
                    ACCEPT BYE;    -- WILL DEADLOCK IF NOT ABORTED.
               END IF;
               PSPY_NUMB (ARG (P2));
          END E1;

     END TT1;

BEGIN

     TEST ("C91004B", "TASK IDENTIFIER IN OWN BODY");

     BEGIN
          OBJ_TT1 (1).E1 (1,1);
          FAILED ("NO TASKING_ERROR RAISED");
-- ABORT DURING RENDEVOUS RAISES TASKING ERROR
     EXCEPTION
          WHEN TASKING_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED");
     END;

     OBJ_TT1 (2).E1 (0,2);

     IF SPYNUMB /= 2 THEN
          FAILED ("WRONG TASK OBJECT REFERENCED");
          COMMENT ("ACTUAL ORDER WAS:" & INTEGER'IMAGE(SPYNUMB));
     END IF;

     RESULT;

END C91004B;
