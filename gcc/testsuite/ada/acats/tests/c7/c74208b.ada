-- C74208B.ADA

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
--     CHECK THAT 'CONSTRAINED FOR OBJECTS OF A PRIVATE TYPE WITH
--     VISIBLE DISCRIMINANTS IS AVAILABLE OUTSIDE THE PACKAGE DECLARING
--     THE TYPE AND IS AVAILABLE BEFORE AND AFTER THE FULL DECLARATION.

-- HISTORY:
--     BCB 07/14/88  CREATED ORIGINAL TEST.
--     GJD 11/15/95  MOVED REC2_VAR OUT OF P DUE TO ADA 95 FREEZING RULES.

WITH REPORT; USE REPORT;

PROCEDURE C74208B IS

     PACKAGE P IS
          TYPE REC (D : INTEGER := 0) IS PRIVATE;
          R1 : CONSTANT REC;
          TYPE REC2 IS RECORD
               COMP : BOOLEAN := R1'CONSTRAINED;
          END RECORD;
     PRIVATE
          TYPE REC (D : INTEGER := 0) IS RECORD
               NULL;
          END RECORD;
          R1 : CONSTANT REC := (D => 5);
          R2 : REC := (D => 0);
          R2A : REC(3);
          R2CON : CONSTANT REC := (D => 3);
          C : BOOLEAN := R2'CONSTRAINED;
          D : BOOLEAN := R2A'CONSTRAINED;
          E : BOOLEAN := R2CON'CONSTRAINED;
     END P;

     REC2_VAR : P.REC2;
     
     R3 : P.REC(0);
     R3A : P.REC;

     A : BOOLEAN := R3'CONSTRAINED;
     B : BOOLEAN := R3A'CONSTRAINED;

     PACKAGE BODY P IS
     BEGIN
          TEST ("C74208B", "CHECK THAT 'CONSTRAINED FOR OBJECTS OF A " &
                           "PRIVATE TYPE WITH VISIBLE DISCRIMINANTS " &
                           "IS AVAILABLE OUTSIDE THE PACKAGE " &
                           "DECLARING THE TYPE AND IS AVAILABLE " &
                           "BEFORE AND AFTER THE FULL DECLARATION");

          IF NOT REC2_VAR.COMP THEN
               FAILED ("IMPROPER VALUE FOR 'CONSTRAINED BEFORE THE " &
                       "FULL DECLARATION OF THE PRIVATE TYPE");
          END IF;

          IF C THEN
               FAILED ("IMPROPER VALUE FOR 'CONSTRAINED AFTER THE " &
                       "FULL DECLARATION OF THE PRIVATE TYPE - 1");
          END IF;

          IF NOT D THEN
               FAILED ("IMPROPER VALUE FOR 'CONSTRAINED AFTER THE " &
                       "FULL DECLARATION OF THE PRIVATE TYPE - 2");
          END IF;

          IF NOT E THEN
               FAILED ("IMPROPER VALUE FOR 'CONSTRAINED AFTER THE " &
                       "FULL DECLARATION OF THE PRIVATE TYPE - 3");
          END IF;
     END P;

BEGIN
     IF NOT A THEN
          FAILED ("IMPROPER VALUE FOR 'CONSTRAINED OUTSIDE THE " &
                  "PACKAGE DECLARING THE PRIVATE TYPE - 1");
     END IF;

     IF B THEN
          FAILED ("IMPROPER VALUE FOR 'CONSTRAINED OUTSIDE THE " &
                  "PACKAGE DECLARING THE PRIVATE TYPE - 2");
     END IF;

     RESULT;
END C74208B;
