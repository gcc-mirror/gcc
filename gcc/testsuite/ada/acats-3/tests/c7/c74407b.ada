-- C74407B.ADA

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
--     CHECK, FOR A LIMITED PRIVATE TYPE, THAT PRE-DEFINED EQUALITY AND
--     ASSIGNMENT ARE DEFINED AND AVAILABLE WITHIN THE PRIVATE PART AND
--     THE BODY OF A PACKAGE, AFTER THE FULL DECLARATION, IF THE FULL
--     DECLARATION IS NOT LIMITED.

-- HISTORY:
--     BCB 07/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C74407B IS

     PACKAGE PP IS
          TYPE PRIV IS PRIVATE;
          C1 : CONSTANT PRIV;
          C2 : CONSTANT PRIV;
     PRIVATE
          TYPE PRIV IS (ONE, TWO, THREE, FOUR, FIVE, SIX);
          C1 : CONSTANT PRIV := ONE;
          C2 : CONSTANT PRIV := TWO;
     END PP;

     USE PP;

     PACKAGE P IS
          TYPE INT IS LIMITED PRIVATE;
          TYPE COMP IS LIMITED PRIVATE;
          TYPE DER IS LIMITED PRIVATE;
     PRIVATE
          TYPE INT IS RANGE 1 .. 100;
          TYPE COMP IS ARRAY(1..5) OF INTEGER;
          TYPE DER IS NEW PRIV;
          D, E : INT := 10;
          F : INT := 20;
          CONS_INT1 : CONSTANT INT := 30;
          G : BOOLEAN := D = E;
          H : BOOLEAN := D /= F;
          CONS_BOOL1 : CONSTANT BOOLEAN := D = E;
          CONS_BOOL2 : CONSTANT BOOLEAN := D /= F;
          I : COMP := (1,2,3,4,5);
          CONS_COMP1 : CONSTANT COMP := (6,7,8,9,10);
          J : DER := DER(C1);
          CONS_DER1 : CONSTANT DER := DER(C2);
     END P;

     PACKAGE BODY P IS
          A, B, C : INT;
          X, Y, Z : COMP;
          L, M, N : DER;
          CONS_INT2 : CONSTANT INT := 10;
          CONS_COMP2 : CONSTANT COMP := (1,2,3,4,5);
          CONS_DER2 : CONSTANT DER := DER(C1);
     BEGIN
          TEST ("C74407B", "CHECK, FOR A LIMITED PRIVATE TYPE, THAT " &
                           "PRE-DEFINED EQUALITY AND ASSIGNMENT ARE " &
                           "DEFINED AND AVAILABLE WITHIN THE PRIVATE " &
                           "PART AND THE BODY OF A PACKAGE, AFTER " &
                           "THE FULL DECLARATION, IF THE FULL " &
                           "DECLARATION IS NOT LIMITED");

          A := 10;

          B := 10;

          C := 20;

          IF A = C THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 1");
          END IF;

          IF A /= B THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 1");
          END IF;

          IF CONS_INT2 = C THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 2");
          END IF;

          IF CONS_INT2 /= B THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 2");
          END IF;

          IF NOT G THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
                       "OPERATION WITHIN THE PRIVATE PART OF THE " &
                       "PACKAGE - 1");
          END IF;

          IF NOT H THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
                       "OPERATION WITHIN THE PRIVATE PART OF THE " &
                       "PACKAGE - 1");
          END IF;

          IF NOT CONS_BOOL1 THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
                       "OPERATION WITHIN THE PRIVATE PART OF THE " &
                       "PACKAGE - 2");
          END IF;

          IF NOT CONS_BOOL2 THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
                       "OPERATION WITHIN THE PRIVATE PART OF THE " &
                       "PACKAGE - 2");
          END IF;

          X := (1,2,3,4,5);

          Y := (1,2,3,4,5);

          Z := (5,4,3,2,1);

          IF X = Z THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 3");
          END IF;

          IF X /= Y THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 3");
          END IF;

          IF CONS_COMP2 = Z THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 4");
          END IF;

          IF CONS_COMP2 /= Y THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 4");
          END IF;

          L := DER(C1);

          M := DER(C1);

          N := DER(C2);

          IF L = N THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 5");
          END IF;

          IF L /= M THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 5");
          END IF;

          IF CONS_DER2 = N THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED EQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 6");
          END IF;

          IF CONS_DER2 /= M THEN
               FAILED ("IMPROPER RESULT FROM PRE-DEFINED INEQUALITY " &
                       "OPERATION WITHIN THE PACKAGE BODY - 6");
          END IF;

          RESULT;
     END P;

     USE P;

BEGIN
     NULL;
END C74407B;
