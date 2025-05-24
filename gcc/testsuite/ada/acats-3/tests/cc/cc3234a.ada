-- CC3234A.ADA

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
--      CHECK THAT A PRIVATE OR LIMITED PRIVATE FORMAL TYPE DENOTES ITS
--      ACTUAL PARAMETER AN ARRAY TYPE, AND OPERATIONS OF THE FORMAL
--      TYPE ARE IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE ACTUAL
--      TYPE.

-- HISTORY:
--      TBN 09/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE CC3234A IS

     GENERIC
          TYPE T IS PRIVATE;
     PACKAGE P IS
          SUBTYPE SUB_T IS T;
          PAC_VAR : T;
     END P;

     GENERIC
          TYPE T IS LIMITED PRIVATE;
     PACKAGE LP IS
          SUBTYPE SUB_T IS T;
          PAC_VAR : T;
     END LP;

BEGIN
     TEST ("CC3234A", "CHECK THAT A PRIVATE OR LIMITED PRIVATE " &
                      "FORMAL TYPE DENOTES ITS ACTUAL PARAMETER AN " &
                      "ARRAY TYPE, AND OPERATIONS OF THE " &
                      "FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING " &
                      "OPERATIONS OF THE ACTUAL TYPE");

     DECLARE  -- PRIVATE TYPE.
          TYPE ARRAY_TYPE IS ARRAY (1..10) OF INTEGER;

          OBJ_ARR : ARRAY_TYPE := (OTHERS => 1);

          PACKAGE P1 IS NEW P (ARRAY_TYPE);
          USE P1;

          TYPE NEW_T IS NEW SUB_T;
          OBJ_NEWT : NEW_T;
     BEGIN
          PAC_VAR := SUB_T'(1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
          IF PAC_VAR /= OBJ_ARR THEN
               FAILED ("INCORRECT RESULTS - 1");
          END IF;
          OBJ_ARR(1) := PAC_VAR(2) + OBJ_ARR(1);
          IF OBJ_ARR(1) <= PAC_VAR(1) THEN
               FAILED ("INCORRECT RESULTS - 2");
          END IF;
          PAC_VAR(1) := PAC_VAR(1) * OBJ_ARR(3);
          IF PAC_VAR NOT IN ARRAY_TYPE THEN
               FAILED ("INCORRECT RESULTS - 3");
          END IF;
          IF OBJ_ARR NOT IN SUB_T THEN
               FAILED ("INCORRECT RESULTS - 4");
          END IF;
          IF ARRAY_TYPE'FIRST /= SUB_T'FIRST THEN
               FAILED ("INCORRECT RESULTS - 5");
          END IF;
          OBJ_ARR(1..5) := PAC_VAR(6..10);
          IF OBJ_ARR(1..5) /= (1, 1, 1, 1, 1) THEN
               FAILED ("INCORRECT RESULTS - 6");
          END IF;
          PAC_VAR := (1, 1, 1, 1, 1, 2, 2, 2, 2, 2);
          OBJ_NEWT := (1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
          OBJ_NEWT := NEW_T(PAC_VAR);
          IF OBJ_NEWT(3..7) /= (1, 1, 1, 2, 2) THEN
               FAILED ("INCORRECT RESULTS - 7");
          END IF;
          IF OBJ_NEWT NOT IN NEW_T THEN
               FAILED ("INCORRECT RESULTS - 8");
          END IF;
     END;

     DECLARE  -- LIMITED PRIVATE TYPE.
          TYPE ARRAY_TYPE IS ARRAY (1..10) OF INTEGER;

          OBJ_ARR : ARRAY_TYPE := (OTHERS => 1);

          PACKAGE P1 IS NEW LP (ARRAY_TYPE);
          USE P1;

          TYPE NEW_T IS NEW SUB_T;
          OBJ_NEWT : NEW_T;
     BEGIN
          PAC_VAR := SUB_T'(1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
          IF PAC_VAR /= OBJ_ARR THEN
               FAILED ("INCORRECT RESULTS - 9");
          END IF;
          OBJ_ARR(1) := PAC_VAR(2) + OBJ_ARR(1);
          IF OBJ_ARR(1) <= PAC_VAR(1) THEN
               FAILED ("INCORRECT RESULTS - 10");
          END IF;
          PAC_VAR(1) := PAC_VAR(1) * OBJ_ARR(3);
          IF PAC_VAR NOT IN ARRAY_TYPE THEN
               FAILED ("INCORRECT RESULTS - 11");
          END IF;
          IF OBJ_ARR NOT IN SUB_T THEN
               FAILED ("INCORRECT RESULTS - 12");
          END IF;
          IF ARRAY_TYPE'FIRST /= SUB_T'FIRST THEN
               FAILED ("INCORRECT RESULTS - 13");
          END IF;
          OBJ_ARR(1..5) := PAC_VAR(6..10);
          IF OBJ_ARR(1..5) /= (1, 1, 1, 1, 1) THEN
               FAILED ("INCORRECT RESULTS - 14");
          END IF;
          PAC_VAR := (1, 1, 1, 1, 1, 2, 2, 2, 2, 2);
          OBJ_NEWT := (1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
          OBJ_NEWT := NEW_T(PAC_VAR);
          IF OBJ_NEWT(3..7) /= (1, 1, 1, 2, 2) THEN
               FAILED ("INCORRECT RESULTS - 15");
          END IF;
          IF OBJ_NEWT NOT IN NEW_T THEN
               FAILED ("INCORRECT RESULTS - 16");
          END IF;
     END;

     RESULT;
END CC3234A;
