-- CC3231A.ADA

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
--      ACTUAL PARAMETER AN INTEGER TYPE, AND OPERATIONS OF THE FORMAL
--      TYPE ARE IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE ACTUAL
--      TYPE.

-- HISTORY:
--      TBN 09/14/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE CC3231A IS

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
     TEST ("CC3231A", "CHECK THAT A PRIVATE OR LIMITED PRIVATE " &
                      "FORMAL TYPE DENOTES ITS ACTUAL PARAMETER AN " &
                      "INTEGER TYPE, AND OPERATIONS OF THE " &
                      "FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING " &
                      "OPERATIONS OF THE ACTUAL TYPE");

     DECLARE  -- PRIVATE TYPE.
          TYPE FIXED IS DELTA 0.125 RANGE 0.0 .. 10.0;

          OBJ_INT : INTEGER := 1;
          OBJ_FLO : FLOAT := 1.0;
          OBJ_FIX : FIXED := 1.0;

          PACKAGE P1 IS NEW P (INTEGER);
          USE P1;

          TYPE NEW_T IS NEW SUB_T;
          OBJ_NEWT : NEW_T;
     BEGIN
          PAC_VAR := SUB_T'(1);
          IF PAC_VAR /= OBJ_INT THEN
               FAILED ("INCORRECT RESULTS - 1");
          END IF;
          OBJ_INT := PAC_VAR + OBJ_INT;
          IF OBJ_INT <= PAC_VAR THEN
               FAILED ("INCORRECT RESULTS - 2");
          END IF;
          PAC_VAR := PAC_VAR * OBJ_INT;
          IF PAC_VAR NOT IN INTEGER THEN
               FAILED ("INCORRECT RESULTS - 3");
          END IF;
          IF OBJ_INT NOT IN SUB_T THEN
               FAILED ("INCORRECT RESULTS - 4");
          END IF;
          IF INTEGER'POS(2) /= SUB_T'POS(2) THEN
               FAILED ("INCORRECT RESULTS - 5");
          END IF;
          PAC_VAR := 1;
          OBJ_FIX := PAC_VAR * OBJ_FIX;
          IF OBJ_FIX /= 1.0 THEN
               FAILED ("INCORRECT RESULTS - 6");
          END IF;
          OBJ_INT := 1;
          OBJ_FIX := OBJ_FIX / OBJ_INT;
          IF OBJ_FIX /= 1.0 THEN
               FAILED ("INCORRECT RESULTS - 7");
          END IF;
          OBJ_INT := OBJ_INT ** PAC_VAR;
          IF OBJ_INT /= 1 THEN
               FAILED ("INCORRECT RESULTS - 8");
          END IF;
          OBJ_FLO := OBJ_FLO ** PAC_VAR;
          IF OBJ_FLO /= 1.0 THEN
               FAILED ("INCORRECT RESULTS - 9");
          END IF;
          OBJ_NEWT := 1;
          OBJ_NEWT := OBJ_NEWT - 1;
          IF OBJ_NEWT NOT IN NEW_T THEN
               FAILED ("INCORRECT RESULTS - 10");
          END IF;
          IF NEW_T'SUCC(2) /= 3 THEN
               FAILED ("INCORRECT RESULTS - 11");
          END IF;
     END;

     DECLARE  -- LIMITED PRIVATE TYPE.
          TYPE FIXED IS DELTA 0.125 RANGE 0.0 .. 10.0;

          OBJ_INT : INTEGER := 1;
          OBJ_FLO : FLOAT := 1.0;
          OBJ_FIX : FIXED := 1.0;

          PACKAGE P1 IS NEW LP (INTEGER);
          USE P1;

          TYPE NEW_T IS NEW SUB_T;
          OBJ_NEWT : NEW_T;
     BEGIN
          PAC_VAR := SUB_T'(1);
          IF PAC_VAR /= OBJ_INT THEN
               FAILED ("INCORRECT RESULTS - 12");
          END IF;
          OBJ_INT := PAC_VAR + OBJ_INT;
          IF OBJ_INT <= PAC_VAR THEN
               FAILED ("INCORRECT RESULTS - 13");
          END IF;
          PAC_VAR := PAC_VAR * OBJ_INT;
          IF PAC_VAR NOT IN INTEGER THEN
               FAILED ("INCORRECT RESULTS - 14");
          END IF;
          IF OBJ_INT NOT IN SUB_T THEN
               FAILED ("INCORRECT RESULTS - 15");
          END IF;
          IF INTEGER'POS(2) /= SUB_T'POS(2) THEN
               FAILED ("INCORRECT RESULTS - 16");
          END IF;
          PAC_VAR := 1;
          OBJ_FIX := PAC_VAR * OBJ_FIX;
          IF OBJ_FIX /= 1.0 THEN
               FAILED ("INCORRECT RESULTS - 17");
          END IF;
          OBJ_INT := 1;
          OBJ_FIX := OBJ_FIX / OBJ_INT;
          IF OBJ_FIX /= 1.0 THEN
               FAILED ("INCORRECT RESULTS - 18");
          END IF;
          OBJ_INT := OBJ_INT ** PAC_VAR;
          IF OBJ_INT /= 1 THEN
               FAILED ("INCORRECT RESULTS - 19");
          END IF;
          OBJ_FLO := OBJ_FLO ** PAC_VAR;
          IF OBJ_FLO /= 1.0 THEN
               FAILED ("INCORRECT RESULTS - 20");
          END IF;
          OBJ_NEWT := 1;
          OBJ_NEWT := OBJ_NEWT - 1;
          IF OBJ_NEWT NOT IN NEW_T THEN
               FAILED ("INCORRECT RESULTS - 21");
          END IF;
          IF NEW_T'SUCC(2) /= 3 THEN
               FAILED ("INCORRECT RESULTS - 22");
          END IF;
     END;

     RESULT;
END CC3231A;
