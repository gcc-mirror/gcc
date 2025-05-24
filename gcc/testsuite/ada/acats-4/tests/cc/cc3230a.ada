-- CC3230A.ADA

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
--      ACTUAL PARAMETER AN ENUMERATION TYPE, AND OPERATIONS OF THE
--      FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE
--      ACTUAL TYPE.

-- HISTORY:
--      TBN 09/14/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE CC3230A IS

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
     TEST ("CC3230A", "CHECK THAT A PRIVATE OR LIMITED PRIVATE " &
                      "FORMAL TYPE DENOTES ITS ACTUAL PARAMETER AN " &
                      "ENUMERATION TYPE, AND OPERATIONS OF THE " &
                      "FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING " &
                      "OPERATIONS OF THE ACTUAL TYPE");

     DECLARE
          TYPE ENUM IS (RED, YELLOW, GREEN, BLUE);
          OBJ_ENU : ENUM := RED;

          PACKAGE P2 IS NEW P (ENUM);
          USE P2;

          TYPE NEW_T IS NEW SUB_T;
          OBJ_NEWT : NEW_T;
     BEGIN
          PAC_VAR := SUB_T'(RED);
          IF (PAC_VAR < OBJ_ENU) OR (PAC_VAR > OBJ_ENU) THEN
               FAILED ("INCORRECT RESULTS - 1");
          END IF;
          IF PAC_VAR NOT IN ENUM THEN
               FAILED ("INCORRECT RESULTS - 2");
          END IF;
          IF OBJ_ENU NOT IN SUB_T THEN
               FAILED ("INCORRECT RESULTS - 3");
          END IF;
          IF ENUM'VAL(0) /= SUB_T'VAL(0) THEN
               FAILED ("INCORRECT RESULTS - 4");
          END IF;
          OBJ_ENU := SUB_T'SUCC(PAC_VAR);
          IF SUB_T'POS(RED) /= 0 AND THEN OBJ_ENU /= BLUE THEN
               FAILED ("INCORRECT RESULTS - 5");
          END IF;
          OBJ_NEWT := BLUE;
          OBJ_NEWT := NEW_T'PRED(OBJ_NEWT);
          IF OBJ_NEWT NOT IN NEW_T THEN
               FAILED ("INCORRECT RESULTS - 6");
          END IF;
          IF NEW_T'WIDTH /= 6 THEN
               FAILED ("INCORRECT RESULTS - 7");
          END IF;
     END;

     DECLARE
          TYPE ENUM IS (RED, YELLOW, GREEN, BLUE);
          OBJ_ENU : ENUM := RED;

          PACKAGE P2 IS NEW LP (ENUM);
          USE P2;

          TYPE NEW_T IS NEW SUB_T;
          OBJ_NEWT : NEW_T;
     BEGIN
          PAC_VAR := SUB_T'(RED);
          IF (PAC_VAR < OBJ_ENU) OR (PAC_VAR > OBJ_ENU) THEN
               FAILED ("INCORRECT RESULTS - 8");
          END IF;
          IF PAC_VAR NOT IN ENUM THEN
               FAILED ("INCORRECT RESULTS - 9");
          END IF;
          IF OBJ_ENU NOT IN SUB_T THEN
               FAILED ("INCORRECT RESULTS - 10");
          END IF;
          IF ENUM'VAL(0) /= SUB_T'VAL(0) THEN
               FAILED ("INCORRECT RESULTS - 11");
          END IF;
          OBJ_ENU := SUB_T'SUCC(PAC_VAR);
          IF SUB_T'POS(RED) /= 0 AND THEN OBJ_ENU /= BLUE THEN
               FAILED ("INCORRECT RESULTS - 12");
          END IF;
          OBJ_NEWT := BLUE;
          OBJ_NEWT := NEW_T'PRED(OBJ_NEWT);
          IF OBJ_NEWT NOT IN NEW_T THEN
               FAILED ("INCORRECT RESULTS - 13");
          END IF;
          IF NEW_T'WIDTH /= 6 THEN
               FAILED ("INCORRECT RESULTS - 14");
          END IF;
     END;

     RESULT;
END CC3230A;
