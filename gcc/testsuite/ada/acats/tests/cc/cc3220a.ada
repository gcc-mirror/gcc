-- CC3220A.ADA

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
-- CHECK THAT A DISCRETE FORMAL TYPE DENOTES ITS ACTUAL PARAMETER, AND
-- OPERATIONS OF THE FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING
-- OPERATIONS OF THE ACTUAL TYPE.

-- TBN  10/08/86

WITH REPORT; USE REPORT;
PROCEDURE CC3220A IS

     GENERIC
          TYPE T IS (<>);
     PACKAGE P IS
          SUBTYPE SUB_T IS T;
          PAC_VAR : T;
     END P;

BEGIN
     TEST ("CC3220A", "CHECK THAT A DISCRETE FORMAL TYPE DENOTES ITS " &
                      "ACTUAL PARAMETER, AND OPERATIONS OF THE " &
                      "FORMAL TYPE ARE IDENTIFIED WITH CORRESPONDING " &
                      "OPERATIONS OF THE ACTUAL TYPE");

     DECLARE
          OBJ_INT : INTEGER := 1;

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
          OBJ_NEWT := 1;
          OBJ_NEWT := OBJ_NEWT + 1;
          IF OBJ_NEWT NOT IN NEW_T THEN
               FAILED ("INCORRECT RESULTS - 6");
          END IF;
          IF NEW_T'SUCC(2) /= 3 THEN
               FAILED ("INCORRECT RESULTS - 7");
          END IF;
     END;

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

     DECLARE
          OBJ_CHR : CHARACTER := 'A';

          PACKAGE P3 IS NEW P (CHARACTER);
          USE P3;

          TYPE NEW_T IS NEW SUB_T;
          OBJ_NEWT : NEW_T;
          ARA_NEWT : ARRAY (1 .. 5) OF NEW_T;
     BEGIN
          PAC_VAR := SUB_T'('A');
          IF (PAC_VAR < OBJ_CHR) OR (PAC_VAR > OBJ_CHR) THEN
               FAILED ("INCORRECT RESULTS - 15");
          END IF;
          IF PAC_VAR NOT IN CHARACTER THEN
               FAILED ("INCORRECT RESULTS - 16");
          END IF;
          IF OBJ_CHR NOT IN SUB_T THEN
               FAILED ("INCORRECT RESULTS - 17");
          END IF;
          IF CHARACTER'VAL(0) /= SUB_T'VAL(0) THEN
               FAILED ("INCORRECT RESULTS - 18");
          END IF;
          OBJ_CHR := SUB_T'SUCC(PAC_VAR);
          IF SUB_T'POS('A') /= 65 AND THEN OBJ_CHR /= 'A' THEN
               FAILED ("INCORRECT RESULTS - 19");
          END IF;
          OBJ_NEWT := 'C';
          OBJ_NEWT := NEW_T'PRED(OBJ_NEWT);
          IF OBJ_NEWT NOT IN NEW_T THEN
               FAILED ("INCORRECT RESULTS - 20");
          END IF;
          IF NEW_T'IMAGE('A') /= "'A'" THEN
               FAILED ("INCORRECT RESULTS - 21");
          END IF;
          ARA_NEWT := "HELLO";
          IF (NEW_T'('H') & NEW_T'('I')) /= "HI" THEN
               FAILED ("INCORRECT RESULTS - 22");
          END IF;
     END;

     RESULT;
END CC3220A;
