-- CC3222A.ADA

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
--     CHECK THAT A FLOATING POINT FORMAL TYPE DENOTES ITS ACTUAL
--     PARAMETER, AND OPERATIONS OF THE FORMAL TYPE ARE IDENTIFIED WITH
--     CORRESPONDING OPERATIONS OF THE ACTUAL TYPE.

-- HISTORY:
--     TBN 10/09/86  CREATED ORIGINAL TEST.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;
PROCEDURE CC3222A IS

     TYPE FLOAT IS DIGITS 5 RANGE 0.0 .. 10.0;

     GENERIC
          TYPE T IS DIGITS <>;
     PACKAGE P IS
          SUBTYPE SUB_T IS T;
          PAC_VAR : T;
     END P;

     FUNCTION IDENT_FLO (X : FLOAT) RETURN FLOAT IS
     BEGIN
          IF EQUAL (3, 3) THEN
               RETURN X;
          ELSE
               RETURN (0.0);
          END IF;
     END IDENT_FLO;

BEGIN
     TEST ("CC3222A", "CHECK THAT A FLOATING POINT FORMAL TYPE " &
                      "DENOTES ITS ACTUAL PARAMETER, AND OPERATIONS " &
                      "OF THE FORMAL TYPE ARE IDENTIFIED WITH " &
                      "CORRESPONDING OPERATIONS OF THE ACTUAL TYPE");

     DECLARE
          OBJ_INT : INTEGER := 1;
          OBJ_FLO : FLOAT := 1.0;

          PACKAGE P1 IS NEW P (FLOAT);
          USE P1;

          TYPE NEW_T IS NEW SUB_T;
          OBJ_NEWT : NEW_T;
     BEGIN
          PAC_VAR := SUB_T'(1.0);
          IF PAC_VAR /= OBJ_FLO THEN
               FAILED ("INCORRECT RESULTS - 1");
          END IF;
          OBJ_FLO := IDENT_FLO (PAC_VAR) + IDENT_FLO (OBJ_FLO);
          IF OBJ_FLO <= PAC_VAR THEN
               FAILED ("INCORRECT RESULTS - 2");
          END IF;
          PAC_VAR := PAC_VAR * OBJ_FLO;
          IF PAC_VAR NOT IN FLOAT THEN
               FAILED ("INCORRECT RESULTS - 3");
          END IF;
          IF OBJ_FLO NOT IN SUB_T THEN
               FAILED ("INCORRECT RESULTS - 4");
          END IF;
          PAC_VAR := 1.0;
          OBJ_FLO := 1.0;
          OBJ_FLO := PAC_VAR * OBJ_FLO;
          IF OBJ_FLO /= 1.0 THEN
               FAILED ("INCORRECT RESULTS - 5");
          END IF;
          OBJ_FLO := 1.0;
          OBJ_FLO := OBJ_FLO / OBJ_FLO;
          IF OBJ_FLO /= 1.0 THEN
               FAILED ("INCORRECT RESULTS - 6");
          END IF;
          PAC_VAR := 1.0;
          OBJ_FLO := PAC_VAR ** OBJ_INT;
          IF OBJ_FLO /= 1.0 THEN
               FAILED ("INCORRECT RESULTS - 7");
          END IF;
          IF SUB_T'DIGITS /= 5 THEN
               FAILED ("INCORRECT RESULTS - 8");
          END IF;
          OBJ_NEWT := 1.0;
          OBJ_NEWT := OBJ_NEWT - 1.0;
          IF OBJ_NEWT NOT IN NEW_T THEN
               FAILED ("INCORRECT RESULTS - 9");
          END IF;
          IF NEW_T'DIGITS /= 5 THEN
               FAILED ("INCORRECT RESULTS - 10");
          END IF;
     END;

     RESULT;
END CC3222A;
