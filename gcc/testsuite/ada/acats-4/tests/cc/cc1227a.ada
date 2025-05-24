-- CC1227A.ADA

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
--     CHECK, WHEN DERIVING FROM A FORMAL TYPE, THAT ALL THE PREDEFINED
--     OPERATIONS ASSOCIATED WITH THE CLASS OF THE FORMAL TYPE ARE
--     DECLARED FOR THE DERIVED TYPE.

-- HISTORY:
--     BCB 04/04/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;

PROCEDURE CC1227A IS

     GENERIC
          TYPE FORM IS RANGE <>;
     PACKAGE P IS
          TYPE DER_FORM IS NEW FORM;
          FUNCTION IDENT_DER(X : DER_FORM) RETURN DER_FORM;
          FUNCTION IDENT_ADR(Y : ADDRESS) RETURN ADDRESS;
     END P;

     PACKAGE BODY P IS
          DER_VAR : DER_FORM;
          DER_FORM_BASE_FIRST : DER_FORM;
          DER_FORM_FIRST : DER_FORM;
          DER_FORM_LAST : DER_FORM;
          DER_FORM_SIZE : DER_FORM;
          DER_FORM_WIDTH : DER_FORM;
          DER_FORM_POS : DER_FORM;
          DER_FORM_VAL : DER_FORM;
          DER_FORM_SUCC : DER_FORM;
          DER_FORM_PRED : DER_FORM;
          DER_FORM_IMAGE : STRING(1..5);
          DER_FORM_VALUE : DER_FORM;
          DER_VAR_SIZE : DER_FORM;
          DER_VAR_ADDRESS : ADDRESS;
          DER_EQUAL, DER_UNEQUAL : DER_FORM;
          DER_GREATER : DER_FORM;
          DER_MOD, DER_REM : DER_FORM;
          DER_ABS, DER_EXP : DER_FORM;
          INT : INTEGER := 5;
          FUNCTION IDENT_DER(X : DER_FORM) RETURN DER_FORM IS
          BEGIN
               IF EQUAL(3,3) THEN
                    RETURN X;
               END IF;
               RETURN 0;
          END IDENT_DER;
          FUNCTION IDENT_ADR(Y : ADDRESS) RETURN ADDRESS IS
               X : DER_FORM;
          BEGIN
               IF EQUAL(3,3) THEN
                    RETURN Y;
               END IF;
               RETURN X'ADDRESS;
          END IDENT_ADR;
     BEGIN
          TEST ("CC1227A", "CHECK, WHEN DERIVING FROM A FORMAL TYPE, " &
                           "THAT ALL THE PREDEFINED OPERATIONS " &
                           "ASSOCIATED WITH THE CLASS OF THE FORMAL " &
                           "TYPE ARE DECLARED FOR THE DERIVED TYPE");

          DER_VAR := IDENT_DER(1);

          IF DER_VAR /= 1 THEN
               FAILED ("IMPROPER VALUE FROM ASSIGNMENT OPERATION");
          END IF;

          IF DER_VAR NOT IN DER_FORM THEN
               FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST");
          END IF;

          DER_VAR := DER_FORM'(2);

          IF DER_VAR /= IDENT_DER(2) THEN
               FAILED ("IMPROPER RESULT FROM QUALIFICATION");
          END IF;

          DER_VAR := DER_FORM(INT);

          IF DER_VAR /= IDENT_DER(5) THEN
               FAILED ("IMPROPER RESULT FROM EXPLICIT CONVERSION - " &
                       "INTEGER");
          END IF;

          DER_VAR := DER_FORM(3.0);

          IF DER_VAR /= IDENT_DER(3) THEN
               FAILED ("IMPROPER RESULT FROM EXPLICIT CONVERSION - " &
                        "FLOAT");
          END IF;

          DER_VAR := 1_000;

          IF DER_VAR /= IDENT_DER(1000) THEN
               FAILED ("IMPROPER RESULT FROM IMPLICIT CONVERSION");
          END IF;

          DER_FORM_BASE_FIRST := DER_FORM'BASE'FIRST;

          DER_FORM_FIRST := DER_FORM'FIRST;

          IF DER_FORM_BASE_FIRST /= IDENT_DER(DER_FORM_FIRST) THEN
               FAILED ("IMPROPER VALUE FOR DER_FORM'BASE'FIRST");
          END IF;

          IF DER_FORM_FIRST /= IDENT_DER(DER_FORM'FIRST) THEN
               FAILED ("IMPROPER VALUE FOR DER_FORM'FIRST");
          END IF;

          DER_FORM_LAST := DER_FORM'LAST;

          IF DER_FORM_LAST /= IDENT_DER(DER_FORM'LAST) THEN
               FAILED ("IMPROPER VALUE FOR DER_FORM'LAST");
          END IF;

          DER_FORM_SIZE := DER_FORM(DER_FORM'SIZE);

          IF DER_FORM_SIZE /= IDENT_DER(DER_FORM(DER_FORM'SIZE)) THEN
               FAILED ("IMPROPER VALUE FOR DER_FORM'SIZE");
          END IF;

          DER_FORM_WIDTH := DER_FORM(DER_FORM'WIDTH);

          IF DER_FORM_WIDTH /= IDENT_DER(DER_FORM(DER_FORM'WIDTH)) THEN
               FAILED ("IMPROPER VALUE FOR DER_FORM'WIDTH");
          END IF;

          DER_FORM_POS := DER_FORM(DER_FORM'POS(DER_VAR));

          IF DER_FORM_POS /= IDENT_DER(DER_FORM(DER_FORM'POS(DER_VAR)))
               THEN FAILED ("IMPROPER VALUE FOR DER_FORM'POS(DER_VAR)");
          END IF;

          DER_FORM_VAL := DER_FORM'VAL(DER_VAR);

          IF DER_FORM_VAL /= IDENT_DER(DER_FORM'VAL(DER_VAR)) THEN
               FAILED ("IMPROPER VALUE FOR DER_FORM'VAL(DER_VAR)");
          END IF;

          DER_FORM_SUCC := DER_FORM'SUCC(DER_VAR);

          IF DER_FORM_SUCC /= IDENT_DER(DER_FORM'SUCC(DER_VAR)) THEN
               FAILED ("IMPROPER VALUE FOR DER_FORM'SUCC(DER_VAR)");
          END IF;

          DER_FORM_PRED := DER_FORM'PRED(DER_VAR);

          IF DER_FORM_PRED /= IDENT_DER(DER_FORM'PRED(DER_VAR)) THEN
               FAILED ("IMPROPER VALUE FOR DER_FORM'PRED(DER_VAR)");
          END IF;

          DER_FORM_IMAGE := DER_FORM'IMAGE(DER_VAR);

          IF DER_FORM_IMAGE(2..5) /= "1000" THEN
               FAILED ("IMPROPER VALUE FOR DER_FORM'IMAGE(DER_VAR)");
          END IF;

          DER_FORM_VALUE := DER_FORM'VALUE(DER_FORM_IMAGE);

          IF DER_FORM_VALUE /= IDENT_DER(1000) THEN
               FAILED ("IMPROPER VALUE FOR DER_FORM'VALUE" &
                       "(DER_FORM_IMAGE)");
          END IF;

          DER_VAR_SIZE := DER_FORM(DER_VAR'SIZE);

          IF DER_VAR_SIZE /= IDENT_DER(DER_FORM(DER_VAR'SIZE)) THEN
               FAILED ("IMPROPER VALUE FOR DER_VAR'SIZE");
          END IF;

          DER_VAR_ADDRESS := DER_VAR'ADDRESS;

          IF DER_VAR_ADDRESS /= IDENT_ADR(DER_VAR'ADDRESS) THEN
               FAILED ("IMPROPER VALUE FOR DER_VAR'ADDRESS");
          END IF;

          DER_EQUAL := IDENT_DER(1000);

          IF DER_VAR /= DER_EQUAL THEN
               FAILED ("IMPROPER RESULT FROM INEQUALITY OPERATOR");
          END IF;

          DER_UNEQUAL := IDENT_DER(500);

          IF DER_VAR = DER_UNEQUAL THEN
               FAILED ("IMPROPER RESULT FROM EQUALITY OPERATOR");
          END IF;

          IF DER_VAR < DER_UNEQUAL THEN
               FAILED ("IMPROPER RESULT FROM LESS THAN OPERATOR");
          END IF;

          IF DER_VAR <= DER_UNEQUAL THEN
               FAILED ("IMPROPER RESULT FROM LESS THAN OR EQUAL TO " &
                       "OPERATOR");
          END IF;

          DER_GREATER := IDENT_DER(1500);

          IF DER_VAR > DER_GREATER THEN
               FAILED ("IMPROPER RESULT FROM GREATER THAN OPERATOR");
          END IF;

          IF DER_VAR >= DER_GREATER THEN
               FAILED ("IMPROPER RESULT FROM GREATER THAN OR EQUAL " &
                       "TO OPERATOR");
          END IF;

          DER_VAR := DER_VAR + DER_EQUAL;

          IF DER_VAR /= IDENT_DER(2000) THEN
               FAILED ("IMPROPER RESULT FROM ADDITION OPERATOR");
          END IF;

          DER_VAR := DER_VAR - DER_EQUAL;

          IF DER_VAR /= IDENT_DER(1000) THEN
               FAILED ("IMPROPER RESULT FROM SUBTRACTION OPERATOR");
          END IF;

          DER_VAR := DER_VAR * IDENT_DER(2);

          IF DER_VAR /= IDENT_DER(2000) THEN
               FAILED ("IMPROPER RESULT FROM MULTIPLICATION OPERATOR");
          END IF;

          DER_VAR := DER_VAR / IDENT_DER(2);

          IF DER_VAR /= IDENT_DER(1000) THEN
               FAILED ("IMPROPER RESULT FROM DIVISION OPERATOR");
          END IF;

          DER_MOD := DER_GREATER MOD DER_VAR;

          IF DER_MOD /= IDENT_DER(500) THEN
               FAILED ("IMPROPER RESULT FROM MOD OPERATOR");
          END IF;

          DER_REM := DER_GREATER REM DER_VAR;

          IF DER_REM /= IDENT_DER(500) THEN
               FAILED ("IMPROPER RESULT FROM REM OPERATOR");
          END IF;

          DER_ABS := ABS(IDENT_DER(-1500));

          IF DER_ABS /= IDENT_DER(DER_GREATER) THEN
               FAILED ("IMPROPER RESULT FROM ABS OPERATOR");
          END IF;

          DER_EXP := IDENT_DER(2) ** IDENT_INT(2);

          IF DER_EXP /= IDENT_DER(4) THEN
               FAILED ("IMPROPER RESULT FROM EXPONENTIATION OPERATOR");
          END IF;

          RESULT;
     END P;

     PACKAGE PACK IS NEW P(INTEGER);

BEGIN
     NULL;
END CC1227A;
