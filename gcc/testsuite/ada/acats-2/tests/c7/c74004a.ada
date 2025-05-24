-- C74004A.ADA

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
--     CHECK THAT OPERATIONS DEPENDING ON THE FULL DECLARATION OF A
--     PRIVATE TYPE ARE AVAILABLE WITHIN THE PACKAGE BODY.

-- HISTORY:
--     BCB 04/05/88  CREATED ORIGINAL TEST.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;

PROCEDURE C74004A IS

     PACKAGE P IS
          TYPE PR IS PRIVATE;
          TYPE ARR1 IS LIMITED PRIVATE;
          TYPE ARR2 IS PRIVATE;
          TYPE REC (D : INTEGER) IS PRIVATE;
          TYPE ACC IS PRIVATE;
          TYPE TSK IS LIMITED PRIVATE;
          TYPE FLT IS LIMITED PRIVATE;
          TYPE FIX IS LIMITED PRIVATE;

          TASK TYPE T IS
               ENTRY ONE(V : IN OUT INTEGER);
          END T;

          PROCEDURE CHECK (V : ARR2);
     PRIVATE
          TYPE PR IS NEW INTEGER;

          TYPE ARR1 IS ARRAY(1..5) OF INTEGER;

          TYPE ARR2 IS ARRAY(1..5) OF BOOLEAN;

          TYPE REC (D : INTEGER) IS RECORD
               COMP1 : INTEGER;
               COMP2 : BOOLEAN;
          END RECORD;

          TYPE ACC IS ACCESS INTEGER;

          TYPE TSK IS NEW T;

          TYPE FLT IS DIGITS 5;

          TYPE FIX IS DELTA 2.0**(-1) RANGE -100.0 .. 100.0;
     END P;

     PACKAGE BODY P IS
          X1, X2, X3 : PR;
          BOOL : BOOLEAN := IDENT_BOOL(FALSE);
          VAL : INTEGER := IDENT_INT(0);
          FVAL : FLOAT := 0.0;
          ST : STRING(1..2);
          O1 : ARR1 := (1,2,3,4,5);
          Y1 : ARR2 := (FALSE,TRUE,FALSE,TRUE,FALSE);
          Y2 : ARR2 := (OTHERS => TRUE);
          Y3 : ARR2 := (OTHERS => FALSE);
          Z1 : REC(0) := (0,1,FALSE);
          W1, W2 : ACC := NEW INTEGER'(0);
          V1 : TSK;

          TASK BODY T IS
          BEGIN
               ACCEPT ONE(V : IN OUT INTEGER) DO
                    V := IDENT_INT(10);
               END ONE;
          END T;

          PROCEDURE CHECK (V : ARR2) IS
          BEGIN
               IF V /= (TRUE,FALSE,TRUE,FALSE,TRUE) THEN
                    FAILED ("IMPROPER VALUE PASSED AS AGGREGATE");
               END IF;
          END CHECK;
     BEGIN
          TEST ("C74004A", "CHECK THAT OPERATIONS DEPENDING ON THE " &
                           "FULL DECLARATION OF A PRIVATE TYPE ARE " &
                           "AVAILABLE WITHIN THE PACKAGE BODY");

          X1 := 10;
          X2 := 5;

          X3 := X1 + X2;

          IF X3 /= 15 THEN
               FAILED ("IMPROPER RESULT FROM ADDITION OPERATOR");
          END IF;

          X3 := X1 - X2;

          IF X3 /= 5 THEN
               FAILED ("IMPROPER RESULT FROM SUBTRACTION OPERATOR");
          END IF;

          X3 := X1 * X2;

          IF X3 /= 50 THEN
               FAILED ("IMPROPER RESULT FROM MULTIPLICATION OPERATOR");
          END IF;

          X3 := X1 / X2;

          IF X3 /= 2 THEN
               FAILED ("IMPROPER RESULT FROM DIVISION OPERATOR");
          END IF;

          X3 := X1 ** 2;

          IF X3 /= 100 THEN
               FAILED ("IMPROPER RESULT FROM EXPONENTIATION OPERATOR");
          END IF;

          BOOL := X1 < X2;

          IF BOOL THEN
               FAILED ("IMPROPER RESULT FROM LESS THAN OPERATOR");
          END IF;

          BOOL := X1 > X2;

          IF NOT BOOL THEN
               FAILED ("IMPROPER RESULT FROM GREATER THAN OPERATOR");
          END IF;

          BOOL := X1 <= X2;

          IF BOOL THEN
               FAILED ("IMPROPER RESULT FROM LESS THAN OR EQUAL TO " &
                       "OPERATOR");
          END IF;

          BOOL := X1 >= X2;

          IF NOT BOOL THEN
               FAILED ("IMPROPER RESULT FROM GREATER THAN OR EQUAL " &
                       "TO OPERATOR");
          END IF;

          X3 := X1 MOD X2;

          IF X3 /= 0 THEN
               FAILED ("IMPROPER RESULT FROM MOD OPERATOR");
          END IF;

          X3 := X1 REM X2;

          IF X3 /= 0 THEN
               FAILED ("IMPROPER RESULT FROM REM OPERATOR");
          END IF;

          X3 := ABS(X1);

          IF X3 /= 10 THEN
               FAILED ("IMPROPER RESULT FROM ABS OPERATOR - 1");
          END IF;

          X1 := -10;

          X3 := ABS(X1);

          IF X3 /= 10 THEN
               FAILED ("IMPROPER RESULT FROM ABS OPERATOR - 2");
          END IF;

          X3 := PR'BASE'FIRST;

          IF X3 /= PR(INTEGER'FIRST) THEN
               FAILED ("IMPROPER RESULT FROM 'BASE'FIRST");
          END IF;

          X3 := PR'FIRST;

          IF X3 /= PR(INTEGER'FIRST) THEN
               FAILED ("IMPROPER RESULT FROM 'FIRST");
          END IF;

          VAL := PR'WIDTH;

          IF NOT EQUAL(VAL,INTEGER'WIDTH) THEN
               FAILED ("IMPROPER RESULT FROM 'WIDTH");
          END IF;

          VAL := PR'POS(X3);

          IF NOT EQUAL(VAL,INTEGER'FIRST) THEN
               FAILED ("IMPROPER RESULT FROM 'POS");
          END IF;

          X3 := PR'VAL(VAL);

          IF X3 /= PR(INTEGER'FIRST) THEN
               FAILED ("IMPROPER RESULT FROM 'VAL");
          END IF;

          X3 := PR'SUCC(X2);

          IF X3 /= 6 THEN
               FAILED ("IMPROPER RESULT FROM 'SUCC");
          END IF;

          X3 := PR'PRED(X2);

          IF X3 /= 4 THEN
               FAILED ("IMPROPER RESULT FROM 'PRED");
          END IF;

          ST := PR'IMAGE(X3);

          IF ST /= INTEGER'IMAGE(INTEGER(X3)) THEN
               FAILED ("IMPROPER RESULT FROM 'IMAGE");
          END IF;

          X3 := PR'VALUE(ST);

          IF X3 /= PR(INTEGER'VALUE(ST)) THEN
               FAILED ("IMPROPER RESULT FROM 'VALUE");
          END IF;

          CHECK ((TRUE,FALSE,TRUE,FALSE,TRUE));

          IF O1(2) /= IDENT_INT(2) THEN
               FAILED ("IMPROPER VALUE FROM INDEXING");
          END IF;

          IF O1(2..4) /= (2,3,4) THEN
               FAILED ("IMPROPER VALUES FROM SLICING");
          END IF;

          IF VAL IN O1'RANGE THEN
               FAILED ("IMPROPER RESULT FROM 'RANGE");
          END IF;

          VAL := O1'LENGTH;

          IF NOT EQUAL(VAL,5) THEN
               FAILED ("IMPROPER RESULT FROM 'LENGTH");
          END IF;

          Y3 := Y1(1..2) & Y2(3..5);

          IF Y3 /= (FALSE,TRUE,TRUE,TRUE,TRUE) THEN
               FAILED ("IMPROPER RESULT FROM CATENATION");
          END IF;

          Y3 := NOT Y1;

          IF Y3 /= (TRUE,FALSE,TRUE,FALSE,TRUE) THEN
               FAILED ("IMPROPER RESULT FROM NOT OPERATOR");
          END IF;

          Y3 := Y1 AND Y2;

          IF Y3 /= (FALSE,TRUE,FALSE,TRUE,FALSE) THEN
               FAILED ("IMPROPER RESULT FROM AND OPERATOR");
          END IF;

          Y3 := Y1 OR Y2;

          IF Y3 /= (TRUE,TRUE,TRUE,TRUE,TRUE) THEN
               FAILED ("IMPROPER RESULT FROM OR OPERATOR");
          END IF;

          Y3 := Y1 XOR Y2;

          IF Y3 /= (TRUE,FALSE,TRUE,FALSE,TRUE) THEN
               FAILED ("IMPROPER RESULT FROM XOR OPERATOR");
          END IF;

          VAL := Z1.COMP1;

          IF NOT EQUAL(VAL,1) THEN
               FAILED ("IMPROPER RESULT FROM SELECTION OF RECORD " &
                       "COMPONENTS");
          END IF;

          W1 := NEW INTEGER'(0);

          IF NOT EQUAL(W1.ALL,0) THEN
               FAILED ("IMPROPER RESULT FROM ALLOCATION");
          END IF;

          W1 := NULL;

          IF W1 /= NULL THEN
               FAILED ("IMPROPER RESULT FROM NULL LITERAL");
          END IF;

          VAL := W2.ALL;

          IF NOT EQUAL(VAL,0) THEN
               FAILED ("IMPROPER RESULT FROM SELECTED COMPONENT");
          END IF;

          BOOL := V1'CALLABLE;

          IF NOT BOOL THEN
               FAILED ("IMPROPER RESULT FROM 'CALLABLE");
          END IF;

          BOOL := V1'TERMINATED;

          IF BOOL THEN
               FAILED ("IMPROPER RESULT FROM 'TERMINATED");
          END IF;

          V1.ONE(VAL);

          IF NOT EQUAL(VAL,10) THEN
               FAILED ("IMPROPER RESULT RETURNED FROM ENTRY SELECTION");
          END IF;

          IF NOT (FLT(1.0) IN FLT) THEN
               FAILED ("IMPROPER RESULT FROM IMPLICIT CONVERSION");
          END IF;

          VAL := FLT'DIGITS;

          IF NOT EQUAL(VAL,5) THEN
               FAILED ("IMPROPER RESULT FROM 'DIGITS");
          END IF;

          BOOL := FLT'MACHINE_ROUNDS;

          BOOL := FLT'MACHINE_OVERFLOWS;

          VAL := FLT'MACHINE_RADIX;

          VAL := FLT'MACHINE_MANTISSA;

          VAL := FLT'MACHINE_EMAX;

          VAL := FLT'MACHINE_EMIN;

          FVAL := FIX'DELTA;

          IF FVAL /= 2.0**(-1) THEN
               FAILED ("IMPROPER RESULT FROM 'DELTA");
          END IF;

          VAL := FIX'FORE;

          VAL := FIX'AFT;

     END P;

     USE P;

BEGIN
     RESULT;
END C74004A;
