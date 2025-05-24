-- CD2A32E.ADA

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
--     CHECK THAT WHEN A SIZE SPECIFICATION IS GIVEN FOR AN
--     INTEGER TYPE, THEN OPERATIONS ON VALUES OF SUCH A TYPE
--     WITH THE SMALLEST APPROPRIATE UNSIGNED SIZE ARE NOT
--     AFFECTED BY THE REPRESENTATION CLAUSE.

-- HISTORY:
--     JET 08/12/87  CREATED ORIGINAL TEST.
--     DHH 04/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   SIZE CLAUSE VALUE TO 7, AND CHANGED OPERATOR ON
--                   'SIZE CHECKS.
--     JRL 03/27/92  ELIMINATED REDUNDANT TESTING.

WITH REPORT;  USE REPORT;
PROCEDURE CD2A32E IS

     BASIC_SIZE : CONSTANT := 7;

     TYPE INT IS RANGE 0 .. 126;

     FOR INT'SIZE USE BASIC_SIZE;

     I0 : INT :=   0;
     I1 : INT :=  63;
     I2 : INT := 126;

     TYPE ARRAY_TYPE IS ARRAY (INTEGER RANGE 0 .. 2) OF INT;
     INTARRAY : ARRAY_TYPE := (0, 63, 126);

     TYPE REC_TYPE IS RECORD
          COMP0 : INT :=   0;
          COMP1 : INT := 63;
          COMP2 : INT := 126;
     END RECORD;

     IREC : REC_TYPE;

     FUNCTION IDENT (I : INT) RETURN INT IS
     BEGIN
          IF EQUAL (0,0) THEN
               RETURN I;
          ELSE
               RETURN 0;
          END IF;
     END IDENT;

     PROCEDURE PROC (PI0,  PI2  :        INT;
                     PIO1, PIO2 : IN OUT INT;
                     PO2        :    OUT INT) IS

     BEGIN
          IF PI0'SIZE < IDENT_INT (BASIC_SIZE) THEN
               FAILED ("INCORRECT VALUE FOR PI0'SIZE");
          END IF;

          IF NOT ((PI0 < IDENT (1))             AND
                  (IDENT (PI2) > IDENT (PIO1))  AND
                  (PIO1 <= IDENT (63))         AND
                  (IDENT (126) = PI2))          THEN
               FAILED ("INCORRECT RESULTS FOR RELATIONAL " &
                       "OPERATORS - 1");
          END IF;

          IF NOT (((PI0 + PI2)   = PIO2)        AND
                  ((PI2 - PIO1)  = PIO1)        AND
                  ((PIO1 * IDENT (2)) = PI2)    AND
                  ((PIO2 / PIO1) = IDENT (2))   AND
                  ((PIO1 ** 1)   = IDENT (63))  AND
                  ((PIO2 REM 10)  = IDENT (6))  AND
                  ((PIO1 MOD 10)  = IDENT (3))) THEN
               FAILED ("INCORRECT RESULTS FOR BINARY ARITHMETIC " &
                       "OPERATORS - 1");
          END IF;

          IF INT'POS (PI0)  /= IDENT_INT (0)   OR
             INT'POS (PIO1) /= IDENT_INT (63) OR
             INT'POS (PI2)  /= IDENT_INT (126) THEN
               FAILED ("INCORRECT VALUE FOR INT'POS - 1");
          END IF;

          IF INT'SUCC (PI0)  /= IDENT (1)   OR
             INT'SUCC (PIO1) /= IDENT (64) THEN
               FAILED ("INCORRECT VALUE FOR INT'SUCC - 1");
          END IF;

          IF INT'IMAGE (PI0)  /= IDENT_STR (" 0")   OR
             INT'IMAGE (PIO1) /= IDENT_STR (" 63") OR
             INT'IMAGE (PI2)  /= IDENT_STR (" 126") THEN
               FAILED ("INCORRECT VALUE FOR INT'IMAGE - 1");
          END IF;

          PO2 := 126;

     END PROC;

BEGIN
     TEST ("CD2A32E", "CHECK THAT WHEN A SIZE SPECIFICATION IS " &
                      "GIVEN FOR AN INTEGER TYPE, THEN " &
                      "OPERATIONS ON VALUES OF SUCH A TYPE WITH " &
                      "THE SMALLEST APPROPRIATE UNSIGNED SIZE ARE " &
                      "NOT AFFECTED BY THE REPRESENTATION CLAUSE");

     PROC (0, 126, I1, I2, I2);

     IF INT'SIZE /= IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR INT'SIZE");
     END IF;

     IF I1'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR I1'SIZE");
     END IF;

     FOR I IN IDENT (I0) .. IDENT (I2) LOOP
          IF NOT (I IN I0 .. I2) OR
             (I NOT IN IDENT(0) .. IDENT(126)) THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                       "OPERATORS - 2");
          END IF;
     END LOOP;

     IF NOT ((+I2 = I2)     AND
             (-I1 = -63)   AND
             (ABS I2 = I2)) THEN
          FAILED ("INCORRECT RESULTS FOR UNARY ARITHMETIC " &
                  "OPERATORS - 2");
     END IF;

     IF INT'VAL (0)   /= IDENT (I0) OR
        INT'VAL (63) /= IDENT (I1) OR
        INT'VAL (126) /= IDENT (I2) THEN
          FAILED ("INCORRECT VALUE FOR INT'VAL - 2");
     END IF;

     IF INT'PRED (I1) /= IDENT (62)  OR
        INT'PRED (I2) /= IDENT (125) THEN
          FAILED ("INCORRECT VALUE FOR INT'PRED - 2");
     END IF;

     IF INT'VALUE ("0")   /= IDENT (I0) OR
        INT'VALUE ("63") /= IDENT (I1) OR
        INT'VALUE ("126") /= IDENT (I2) THEN
          FAILED ("INCORRECT VALUE FOR INT'VALUE - 2");
     END IF;

     IF INTARRAY(1)'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR INTARRAY(1)'SIZE");
     END IF;

     IF NOT ((INTARRAY(0) < IDENT (1))                   AND
             (IDENT (INTARRAY(2)) > IDENT (INTARRAY(1))) AND
             (INTARRAY(1) <= IDENT (63))                AND
             (IDENT (126) = INTARRAY(2)))                THEN
          FAILED ("INCORRECT RESULTS FOR RELATIONAL " &
                  "OPERATORS - 3");
     END IF;

     FOR I IN IDENT (INTARRAY(0)) .. IDENT (INTARRAY(2)) LOOP
          IF NOT (I IN INTARRAY(0) .. INTARRAY(2)) OR
             (I NOT IN IDENT(0) .. IDENT(126))     THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                       "OPERATORS - 3");
          END IF;
     END LOOP;

     IF NOT (((INTARRAY(0) + INTARRAY(2))  = INTARRAY(2)) AND
             ((INTARRAY(2) - INTARRAY(1))  = INTARRAY(1)) AND
             ((INTARRAY(1) * IDENT (2))    = INTARRAY(2)) AND
             ((INTARRAY(2) / INTARRAY(1))  = IDENT (2))   AND
             ((INTARRAY(1) ** 1)           = IDENT (63))  AND
             ((INTARRAY(2) REM 10)         = IDENT (6))   AND
             ((INTARRAY(1) MOD 10)         = IDENT (3)))  THEN
          FAILED ("INCORRECT RESULTS FOR BINARY ARITHMETIC " &
                  "OPERATORS - 3");
     END IF;

     IF INT'POS (INTARRAY(0)) /= IDENT_INT (0)   OR
        INT'POS (INTARRAY(1)) /= IDENT_INT (63) OR
        INT'POS (INTARRAY(2)) /= IDENT_INT (126) THEN
          FAILED ("INCORRECT VALUE FOR INT'POS - 3");
     END IF;

     IF INT'SUCC (INTARRAY(0)) /= IDENT (1)   OR
        INT'SUCC (INTARRAY(1)) /= IDENT (64) THEN
          FAILED ("INCORRECT VALUE FOR INT'SUCC - 3");
     END IF;

     IF INT'IMAGE (INTARRAY(0)) /= IDENT_STR (" 0")   OR
        INT'IMAGE (INTARRAY(1)) /= IDENT_STR (" 63") OR
        INT'IMAGE (INTARRAY(2)) /= IDENT_STR (" 126") THEN
          FAILED ("INCORRECT VALUE FOR INT'IMAGE - 3");
     END IF;

     IF IREC.COMP2'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR IREC.COMP2'SIZE");
     END IF;

     IF NOT ((IREC.COMP0 < IDENT (1))                  AND
             (IDENT (IREC.COMP2) > IDENT (IREC.COMP1)) AND
             (IREC.COMP1 <= IDENT (63))               AND
             (IDENT (126) = IREC.COMP2))               THEN
          FAILED ("INCORRECT RESULTS FOR RELATIONAL " &
                  "OPERATORS - 4");
     END IF;

     FOR I IN IDENT (IREC.COMP0) .. IDENT (IREC.COMP2) LOOP
          IF NOT (I IN IREC.COMP0 .. IREC.COMP2) OR
             (I NOT IN IDENT(0) .. IDENT(126))   THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                       "OPERATORS - 4");
          END IF;
     END LOOP;

     IF NOT ((+IREC.COMP2 = IREC.COMP2)     AND
             (-IREC.COMP1 = -63)           AND
             (ABS IREC.COMP2 = IREC.COMP2)) THEN
          FAILED ("INCORRECT RESULTS FOR UNARY ARITHMETIC " &
                  "OPERATORS - 4");
     END IF;

     IF INT'VAL (0)   /= IDENT (IREC.COMP0) OR
        INT'VAL (63) /= IDENT (IREC.COMP1) OR
        INT'VAL (126) /= IDENT (IREC.COMP2) THEN
          FAILED ("INCORRECT VALUE FOR INT'VAL - 4");
     END IF;

     IF INT'PRED (IREC.COMP1) /= IDENT (62)  OR
        INT'PRED (IREC.COMP2) /= IDENT (125) THEN
          FAILED ("INCORRECT VALUE FOR INT'PRED - 4");
     END IF;

     IF INT'VALUE ("0")   /= IDENT (IREC.COMP0) OR
        INT'VALUE ("63") /= IDENT (IREC.COMP1) OR
        INT'VALUE ("126") /= IDENT (IREC.COMP2) THEN
          FAILED ("INCORRECT VALUE FOR INT'VALUE - 4");
     END IF;

     RESULT;

END CD2A32E;
