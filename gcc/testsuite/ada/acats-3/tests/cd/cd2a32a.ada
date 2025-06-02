-- CD2A32A.ADA

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
--     WITH THE SMALLEST APPROPRIATE SIGNED SIZE ARE NOT
--     AFFECTED BY THE REPRESENTATION CLAUSE.

-- HISTORY:
--     JET 08/12/87  CREATED ORIGINAL TEST.
--     DHH 04/10/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   SIZE CLAUSE VALUE TO 7, CHANGED OPERATOR ON 'SIZE
--                   CHECKS AND ADDED REPRESENTAION CLAUSE CHECK.
--     RJW 03/28/90  REMOVED ERRONEOUS REFERENCES TO LENGTH_CHECK.
--     JRL 03/27/92  ELIMINATED REDUNDANT TESTING.

WITH REPORT;  USE REPORT;
WITH LENGTH_CHECK;                      -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD2A32A IS

     BASIC_SIZE : CONSTANT := 7;

     TYPE INT IS RANGE -63 .. 63;

     FOR INT'SIZE USE BASIC_SIZE;

     I1 : INT := -63;
     I2 : INT :=    0;
     I3 : INT :=  63;

     TYPE ARRAY_TYPE IS ARRAY (INTEGER RANGE -1 .. 1) OF INT;
     PRAGMA PACK (ARRAY_TYPE);
     INTARRAY : ARRAY_TYPE := (-63, 0, 63);

     TYPE REC_TYPE IS RECORD
          COMPN : INT := -63;
          COMPZ : INT :=    0;
          COMPP : INT :=  63;
     END RECORD;
     PRAGMA PACK (REC_TYPE);

     IREC : REC_TYPE;

     FUNCTION IDENT (I : INT) RETURN INT IS
     BEGIN
          IF EQUAL (0,0) THEN
               RETURN I;
          ELSE
               RETURN 0;
          END IF;
     END IDENT;

     PROCEDURE CHECK_1 IS NEW LENGTH_CHECK (INT);


     PROCEDURE PROC (PIN,  PIP  :        INT;
                     PIOZ, PIOP : IN OUT INT;
                     POP        :    OUT INT) IS

     BEGIN
          IF PIN'SIZE < IDENT_INT (BASIC_SIZE) THEN
               FAILED ("INCORRECT VALUE FOR PIN'SIZE");
          END IF;

          FOR P1 IN IDENT (PIN) .. IDENT (PIOP) LOOP
               IF NOT (P1 IN PIN .. PIP) OR
                  (P1 NOT IN IDENT(-63) .. IDENT(63)) THEN
                    FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                       "OPERATORS - 1");
               END IF;
          END LOOP;

          IF NOT ((+PIP = PIOP)     AND
                  (-PIN = PIP)      AND
                  (ABS PIN = PIOP)) THEN
               FAILED ("INCORRECT RESULTS FOR UNARY ARITHMETIC " &
                       "OPERATORS - 1");
          END IF;

          IF INT'VAL (-63) /= IDENT (PIN)  OR
             INT'VAL (0)    /= IDENT (PIOZ) OR
             INT'VAL (63)  /= IDENT (PIOP) THEN
               FAILED ("INCORRECT VALUE FOR INT'VAL - 1");
          END IF;

          IF INT'PRED (PIOZ) /= IDENT (-1) OR
             INT'PRED (PIP)  /= IDENT (62) THEN
               FAILED ("INCORRECT VALUE FOR INT'PRED - 1");
          END IF;

          IF INT'VALUE ("-63") /= IDENT (PIN)  OR
             INT'VALUE ("0")    /= IDENT (PIOZ) OR
             INT'VALUE ("63")  /= IDENT (PIOP) THEN
               FAILED ("INCORRECT VALUE FOR INT'VALUE - 1");
          END IF;

          POP := 63;

     END PROC;

BEGIN
     TEST ("CD2A32A", "CHECK THAT WHEN A SIZE SPECIFICATION IS " &
                      "GIVEN FOR AN INTEGER TYPE, THEN " &
                      "OPERATIONS ON VALUES OF SUCH A TYPE WITH " &
                      "THE SMALLEST APPROPRIATE SIGNED SIZE ARE " &
                      "NOT AFFECTED BY THE REPRESENTATION CLAUSE");

     CHECK_1 (I1, 7, "INT");

     PROC (-63, 63, I2, I3, I3);

     IF INT'SIZE /= IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR INT'SIZE");
     END IF;

     IF I1'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR I1'SIZE");
     END IF;

     IF NOT ((I1 < IDENT (0))              AND
             (IDENT (I3) > IDENT (I2))     AND
             (I2 <= IDENT (0))             AND
             (IDENT (63) = I3))           THEN
          FAILED ("INCORRECT RESULTS FOR RELATIONAL OPERATORS - 2");
     END IF;

     IF NOT (((I1 + I3)   = I2)         AND
             ((I2 - I3)   = I1)         AND
             ((I3 * I2)   = I2)         AND
             ((I2 / I1)   = I2)         AND
             ((I1 ** 1)   = I1)         AND
             ((I1 REM 10) = IDENT (-3)) AND
             ((I3 MOD 10) = IDENT (3))) THEN
          FAILED ("INCORRECT RESULTS FOR BINARY ARITHMETIC " &
                  "OPERATORS - 2");
     END IF;

     IF INT'FIRST /= IDENT (-63) THEN
          FAILED ("INCORRECT VALUE FOR INT'FIRST - 2");
     END IF;

     IF INT'POS (I1) /= IDENT_INT (-63) OR
        INT'POS (I2) /= IDENT_INT (   0) OR
        INT'POS (I3) /= IDENT_INT ( 63) THEN
          FAILED ("INCORRECT VALUE FOR INT'POS - 2");
     END IF;

     IF INT'SUCC (I1) /= IDENT (-62) OR
        INT'SUCC (I2) /= IDENT (1)   THEN
          FAILED ("INCORRECT VALUE FOR INT'SUCC - 2");
     END IF;

     IF INT'IMAGE (I1) /= IDENT_STR ("-63") OR
        INT'IMAGE (I2) /= IDENT_STR (" 0")    OR
        INT'IMAGE (I3) /= IDENT_STR (" 63")  THEN
          FAILED ("INCORRECT VALUE FOR INT'IMAGE - 2");
     END IF;

     IF INTARRAY(0)'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR INTARRAY(0)'SIZE");
     END IF;

     IF NOT ((INTARRAY(-1) < IDENT (0))                       AND
             (IDENT (INTARRAY (1)) > IDENT (INTARRAY(0)))     AND
             (INTARRAY(0) <= IDENT (0))                       AND
             (IDENT (63) = INTARRAY (1)))                    THEN
          FAILED ("INCORRECT RESULTS FOR RELATIONAL OPERATORS - 3");
     END IF;

     FOR I IN IDENT (INTARRAY(-1)) .. IDENT (INTARRAY(1)) LOOP
          IF NOT (I IN INTARRAY(-1) .. INTARRAY(1)) OR
             (I NOT IN IDENT(-63) .. IDENT(63)) THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                       "OPERATORS - 3");
          END IF;
     END LOOP;

     IF NOT ((+INTARRAY(-1) = INTARRAY(-1))     AND
             (-INTARRAY( 1) = INTARRAY(-1))     AND
             (ABS INTARRAY(-1) = INTARRAY(1)))  THEN
          FAILED ("INCORRECT RESULTS FOR UNARY ARITHMETIC " &
                  "OPERATORS - 3");
     END IF;

     IF INT'VAL (-63) /= IDENT (INTARRAY (-1)) OR
        INT'VAL (   0) /= IDENT (INTARRAY ( 0)) OR
        INT'VAL ( 63) /= IDENT (INTARRAY ( 1)) THEN
          FAILED ("INCORRECT VALUE FOR INT'VAL - 3");
     END IF;

     IF INT'PRED (INTARRAY (0)) /= IDENT (-1) OR
        INT'PRED (INTARRAY (1)) /= IDENT (62) THEN
          FAILED ("INCORRECT VALUE FOR INT'PRED - 3");
     END IF;

     IF INT'VALUE ("-63") /= IDENT (INTARRAY (-1)) OR
        INT'VALUE ("0")    /= IDENT (INTARRAY ( 0)) OR
        INT'VALUE ("63")  /= IDENT (INTARRAY ( 1)) THEN
          FAILED ("INCORRECT VALUE FOR INT'VALUE - 3");
     END IF;

     IF IREC.COMPP'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR IREC.COMPP'SIZE");
     END IF;

     IF NOT ((IREC.COMPN < IDENT (0))                      AND
             (IDENT (IREC.COMPP) > IDENT (IREC.COMPZ))     AND
             (IREC.COMPZ <= IDENT (0))                     AND
             (IDENT (63) = IREC.COMPP))                   THEN
          FAILED ("INCORRECT RESULTS FOR RELATIONAL OPERATORS - 4");
     END IF;

     FOR I IN IDENT (IREC.COMPN) .. IDENT (IREC.COMPP) LOOP
          IF NOT (I IN IREC.COMPN .. IREC.COMPP) OR
             (I NOT IN IDENT(-63) .. IDENT(63)) THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                       "OPERATORS - 4");
          END IF;
     END LOOP;

     IF NOT (((IREC.COMPN + IREC.COMPP) = IREC.COMPZ)  AND
             ((IREC.COMPZ - IREC.COMPP) = IREC.COMPN)  AND
             ((IREC.COMPP * IREC.COMPZ) = IREC.COMPZ)  AND
             ((IREC.COMPZ / IREC.COMPN) = IREC.COMPZ)  AND
             ((IREC.COMPN ** 1)         = IREC.COMPN)  AND
             ((IREC.COMPN REM 10)       = IDENT (-3))  AND
             ((IREC.COMPP MOD 10)       = IDENT ( 3))) THEN
          FAILED ("INCORRECT RESULTS FOR BINARY ARITHMETIC " &
                  "OPERATORS - 4");
     END IF;

     IF INT'POS (IREC.COMPN) /= IDENT_INT (-63) OR
        INT'POS (IREC.COMPZ) /= IDENT_INT (   0) OR
        INT'POS (IREC.COMPP) /= IDENT_INT ( 63) THEN
          FAILED ("INCORRECT VALUE FOR INT'POS - 4");
     END IF;

     IF INT'SUCC (IREC.COMPN) /= IDENT (-62) OR
        INT'SUCC (IREC.COMPZ) /= IDENT (  1) THEN
          FAILED ("INCORRECT VALUE FOR INT'SUCC - 4");
     END IF;

     IF INT'IMAGE (IREC.COMPN) /= IDENT_STR ("-63") OR
        INT'IMAGE (IREC.COMPZ) /= IDENT_STR (" 0")    OR
        INT'IMAGE (IREC.COMPP) /= IDENT_STR (" 63")  THEN
          FAILED ("INCORRECT VALUE FOR INT'IMAGE - 4");
     END IF;

     RESULT;
END CD2A32A;
