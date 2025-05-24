-- CD2A31A.ADA

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
--     ARE NOT AFFECTED BY THE REPRESENTATION CLAUSE.

-- HISTORY:
--     JET 08/06/87  CREATED ORIGINAL TEST.
--     DHH 04/06/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   SIZE CLAUSE VALUE TO 9, AND ADDED REPRESENTAION
--                   CLAUSE CHECK.
--     JRL 03/27/92  ELIMINATED REDUNDANT TESTING.

WITH REPORT;  USE REPORT;
WITH LENGTH_CHECK;                      -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD2A31A IS

     BASIC_SIZE : CONSTANT := 9;

     TYPE INT IS RANGE -100 .. 100;

     FOR INT'SIZE USE BASIC_SIZE;

     I1 : INT := -100;
     I2 : INT :=    0;
     I3 : INT :=  100;

     TYPE ARRAY_TYPE IS ARRAY (INTEGER RANGE -1 .. 1) OF INT;
     INTARRAY : ARRAY_TYPE := (-100, 0, 100);

     TYPE REC_TYPE IS RECORD
          COMPN : INT := -100;
          COMPZ : INT :=    0;
          COMPP : INT :=  100;
     END RECORD;

     IREC : REC_TYPE;

     PROCEDURE CHECK_1 IS NEW LENGTH_CHECK (INT);

     FUNCTION IDENT (I : INT) RETURN INT IS
     BEGIN
          IF EQUAL (0,0) THEN
               RETURN I;
          ELSE
               RETURN 0;
          END IF;
     END IDENT;

     PROCEDURE PROC (PIN,  PIP  :        INT;
                     PIOZ, PIOP : IN OUT INT;
                     POP        :    OUT INT) IS

     BEGIN
          IF PIN'SIZE < IDENT_INT (BASIC_SIZE) THEN
               FAILED ("INCORRECT VALUE FOR PIN'SIZE");
          END IF;

          IF NOT ((PIN < IDENT (0))             AND
                  (IDENT (PIP) > IDENT (PIOZ))  AND
                  (PIOZ <= IDENT (1))           AND
                  (IDENT (100) = PIP))          THEN
               FAILED ("INCORRECT RESULTS FOR RELATIONAL " &
                       "OPERATORS - 1");
          END IF;

          IF NOT (((PIN + PIP)   = PIOZ)       AND
                  ((PIP - PIOZ)  = PIOP)       AND
                  ((PIOP * PIOZ) = PIOZ)       AND
                  ((PIOZ / PIN)  = PIOZ)       AND
                  ((PIN ** 1)    = PIN)        AND
                  ((PIN REM 9)   = IDENT (-1)) AND
                  ((PIP MOD 9)   = IDENT (1))) THEN
               FAILED ("INCORRECT RESULTS FOR BINARY ARITHMETIC " &
                       "OPERATORS - 1");
          END IF;

          IF INT'VAL (-100) /= IDENT (PIN)  OR
             INT'VAL (0)    /= IDENT (PIOZ) OR
             INT'VAL (100)  /= IDENT (PIOP) THEN
               FAILED ("INCORRECT VALUE FOR INT'VAL - 1");
          END IF;

          IF INT'PRED (PIOZ) /= IDENT (-1) OR
             INT'PRED (PIP)  /= IDENT (99) THEN
               FAILED ("INCORRECT VALUE FOR INT'PRED - 1");
          END IF;

          IF INT'VALUE ("-100") /= IDENT (PIN)  OR
             INT'VALUE ("0")    /= IDENT (PIOZ) OR
             INT'VALUE ("100")  /= IDENT (PIOP) THEN
               FAILED ("INCORRECT VALUE FOR INT'VALUE - 1");
          END IF;

          POP := 100;

     END PROC;

BEGIN
     TEST ("CD2A31A", "CHECK THAT WHEN A SIZE SPECIFICATION IS " &
                      "GIVEN FOR AN INTEGER TYPE, THEN " &
                      "OPERATIONS ON VALUES OF SUCH A TYPE ARE " &
                      "NOT AFFECTED BY THE REPRESENTATION CLAUSE");

     CHECK_1 (I1, 9, "INT");
     PROC (-100, 100, I2, I3, I3);

     IF INT'SIZE /= IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR INT'SIZE");
     END IF;

     IF I1'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR I1'SIZE");
     END IF;

     FOR I IN IDENT (I1) .. IDENT (I3) LOOP
          IF NOT (I IN I1 .. I3) OR
             (I NOT IN IDENT(-100) .. IDENT(100)) THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                       "OPERATORS - 2");
          END IF;
     END LOOP;

     IF NOT ((+I1 = I1)     AND
             (-I3 = I1)      AND
             (ABS I1 = I3)) THEN
          FAILED ("INCORRECT RESULTS FOR UNARY ARITHMETIC " &
                  "OPERATORS - 2");
     END IF;

     IF INT'FIRST /= IDENT (-100) THEN
          FAILED ("INCORRECT VALUE FOR INT'FIRST - 2");
     END IF;

     IF INT'POS (I1) /= IDENT_INT (-100) OR
        INT'POS (I2) /= IDENT_INT (   0) OR
        INT'POS (I3) /= IDENT_INT ( 100) THEN
          FAILED ("INCORRECT VALUE FOR INT'POS - 2");
     END IF;

     IF INT'SUCC (I1) /= IDENT (-99) OR
        INT'SUCC (I2) /= IDENT (1)   THEN
          FAILED ("INCORRECT VALUE FOR INT'SUCC - 2");
     END IF;

     IF INT'IMAGE (I1) /= IDENT_STR ("-100") OR
        INT'IMAGE (I2) /= IDENT_STR (" 0")    OR
        INT'IMAGE (I3) /= IDENT_STR (" 100")  THEN
          FAILED ("INCORRECT VALUE FOR INT'IMAGE - 2");
     END IF;

     IF INTARRAY(0)'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR INTARRAY(0)'SIZE");
     END IF;

     IF NOT ((INTARRAY(-1) < IDENT (0))                       AND
             (IDENT (INTARRAY (1)) > IDENT (INTARRAY(0)))     AND
             (INTARRAY(0) <= IDENT (0))                       AND
             (IDENT (100) = INTARRAY (1)))                    THEN
          FAILED ("INCORRECT RESULTS FOR RELATIONAL OPERATORS - 3");
     END IF;

     FOR I IN IDENT (INTARRAY(-1)) .. IDENT (INTARRAY(1)) LOOP
          IF NOT (I IN INTARRAY(-1) .. INTARRAY(1)) OR
             (I NOT IN IDENT(-100) .. IDENT(100)) THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                       "OPERATORS - 3");
          END IF;
     END LOOP;

     IF NOT (((INTARRAY(-1) + INTARRAY( 1)) = INTARRAY( 0)) AND
             ((INTARRAY( 0) - INTARRAY( 1)) = INTARRAY(-1)) AND
             ((INTARRAY( 1) * INTARRAY( 0)) = INTARRAY( 0)) AND
             ((INTARRAY( 0) / INTARRAY(-1)) = INTARRAY( 0)) AND
             ((INTARRAY(-1) ** 1)           = INTARRAY(-1)) AND
             ((INTARRAY(-1) REM 9)          = IDENT (-1))   AND
             ((INTARRAY( 1) MOD 9)          = IDENT ( 1)))  THEN
          FAILED ("INCORRECT RESULTS FOR BINARY ARITHMETIC " &
                  "OPERATORS - 3");
     END IF;

     IF INT'POS (INTARRAY (-1)) /= IDENT_INT (-100) OR
        INT'POS (INTARRAY ( 0)) /= IDENT_INT (   0) OR
        INT'POS (INTARRAY ( 1)) /= IDENT_INT ( 100) THEN
          FAILED ("INCORRECT VALUE FOR INT'POS - 3");
     END IF;

     IF INT'SUCC (INTARRAY (-1)) /= IDENT (-99) OR
        INT'SUCC (INTARRAY ( 0)) /= IDENT (1)   THEN
          FAILED ("INCORRECT VALUE FOR INT'SUCC - 3");
     END IF;

     IF INT'IMAGE (INTARRAY (-1)) /= IDENT_STR ("-100") OR
        INT'IMAGE (INTARRAY ( 0)) /= IDENT_STR (" 0")    OR
        INT'IMAGE (INTARRAY ( 1)) /= IDENT_STR (" 100")  THEN
          FAILED ("INCORRECT VALUE FOR INT'IMAGE - 3");
     END IF;

     IF IREC.COMPP'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR IREC.COMPP'SIZE");
     END IF;

     IF NOT ((IREC.COMPN < IDENT (0))                      AND
             (IDENT (IREC.COMPP) > IDENT (IREC.COMPZ))     AND
             (IREC.COMPZ <= IDENT (0))                     AND
             (IDENT (100) = IREC.COMPP))                   THEN
          FAILED ("INCORRECT RESULTS FOR RELATIONAL OPERATORS - 4");
     END IF;

     FOR I IN IDENT (IREC.COMPN) .. IDENT (IREC.COMPP) LOOP
          IF NOT (I IN IREC.COMPN .. IREC.COMPP) OR
             (I NOT IN IDENT(-100) .. IDENT(100)) THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                       "OPERATORS - 4");
          END IF;
     END LOOP;

     IF NOT ((+IREC.COMPN = IREC.COMPN)     AND
             (-IREC.COMPP = IREC.COMPN)     AND
             (ABS IREC.COMPN = IREC.COMPP)) THEN
          FAILED ("INCORRECT RESULTS FOR UNARY ARITHMETIC " &
                  "OPERATORS - 4");
     END IF;

     IF INT'VAL (-100) /= IDENT (IREC.COMPN) OR
        INT'VAL (   0) /= IDENT (IREC.COMPZ) OR
        INT'VAL ( 100) /= IDENT (IREC.COMPP) THEN
          FAILED ("INCORRECT VALUE FOR INT'VAL - 4");
     END IF;

     IF INT'PRED (IREC.COMPZ) /= IDENT (-1) OR
        INT'PRED (IREC.COMPP) /= IDENT (99) THEN
          FAILED ("INCORRECT VALUE FOR INT'PRED - 4");
     END IF;

     IF INT'VALUE ("-100") /= IDENT (IREC.COMPN) OR
        INT'VALUE (   "0") /= IDENT (IREC.COMPZ) OR
        INT'VALUE ( "100") /= IDENT (IREC.COMPP) THEN
          FAILED ("INCORRECT VALUE FOR INT'VALUE - 4");
     END IF;

     RESULT;
END CD2A31A;
