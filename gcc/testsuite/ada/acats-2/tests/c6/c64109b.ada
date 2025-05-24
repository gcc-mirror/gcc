-- C64109B.ADA

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
-- CHECK THAT ARRAYS THAT ARE COMPONENTS OF RECORDS ARE PASSED CORRECTLY
-- TO SUBPROGRAMS.  SPECIFICALLY,
--   (B) CHECK MULTIDIMENSIONAL ARRAYS.

-- CPP 8/20/84

WITH REPORT;  USE REPORT;
PROCEDURE C64109B IS

BEGIN
     TEST ("C64109B", "CHECK THAT ARRAYS WHICH ARE COMPONENTS OF " &
           "RECORDS ARE PASSED CORRECTLY TO SUBPROGRAMS - " &
           "MULTIDIMENSIONAL ARRAYS");

     DECLARE   -- (B)

          TYPE MULTI_TYPE IS ARRAY (POSITIVE RANGE <>, 
                                    POSITIVE RANGE <>) OF BOOLEAN;
          SUBTYPE MULTI_SUBTYPE IS MULTI_TYPE (1..2, 1..3);
          TYPE RECORD_TYPE IS
               RECORD
                    I : BOOLEAN;
                    A : MULTI_SUBTYPE;
               END RECORD;
          REC : RECORD_TYPE :=
                    (I => FALSE,
                     A => (1..2 => (1..3 => IDENT_BOOL(TRUE))));
          BOOL : BOOLEAN;

          PROCEDURE P1 (ARR : MULTI_TYPE) IS
          BEGIN
               IF ARR /= (1..2 => (1..3 => TRUE)) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY");
               END IF;

               IF ARR'FIRST /= 1 OR ARR'LAST /= IDENT_INT(2) THEN
                    FAILED ("FIRST DIM NOT CORRECT - IN PARAMETER");
               ELSIF ARR'FIRST(2) /= IDENT_INT(1) OR ARR'LAST(2) /= 3
                  THEN 
                    FAILED ("2ND DIM NOT CORRECT - IN PARAMETER");
               END IF;                  
          END P1;

          FUNCTION F1 (ARR : MULTI_TYPE) RETURN BOOLEAN IS
          BEGIN
               IF ARR /= (1..2 => (1..3 => TRUE)) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY TO FN");
               END IF;

               IF ARR'FIRST /= 1 OR ARR'LAST /= IDENT_INT(2) THEN
                    FAILED ("FIRST DIM NOT CORRECT - IN PARAMETER FN");
               ELSIF ARR'FIRST(2) /= IDENT_INT(1) OR ARR'LAST(2) /= 3
                  THEN 
                    FAILED ("2ND DIM NOT CORRECT - IN PARAMETER FN");
               END IF;                  
               RETURN TRUE;
          END F1;

          PROCEDURE P2 (ARR : IN OUT MULTI_TYPE) IS
          BEGIN
               IF ARR /= (1..2 => (1..3 => TRUE)) THEN
                    FAILED ("IN OUT PARAM NOT PASSED CORRECTLY");
               END IF;

               IF ARR'FIRST /= 1 OR ARR'LAST /= IDENT_INT(2) THEN
                    FAILED ("FIRST DIM NOT CORRECT - IN OUT PARAMETER");
               ELSIF ARR'FIRST(2) /= IDENT_INT(1) OR ARR'LAST(2) /= 3
                  THEN 
                    FAILED ("2ND DIM NOT CORRECT - IN OUT PARAMETER");
               END IF;                  
               ARR := (ARR'RANGE(1) => (ARR'RANGE(2) => FALSE));
          END P2;

          PROCEDURE P3 (ARR : OUT MULTI_TYPE) IS
          BEGIN
               FOR I IN 1 .. 2 LOOP
                    FOR J IN 1 .. 3 LOOP
                         IF (J MOD 2) = 0 THEN
                              ARR(I, J) := TRUE;
                         ELSE
                              ARR(I, J) := FALSE;
                         END IF;
                    END LOOP;
               END LOOP;

               IF ARR'FIRST /= 1 OR ARR'LAST /= IDENT_INT(2) THEN
                    FAILED ("FIRST DIM NOT CORRECT - OUT PARAMETER");
               ELSIF ARR'FIRST(2) /= IDENT_INT(1) OR ARR'LAST(2) /= 3
                  THEN 
                    FAILED ("2ND DIM NOT CORRECT - OUT PARAMETER");
               END IF;                  
          END P3;

     BEGIN     -- (B)

          P1 (REC.A);
          IF REC.A /= (1..2 => (1..3 => TRUE)) THEN
               FAILED ("IN PARAM CHANGED BY PROCEDURE");
          END IF;

          BOOL := F1 (REC.A);
          IF REC.A /= (1..2 => (1..3 => TRUE)) THEN
               FAILED ("IN PARAM CHANGED BY FUNCTION");
          END IF;

          P2 (REC.A);
          IF REC.A /= (1..2 => (1..3 => FALSE)) THEN
               FAILED ("IN OUT PARAM CHANGED BY PROCEDURE");
          END IF;

          P3 (REC.A);
          FOR I IN 1 .. 2 LOOP
               FOR J IN 1 .. 3 LOOP
                    IF (J MOD 2) = 0 THEN
                         IF REC.A(I, J) /= TRUE THEN
                              FAILED ("OUT PARAM RETURNED " &
                                      "INCORRECTLY - (B)");
                         END IF;
                    ELSE
                         IF REC.A(I, J) /= FALSE THEN
                              FAILED ("OUT PARAM RETURNED " &
                                      "INCORRECTLY - (B)2");
                         END IF;
                    END IF;
               END LOOP;
          END LOOP;

     END; -- (B)

     RESULT;
END C64109B;
