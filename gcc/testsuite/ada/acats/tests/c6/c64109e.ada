-- C64109E.ADA

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
--   (E) CHECK THE CASE WHERE THE FORMAL IS UNCONSTRAINED, AND ARRAYS
--       WITH DIFFERENT BOUNDS ARE PASSED AS ACTUALS.

-- CPP 8/20/84

WITH REPORT;  USE REPORT;
PROCEDURE C64109E IS

BEGIN
     TEST ("C64109E", "CHECK THAT ARRAYS WHICH ARE COMPONENTS OF " &
           "RECORDS ARE PASSED CORRECTLY TO SUBPROGRAMS - " &
           "ARRAYS WITH DIFFERENT BOUNDS PASSED TO UNCONSTRAINED " &
           "FORMAL");

     DECLARE   -- (E)

          SUBTYPE SUBINT IS INTEGER RANGE 0..5;
          TYPE ARRAY_TYPE IS ARRAY (SUBINT RANGE <>) OF BOOLEAN;
          TYPE RECORD_TYPE IS
               RECORD
                    A : ARRAY_TYPE (IDENT_INT(0)..IDENT_INT(2));
                    B : ARRAY_TYPE (1..3);
               END RECORD;
          REC : RECORD_TYPE := (A => (0..2 => IDENT_BOOL(TRUE)),
                                B => (1..3 => IDENT_BOOL(FALSE)));
          BOOL : BOOLEAN;

          PROCEDURE P1 (ARR : ARRAY_TYPE; ARR2 : ARRAY_TYPE) IS
          BEGIN
               IF ARR /= (TRUE, TRUE, TRUE) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY");
               END IF;
               IF ARR'FIRST /= IDENT_INT(0) OR ARR'LAST /= 2 THEN
                    FAILED ("WRONG IN PARAMETER BOUNDS - 1");
               END IF;
               IF ARR2 /= (FALSE, FALSE, FALSE) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY - 2");
               END IF;
               IF ARR2'FIRST /= 1 OR ARR2'LAST /= IDENT_INT(3) THEN
                    FAILED ("WRONG IN PARAMETER BOUNDS - 2");
               END IF;
          END P1;

          FUNCTION F1 ( ARR : ARRAY_TYPE; ARR2 : ARRAY_TYPE) 
               RETURN BOOLEAN IS
          BEGIN
               IF ARR /= (TRUE, TRUE, TRUE) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY TO FN");
               END IF;
               IF ARR'FIRST /= IDENT_INT(0) OR ARR'LAST /= 2 THEN
                    FAILED ("WRONG IN PARAMETER BOUNDS FOR FN - 1");
               END IF;
               IF ARR2 /= (FALSE, FALSE, FALSE) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY TO FN");
               END IF;
               IF ARR2'FIRST /= 1 OR ARR2'LAST /= IDENT_INT(3) THEN
                    FAILED ("WRONG IN PARAMETER BOUNDS FOR FN - 2");
               END IF;
               RETURN TRUE;
          END F1;

          PROCEDURE P2 (ARR : IN OUT ARRAY_TYPE;
                        ARR2 : IN OUT ARRAY_TYPE) IS
          BEGIN
               IF ARR /= (TRUE, TRUE, TRUE) THEN
                    FAILED ("IN OUT PARAM NOT PASSED CORRECTLY");
               END IF;
               IF ARR'FIRST /= IDENT_INT(0) OR ARR'LAST /= 2 THEN
                    FAILED ("WRONG IN OUT PARAMETER BOUNDS - 1");
               END IF;
               IF ARR2 /= (FALSE, FALSE, FALSE) THEN
                    FAILED ("IN OUT PARAM NOT PASSED CORRECTLY");
               END IF;
               IF ARR2'FIRST /= 1 OR ARR2'LAST /= IDENT_INT(3) THEN
                    FAILED ("WRONG IN OUT PARAMETER BOUNDS - 2");
               END IF;
               ARR := (ARR'RANGE => FALSE);
               ARR2 := (ARR2'RANGE => TRUE);
          END P2;

          PROCEDURE P3 (ARR : OUT ARRAY_TYPE; ARR2 : OUT ARRAY_TYPE) IS
          BEGIN
               IF ARR'FIRST /= IDENT_INT(0) OR ARR'LAST /= 2 THEN
                    FAILED ("WRONG OUT PARAMETER BOUNDS - 1");
               END IF;
               IF ARR2'FIRST /= 1 OR ARR2'LAST /= IDENT_INT(3) THEN
                    FAILED ("WRONG OUT  PARAMETER BOUNDS - 2");
               END IF;
               ARR := (ARR'RANGE => FALSE);
               ARR2 := (ARR2'RANGE => TRUE);
          END P3;

     BEGIN     -- (E)

          P1 (REC.A, REC.B);
          IF REC.A /= (TRUE, TRUE, TRUE) THEN
               FAILED ("IN PARAM CHANGED BY PROCEDURE");
          END IF;
          IF REC.B /= (FALSE, FALSE, FALSE) THEN
               FAILED ("IN PARAM CHANGED BY PROCEDURE - 2");
          END IF;

          BOOL := F1 (REC.A, REC.B);
          IF REC.A /= (TRUE, TRUE, TRUE) THEN
               FAILED ("IN PARAM CHANGED BY FUNCTION");
          END IF;
          IF REC.B /= (FALSE, FALSE, FALSE) THEN
               FAILED ("IN PARAM CHANGED BY FUNCTION - 2");
          END IF;

          P2 (REC.A, REC.B);
          IF REC.A /= (FALSE, FALSE, FALSE) THEN
               FAILED ("IN OUT PARAM RETURNED INCORRECTLY");
          END IF;
          IF REC.B /= (TRUE, TRUE, TRUE) THEN
               FAILED ("IN OUT PARAM RETURNED INCORRECTLY - 2");
          END IF;

          P3 (REC.A, REC.B);
          IF REC.A /= (FALSE, FALSE, FALSE) THEN
               FAILED ("OUT PARAM RETURNED INCORRECTLY");
          END IF;
          IF REC.B /= (TRUE, TRUE, TRUE) THEN
               FAILED ("OUT PARAM RETURNED INCORRECTLY - 2");
          END IF;

     END; -- (E)

     RESULT;
END C64109E;
