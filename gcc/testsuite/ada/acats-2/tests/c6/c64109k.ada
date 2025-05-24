-- C64109K.ADA

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
--    CHECK THAT SLICES OF ARRAYS WHICH ARE COMPONENTS OF RECORDS ARE
--    PASSED CORRECTLY TO SUBPROGRAMS.  SPECIFICALLY,
--    (E) CHECK THE CASE WHERE THE FORMAL IS UNCONSTRAINED, AND ARRAYS
--       WITH DIFFERENT BOUNDS ARE PASSED AS ACTUALS.

-- HISTORY:
--    TBN 07/11/86          CREATED ORIGINAL TEST.
--    JET 08/04/87          MODIFIED REC.A REFERENCES.

WITH REPORT; USE REPORT;
PROCEDURE C64109K IS

BEGIN
     TEST ("C64109K", "CHECK THAT SLICES OF ARRAYS WHICH ARE " &
                      "COMPONENTS OF RECORDS ARE PASSED CORRECTLY " &
                      "TO SUBPROGRAMS - ARRAYS WITH DIFFERENT BOUNDS " &
                      "PASSED TO UNCONSTRAINED FORMAL");

     DECLARE   -- (E)

          SUBTYPE SUBINT IS INTEGER RANGE 0..5;
          TYPE ARRAY_TYPE IS ARRAY (SUBINT RANGE <>) OF BOOLEAN;
          TYPE RECORD_TYPE IS
               RECORD
                    A : ARRAY_TYPE (IDENT_INT(0)..IDENT_INT(4));
                    B : ARRAY_TYPE (1..5);
               END RECORD;
          REC : RECORD_TYPE := (A => (0..4 => IDENT_BOOL(TRUE)),
                                B => (1..5 => IDENT_BOOL(FALSE)));
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
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE P1");
          END P1;

          FUNCTION F1 ( ARR : ARRAY_TYPE; ARR2 : ARRAY_TYPE)
               RETURN BOOLEAN IS
          BEGIN
               IF ARR /= (TRUE, TRUE, TRUE) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY TO FN");
               END IF;
               IF ARR'FIRST /= IDENT_INT(1) OR ARR'LAST /= 3 THEN
                    FAILED ("WRONG IN PARAMETER BOUNDS FOR FN - 1");
               END IF;
               IF ARR2 /= (FALSE, FALSE, FALSE) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY TO FN");
               END IF;
               IF ARR2'FIRST /= 3 OR ARR2'LAST /= IDENT_INT(5) THEN
                    FAILED ("WRONG IN PARAMETER BOUNDS FOR FN - 2");
               END IF;
               RETURN TRUE;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN FUNCTION F1");
          END F1;

          PROCEDURE P2 (ARR : IN OUT ARRAY_TYPE;
                        ARR2 : IN OUT ARRAY_TYPE) IS
          BEGIN
               IF ARR /= (TRUE, TRUE, TRUE) THEN
                    FAILED ("IN OUT PARAM NOT PASSED CORRECTLY");
               END IF;
               IF ARR'FIRST /= IDENT_INT(2) OR ARR'LAST /= 4 THEN
                    FAILED ("WRONG IN OUT PARAMETER BOUNDS - 1");
               END IF;
               IF ARR2 /= (FALSE, FALSE, FALSE) THEN
                    FAILED ("IN OUT PARAM NOT PASSED CORRECTLY");
               END IF;
               IF ARR2'FIRST /= 2 OR ARR2'LAST /= IDENT_INT(4) THEN
                    FAILED ("WRONG IN OUT PARAMETER BOUNDS - 2");
               END IF;
               ARR := (ARR'RANGE => FALSE);
               ARR2 := (ARR2'RANGE => TRUE);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE P2");
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
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE P3");
          END P3;

     BEGIN     -- (E)

          BEGIN     -- (F)
               P1 (REC.A (0..2), REC.B (1..3));
               IF REC.A /= (TRUE, TRUE, TRUE, TRUE, TRUE) THEN
                    FAILED ("IN PARAM CHANGED BY PROCEDURE");
               END IF;
               IF REC.B /= (FALSE, FALSE, FALSE, FALSE, FALSE) THEN
                    FAILED ("IN PARAM CHANGED BY PROCEDURE - 2");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF P1");
          END;     -- (F)

          BEGIN     -- (G)
               BOOL := F1 (REC.A (1..3), REC.B (3..5));
               IF REC.A /= (TRUE, TRUE, TRUE, TRUE, TRUE) THEN
                    FAILED ("IN PARAM CHANGED BY FUNCTION");
               END IF;
               IF REC.B /= (FALSE, FALSE, FALSE, FALSE, FALSE) THEN
                    FAILED ("IN PARAM CHANGED BY FUNCTION - 2");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF F1");
          END;     -- (G)

          BEGIN     -- (H)
               P2 (REC.A (2..4), REC.B (2..4));
               IF REC.A /= (TRUE, TRUE, FALSE, FALSE, FALSE) THEN
                    FAILED ("IN OUT PARAM RETURNED INCORRECTLY");
               END IF;
               IF REC.B /= (FALSE, TRUE, TRUE, TRUE, FALSE) THEN
                    FAILED ("IN OUT PARAM RETURNED INCORRECTLY - 2");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF P2");
          END;     -- (H)

          BEGIN     -- (I)
               P3 (REC.A (0..2), REC.B (1..3));
               IF REC.A /= (FALSE, FALSE, FALSE, FALSE, FALSE) THEN
                    FAILED ("OUT PARAM RETURNED INCORRECTLY");
               END IF;
               IF REC.B /= (TRUE, TRUE, TRUE, TRUE, FALSE) THEN
                    FAILED ("OUT PARAM RETURNED INCORRECTLY - 2");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF P3");
          END;     -- (I)

     END; -- (E)

     RESULT;
END C64109K;
