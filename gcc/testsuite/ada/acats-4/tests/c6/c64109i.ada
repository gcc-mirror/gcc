-- C64109I.ADA

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
--    (C) CHECK RECORDS HAVING A DISCRIMINANT, WITH MORE THAN ONE ARRAY
--       COMPONENT, WHERE THE BOUNDS OF THE ARRAY DEPEND ON THE
--       DISCRIMINANT.

-- HISTORY:
--    TBN 07/10/86          CREATED ORIGINAL TEST.
--    JET 08/04/87          REMOVED PARTIAL ARRAY REFERENCES IN
--                          RECORD FIELDS.

WITH REPORT;  USE REPORT;
PROCEDURE C64109I IS

BEGIN
     TEST ("C64109I", "CHECK THAT SLICES OF ARRAYS WHICH ARE " &
                      "COMPONENTS OF RECORDS ARE PASSED CORRECTLY " &
                      "TO SUBPROGRAMS - RECORDS WITH DISCRIMINANTS");

     DECLARE   -- (C)

          SUBTYPE SUBINT IS INTEGER RANGE 1..6;
          TYPE ARRAY_TYPE IS ARRAY (SUBINT RANGE <>) OF INTEGER;
          TYPE RECORD_TYPE (BOUND : INTEGER) IS
               RECORD
                    B : BOOLEAN;
                    A : ARRAY_TYPE (1..BOUND);
                    AA : ARRAY_TYPE (BOUND..6);
               END RECORD;
          REC : RECORD_TYPE (BOUND => IDENT_INT(4)) :=
                               (BOUND => 4,
                                B => TRUE,
                                A => (1..IDENT_INT(4) => 6),
                                AA => (4..6 => 8));
          BOOL : BOOLEAN;

          PROCEDURE P1 (ARR : ARRAY_TYPE) IS
          BEGIN
               IF ARR /= (6, 6, 6) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY");
               END IF;

               IF ARR'FIRST /= 1 OR ARR'LAST /= IDENT_INT(3) THEN
                    FAILED ("WRONG BOUNDS - IN PARAMETER");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE P1");
          END P1;

          FUNCTION F1 (ARR : ARRAY_TYPE) RETURN BOOLEAN IS
          BEGIN
               IF ARR /= (6, 6, 6) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY TO FN");
               END IF;

               IF ARR'FIRST /= 2 OR ARR'LAST /= IDENT_INT(4) THEN
                    FAILED ("WRONG BOUNDS - IN PARAMETER FOR FN");
               END IF;
               RETURN TRUE;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN FUNCTION F1");
          END F1;

          PROCEDURE P2 (ARR : IN OUT ARRAY_TYPE) IS
          BEGIN
               IF ARR /= (8, 8) THEN
                    FAILED ("IN OUT PARAM NOT PASSED CORRECTLY");
               END IF;

               IF ARR'FIRST /= 4 OR ARR'LAST /= IDENT_INT(5) THEN
                    FAILED ("WRONG BOUNDS - IN OUT PARAMETER");
               END IF;

               ARR := (ARR'RANGE => 10);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE P2");
          END P2;

          PROCEDURE P3 (ARR : OUT ARRAY_TYPE) IS
          BEGIN
               IF ARR'FIRST /= 2 OR ARR'LAST /= IDENT_INT(3) THEN
                    FAILED ("WRONG BOUNDS - OUT PARAMETER");
               END IF;
               ARR := (ARR'RANGE => 4);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE P3");
          END P3;

     BEGIN     -- (C)

          BEGIN     -- (D)
               P1 (REC.A (1..3));
               IF REC.A /= (6, 6, 6, 6) THEN
                    FAILED ("IN PARAM CHANGED BY PROCEDURE");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF P1");
          END;     -- (D)

          BEGIN     -- (E)
               BOOL := F1 (REC.A (2..4));
               IF REC.A /= (6, 6, 6, 6) THEN
                    FAILED ("IN PARAM CHANGED BY FUNCTION");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF F1");
          END;     -- (E)

          BEGIN     -- (F)
               P2 (REC.AA (4..5));
               IF REC.AA /= (10, 10, 8) THEN
                    FAILED ("IN OUT PARAM NOT RETURNED CORRECTLY");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF P2");
          END;     -- (F)

          BEGIN     -- (G)
               P3 (REC.A (2..3));
               IF REC.A /= (6, 4, 4, 6) THEN
                    FAILED ("OUT PARAM NOT RETURNED CORRECTLY");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF P3");
          END;     -- (G)

     END; -- (C)

     RESULT;
END C64109I;
