-- C64109J.ADA

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
--      (D) CHECK OBJECTS DESIGNATED BY ACCESS TYPES.

-- HISTORY:
--    TBN 07/10/86          CREATED ORIGINAL TEST.
--    JET 08/04/87          MODIFIED PTR.A REFERENCES.

WITH REPORT;  USE REPORT;
PROCEDURE C64109J IS

BEGIN
     TEST ("C64109J", "CHECK THAT SLICES OF ARRAYS WHICH ARE " &
                      "COMPONENTS OF RECORDS ARE PASSED CORRECTLY " &
                      "TO SUBPROGRAMS - OBJECTS DESIGNATED BY ACCESS " &
                      "TYPES");

     DECLARE   -- (D)

          SUBTYPE INDEX IS INTEGER RANGE 1..5;
          TYPE ARRAY_TYPE IS ARRAY (INDEX RANGE <>) OF INTEGER;
          SUBTYPE ARRAY_SUBTYPE IS ARRAY_TYPE(1..IDENT_INT(5));
          TYPE NODE_TYPE;
          TYPE ACCESS_TYPE IS ACCESS NODE_TYPE;
          TYPE NODE_TYPE IS
               RECORD
                    A    : ARRAY_SUBTYPE;
                    NEXT : ACCESS_TYPE;
               END RECORD;
          PTR : ACCESS_TYPE := NEW NODE_TYPE'
                    (A => (IDENT_INT(1)..5 => IDENT_INT(5)),
                     NEXT => NULL);
          BOOL : BOOLEAN;

          PROCEDURE P1 (ARR : ARRAY_TYPE) IS
          BEGIN
               IF ARR /= (5, 5, 5) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY");
               END IF;

               IF ARR'FIRST /= IDENT_INT(1) OR ARR'LAST /= 3 THEN
                    FAILED ("WRONG BOUNDS - IN PARAMETER");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE P1");
          END P1;

          FUNCTION F1 (ARR : ARRAY_TYPE) RETURN BOOLEAN IS
          BEGIN
               IF ARR /= (5, 5, 5) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY TO FN");
               END IF;

               IF ARR'FIRST /= IDENT_INT(2) OR ARR'LAST /= 4 THEN
                    FAILED ("WRONG BOUNDS - IN PARAMETER FOR FN");
               END IF;

               RETURN TRUE;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN FUNCTION F1");
          END F1;

          PROCEDURE P2 (ARR : IN OUT ARRAY_TYPE) IS
          BEGIN
               IF ARR /= (5, 5, 5) THEN
                    FAILED ("IN OUT PARAM NOT PASSED CORRECTLY");
               END IF;

               IF ARR'FIRST /= IDENT_INT(1) OR ARR'LAST /= 3 THEN
                    FAILED ("WRONG BOUNDS - IN OUT PARAMETER");
               END IF;

               ARR := (ARR'RANGE => 6);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE P2");
          END P2;

          PROCEDURE P3 (ARR : OUT ARRAY_TYPE) IS
          BEGIN

               IF ARR'FIRST /= IDENT_INT(3) OR ARR'LAST /= 5 THEN
                    FAILED ("WRONG BOUNDS - OUT PARAMETER");
               END IF;

               ARR := (ARR'RANGE => 7);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE P3");
          END P3;

     BEGIN     -- (D)

          BEGIN     -- (E)
               P1 (PTR.A (1..3));
               IF PTR.A /= (5, 5, 5, 5, 5) THEN
                    FAILED ("IN PARAM CHANGED BY PROCEDURE");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF P1");
          END;     -- (E)

          BEGIN     -- (F)
               BOOL := F1 (PTR.A (2..4));
               IF PTR.A /= (5, 5, 5, 5, 5) THEN
                    FAILED ("IN PARAM CHANGED BY FUNCTION");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF F1");
          END;     -- (F)

          BEGIN     -- (G)
               P2 (PTR.A (1..3));
               IF PTR.A /= (6, 6, 6, 5, 5) THEN
                    FAILED ("IN OUT PARAM NOT RETURNED CORRECTLY");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF P2");
          END;     -- (G)

          BEGIN     -- (H)
               P3 (PTR.A (3..5));
               IF PTR.A /= (6, 6, 7, 7, 7) THEN
                    FAILED ("OUT PARAM NOT RETURNED CORRECTLY");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED DURING CALL OF P3");
          END;     -- (H)

     END; -- (D)

     RESULT;
END C64109J;
