-- C64109D.ADA

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
--   (D) CHECK OBJECTS DESIGNATED BY ACCESS TYPES.

-- CPP 8/20/84

WITH REPORT;  USE REPORT;
PROCEDURE C64109D IS

BEGIN
     TEST ("C64109D", "CHECK THAT ARRAYS WHICH ARE COMPONENTS OF " &
           "RECORDS ARE PASSED CORRECTLY TO SUBPROGRAMS - " &
           "OBJECTS DESIGNATED BY ACCESS TYPES");

     DECLARE   -- (D)

          SUBTYPE INDEX IS INTEGER RANGE 1..3;
          TYPE ARRAY_TYPE IS ARRAY (INDEX RANGE <>) OF INTEGER;
          SUBTYPE ARRAY_SUBTYPE IS ARRAY_TYPE(1..IDENT_INT(3));
          TYPE NODE_TYPE;
          TYPE ACCESS_TYPE IS ACCESS NODE_TYPE;
          TYPE NODE_TYPE IS
               RECORD
                    A    : ARRAY_SUBTYPE;
                    NEXT : ACCESS_TYPE;
               END RECORD;
          PTR : ACCESS_TYPE := NEW NODE_TYPE'
                    (A => (IDENT_INT(1)..3 => IDENT_INT(5)),
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
          END P1;

          FUNCTION F1 (ARR : ARRAY_TYPE) RETURN BOOLEAN IS
          BEGIN
               IF ARR /= (5, 5, 5) THEN
                    FAILED ("IN PARAM NOT PASSED CORRECTLY TO FN");
               END IF;

               IF ARR'FIRST /= IDENT_INT(1) OR ARR'LAST /= 3 THEN
                    FAILED ("WRONG BOUNDS - IN PARAMETER FOR FN");
               END IF;

               RETURN TRUE;
          END F1;

          PROCEDURE P2 (ARR : IN OUT ARRAY_SUBTYPE) IS
          BEGIN
               IF ARR /= (5, 5, 5) THEN
                    FAILED ("IN OUT PARAM NOT PASSED CORRECTLY");
               END IF;

               IF ARR'FIRST /= IDENT_INT(1) OR ARR'LAST /= 3 THEN
                    FAILED ("WRONG BOUNDS - IN OUT PARAMETER");
               END IF;

               ARR := (OTHERS => 6);
          END P2;

          PROCEDURE P3 (ARR : OUT ARRAY_TYPE) IS
          BEGIN

               IF ARR'FIRST /= IDENT_INT(1) OR ARR'LAST /= 3 THEN
                    FAILED ("WRONG BOUNDS - OUT PARAMETER");
               END IF;

               ARR := (ARR'RANGE => 7);
          END P3;

     BEGIN     -- (D)

          P1 (PTR.A);
          IF PTR.A /= (5, 5, 5) THEN
               FAILED ("IN PARAM CHANGED BY PROCEDURE");
          END IF;

          BOOL := F1 (PTR.A);
          IF PTR.A /= (5, 5, 5) THEN
               FAILED ("IN PARAM CHANGED BY FUNCTION");
          END IF;

          P2 (PTR.A);
          IF PTR.A /= (6, 6, 6) THEN
               FAILED ("IN OUT PARAM NOT RETURNED CORRECTLY");
          END IF;

          P3 (PTR.A);
          IF PTR.A /= (7, 7, 7) THEN
               FAILED ("OUT PARAM NOT RETURNED CORRECTLY");
          END IF;

     END; -- (D)

     RESULT;
END C64109D;
