-- C45262A.ADA

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
-- CHECK THAT ORDERING COMPARISONS YIELD CORRECT RESULTS FOR
-- ONE-DIMENSIONAL DISCRETE ARRAY TYPES.  THIS TEST CHECKS ARRAYS OF
-- INTEGERS.

-- JWC 8/19/85
-- JRK 6/24/86   FIXED SPELLING IN FAILURE MESSAGE.

WITH REPORT; USE REPORT;

PROCEDURE C45262A IS
BEGIN
     TEST ("C45262A", "ORDERING COMPARISONS OF ONE-DIMENSIONAL " &
                      "DISCRETE ARRAY TYPES - INTEGER COMPONENTS");

     DECLARE

          TYPE ARR IS ARRAY( INTEGER RANGE <> ) OF INTEGER;
          ARR1 : ARR(1 .. IDENT_INT(0));
          ARR2 : ARR(2 .. IDENT_INT(0));
          ARR3 : ARR(1 .. IDENT_INT(1)) := (IDENT_INT(1) => 0);
          ARR4 : ARR(0 .. IDENT_INT(0)) := (IDENT_INT(0) => 0);
          ARR5 : ARR(0 .. IDENT_INT(0)) := (IDENT_INT(0) => 1);
          ARR6 : ARR(1 .. IDENT_INT(5)) := (1 .. IDENT_INT(5) => 0);
          ARR7 : ARR(0 .. 4) := (0 .. 3 => 0, 4 => 1);
          ARR8 : ARR(0 .. IDENT_INT(4)) := (0 .. IDENT_INT(4) => 0);
          ARR9 : ARR(0 .. IDENT_INT(3)) := (0 .. IDENT_INT(3) => 0);
          ARRA : ARR(0 .. IDENT_INT(3)) := (0 .. IDENT_INT(3) => 1);

     BEGIN
          IF ARR1 < ARR2 THEN
               FAILED ("NULL ARRAYS ARR1 AND ARR2 NOT EQUAL - <");
          END IF;

          IF NOT (ARR1 <= ARR2) THEN
               FAILED ("NULL ARRAYS ARR1 AND ARR2 NOT EQUAL - <=");
          END IF;

          IF ARR1 > ARR2 THEN
               FAILED ("NULL ARRAYS ARR1 AND ARR2 NOT EQUAL - >");
          END IF;

          IF NOT ( ">=" (ARR1, ARR2) ) THEN
               FAILED ("NULL ARRAYS ARR1 AND ARR2 NOT EQUAL - >=");
          END IF;

          IF ARR3 < ARR1 THEN
               FAILED ("NON-NULL ARRAY ARR3 LESS THAN NULL ARR1");
          END IF;

          IF ARR3 <= ARR1 THEN
               FAILED ("NON-NULL ARRAY ARR3 LESS THAN EQUAL NULL ARR1");
          END IF;

          IF NOT ( ">" (ARR3, ARR1) ) THEN
               FAILED ("NON-NULL ARRAY ARR3 NOT GREATER THAN NULL " &
                       "ARR1");
          END IF;

          IF NOT (ARR3 >= ARR1) THEN
               FAILED ("NON-NULL ARRAY ARR3 NOT GREATER THAN EQUAL " &
                       "NULL ARR1");
          END IF;

          IF ARR3 < ARR4 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - <");
          END IF;

          IF NOT ( "<=" (ARR3, ARR4) ) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - <=");
          END IF;

          IF ARR3 > ARR4 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - >");
          END IF;

          IF NOT (ARR3 >= ARR4) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - >=");
          END IF;

          IF NOT ( "<" (ARR3, ARR5) ) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - <");
          END IF;

          IF NOT (ARR3 <= ARR5) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - <=");
          END IF;

          IF ARR3 > ARR5 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - >");
          END IF;

          IF ARR3 >= ARR5 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - >=");
          END IF;

          IF NOT (ARR6 < ARR7) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - <");
          END IF;

          IF NOT (ARR6 <= ARR7) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - " &
                       "<=");
          END IF;

          IF ARR6 > ARR7 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - >");
          END IF;

          IF ">=" (LEFT => ARR6, RIGHT => ARR7) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - " &
                       ">=");
          END IF;

          IF ARR6 < ARR8 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS EQUAL - <");
          END IF;

          IF NOT (ARR6 <= ARR8) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS EQUAL - <=");
          END IF;

          IF ">" (RIGHT => ARR8, LEFT => ARR6) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS EQUAL - >");
          END IF;

          IF NOT (ARR6 >= ARR8) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS EQUAL - >=");
          END IF;

          IF ARR8 < ARR9 THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - <");
          END IF;

          IF ARR8 <= ARR9 THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - <=");
          END IF;

          IF NOT (ARR8 > ARR9) THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - >");
          END IF;

          IF NOT (ARR8 >= ARR9) THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - >=");
          END IF;

          IF NOT (ARR8 < ARRA) THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - <");
          END IF;

          IF NOT (ARR8 <= ARRA) THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - <=");
          END IF;

          IF ARR8 > ARRA THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - >");
          END IF;

          IF ARR8 >= ARRA THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - >=");
          END IF;

     END;

     RESULT;

END C45262A;
