-- C45262B.ADA

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
-- ONE-DIMENSIONAL DISCRETE ARRAY TYPES.  THIS TEST CHECKS STRING TYPES.

-- JWC 9/9/85
-- JRK 6/24/86   FIXED SPELLING IN FAILURE MESSAGE.

WITH REPORT; USE REPORT;

PROCEDURE C45262B IS
BEGIN
     TEST ("C45262B", "ORDERING COMPARISONS OF ONE-DIMENSIONAL " &
                      "DISCRETE ARRAY TYPES - TYPE STRING");

     DECLARE

          STRING1 : STRING(2 .. IDENT_INT(1));
          STRING2 : STRING(3 .. IDENT_INT(1));
          STRING3 : STRING(2 .. IDENT_INT(2)) := (IDENT_INT(2) => 'A');
          STRING4 : STRING(1 .. IDENT_INT(1)) := (IDENT_INT(1) => 'A');
          STRING5 : STRING(1 .. IDENT_INT(1)) := (IDENT_INT(1) => 'B');
          STRING6 : STRING(2 .. IDENT_INT(6)) :=
                                             (2 .. IDENT_INT(6) => 'A');
          STRING7 : STRING(1 .. 5) := (1 .. 4 => 'A', 5 => 'B');
          STRING8 : STRING(1 .. IDENT_INT(5)) :=
                                             (1 .. IDENT_INT(5) => 'A');
          STRING9 : STRING(1 .. IDENT_INT(4)) :=
                                             (1 .. IDENT_INT(4) => 'A');
          STRINGA : STRING(1 .. IDENT_INT(4)) :=
                                             (1 .. IDENT_INT(4) => 'B');

     BEGIN
          IF STRING1 < STRING2 THEN
               FAILED ("NULL ARRAYS STRING1 AND STRING2 NOT EQUAL - <");
          END IF;

          IF NOT (STRING1 <= STRING2) THEN
               FAILED ("NULL ARRAYS STRING1 AND STRING2 NOT EQUAL - " &
                       "<=");
          END IF;

          IF STRING1 > STRING2 THEN
               FAILED ("NULL ARRAYS STRING1 AND STRING2 NOT EQUAL - >");
          END IF;

          IF NOT ( ">=" (STRING1, STRING2) ) THEN
               FAILED ("NULL ARRAYS STRING1 AND STRING2 NOT EQUAL - " &
                       ">=");
          END IF;

          IF STRING3 < STRING1 THEN
               FAILED ("NON-NULL ARRAY STRING3 LESS THAN NULL STRING1");
          END IF;

          IF STRING3 <= STRING1 THEN
               FAILED ("NON-NULL ARRAY STRING3 LESS THAN EQUAL NULL " &
                        "STRING1");
          END IF;

          IF NOT ( ">" (STRING3, STRING1) ) THEN
               FAILED ("NON-NULL ARRAY STRING3 NOT GREATER THAN NULL " &
                       "STRING1");
          END IF;

          IF NOT (STRING3 >= STRING1) THEN
               FAILED ("NON-NULL ARRAY STRING3 NOT GREATER THAN " &
                       "EQUAL NULL STRING1");
          END IF;

          IF STRING3 < STRING4 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - <");
          END IF;

          IF NOT ( "<=" (STRING3, STRING4) ) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - <=");
          END IF;

          IF STRING3 > STRING4 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - >");
          END IF;

          IF NOT (STRING3 >= STRING4) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - >=");
          END IF;

          IF NOT ( "<" (STRING3, STRING5) ) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - <");
          END IF;

          IF NOT (STRING3 <= STRING5) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - <=");
          END IF;

          IF STRING3 > STRING5 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - >");
          END IF;

          IF STRING3 >= STRING5 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - >=");
          END IF;

          IF NOT (STRING6 < STRING7) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - <");
          END IF;

          IF NOT (STRING6 <= STRING7) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - " &
                       "<=");
          END IF;

          IF STRING6 > STRING7 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - >");
          END IF;

          IF ">=" (LEFT => STRING6, RIGHT => STRING7) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS NOT EQUAL - " &
                       ">=");
          END IF;

          IF STRING6 < STRING8 THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS EQUAL - <");
          END IF;

          IF NOT (STRING6 <= STRING8) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS EQUAL - <=");
          END IF;

          IF ">" (RIGHT => STRING8, LEFT => STRING6) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS EQUAL - >");
          END IF;

          IF NOT (STRING6 >= STRING8) THEN
               FAILED ("DIFFERENT BOUNDS, SAME NUMBER OF COMPONENTS, " &
                       "MULTIPLE COMPONENTS, COMPONENTS EQUAL - >=");
          END IF;

          IF STRING8 < STRING9 THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - <");
          END IF;

          IF STRING8 <= STRING9 THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - <=");
          END IF;

          IF NOT (STRING8 > STRING9) THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - >");
          END IF;

          IF NOT (STRING8 >= STRING9) THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS EQUAL - >=");
          END IF;

          IF NOT (STRING8 < STRINGA) THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - <");
          END IF;

          IF NOT (STRING8 <= STRINGA) THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - <=");
          END IF;

          IF STRING8 > STRINGA THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - >");
          END IF;

          IF STRING8 >= STRINGA THEN
               FAILED ("DIFFERENT NUMBER OF COMPONENTS, " &
                       "COMPONENTS NOT EQUAL - >=");
          END IF;

     END;

     RESULT;

END C45262B;
