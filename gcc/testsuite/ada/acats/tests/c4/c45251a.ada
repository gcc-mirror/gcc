-- C45251A.ADA

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
-- CHECK THAT FOR RELATIONAL OPERATIONS ON FIXED POINT TYPES THE
-- FOLLOWING HOLD:
--       (A) A /= B IS THE SAME AS NOT (A = B).
--       (B) A < B IS THE SAME AS NOT (A >= B).
--       (C) A > B IS THE SAME AS NOT (A <= B).
--       (D) ADJACENT MODEL NUMBERS GIVE CORRECT RESULTS.
--       (E) NON-MODEL NUMBERS WITH DISTINCT MODEL INTERVALS GIVE
--           CORRECT RESULTS.
--       (F) CASE WHERE MODEL INTERVALS INTERSECT IN A SINGLE MODEL
--           NUMBER GIVES CORRECT RESULT.

-- CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE.

-- WRG 8/26/86

WITH REPORT; USE REPORT;
PROCEDURE C45251A IS

     -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S
     -- 'MANTISSA VALUE.

     TYPE LIKE_DURATION_M23 IS DELTA 0.020 RANGE -86_400.0 .. 86_400.0;
     TYPE DECIMAL_M4        IS DELTA 100.0 RANGE   -1000.0 ..   1000.0;

BEGIN

     TEST ("C45251A", "CHECK RELATIONAL OPERATIONS FOR FIXED POINT " &
                      "TYPES - BASIC TYPES");

     -------------------------------------------------------------------

     DECLARE
          A, B : LIKE_DURATION_M23 := 0.0;
          C, D : DECIMAL_M4        := 0.0;
     BEGIN
          IF EQUAL (3, 3) THEN
               A := 2#0.0000_0011#; -- JUST BELOW LIKE_DURATION'SMALL.
               B := 2#0.0000_0101#; -- JUST ABOVE LIKE_DURATION'SMALL.
          END IF;

          -- (A)
          IF A /= B XOR NOT (A = B) THEN
               FAILED ("A /= B IS NOT THE SAME AS NOT (A = B)");
          END IF;

          -- (B)
          IF A < B XOR NOT (A >= B) THEN
               FAILED ("A < B IS NOT THE SAME AS NOT (A >= B)");
          END IF;

          -- (C)
          IF A > B XOR NOT (A <= B) THEN
               FAILED ("A > B IS NOT THE SAME AS NOT (A <= B)");
          END IF;

          -- (D)
          IF EQUAL (3, 3) THEN
               A := -(16#1_5180.00#); -- (-86_400.0)
               B := -(16#1_517F.FC#); -- (-86_400.0 + 1.0/64)

               C :=  64.0; -- DECIMAL_M4'SMALL.
               D := 128.0; -- 2 * DECIMAL_M4'SMALL.
          END IF;
          IF "=" (LEFT => A, RIGHT => B) THEN
               FAILED ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " &
                       "- (A = B)");
          END IF;
          IF NOT "/=" (LEFT => C, RIGHT => D) THEN
               FAILED ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " &
                       "- (C /= D)");
          END IF;
          IF "<" (LEFT => B, RIGHT => A) THEN
               FAILED ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " &
                       "- (B < A)");
          END IF;
          IF ">" (LEFT => C, RIGHT => D) THEN
               FAILED ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " &
                       "- (C > D)");
          END IF;
          IF ">=" (LEFT => A, RIGHT => B) THEN
               FAILED ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " &
                       "- (A >= B)");
          END IF;
          IF "<=" (LEFT => D, RIGHT => C) THEN
               FAILED ("ADJACENT MODEL NUMBERS GIVE INCORRECT RESULT " &
                       "- (D <= C)");
          END IF;

          -- (E)
          IF EQUAL (3, 3) THEN
               A :=  0.02;  -- INTERVAL IS  1.0/64 ..  2.0/64.
               B := -0.02;  -- INTERVAL IS -2.0/64 .. -1.0/64.

               C :=  800.0; -- INTERVAL IS 768.0 .. 832.0.
               D :=  900.0; -- INTERVAL IS 896.0 .. 960.0.
          END IF;
          IF A = B THEN
               FAILED ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
                       "INTERVALS GIVE INCORRECT RESULT - (A = B)");
          END IF;
          IF NOT (C /= D) THEN
               FAILED ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
                       "INTERVALS GIVE INCORRECT RESULT - (C /= D)");
          END IF;
          IF A < B THEN
               FAILED ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
                       "INTERVALS GIVE INCORRECT RESULT - (A < B)");
          END IF;
          IF C > D THEN
               FAILED ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
                       "INTERVALS GIVE INCORRECT RESULT - (C > D)");
          END IF;
          IF B >= A THEN
               FAILED ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
                       "INTERVALS GIVE INCORRECT RESULT - (B >= A)");
          END IF;
          IF D <= C THEN
               FAILED ("NON-MODEL NUMBERS WITH DISTINCT MODEL " &
                       "INTERVALS GIVE INCORRECT RESULT - (D <= C)");
          END IF;

          -- (F)
          IF EQUAL (3, 3) THEN
               B := 0.035;  -- INTERVAL IS 2.0/64 .. 3.0/64.

               C := 850.0;  -- INTERVAL IS 832.0 .. 896.0.
          END IF;
          IF NOT (A <= B) THEN
               FAILED ("COMPARISON OF NON-MODEL NUMBERS WITH ONE " &
                       "COMMON MODEL INTERVAL END-POINT GIVES " &
                       "INCORRECT RESULT - (A <= B)");
          END IF;
          IF A > B THEN
               FAILED ("COMPARISON OF NON-MODEL NUMBERS WITH ONE " &
                       "COMMON MODEL INTERVAL END-POINT GIVES " &
                       "INCORRECT RESULT - (A > B)");
          END IF;
          IF NOT (D >= C) THEN
               FAILED ("COMPARISON OF NON-MODEL NUMBERS WITH ONE " &
                       "COMMON MODEL INTERVAL END-POINT GIVES " &
                       "INCORRECT RESULT - (D >= C)");
          END IF;
          IF D < C THEN
               FAILED ("COMPARISON OF NON-MODEL NUMBERS WITH ONE " &
                       "COMMON MODEL INTERVAL END-POINT GIVES " &
                       "INCORRECT RESULT - (D < C)");
          END IF;
     END;

     -------------------------------------------------------------------

     RESULT;

END C45251A;
