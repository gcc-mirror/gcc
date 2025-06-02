-- C45331A.ADA

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
-- CHECK THAT FOR FIXED POINT TYPES THE OPERATORS "+" AND "-" PRODUCE
-- CORRECT RESULTS WHEN:
--      (A) A, B, A+B, AND A-B ARE ALL MODEL NUMBERS.
--      (B) A IS A MODEL NUMBER BUT B, A+B, AND A-B ARE NOT.
--      (C) A, B, A+B, AND A-B ARE ALL MODEL NUMBERS WITH DIFFERENT
--          SUBTYPES.

-- CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE.

-- WRG 8/27/86
-- KAS 11/14/95 REDUCE EXPECTATION FOR T'SMALL
-- KAS 11/30/95 ONE MORE CHANGE...
-- PWN 02/28/96 CLEANED COMMENTS FOR RELEASE
-- KAS 03/18/96 ELIDED TWO 'SMALL CASES FOR 2.1

WITH REPORT; USE REPORT;
PROCEDURE C45331A IS

     TYPE LIKE_DURATION IS DELTA 0.020 RANGE -86_400.0 .. 86_400.0;
          -- 'MANTISSA = 23.
     SUBTYPE F     IS LIKE_DURATION DELTA 0.25 RANGE -1000.0 .. 1000.0;
     SUBTYPE ST_F1 IS LIKE_DURATION DELTA 0.5  RANGE    -4.0 ..    3.0;
     SUBTYPE ST_F2 IS LIKE_DURATION DELTA 1.0 / 16
             RANGE -13.0 / 16 .. 5.0 + 1.0 / 16;

BEGIN

     TEST ("C45331A", "CHECK THAT FOR FIXED POINT TYPES THE " &
                      "OPERATORS ""+"" AND ""-"" PRODUCE CORRECT " &
                      "RESULTS - BASIC TYPES");

     -------------------------------------------------------------------

A:   DECLARE
          SMALL, MAX, MIN, ZERO : F := 0.5;
          X                     : F := 0.0;
     BEGIN
          -- INITIALIZE "CONSTANTS":
          IF EQUAL (3, 3) THEN
               SMALL := F'SMALL;
               MAX   := F'LAST;  -- BECAUSE F'LAST < F'LARGE AND F'LAST
                                 -- IS A MODEL NUMBER.
               MIN   := F'FIRST; -- F'FIRST IS A MODEL NUMBER.
               ZERO  := 0.0;
          END IF;

          -- CHECK SMALL + OR - ZERO = SMALL:
          IF "+"(LEFT => SMALL, RIGHT => ZERO) /= SMALL OR
             0.0 + SMALL /= SMALL THEN
               FAILED ("F'SMALL + 0.0 /= F'SMALL");
          END IF;
          IF "-"(LEFT => SMALL, RIGHT => ZERO) /= SMALL OR
             SMALL - 0.0 /= SMALL THEN
               FAILED ("F'SMALL - 0.0 /= F'SMALL");
          END IF;

          -- CHECK MAX + OR - ZERO = MAX:
          IF MAX + ZERO /= MAX OR 0.0 + MAX /= MAX THEN
               FAILED ("F'LAST + 0.0 /= F'LAST");
          END IF;
          IF MAX - ZERO /= MAX OR MAX - 0.0 /= MAX THEN
               FAILED ("F'LAST - 0.0 /= F'LAST");
          END IF;

          -- CHECK SMALL - SMALL = 0.0:
          IF EQUAL (3, 3) THEN
               X := SMALL;
          END IF;
          IF SMALL - X /= 0.0 OR SMALL - SMALL /= 0.0 OR
             F'SMALL - F'SMALL /= 0.0 THEN
               FAILED ("F'SMALL - F'SMALL /= 0.0");
          END IF;

          -- CHECK MAX - MAX = 0.0:
          IF EQUAL (3, 3) THEN
               X := MAX;
          END IF;
          IF MAX - X /= 0.0 OR MAX - MAX /= 0.0 OR
             F'LAST - F'LAST /= 0.0 THEN
               FAILED ("F'LAST - F'LAST /= 0.0");
          END IF;

          -- CHECK ZERO - MAX = MIN, MIN - MIN = 0.0,
          -- AND MIN + MAX = 0.0:
          IF EQUAL (3, 3) THEN
               X := ZERO - MAX;
          END IF;
          IF X /= MIN THEN
               FAILED ("0.0 - 1000.0 /= -1000.0");
          END IF;
          IF EQUAL (3, 3) THEN
               X := MIN;
          END IF;
          IF MIN - X /= 0.0 OR MIN - MIN /= 0.0 OR
             F'FIRST - F'FIRST /= 0.0 THEN
               FAILED ("F'FIRST - F'FIRST /= 0.0");
          END IF;
          IF MIN + MAX /= 0.0 OR MAX + MIN /= 0.0 OR
             F'FIRST + F'LAST /= 0.0 THEN
               FAILED ("-1000.0 + 1000.0 /= 0.0");
          END IF;

          -- CHECK ADDITION AND SUBTRACTION FOR ARBITRARY MID-RANGE
          -- NUMBERS:
          IF EQUAL (3, 3) THEN
               X := 100.75;
          END IF;
          IF (X + SMALL) /= (SMALL + X) OR
             (X + SMALL) > (X + 0.25) THEN -- X + SMALL SB <= X + DELTA
               FAILED("X + SMALL DELIVERED BAD RESULT");
          END IF;

          -- CHECK (MAX - SMALL) + SMALL = MAX:
          IF EQUAL (3, 3) THEN
               X := MAX - SMALL;
          END IF;
          IF X + SMALL /= MAX THEN
              FAILED("(MAX - SMALL) + SMALL /= MAX");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - A");
     END A;

     -------------------------------------------------------------------

B:   DECLARE
          NON_MODEL_CONST : CONSTANT := 2.0 / 3;
          NON_MODEL_VAR   : F        := 0.0;

          SMALL, MAX, MIN, ZERO : F := 0.5;
          X                     : F := 0.0;
     BEGIN
          -- INITIALIZE "CONSTANTS":
          IF EQUAL (3, 3) THEN
               SMALL         := F'SMALL;
               MAX           := F'LAST;  -- BECAUSE F'LAST < F'LARGE AND
                                         -- F'LAST  IS A MODEL NUMBER.
               MIN           := F'FIRST; -- F'FIRST IS A MODEL NUMBER.
               ZERO          := 0.0;
               NON_MODEL_VAR := NON_MODEL_CONST;
          END IF;

          -- CHECK VALUE OF NON_MODEL_VAR:
          IF NON_MODEL_VAR NOT IN 0.5 .. 0.75 THEN
               FAILED ("VALUE OF NON_MODEL_VAR NOT IN CORRECT RANGE");
          END IF;

          -- CHECK NON-MODEL VALUE + OR - ZERO:
          IF NON_MODEL_VAR + ZERO NOT IN 0.5 .. 0.75 OR
             F'(0.0) + NON_MODEL_CONST NOT IN 0.5 .. 0.75 THEN
               FAILED ("(2.0 / 3) + 0.0 NOT IN 0.5 .. 0.75");
          END IF;
          IF NON_MODEL_VAR  - ZERO NOT IN 0.5 .. 0.75 OR
             NON_MODEL_CONST - F'(0.0) NOT IN 0.5 .. 0.75 THEN
               FAILED ("(2.0 / 3) - 0.0 NOT IN 0.5 .. 0.75");
          END IF;

          -- CHECK ZERO - NON-MODEL:
          IF F'(0.0) - NON_MODEL_CONST NOT IN -0.75 .. -0.5 THEN
               FAILED ("0.0 - (2.0 / 3) NOT IN -0.75 .. -0.5");
          END IF;

          IF F'(1.0) - NON_MODEL_CONST NOT IN 0.25 .. 0.5 THEN
               FAILED ("1.0 - (2.0 / 3) NOT IN 0.25 .. 0.5");
          END IF;

          -- CHECK ADDITION AND SUBTRACTION OF NON-MODEL NEAR MIN AND
          -- MAX:
          IF MIN + NON_MODEL_VAR NOT IN -999.5 .. -999.25 OR
             NON_MODEL_CONST + F'FIRST NOT IN -999.5 .. -999.25 THEN
               FAILED ("-1000.0 + (2.0 / 3) NOT IN -999.5 .. -999.25");
          END IF;
          IF MAX - NON_MODEL_VAR NOT IN 999.25 .. 999.5 OR
             F'LAST - NON_MODEL_CONST NOT IN 999.25 .. 999.5 THEN
               FAILED ("1000.0 - (2.0 / 3) NOT IN 999.25 .. 999.5");
          END IF;

          -- CHECK ADDITION AND SUBTRACTION FOR ARBITRARY MID-RANGE
          -- MODEL NUMBER WITH NON-MODEL:
          IF EQUAL (3, 3) THEN
               X := -213.25;
          END IF;
          IF X + NON_MODEL_CONST NOT IN -212.75 .. -212.5 THEN
               FAILED ("-213.25 + (2.0 / 3) NOT IN -212.75 .. -212.5");
          END IF;
          IF NON_MODEL_VAR - X NOT IN 213.75 .. 214.0 THEN
               FAILED ("(2.0 / 3) - (-213.25) NOT IN 213.75 .. 214.0");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - B");
     END B;

     -------------------------------------------------------------------

C:   DECLARE
          A_SMALL, A_MAX, A_MIN : ST_F1 := 0.0;
          B_SMALL, B_MAX, B_MIN : ST_F2 := 0.0;
          X                     : F;
     BEGIN
          -- INITIALIZE "CONSTANTS":
          IF EQUAL (3, 3) THEN
               A_SMALL := ST_F1'SMALL;
               A_MAX   := ST_F1'LAST;  -- BECAUSE 'LAST < 'LARGE AND
                                       -- 'LAST  IS A MODEL NUMBER.
               A_MIN   := ST_F1'FIRST; -- 'FIRST IS A MODEL NUMBER.

               B_SMALL := ST_F2'SMALL;
               B_MAX   := ST_F2'LAST;  -- BECAUSE 'LAST <= 'LARGE AND
                                       -- 'LAST  IS A MODEL NUMBER.
               B_MIN   := ST_F2'FIRST; -- 'FIRST IS A MODEL NUMBER.
          END IF;

          IF A_MIN + B_MIN /= -4.8125 THEN
               FAILED ("-4.0 + (-0.8125) /= -4.8125");
          END IF;

          IF A_MIN - B_MIN /= -3.1875 THEN
               FAILED ("-4.0 - (-0.8125) /= -3.1875");
          END IF;

          IF (A_MIN + B_SMALL) NOT IN A_MIN .. -3.9375 THEN
               FAILED ("(A_MIN + B_SMALL) NOT IN A_MIN .. -3.9375");
          END IF;

          IF (A_MIN - B_SMALL) NOT IN -4.0625 .. -4.0 THEN
              FAILED ("(A_MIN - B_SMALL) NOT IN -4.0 .. -4.0625");
          END IF;

          IF A_MIN + B_MAX /= 1.0625 THEN
               FAILED ("-4.0 + 5.0625 /= 1.0625");
          END IF;

          IF A_MIN - B_MAX /= -9.0625 THEN
               FAILED ("-4.0 - 5.0625 /= -9.0625");
          END IF;

          IF (A_SMALL + B_MIN) NOT IN  B_MIN..-0.3125 THEN
               FAILED ("(A_SMALL + B_MIN) NOT IN  B_MIN..-0.3125");
          END IF;

          IF (A_SMALL - B_MIN) NOT IN +0.8125 .. 1.3125 THEN
               FAILED ("(A_SMALL - B_MIN) NOT IN -0.8125 .. 1.3125");
          END IF;



          IF (A_SMALL + B_MAX) NOT IN 5.0625 .. 5.5625 THEN
               FAILED ("(A_SMALL + B_MAX) NOT IN 5.0625 .. 5.5625");
          END IF;

          IF (A_SMALL - B_MAX) NOT IN -5.0625 .. -4.5625 THEN
               FAILED ("(A_SMALL - B_MAX) NOT IN -5.0625 .. -4.5625");
          END IF;

          IF A_MAX + B_MIN /= 2.1875 THEN
               FAILED ("3.0 + (-0.8125) /= 2.1875");
          END IF;

          IF A_MAX - B_MIN /= 3.8125 THEN
               FAILED ("3.0 - (-0.8125) /= 3.8125");
          END IF;

          IF (A_MAX + B_SMALL) NOT IN 3.0 .. 3.0625 THEN
               FAILED ("(A_MAX + B_SMALL) NOT IN 3.0 .. 3.0625");
          END IF;

          IF (A_MAX - B_SMALL) NOT IN 2.9375..3.0 THEN
               FAILED ("(A_MAX - B_SMALL) NOT IN 2.9375..3.0");
          END IF;

          IF A_MAX + B_MAX /= 8.0625 THEN
               FAILED ("3.0 + 5.0625 /= 8.0625");
          END IF;

          IF A_MAX - B_MAX /= -2.0625 THEN
               FAILED ("3.0 - 5.0625 /= -2.0625");
          END IF;

          X := B_MIN - A_MIN;
          IF X NOT IN 3.0 .. 3.25 THEN
               FAILED ("-0.8125 - (-4.0) NOT IN RANGE");
          END IF;

          X := B_MIN - A_SMALL;
          IF X NOT IN -1.3125 .. -0.8125 THEN
               FAILED ("B_MIN - A_SMALL NOT IN RANGE");
          END IF;

          X := B_MIN - A_MAX;
          IF X NOT IN -4.0 .. -3.75 THEN
               FAILED ("-0.8125 - 3.0 NOT IN RANGE");
          END IF;

          X := B_SMALL - A_MIN;
          IF X NOT IN 4.0 .. 4.0625 THEN
               FAILED ("B_SMALL - A_MIN NOT IN RANGE");
          END IF;


          X := B_SMALL - A_MAX;
          IF X NOT IN -3.0 .. -2.75 THEN
               FAILED ("B_SMALL - A_MAX NOT IN RANGE");
          END IF;

          X := B_MAX - A_MIN;
          IF X NOT IN 9.0 .. 9.25 THEN
               FAILED ("5.0625 - (-4.0) NOT IN RANGE");
          END IF;

          X := B_MAX - A_SMALL;
         IF X NOT IN 4.56 .. 5.0625 THEN
               FAILED ("5.0625 - 0.5 NOT IN RANGE");
          END IF;

          X := B_MAX - A_MAX;
          IF X NOT IN 2.0 .. 2.25 THEN
               FAILED ("5.0625 - 3.0 NOT IN RANGE");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - C");
     END C;

     -------------------------------------------------------------------

     RESULT;

END C45331A;
