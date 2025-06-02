-- C45651A.ADA

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
--     FOR FIXED POINT TYPES, CHECK:
--          (A) FOR MODEL NUMBERS A >= 0.0, THAT ABS A = A.
--          (B) FOR MODEL NUMBERS A <= 0.0. THAT ABS A = -A.
--          (C) FOR NON-MODEL NUMBERS A > 0.0, THAT ABS A VALUES ARE
--              WITHIN THE APPROPRIATE MODEL INTERVAL.
--          (D) FOR NON-MODEL NUMBERS A < 0.0, THAT ABS A VALUES ARE
--              WITHIN THE APPROPRIATE MODEL INTERVAL.

--     CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF
--     DURATION'BASE.

-- HISTORY:
--     WRG 9/11/86
--     PWB 3/31/88  CHANGED RANGE FOR MEMBERSHIP TEST INVOLVING
--                  ABS (DECIMAL_M4'FIRST + DECIMAL_M4'SMALL / 2).
--     RJW 8/21/89  REMOVED CHECKS INVOLVING HARD-CODED FIXED-POINT
--                  UPPER BOUNDS WHICH WERE INCORRECT FOR SOME
--                  IMPLEMENTATIONS.  REVISED HEADER.
--     PWN 02/02/95 REMOVED INCONSISTENCIES WITH ADA 9X.
--     KAS 11/14/95 REMOVED CASES THAT DEPEND ON SPECIFIC VALUE FOR 'SMALL
--     TMB 11/19/94 REMOVED CASES RELATING TO 3.5.9(8) RULES - SMALL
--                  MAY BE LESS THAN OR EQUAL TO DELTA FOR FIXED POINT.

WITH REPORT; USE REPORT;
PROCEDURE C45651A IS

     -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S
     -- 'MANTISSA VALUE.

BEGIN

     TEST ("C45651A", "CHECK THAT, FOR FIXED POINT TYPES, THE ABS " &
                      "OPERATOR PRODUCES CORRECT RESULTS - BASIC " &
                      "TYPES");

     -------------------------------------------------------------------

A:   DECLARE
          TYPE LIKE_DURATION_M23 IS DELTA 0.020
               RANGE -86_400.0 .. 86_400.0;

          NON_MODEL_CONST : CONSTANT          := 2.0 / 3;
          NON_MODEL_VAR   : LIKE_DURATION_M23 := 0.0;

          SMALL, MAX, MIN, ZERO : LIKE_DURATION_M23 := 0.5;
          X                     : LIKE_DURATION_M23 := 1.0;
     BEGIN
          -- INITIALIZE "CONSTANTS":
          IF EQUAL (3, 3) THEN
               SMALL         := LIKE_DURATION_M23'SMALL;
               MAX           := LIKE_DURATION_M23'LAST;
               MIN           := LIKE_DURATION_M23'FIRST;
               ZERO          := 0.0;
               NON_MODEL_VAR := NON_MODEL_CONST;
          END IF;

          -- (A)
          IF EQUAL (3, 3) THEN
               X := SMALL;
          END IF;
          IF ABS X /= SMALL OR X /= ABS LIKE_DURATION_M23'SMALL THEN
               FAILED ("ABS (1.0 / 64) /= (1.0 / 64)");
          END IF;
          IF EQUAL (3, 3) THEN
               X := MAX;
          END IF;
          IF ABS X /= MAX OR X /= ABS LIKE_DURATION_M23'LAST THEN
               FAILED ("ABS 86_400.0 /= 86_400.0");
          END IF;

          -- (B)
          IF EQUAL (3, 3) THEN
               X := -SMALL;
          END IF;
          IF ABS X /= SMALL OR
             ABS (-LIKE_DURATION_M23'SMALL) /= SMALL THEN
               FAILED ("ABS -(1.0 / 64) /= (1.0 / 64)");
          END IF;
          IF EQUAL (3, 3) THEN
               X := MIN;
          END IF;
          IF ABS X /= MAX OR ABS LIKE_DURATION_M23'FIRST /= MAX THEN
               FAILED ("ABS -86_400.0 /= 86_400.0");
          END IF;

          -- (A) AND (B)
          IF EQUAL (3, 3) THEN
               X := 0.0;
          END IF;
          IF "ABS" (RIGHT => X) /= ZERO OR X /= ABS 0.0 THEN
               FAILED ("ABS 0.0 /= 0.0 -- (LIKE_DURATION_M23)");
          END IF;

          -- CHECK THAT VALUE OF NON_MODEL_VAR IS IN THE RANGE
          -- 42 * 'SMALL .. 43 * 'SMALL:
          IF NON_MODEL_VAR NOT IN 0.65625 .. 0.671875 THEN
               FAILED ("VALUE OF NON_MODEL_VAR NOT IN CORRECT RANGE " &
                       "- A");
          END IF;

          -- (C)
          IF ABS NON_MODEL_VAR NOT IN 0.65625 .. 0.671875 OR
             ABS LIKE_DURATION_M23'(NON_MODEL_CONST) NOT IN
                 0.65625 .. 0.671875 THEN
               FAILED ("ABS (2.0 / 3) NOT IN CORRECT RANGE - A");
          END IF;
          IF EQUAL (3, 3) THEN
               X := 86_399.992_187_5;  -- LIKE_DURATION_M23'LAST -
                                       -- 1.0 / 128.
          END IF;
          IF ABS X NOT IN 86_399.984_375 .. 86_400.0 OR
             ABS (LIKE_DURATION_M23'LAST - LIKE_DURATION_M23'SMALL / 2)
                 NOT IN 86_399.984_375 .. 86_400.0 THEN
               FAILED ("ABS (LIKE_DURATION_M23'LAST - " &
                       "LIKE_DURATION_M23'SMALL / 2) NOT IN CORRECT " &
                       "RANGE");
          END IF;

          -- (D)
          IF EQUAL (3, 3) THEN
               X := -NON_MODEL_CONST;
          END IF;
          IF ABS X NOT IN 0.65625 .. 0.671875 OR
             ABS (-LIKE_DURATION_M23'(NON_MODEL_CONST)) NOT IN
                 0.65625 .. 0.671875 THEN
               FAILED ("ABS (-2.0 / 3) NOT IN CORRECT RANGE - A");
          END IF;
          IF EQUAL (3, 3) THEN
               X := -86_399.992_187_5;  -- LIKE_DURATION_M23'FIRST +
                                        -- 1.0 / 128.
          END IF;
          IF ABS X NOT IN 86_399.984_375 .. 86_400.0 OR
             ABS (LIKE_DURATION_M23'FIRST + LIKE_DURATION_M23'SMALL / 2)
                 NOT IN 86_399.984_375 .. 86_400.0 THEN
               FAILED ("ABS (LIKE_DURATION_M23'FIRST +" &
                       "LIKE_DURATION_M23'SMALL / 2) NOT IN CORRECT " &
                       "RANGE");
          END IF;
     END A;

     -------------------------------------------------------------------

B:   DECLARE
          TYPE DECIMAL_M4 IS DELTA 100.0 RANGE -1000.0 .. 1000.0;

          NON_MODEL_CONST : CONSTANT   := 2.0 / 3;
          NON_MODEL_VAR   : DECIMAL_M4 := 0.0;

          SMALL, MAX, MIN, ZERO : DECIMAL_M4 := 128.0;
          X                     : DECIMAL_M4 :=   0.0;
     BEGIN
          -- INITIALIZE "CONSTANTS":
          IF EQUAL (3, 3) THEN
               SMALL         :=  DECIMAL_M4'SMALL;
               ZERO          :=  0.0;
               NON_MODEL_VAR := NON_MODEL_CONST;
          END IF;

          -- (A)
          IF EQUAL (3, 3) THEN
               X := SMALL;
          END IF;
          IF ABS X /= SMALL OR X /= ABS DECIMAL_M4'SMALL THEN
               FAILED ("ABS 64.0 /= 64.0");
          END IF;

          -- (B)
          IF EQUAL (3, 3) THEN
               X := -SMALL;
          END IF;
          IF ABS X /= SMALL OR ABS (-DECIMAL_M4'SMALL) /= SMALL THEN
               FAILED ("ABS -64.0 /= 64.0");
          END IF;

          -- (A) AND (B)
          IF EQUAL (3, 3) THEN
               X := 0.0;
          END IF;
          IF ABS X /= ZERO OR X /= ABS 0.0 THEN
               FAILED ("ABS 0.0 /= 0.0 -- (DECIMAL_M4)");
          END IF;

          -- CHECK THE VALUE OF NON_MODEL_VAR:
          IF NON_MODEL_VAR NOT IN 0.0 .. 64.0 THEN
               FAILED ("VALUE OF NON_MODEL_VAR NOT IN CORRECT RANGE " &
                       "- B");
          END IF;

          -- (C)
          IF ABS NON_MODEL_VAR NOT IN 0.0 .. 64.0 OR
             ABS DECIMAL_M4'(NON_MODEL_CONST) NOT IN 0.0 .. 64.0 THEN
               FAILED ("ABS (2.0 / 3) NOT IN CORRECT RANGE - B");
          END IF;
          IF EQUAL (3, 3) THEN
               X := 37.0;  -- INTERVAL IS 0.0 .. 64.0.
          END IF;
          IF EQUAL (3, 3) THEN
               X := 928.0;
          END IF;

          -- (D)
          IF EQUAL (3, 3) THEN
               X := -NON_MODEL_CONST;
          END IF;
          IF ABS X NOT IN 0.0 .. 64.0 OR
             ABS (-DECIMAL_M4'(NON_MODEL_CONST)) NOT IN 0.0 .. 64.0 THEN
               FAILED ("ABS -(2.0 / 3) NOT IN CORRECT RANGE - B");
          END IF;
          IF EQUAL (3, 3) THEN
               X := -37.0;  -- INTERVAL IS -SMALL .. 0.0.
          END IF;
          IF EQUAL (3, 3) THEN
               X := -928.0;
          END IF;
     END B;

     -------------------------------------------------------------------

     RESULT;

END C45651A;
