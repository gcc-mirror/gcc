-- C45532H.ADA

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
--     CHECK THAT THE OPERATOR "/" PRODUCES CORRECT RESULTS
--     FOR FIXED POINT TYPES USING 3 SUBTESTS.
--       THIS TEST REQUIRES MIN_WORD_LENGTH = 16.
--       THIS TEST USES VALUES OF DELTA WHICH ARE GREATER THAN OR
--       EQUAL TO 0.5.
--
--     TEST CASES ARE:
--       A) THE OPERATOR /, A, B, AND A / B ALL MODEL NUMBERS.
--       B) THE OPERATOR /, A, B MODEL NUMBERS A / B NOT.
--       C) THE OPERATOR /, USING NO MODEL NUMBERS.
--
--     REPEAT FOR MINIMUM REQUIRED WORD LENGTHS OF 12, 16, 32 AND 48,
--     WITH RANGE <, =, AND > THAN 1.0 AND
--     WITH DELTA <, =, AND > THAN 1.0.

-- HISTORY:
--     NTW 09/08/86 CREATED ORIGINAL TEST.
--     RJW 11/05/86 REVISED COMMENTS.
--     DHH 01/13/88 ADDED APPLICABILITY CRITERIA AND STANDARD HEADER.
--     RDH 04/27/90 REVISED APPLICABILITY CRITERIA.
--     BCB 10/03/90 REMOVED APPLICABILITY CRITERIA AND N/A => ERROR
--                  LINE.  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH REPORT;
PROCEDURE C45532H IS

     USE REPORT;

     MIN_WORD_LENGTH : CONSTANT := 16;
     FULL_SCALE      : CONSTANT := 2 ** (MIN_WORD_LENGTH - 1);
     A_THIRD         : CONSTANT := FULL_SCALE / 3;
     RNG1            : CONSTANT := FULL_SCALE * 0.5;
     TYPE FX_0P5  IS DELTA 0.5 RANGE -RNG1 * 1 .. RNG1 * 1 - 0.5;
     TYPE FX_1    IS DELTA 1.0 RANGE -RNG1 * 2 .. RNG1 * 2 - 1.0;
     TYPE FX_RNG1 IS DELTA RNG1
                    RANGE -RNG1 * FULL_SCALE .. RNG1 * (FULL_SCALE - 1);

BEGIN TEST ("C45532H", "FIXED POINT OPERATOR ""/""" );

     --------------------------------------------------

     -- CASE A) THE OPERATOR /, A, B, AND A / B ALL MODEL NUMBERS.

A:   DECLARE
          A              : FX_RNG1 := 0.0;
          B              : FX_0P5  := 0.0;
          RESULT_VALUE   : FX_0P5  := 0.0;
          LOWEST_ACCEPTABLE_VALUE  : FX_0P5 := FX_0P5 (RNG1 / 2);
          HIGHEST_ACCEPTABLE_VALUE : FX_0P5 := FX_0P5 (RNG1 / 2);
     BEGIN
          IF EQUAL (3, 3) THEN
               A := FX_RNG1 (RNG1 * RNG1 / 4);    -- A MODEL NUMBER
               B := FX_0P5  (RNG1 / 2);           -- A MODEL NUMBER
          END IF;

          RESULT_VALUE := FX_0P5 (A / B);

          IF    (RESULT_VALUE < LOWEST_ACCEPTABLE_VALUE)
             OR (RESULT_VALUE > HIGHEST_ACCEPTABLE_VALUE) THEN
               FAILED ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL "
                    &  "WHEN A, B, AND A / B ARE ALL MODEL NUMBERS");
          END IF;
     END A;

     --------------------------------------------------

     -- CASE B) THE OPERATOR /, A, B MODEL NUMBERS A / B NOT.

B:   DECLARE
          A              : FX_RNG1 := 0.0;
          B              : FX_1    := 0.0;
          RESULT_VALUE   : FX_0P5  := 0.0;
          LOWEST_ACCEPTABLE_VALUE  :  FX_0P5
                                   := FX_0P5 (0.5 * A_THIRD);
          HIGHEST_ACCEPTABLE_VALUE :  FX_0P5
                                   := FX_0P5 (0.5 * (A_THIRD + 1) );
     BEGIN
          IF EQUAL (3, 3) THEN
               A := FX_RNG1 (RNG1);               -- A MODEL NUMBER
               B := FX_1    (3.0);                -- A MODEL NUMBER
          END IF;

          RESULT_VALUE := FX_0P5 (A / B);

          IF    (RESULT_VALUE < LOWEST_ACCEPTABLE_VALUE)
             OR (RESULT_VALUE > HIGHEST_ACCEPTABLE_VALUE) THEN
               FAILED ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL "
                    &  "WHEN A, B MODEL NUMBERS A / B NOT");
          END IF;
     END B;

     --------------------------------------------------

     -- CASE C) THE OPERATOR /, USING NO MODEL NUMBERS

C:   DECLARE
          A              : FX_RNG1 := 0.0;
          B              : FX_1    := 0.0;
          RESULT_VALUE   : FX_1    := 0.0;
          LOWEST_ACCEPTABLE_VALUE  :  FX_1
                                   := FX_1 ( RNG1 - 3.0);
          HIGHEST_ACCEPTABLE_VALUE :  FX_1
                                   := FX_1 ( RNG1 + 4.0);
     BEGIN
          IF EQUAL (3, 3) THEN
               A := FX_RNG1 (RNG1 * RNG1 / 3);    -- NOT A MODEL NUMBER
               B := FX_1    (RNG1 / 3);           -- NOT A MODEL NUMBER
          END IF;

          RESULT_VALUE := FX_1 (A / B);

          IF    (RESULT_VALUE < LOWEST_ACCEPTABLE_VALUE)
             OR (RESULT_VALUE > HIGHEST_ACCEPTABLE_VALUE) THEN
               FAILED ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL "
                    &  "WHEN USING NO MODEL NUMBERS");
          END IF;
     END C;

     --------------------------------------------------


     RESULT;

END C45532H;
