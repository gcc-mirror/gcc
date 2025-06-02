-- C45532B.ADA

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
--       THIS TEST REQUIRES MIN_WORD_LENGTH = 12.
--       THIS TEST USES VALUES OF DELTA WHICH ARE LESS THAN 0.5.
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
--     DHH 10/20/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.
--     RDH 04/27/90 REVISED APPLICABILITY CRITERIA.
--     BCB 10/03/90 REMOVED APPLICABILITY CRITERIA AND N/A => ERROR
--                  LINE.  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH REPORT;
PROCEDURE C45532B IS

     USE REPORT;

     MIN_WORD_LENGTH : CONSTANT := 12;            -- MUST BE EVEN & >= 6
     FULL_SCALE      : CONSTANT := 2 ** (MIN_WORD_LENGTH - 1);
     FORTH           : CONSTANT := FULL_SCALE / 4;
     A_THIRD         : CONSTANT := FULL_SCALE / 3;
     DEL1            : CONSTANT := 0.5 / FULL_SCALE;
     TYPE FX_0P5 IS DELTA DEL1 * 1 RANGE -0.5 ..
                                       0.5 - DEL1 * 1;
     TYPE FX_1   IS DELTA DEL1 * 2 RANGE -1.0 ..
                                       1.0 - DEL1 * 2;
     TYPE FX_2   IS DELTA DEL1 * 4 RANGE -2.0 ..
                                       2.0 - DEL1 * 4;

BEGIN TEST ("C45532B", "FIXED POINT OPERATOR ""/"" "
                     & "FOR RANGE <, =, AND > 1.0");

     --------------------------------------------------

     -- CASE A) THE OPERATOR /, A, B, AND A / B ALL MODEL NUMBERS.

A:   DECLARE
          A              : FX_0P5  := 0.0;
          B              : FX_1    := 0.0;
          RESULT_VALUE   : FX_2    := 0.0;
          LOWEST_ACCEPTABLE_VALUE  : FX_2 := FX_2 (0.5);
          HIGHEST_ACCEPTABLE_VALUE : FX_2 := FX_2 (0.5);
     BEGIN
          IF EQUAL (3, 3) THEN
               A := FX_0P5 (0.125);               -- A MODEL NUMBER
               B := FX_1   (0.25);                -- A MODEL NUMBER
          END IF;

          RESULT_VALUE := FX_2 (A / B);

          IF    (RESULT_VALUE < LOWEST_ACCEPTABLE_VALUE)
             OR (RESULT_VALUE > HIGHEST_ACCEPTABLE_VALUE) THEN
               FAILED ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL "
                    &  "WHEN A, B, AND A / B ARE ALL MODEL NUMBERS");
          END IF;
     END A;

     --------------------------------------------------

     -- CASE B) THE OPERATOR /, A, B MODEL NUMBERS A / B NOT.

B:   DECLARE
          A              : FX_0P5  := 0.0;
          B              : FX_0P5  := 0.0;
          RESULT_VALUE   : FX_1    := 0.0;
          LOWEST_ACCEPTABLE_VALUE  :  FX_1
                                   := FX_1 (2 * DEL1 * A_THIRD);
          HIGHEST_ACCEPTABLE_VALUE :  FX_1
                                   := FX_1 (2 * DEL1 * (A_THIRD + 1) );
     BEGIN
          IF EQUAL (3, 3) THEN
               A := FX_0P5 (DEL1 * 1);            -- A MODEL NUMBER
               B := FX_0P5 (DEL1 * 3);            -- A MODEL NUMBER
          END IF;

          RESULT_VALUE := FX_1 (A / B);

          IF    (RESULT_VALUE < LOWEST_ACCEPTABLE_VALUE)
             OR (RESULT_VALUE > HIGHEST_ACCEPTABLE_VALUE) THEN
               FAILED ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL "
                    &  "WHEN A, B MODEL NUMBERS A / B NOT");
          END IF;
     END B;

     --------------------------------------------------

     -- CASE C) THE OPERATOR /, USING NO MODEL NUMBERS

C:   DECLARE
          A              : FX_1    := 0.0;
          B              : FX_1    := 0.0;
          RESULT_VALUE   : FX_0P5  := 0.0;
          LOW_COUNT    : CONSTANT  := 2 * A_THIRD;
                            --   := (2 * FULL_SCALE * (2 * FORTH + 0))
                            --                      / (6 * FORTH + 2);
          HIGH_COUNT   : CONSTANT  := 2 * A_THIRD + 4;
                            --   := (2 * FULL_SCALE * (2 * FORTH + 2))
                            --                      / (6 * FORTH + 0);
          LOWEST_ACCEPTABLE_VALUE  :  FX_0P5
                                   := FX_0P5 (DEL1 * LOW_COUNT );
          HIGHEST_ACCEPTABLE_VALUE :  FX_0P5
                                   := FX_0P5 (DEL1 * HIGH_COUNT );
     BEGIN
          IF EQUAL (3, 3) THEN     -- A AND B NOT MODEL NUMBERS
               A := FX_1 (DEL1 * (2 * FORTH + 1));
               B := FX_1 (DEL1 * (6 * FORTH + 1));
          END IF;

          RESULT_VALUE := FX_0P5 (A / B);

          IF    (RESULT_VALUE < LOWEST_ACCEPTABLE_VALUE)
             OR (RESULT_VALUE > HIGHEST_ACCEPTABLE_VALUE) THEN
               FAILED ("RESULT OF ""/"" OUTSIDE RESULT MODEL INTERVAL "
                    &  "WHEN USING NO MODEL NUMBERS");
          END IF;
     END C;

     --------------------------------------------------


     RESULT;

END C45532B;
