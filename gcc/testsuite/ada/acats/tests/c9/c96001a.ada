-- C96001A.ADA

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
--      CHECK THAT A DELAY STATEMENT DELAYS EXECUTION FOR AT LEAST THE
--      SPECIFIED TIME. SPECIFICALLY,
--        (A) POSITIVE DELAY ARGUMENT.
--        (B) NEGATIVE DELAY ARGUMENT.
--        (C) ZERO DELAY ARGUMENT.
--        (D) DURATION'SMALL DELAY ARGUMENT.
--        (E) EXPRESSION OF TYPE DURATION AS DELAY ARGUMENT.

-- HISTORY:
--     CPP  8/14/84  CREATED ORIGINAL TEST.
--     RJW 11/13/87  ADDED CODE WHICH ALLOWS TEST TO REPORT "PASSED"
--                   IF TICK > DURATION'SMALL.

with Impdef;
WITH CALENDAR;  USE CALENDAR;
WITH SYSTEM;    USE SYSTEM;
WITH REPORT;    USE REPORT;
PROCEDURE C96001A IS

     SUBTYPE INT IS INTEGER RANGE 0 .. 20_000;

BEGIN
     TEST ("C96001A", "CHECK THAT DELAY STATEMENT DELAYS " &
           "EXECUTION FOR AT LEAST THE SPECIFIED TIME");

     ---------------------------------------------

     DECLARE   -- (A)
          X : DURATION := 5.0 * Impdef.One_Second;
          OLD_TIME : TIME;
          LAPSE : DURATION;
     BEGIN     -- (A)
          LOOP
               OLD_TIME := CLOCK;
               DELAY X;
               LAPSE := CLOCK - OLD_TIME;
               EXIT;
          END LOOP;
          IF LAPSE < X THEN
               FAILED ("DELAY DID NOT LAPSE AT LEAST 5.0 " &
                       "SECONDS - (A)");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (A)");
     END;

     ---------------------------------------------

     DECLARE   -- (B)
          OLD_TIME : TIME;
          LAPSE : DURATION;
     BEGIN     -- (B)
          LOOP
               OLD_TIME := CLOCK;
               DELAY -5.0;
               LAPSE := CLOCK - OLD_TIME;
               EXIT;
          END LOOP;
          COMMENT ("(B) - NEGATIVE DELAY LAPSED FOR " &
                   INT'IMAGE (INT (LAPSE * 1_000)) & " MILLISECONDS");
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (B)");
     END;

     ---------------------------------------------

     DECLARE   -- (C)
          X : DURATION := 0.0;
          OLD_TIME : TIME;
          LAPSE : DURATION;
     BEGIN     -- (C)
          LOOP
               OLD_TIME := CLOCK;
               DELAY X;
               LAPSE := CLOCK - OLD_TIME;
               EXIT;
          END LOOP;
          COMMENT ("(C) - ZERO DELAY LAPSED FOR " &
                   INT'IMAGE (INT (LAPSE * 1_000)) & " MILLISECONDS");
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (C)");
     END;

     ---------------------------------------------

     DECLARE   -- (D)
          X : DURATION := DURATION'SMALL;
          OLD_TIME : TIME;
          LAPSE : DURATION;
     BEGIN     -- (D)
          LOOP
               OLD_TIME := CLOCK;
               DELAY X;
               LAPSE := CLOCK - OLD_TIME;
               EXIT;
          END LOOP;
          IF LAPSE < X THEN
               IF TICK < DURATION'SMALL THEN
                    FAILED ("DELAY DID NOT LAPSE AT LEAST " &
                            "DURATION'SMALL SECONDS - (D)");
               ELSE
                    COMMENT ("TICK > DURATION'SMALL SO DELAY IN " &
                             "'(D)' IS NOT MEASURABLE");
               END IF;
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (D)");
     END;

     ---------------------------------------------

     DECLARE   -- (E)
          INC1 : DURATION := 2.0 * Impdef.One_Second;
          INC2 : DURATION := 3.0 * Impdef.One_Second;
          OLD_TIME : TIME;
          LAPSE : DURATION;
     BEGIN     -- (E)
          LOOP
               OLD_TIME := CLOCK;
               DELAY INC1 + INC2;
               LAPSE := CLOCK - OLD_TIME;
               EXIT;
          END LOOP;
          IF LAPSE < (INC1 + INC2) THEN
               FAILED ("DELAY DID NOT LAPSE AT LEAST " &
                       "INC1 + INC2 SECONDS - (E)");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (E)");
     END;

     RESULT;
END C96001A;
