-- C96006A.ADA

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
-- CHECK THAT FOR THE PACKAGE CALENDAR, THE RELATIONAL OPERATORS WORK
-- CORRECTLY FOR OPERANDS OF TYPE TIME AND TYPE DURATION. PARTICULARLY,
--   (A) RELATIONS BASED ON YEARS.
--   (B) RELATIONS BASED ON MONTH.
--   (C) RELATIONS BASED ON SECONDS.
--   (D) RELATIONS AT EXTREMES OF THE PERMITTED RANGE OF TIME.

-- CPP 8/16/84

WITH CALENDAR;  USE CALENDAR;
WITH REPORT;  USE REPORT;
PROCEDURE C96006A IS

BEGIN
     TEST ("C96006A", "CHECK THAT RELATIONAL OPERATORS WORK " &
           "CORRECTLY IN THE PACKAGE CALENDAR");

     --------------------------------------------

     DECLARE   -- (A)
     -- RELATIONS BASED ON YEARS.
          NOW, LATER : TIME;
     BEGIN     -- (A)
          NOW := TIME_OF (1984, 8, 12, 500.0);
          LATER := TIME_OF (1985, 8, 12, 500.0);

          IF NOW < LATER THEN
               COMMENT ("< OPERATOR OK - (A)");
          ELSE
               FAILED ("< OPERATOR INCORRECT - (A)");
          END IF;

          IF NOW <= LATER THEN
               COMMENT ("<= OPERATOR OK - (A)");
          ELSE
               FAILED ("<= OPERATOR INCORRECT - (A)");
          END IF;

          IF NOW <= NOW THEN
               COMMENT ("<= OPERATOR OK - (A)2");
          ELSE
               FAILED ("<= OPERATOR INCORRECT - (A)2");
          END IF;

          IF LATER > NOW THEN
               COMMENT ("> OPERATOR OK - (A)");
          ELSE
               FAILED ("> OPERATOR INCORRECT - (A)");
          END IF;

          IF LATER >= NOW THEN
               COMMENT (">= OPERATOR OK - (A)");
          ELSE
               FAILED (">= OPERATOR INCORRECT - (A)");
          END IF;

          IF LATER >= LATER THEN
               COMMENT (">= OPERATOR OK - (A)2");
          ELSE
               FAILED (">= OPERATOR INCORRECT - (A)2");
          END IF;

     END; -- (A)

     --------------------------------------------

     DECLARE   -- (B)
     -- RELATIONS BASED ON MONTH.
          NOW, LATER : TIME;
     BEGIN     -- (B)
          NOW := TIME_OF (1984, 8, 12, 500.0);
          LATER := TIME_OF (1984, 9, 12, 500.0);

          IF NOW < LATER THEN
               COMMENT ("< OPERATOR OK - (B)");
          ELSE
               FAILED ("< OPERATOR INCORRECT - (B)");
          END IF;

          IF NOW <= LATER THEN
               COMMENT ("<= OPERATOR OK - (B)");
          ELSE
               FAILED ("<= OPERATOR INCORRECT - (B)");
          END IF;

          IF NOW <= NOW THEN
               COMMENT ("<= OPERATOR OK - (B)2");
          ELSE
               FAILED ("<= OPERATOR INCORRECT - (B)2");
          END IF;

          IF LATER > NOW THEN
               COMMENT ("> OPERATOR OK - (B)");
          ELSE
               FAILED ("> OPERATOR INCORRECT - (B)");
          END IF;

          IF LATER >= NOW THEN
               COMMENT (">= OPERATOR OK - (B)");
          ELSE
               FAILED (">= OPERATOR INCORRECT - (B)");
          END IF;

          IF LATER >= LATER THEN
               COMMENT (">= OPERATOR OK - (B)2");
          ELSE
               FAILED (">= OPERATOR INCORRECT - (B)2");
          END IF;

          IF NOW = NOW THEN
               COMMENT ("= OPERATOR OK - (B)");
          ELSE
               FAILED ("= OPERATOR INCORRECT - (B)");
          END IF;

          IF LATER /= NOW THEN
               COMMENT ("/= OPERATOR OK - (B)");
          ELSE
               FAILED ("/= OPERATOR INCORRECT - (B)");
          END IF;

     END; -- (B)

     --------------------------------------------

     DECLARE   -- (C)
     -- RELATIONS BASED ON SECONDS.
          NOW, LATER : TIME;
          INCREMENT : DURATION := 99.9;
     BEGIN     -- (C)
          NOW := TIME_OF (1984, 8, 12, 500.0);
          LATER := NOW + INCREMENT;

          IF NOW < LATER THEN
               COMMENT ("< OPERATOR OK - (C)");
          ELSE
               FAILED ("< OPERATOR INCORRECT - (C)");
          END IF;

          IF NOW <= LATER THEN
               COMMENT ("<= OPERATOR OK - (C)");
          ELSE
               FAILED ("<= OPERATOR INCORRECT - (C)");
          END IF;

          IF NOW <= NOW THEN
               COMMENT ("<= OPERATOR OK - (C)2");
          ELSE
               FAILED ("<= OPERATOR INCORRECT - (C)2");
          END IF;

          IF LATER > NOW THEN
               COMMENT ("> OPERATOR OK - (C)");
          ELSE
               FAILED ("> OPERATOR INCORRECT - (C)");
          END IF;

          IF LATER >= NOW THEN
               COMMENT (">= OPERATOR OK - (C)");
          ELSE
               FAILED (">= OPERATOR INCORRECT - (C)");
          END IF;

          IF LATER >= LATER THEN
               COMMENT (">= OPERATOR OK - (C)2");
          ELSE
               FAILED (">= OPERATOR INCORRECT - (C)2");
          END IF;

          IF LATER = LATER THEN
               COMMENT ("= OPERATOR OK - (C)");
          ELSE
               FAILED ("= OPERATOR INCORRECT - (C)");
          END IF;

          IF NOW /= LATER THEN
               COMMENT ("/= OPERATOR OK - (C)");
          ELSE
               FAILED ("/= OPERATOR INCORRECT - (C)");
          END IF;

          IF NOW < NOW THEN
               FAILED ("NOW < NOW INCORRECT - (C)");
          ELSIF NOW /= NOW THEN
               FAILED ("NOW = NOW INCORRECT - (C)");
          ELSIF LATER < NOW THEN
               FAILED ("LATER < NOW INCORRECT - (C)");
          ELSIF LATER <= NOW THEN
               FAILED ("LATER <= NOW INCORRECT - (C)");
          ELSIF LATER = NOW THEN
               FAILED ("NOW = LATER INCORRECT - (C)");
          ELSIF NOW > LATER THEN
               FAILED ("NOW > LATER INCORRECT - (C)");
          ELSIF NOW > NOW THEN
               FAILED ("NOW > NOW INCORRECT - (C)");
          ELSIF NOW >= LATER THEN
               FAILED ("NOW >= LATER INCORRECT - (C)");
          ELSIF NOW = LATER THEN
               FAILED ("NOW = LATER INCORRECT - (C)");
          END IF;

     END; -- (C)

     --------------------------------------------

     DECLARE   -- (D)

          NOW, WAY_BACK_THEN : TIME;

     BEGIN     -- (D)

          NOW := TIME_OF (2099, 12, 31);
          WAY_BACK_THEN := TIME_OF (1901, 1, 1);

          BEGIN
               IF NOW < WAY_BACK_THEN THEN
                    FAILED ("TEST < AT EXTREMES INCORRECT - (D)");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("< AT EXTREMES RAISED EXCEPTION - (D)");
          END;

          BEGIN
               IF NOW <= WAY_BACK_THEN THEN
                    FAILED ("TEST <= AT EXTREMES INCORRECT - (D)");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("<= AT EXTREMES RAISED EXCEPTION - (D)");
          END;

          BEGIN
               IF WAY_BACK_THEN > NOW THEN
                    FAILED ("TEST > AT EXTREMES INCORRECT - (D)");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("> AT EXTREMES RAISED EXCEPTION - (D)");
          END;

          BEGIN
               IF WAY_BACK_THEN >= NOW THEN
                    FAILED ("TEST >= AT EXTREMES INCORRECT - (D)");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED (">= AT EXTREMES RAISED EXCEPTION - (D)");
          END;

          BEGIN
               IF WAY_BACK_THEN /= WAY_BACK_THEN THEN
                    FAILED ("TEST /= AT EXTREMES INCORRECT - (D)");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("/= AT EXTREMES RAISED EXCEPTION - (D)");
          END;

          BEGIN
               IF NOW = WAY_BACK_THEN THEN
                    FAILED ("TEST = AT EXTREMES INCORRECT - (D)");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("= AT EXTREMES RAISED EXCEPTION - (D)");
          END;

     END; -- (D)

     --------------------------------------------

     RESULT;
END C96006A;
