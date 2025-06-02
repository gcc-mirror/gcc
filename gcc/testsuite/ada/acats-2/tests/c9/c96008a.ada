-- C96008A.ADA

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
-- MISCELLANEOUS CHECKS ON THE PRE-DEFINED FUNCTIONS IN THE PACKAGE
-- CALENDAR.  SUBTESTS ARE:
--   (A) TIME_OF() AND SPLIT() ARE INVERSE FUNCTIONS.
--   (B) FORMAL PARAMETERS OF TIME_OF() AND SPLIT() ARE NAMED CORRECTLY.
--   (C) TIME_OF() GIVES THE PARAMETER SECONDS A DEFAULT VALUE OF 0.0.
--   (D) THE FUNCTIONS YEAR(), MONTH(), DAY(), AND SECONDS() RETURN
--       CORRECT VALUES USING NAMED NOTATION.
--   (E) A VALUE RETURNED FROM CLOCK() CAN BE PROCESSED BY SPLIT().
--   (F) DURATION'SMALL MEETS REQUIRED LIMIT.

-- CPP 8/16/84

WITH SYSTEM;
WITH CALENDAR;  USE CALENDAR;
WITH REPORT;  USE REPORT;
PROCEDURE C96008A IS

BEGIN
     TEST ("C96008A", "CHECK MISCELLANEOUS FUNCTIONS IN THE " &
           "PACKAGE CALENDAR");

     ---------------------------------------------

     DECLARE   -- (A)
          NOW : TIME;
          YR : YEAR_NUMBER;
          MO : MONTH_NUMBER;
          DY : DAY_NUMBER;
          SEC : DAY_DURATION;
     BEGIN     -- (A)
          BEGIN
               NOW := TIME_OF (1984, 8, 13, DURATION(1.0/3.0));
               SPLIT (NOW, YR, MO, DY, SEC);
               IF NOW /= TIME_OF (YR, MO, DY, SEC) THEN
                    COMMENT ("TIME_OF AND SPLIT ARE NOT INVERSES " &
                             "WHEN SECONDS IS A NON-MODEL NUMBER " &
                             "- (A)");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("TIME_OF(SPLIT) RAISED EXCEPTION - (A)");
          END;


          BEGIN
               -- RESET VALUES.
               YR := 1984;
               MO := 8;
               DY := 13;
               SEC := 1.0;

               SPLIT (TIME_OF (YR, MO, DY, SEC), YR, MO, DY, SEC);

               IF YR /= 1984 THEN
                    FAILED ("SPLIT(TIME_OF) CHANGED VALUE OF YR - (A)");
               END IF;

               IF MO /= 8 THEN
                    FAILED ("SPLIT(TIME_OF) CHANGED VALUE OF MO - (A)");
               END IF;

               IF DY /= 13 THEN
                    FAILED ("SPLIT(TIME_OF) CHANGED VALUE OF DY - (A)");
               END IF;

               IF SEC /= 1.0 THEN
                    FAILED ("SPLIT(TIME_OF) CHANGED VALUE OF " &
                            "SEC - (A)");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("SPLIT(TIME_OF) PROCESSING RAISED " &
                            "EXCEPTION - (A)");
          END;
     END; -- (A)

     ---------------------------------------------

     BEGIN     -- (B)
          DECLARE
               NOW : TIME;
          BEGIN
               NOW := TIME_OF (YEAR => 1984,
                               MONTH => 8,
                               DAY => 13,
                               SECONDS => 60.0);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("NAMED ASSOCIATION ON TIME_OF() RAISED " &
                            "EXCEPTION - (B)");
          END;


          DECLARE
               NOW : TIME := CLOCK;
               YR : YEAR_NUMBER := 1984;
               MO : MONTH_NUMBER := 8;
               DY : DAY_NUMBER := 13;
               SEC : DAY_DURATION := 0.0;
          BEGIN
               SPLIT (DATE => NOW,
                      YEAR => YR,
                      MONTH => MO,
                      DAY => DY,
                      SECONDS => SEC);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("NAMED ASSOCIATION ON SPLIT() RAISED " &
                            "EXCEPTION - (B)2");
          END;
     END; -- (B)

     ---------------------------------------------

     DECLARE   -- (C)
          NOW : TIME;
     BEGIN     -- (C)
          NOW := TIME_OF (1984, 8, 13);
          IF SECONDS (NOW) /= 0.0 THEN
               FAILED ("TIME_OF() DID NOT ZERO SECONDS - (C)");
          END IF;
     END; -- (C)

     ---------------------------------------------

     DECLARE   -- (D)
     -- ASSUMES TIME_OF() WORKS CORRECTLY.
          HOLIDAY : TIME;
     BEGIN     -- (D)
          HOLIDAY := TIME_OF (1958, 9, 9, 1.0);

          IF YEAR (DATE => HOLIDAY) /= 1958 THEN
               FAILED ("YEAR() DID NOT RETURN CORRECT VALUE - (D)");
          END IF;

          IF MONTH (DATE => HOLIDAY) /= 9 THEN
               FAILED ("MONTH() DID NOT RETURN CORRECT VALUE - (D)");
          END IF;

          IF DAY (DATE => HOLIDAY) /= 9 THEN
               FAILED ("DAY() DID NOT RETURN CORRECT VALUE - (D)");
          END IF;

          IF SECONDS (HOLIDAY) /= 1.0 THEN
               FAILED ("SECONDS() DID NOT RETURN CORRECT VALUE - (D)");
          END IF;
     END; -- (D)

     ---------------------------------------------

     DECLARE   -- (E)
          YR : YEAR_NUMBER;
          MO : MONTH_NUMBER;
          DY : DAY_NUMBER;
          SEC : DAY_DURATION;
     BEGIN     -- (E)
          SPLIT (CLOCK, YR, MO, DY, SEC);
          DELAY SYSTEM.TICK;

          IF TIME_OF (YR, MO, DY, SEC) > CLOCK THEN
               FAILED ("SPLIT() ON CLOCK INCORRECT - (E)");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("SPLIT() ON CLOCK RAISED EXCEPTION - (E)");
     END; -- (E)

     ---------------------------------------------

     BEGIN     -- (F)
          IF DURATION'SMALL > 0.020 THEN
               FAILED ("DURATION'SMALL LARGER THAN SPECIFIED - (F)");
          END IF;
     END; -- (F)

     ---------------------------------------------

     RESULT;
END C96008A;
