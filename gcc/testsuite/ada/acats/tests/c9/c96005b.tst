-- C96005B.TST

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
-- CHECK THE CORRECTNESS OF THE ADDITION AND SUBTRACTION FUNCTIONS IN
-- THE PREDEFINED PACKAGE CALENDAR, AND APPROPRIATE EXCEPTION HANDLING.
-- SPECIFICALLY,
--   (B) ADDITION AND SUBTRACTION OPERATORS RAISE CONSTRAINT_ERROR WHEN
--       CALLED WITH AN OUT OF RANGE DURATION PARAMETER.

-- CPP 8/16/84

WITH CALENDAR;  USE CALENDAR;
WITH REPORT;  USE REPORT;
PROCEDURE C96005B IS

BEGIN
     TEST ("C96005B", "CHECK THAT ADDITION AND SUBTRACTION " &
           "OPERATORS RAISE CONSTRAINT_ERROR WHEN CALLED WITH " &
           "OUT OF RANGE DURATION PARAMETER");

     -----------------------------------------------

     BEGIN     -- (B)

          -- ADDITION TESTS FOLLOW.
          DECLARE
               BEFORE : TIME := CLOCK;
          BEGIN
               IF DURATION'BASE'FIRST < DURATION'FIRST THEN
                    COMMENT("LOW VALUES EXIST - (B)1");
                    BEFORE := BEFORE + ($LESS_THAN_DURATION);
                    FAILED ("EXCEPTION NOT RAISED - (B)1");
               ELSE
                    NOT_APPLICABLE ("NO LOW VALUES EXIST - (B)1");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN TIME_ERROR =>
                    FAILED ("TIME_ERROR RAISED INSTEAD OF " &
                            "CONSTRAINT_ERROR - (B)1");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (B)1");
          END;

          DECLARE
               BEFORE : TIME := CLOCK;
          BEGIN
               IF DURATION'LAST < DURATION'BASE'LAST THEN
                    COMMENT("HIGH VALUES EXIST - (B)2");
                    BEFORE := $GREATER_THAN_DURATION + BEFORE;
                    FAILED ("EXCEPTION NOT RAISED - (B)2");
               ELSE
                    NOT_APPLICABLE ("NO HIGH VALUES EXIST - (B)2");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN TIME_ERROR =>
                    FAILED ("TIME_ERROR RAISED INSTEAD OF " &
                            "CONSTRAINT_ERROR - (B)2");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (B)2");
          END;


          -- SUBTRACTION TESTS FOLLOW.
          DECLARE
               BEFORE : TIME := CLOCK;
          BEGIN
               IF DURATION'BASE'FIRST < DURATION'FIRST THEN
                    COMMENT("LOW VALUES EXIST - (B)3");
                    BEFORE := BEFORE - ($LESS_THAN_DURATION);
                    FAILED ("EXCEPTION NOT RAISED - (B)3");
               ELSE
                    NOT_APPLICABLE ("NO LOW VALUES EXIST - (B)3");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN TIME_ERROR =>
                    FAILED ("TIME_ERROR RAISED INSTEAD OF " &
                            "CONSTRAINT_ERROR - (B)3");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (B)3");
          END;

          DECLARE
               BEFORE : TIME := CLOCK;
          BEGIN
               IF DURATION'LAST < DURATION'BASE'LAST THEN
                    COMMENT("HIGH VALUES EXIST - (B)4");
                    BEFORE := BEFORE - $GREATER_THAN_DURATION;
                    FAILED ("EXCEPTION NOT RAISED - (B)4");
               ELSE
                    NOT_APPLICABLE ("NO HIGH VALUES EXIST - (B)4");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN TIME_ERROR =>
                    FAILED ("TIME_ERROR RAISED INSTEAD OF " &
                            "CONSTRAINT_ERROR - (B)4");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (B)4");
          END;


     END; -- (B)

     -----------------------------------------------

     RESULT;
END C96005B;
