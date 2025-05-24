-- C96005A.ADA

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
--   (A) CHECK THAT ADDITION AND SUBTRACTION OPERATORS WORK CORRECTLY ON
--       VALUES OF TYPE TIME.

-- CPP 8/16/84

WITH CALENDAR;  USE CALENDAR;
WITH REPORT;  USE REPORT;
-- WITH TEXT_IO;  USE TEXT_IO;
PROCEDURE C96005A IS

     -- PACKAGE DURATION_IO IS NEW FIXED_IO (DURATION);
     -- USE DURATION_IO;

BEGIN
     TEST ("C96005A", "CHECK THAT THE ADDITION AND SUBTRACTION " &
           "FUNCTIONS FOR VALUES OF TYPE TIME WORK CORRECTLY");

     -----------------------------------------------

     BEGIN     -- (A)

          -- ADDITION TESTS FOLLOW.
          DECLARE
               NOW, NEW_TIME : TIME;
               INCREMENT : DURATION := 1.0;
          BEGIN
               NOW := TIME_OF (1984, 8, 13, 0.0);
               NEW_TIME := NOW + INCREMENT;
               IF NEW_TIME /= TIME_OF (1984, 8, 13, 1.0) THEN
                    FAILED ("SUM OF TIMES IS INCORRECT - (A)1");
               END IF;
          END;


          DECLARE
               NOW, NEW_TIME : TIME;
               INCREMENT : DURATION := 1.0;
          BEGIN
               NOW := TIME_OF (1984, 8, 13, 0.0);
               NEW_TIME :=  INCREMENT + NOW;
               IF NEW_TIME /= TIME_OF (1984, 8, 13, 1.0) THEN
                    FAILED ("SUM OF TIMES IS INCORRECT - (A)2");
               END IF;
          END;


          DECLARE
               NOW, NEW_TIME : TIME;
               INCREMENT : DURATION := 1.0;
          BEGIN
               NOW := TIME_OF (1984, 8, 13, 0.0);
               NEW_TIME := "+"(INCREMENT, NOW);
               IF NEW_TIME /= TIME_OF (1984, 8, 13, 1.0) THEN
                    FAILED ("SUM OF TIMES IS INCORRECT - (A)3");
               END IF;
          END;


          DECLARE
               NOW, NEW_TIME : TIME;
               INCREMENT : DURATION := 1.0;
          BEGIN
               NOW := TIME_OF (1984, 8, 13, 0.0);
               NEW_TIME := "+"(LEFT => NOW,
                               RIGHT => INCREMENT);
               IF NEW_TIME /= TIME_OF (1984, 8, 13, 1.0) THEN
                    FAILED ("SUM OF TIMES IS INCORRECT - (A)4");
               END IF;
          END;


          -- SUBTRACTION TESTS FOLLOW.
          DECLARE
               NOW, ONCE : TIME;
               DIFFERENCE : DURATION;
          BEGIN
               NOW := TIME_OF (1984, 8, 13, 45_000.0);
               ONCE := TIME_OF (1984, 8, 12, 45_000.0);
               DIFFERENCE := NOW - ONCE;
               IF DIFFERENCE /= 86_400.0 THEN
                    FAILED ("DIFFERENCE OF TIMES IS INCORRECT - (A)1");
                    -- COMMENT ("DIFFERENCE YIELDS: ");
                    -- PUT (DIFFERENCE);
               END IF;
          END;


          DECLARE
          -- TIMES IN DIFFERENT MONTHS.
               NOW, ONCE : TIME;
               DIFFERENCE : DURATION;
          BEGIN
               NOW := TIME_OF (1984, 8, IDENT_INT(1), 60.0);
               ONCE := TIME_OF (1984, 7, 31, 86_399.0);
               DIFFERENCE := "-"(NOW, ONCE);
               IF DIFFERENCE /= 61.0 THEN
                    FAILED ("DIFFERENCE OF TIMES IS INCORRECT - (A)2");
                    -- COMMENT ("DIFFERENCE YIELDS: ");
                    -- PUT (DIFFERENCE);
               END IF;
          END;


          DECLARE
          -- TIMES IN DIFFERENT YEARS.
               NOW, AFTER : TIME;
               DIFFERENCE : DURATION;
          BEGIN
               NOW := TIME_OF (IDENT_INT(1999), 12, 31, 86_399.0);
               AFTER := TIME_OF (2000, 1, 1, 1.0);
               DIFFERENCE := "-"(LEFT => AFTER,
                                 RIGHT => NOW);
               IF DIFFERENCE /= 2.0 THEN
                    FAILED ("DIFFERENCE OF TIMES IS INCORRECT - (A)3");
                    -- COMMENT ("DIFFERENCE YIELDS: ");
                    -- PUT (DIFFERENCE);
               END IF;
          END;


          DECLARE
          -- TIMES IN A LEAP YEAR.
               NOW, LEAP : TIME;
               DIFFERENCE : DURATION;
          BEGIN
               NOW := TIME_OF (1984, 3, 1);
               LEAP := TIME_OF (1984, 2, 29, 86_399.0);
               DIFFERENCE := NOW - LEAP;
               IF DIFFERENCE /= 1.0 THEN
                    FAILED ("DIFFERENCE OF TIMES IS INCORRECT - (A)4");
                    -- COMMENT ("DIFFERENCE YIELDS: ");
                    -- PUT (DIFFERENCE);
               END IF;
          END;


          DECLARE
          -- TIMES IN A NON-LEAP YEAR.
               NOW, NON_LEAP : TIME;
               DIFFERENCE : DURATION;
          BEGIN
               NOW := TIME_OF (1983, 3, 1);
               NON_LEAP := TIME_OF (1983, 2, 28, 86_399.0);
               DIFFERENCE := NOW - NON_LEAP;
               IF DIFFERENCE /= 1.0 THEN
                    FAILED ("DIFFERENCE OF TIMES IS INCORRECT - (A)5");
                    -- COMMENT ("DIFFERENCE YIELDS: ");
                    -- PUT (DIFFERENCE);
               END IF;
          END;


          -- SUBTRACTION TESTS FOLLOW: TIME - DURATION.
          DECLARE
               NOW, NEW_TIME : TIME;
               INCREMENT : DURATION := 1.0;
          BEGIN
               NOW := TIME_OF (1984, 8, 13, 0.0);
               NEW_TIME := NOW - INCREMENT;
               IF NEW_TIME /= TIME_OF (1984, 8, 12, 86_399.0) THEN
                    FAILED ("DIFFERENCE OF TIME AND DURATION IS " &
                            "INCORRECT - (A)6");
               END IF;
          END;


          DECLARE
               NOW, NEW_TIME : TIME;
               INCREMENT : DURATION := 1.0;
          BEGIN
               NOW := TIME_OF (1984, 8, 1, 0.0);
               NEW_TIME := NOW - INCREMENT;
               IF NEW_TIME /= TIME_OF (1984, 7, 31, 86_399.0) THEN
                    FAILED ("DIFFERENCE OF TIME AND DURATION IS " &
                            "INCORRECT - (A)7");
               END IF;
          END;


          DECLARE
               NOW, NEW_TIME : TIME;
               INCREMENT : DURATION := 1.0;
          BEGIN
               NOW := TIME_OF (1984, 8, 1, 0.0);
               NEW_TIME := "-"(LEFT => NOW,
                               RIGHT => INCREMENT);
               IF NEW_TIME /= TIME_OF (1984, 7, 31, 86_399.0) THEN
                    FAILED ("DIFFERENCE OF TIME AND DURATION IS " &
                            "INCORRECT - (A)8");
               END IF;
          END;


          DECLARE
               NOW, NEW_TIME : TIME;
               INCREMENT : DURATION := 1.0;
          BEGIN
               NOW := TIME_OF (1984, 8, 1, 0.0);
               NEW_TIME := "-"(NOW, INCREMENT);
               IF NEW_TIME /= TIME_OF (1984, 7, 31, 86_399.0) THEN
                    FAILED ("DIFFERENCE OF TIME AND DURATION IS " &
                            "INCORRECT - (A)7");
               END IF;
          END;


     END; -- (A)

     -----------------------------------------------

     RESULT;
END C96005A;
