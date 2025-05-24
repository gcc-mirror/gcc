-- C96004A.ADA

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
--     CHECK THAT THE PRE-DEFINED SUBTYPES FROM THE PACKAGE CALENDAR,
--     NAMELY YEAR_NUMBER, MONTH_NUMBER, DAY_NUMBER, AND DAY_DURATION,
--     HAVE THE CORRECT RANGE CONSTRAINTS. SUBTESTS ARE:
--       (A) YEAR_NUMBER.
--       (B) MONTH_NUMBER.
--       (C) DAY_NUMBER.
--       (D) DAY_DURATION.

-- HISTORY:
--     CPP 08/15/84  CREATED ORIGINAL TEST.
--     JET 01/06/88  UPDATED HEADER FORMAT AND ADDED CODE TO PREVENT
--                   OPTIMIZATION.
--     RLB 12/18/06  Changed so that the test will work for Ada 2005
--                   implementations.

WITH CALENDAR;  USE CALENDAR;
WITH REPORT;  USE REPORT;
PROCEDURE C96004A IS

BEGIN
     TEST("C96004A", "CHECK THAT PRE-DEFINED SUBTYPES FROM THE " &
          "CALENDAR PACKAGE HAVE CORRECT RANGE CONSTRAINTS");

     ---------------------------------------------

     DECLARE   -- (A)

          YR : YEAR_NUMBER;

     BEGIN     -- (A)

          BEGIN
               YR := 1900;
               FAILED ("EXCEPTION NOT RAISED - (A)1");
               IF NOT EQUAL (YR, YR) THEN
                    COMMENT ("NO EXCEPTION RAISED");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (A)1");
          END;

          BEGIN
               YR := 84;
               FAILED ("EXCEPTION NOT RAISED - (A)2");
               IF NOT EQUAL (YR, YR) THEN
                    COMMENT ("NO EXCEPTION RAISED");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (A)2");
          END;

          BEGIN
               YR := 2099;
               IF NOT EQUAL (YR, YR) THEN
                    COMMENT ("NO EXCEPTION RAISED");
               END IF;

          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("OK CASE RAISED EXCEPTION ON 2099 - (A)");
          END;

          BEGIN
               YR := IDENT_INT(2100);
               IF NOT EQUAL (YR, YR) THEN
                    COMMENT ("NO EXCEPTION RAISED");
               END IF;
               BEGIN
                    YR := 2399;
                    IF NOT EQUAL (YR, YR) THEN
                         COMMENT ("NO EXCEPTION RAISED");
                    END IF;

               EXCEPTION
                   WHEN OTHERS =>
                        FAILED ("ADA 2005 CASE RAISED EXCEPTION ON 2399 - (A)");
               END;
               BEGIN
                    YR := IDENT_INT(2400);
                    IF NOT EQUAL (YR, YR) THEN
                        COMMENT ("NO EXCEPTION RAISED");
                    END IF;
                    FAILED ("EXCEPTION NOT RAISED - (A)3");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         Comment ("Upper bound of Year_Number is appropriate" &
                                  " for Ada 2005");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    Comment ("Upper bound of Year_Number is appropriate" &
                             " for Ada 95");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (A)3");
          END;

     END; -- (A)

     ---------------------------------------------

     DECLARE   -- (B)

          MO : MONTH_NUMBER;

     BEGIN     -- (B)

          BEGIN
               MO := IDENT_INT(0);
               FAILED ("EXCEPTION NOT RAISED - (B)1");
               IF NOT EQUAL (MO, MO) THEN
                    COMMENT ("NO EXCEPTION RAISED");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (B)1");
          END;

          BEGIN
               MO := 12;
               IF NOT EQUAL (MO, MO) THEN
                    COMMENT ("NO EXCEPTION RAISED");
               END IF;

          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("OK CASE RAISED EXCEPTION ON 12 - (B)");
          END;

          BEGIN
               MO := 13;
               FAILED ("EXCEPTION NOT RAISED - (B)2");
               IF NOT EQUAL (MO, MO) THEN
                    COMMENT ("NO EXCEPTION RAISED");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (B)2");
          END;

     END; -- (B)

     ---------------------------------------------

     DECLARE   -- (C)

          DY : DAY_NUMBER;

     BEGIN     -- (C)

          BEGIN
               DY := 0;
               FAILED ("EXCEPTION NOT RAISED - (C)1");
               IF NOT EQUAL (DY, DY) THEN
                    COMMENT ("NO EXCEPTION RAISED");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (C)1");
          END;

          BEGIN
               DY := IDENT_INT(32);
               FAILED ("EXCEPTION NOT RAISED - (C)2");
               IF NOT EQUAL (DY, DY) THEN
                    COMMENT ("NO EXCEPTION RAISED");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (C)2");
          END;

     END; -- (C)

     ---------------------------------------------

     DECLARE   -- (D)

          SEGMENT : DAY_DURATION;

          FUNCTION CHECK_OK (X : DAY_DURATION) RETURN BOOLEAN IS
               I : INTEGER := INTEGER (X);
          BEGIN
               RETURN EQUAL (I,I);
          END CHECK_OK;

     BEGIN     -- (D)

          BEGIN
               SEGMENT := 86_400.0;
               IF CHECK_OK (SEGMENT - 86_000.0) THEN
                    COMMENT ("NO EXCEPTION RAISED (D1)");
               ELSE
                    COMMENT ("NO EXCEPTION RAISED (D2)");
               END IF;

          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("OK CASE RAISED EXCEPTION ON 86_400 - (D)");
          END;

          BEGIN
               SEGMENT := -4.0;
               FAILED ("EXCEPTION NOT RAISED - (D)1");
               IF NOT EQUAL (INTEGER(SEGMENT), INTEGER(SEGMENT)) THEN
                    COMMENT ("NO EXCEPTION RAISED (D3)");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (D)1");
          END;

          BEGIN
               SEGMENT := 86_401.00;
               IF CHECK_OK (SEGMENT - 86_000.0) THEN
                    FAILED ("NO EXCEPTION RAISED (D4)");
               ELSE
                    FAILED ("NO EXCEPTION RAISED (D5)");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (D)2");
          END;

     END; -- (D)

     ---------------------------------------------

     RESULT;
END C96004A;
