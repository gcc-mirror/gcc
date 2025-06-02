-- CB3003B.ADA

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
-- CHECK THAT A NON-EXPLICIT RAISE STATEMENT MAY APPEAR IN A BLOCK
-- STATEMENT WITHIN AN EXCEPTION HANDLER; IF THE BLOCK STATEMENT
-- INCLUDES A HANDLER FOR THE CURRENT EXCEPTION, THEN THE INNER
-- HANDLER RECEIVES CONTROL.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- L.BROWN  10/08/86
-- MRM  03/30/93   REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;

PROCEDURE  CB3003B  IS

     MY_ERROR : EXCEPTION;

BEGIN
     TEST("CB3003B","A NON-EXPLICIT RAISE STATEMENT MAY APPEAR IN A "&
                    "BLOCK STATEMENT WITHIN AN EXCEPTION HANDLER");

     BEGIN
          BEGIN
               IF EQUAL(3,3) THEN
                    RAISE MY_ERROR;
               END IF;
               FAILED("MY_ERROR WAS NOT RAISED 1");
          EXCEPTION
               WHEN MY_ERROR =>
                    BEGIN
                         IF EQUAL(3,3) THEN
                              RAISE;
                         END IF;
                         FAILED("MY_ERROR WAS NOT RAISED 2");
                    EXCEPTION
                         WHEN MY_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED("WRONG EXCEPTION RAISED 1");
                    END;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED 2");
          END;
     EXCEPTION
          WHEN MY_ERROR =>
               FAILED("CONTROL PASSED TO OUTER HANDLER 1");
          WHEN OTHERS =>
               FAILED("UNEXPECTED EXCEPTION RAISED 1");
     END;

     BEGIN
          BEGIN
               IF EQUAL(3,3) THEN
                    RAISE MY_ERROR;
               END IF;
               FAILED("MY_ERROR WAS NOT RAISED 3");
          EXCEPTION
               WHEN CONSTRAINT_ERROR | MY_ERROR | TASKING_ERROR =>
                    BEGIN
                         IF EQUAL(3,3) THEN
                              RAISE;
                         END IF;
                         FAILED("MY_ERROR WAS NOT RAISED 4");
                    EXCEPTION
                         WHEN MY_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED("WRONG EXCEPTION RAISED 3");
                    END;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED 4");
          END;
     EXCEPTION
          WHEN MY_ERROR =>
               FAILED("CONTROL PASSED TO OUTER HANDLER 2");
          WHEN OTHERS =>
               FAILED("UNEXPECTED EXCEPTION RAISED 2");
     END;

     BEGIN
          BEGIN
               IF EQUAL(3,3) THEN
                    RAISE MY_ERROR;
               END IF;
               FAILED("MY_ERROR WAS NOT RAISED 5");
          EXCEPTION
               WHEN OTHERS =>
                    BEGIN
                         IF EQUAL(3,3) THEN
                              RAISE;
                         END IF;
                         FAILED("MY_ERROR WAS NOT RAISED 6");
                    EXCEPTION
                         WHEN MY_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED("WRONG EXCEPTION RAISED 5");
                    END;
          END;
     EXCEPTION
          WHEN MY_ERROR =>
               FAILED("CONTROL PASSED TO OUTER HANDLER 3");
          WHEN OTHERS =>
               FAILED("UNEXPECTED EXCEPTION RAISED 3");
     END;

     RESULT;

END CB3003B;
