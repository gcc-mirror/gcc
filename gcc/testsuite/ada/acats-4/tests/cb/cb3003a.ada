-- CB3003A.ADA

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
-- CHECK THAT THE NON-SPECIFIC RAISE STATEMENT PROPAGATES THE EXCEPTION
--    FOR FURTHER PROCESSING(HANDLING) IN ANOTHER HANDLER.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- DCB 04/01/80
-- JRK 11/19/80
-- SPS 11/2/82
-- MRM 03/30/93   REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT;
PROCEDURE CB3003A IS

     USE REPORT;

     FLOW_COUNT : INTEGER := 0;
     E1,E2 : EXCEPTION;

BEGIN
     TEST("CB3003A","CHECK THAT THE NON-SPECIFIC RAISE STATEMENT" &
          " PROPAGATES THE ERROR FOR FURTHER HANDLING IN ANOTHER" &
          " HANDLER");

     -------------------------------------------------------

     BEGIN
          BEGIN
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 1;
                    RAISE E1;
                    FAILED("EXCEPTION NOT RAISED (CASE 1)");
               EXCEPTION
                    WHEN OTHERS =>
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE;
                         FAILED("EXCEPTION NOT RERAISED (CASE 1; " &
                                "INNER)");
               END;

          EXCEPTION
          -- A HANDLER SPECIFIC TO THE RAISED EXCEPTION (E1).
               WHEN E1 =>
                    FLOW_COUNT := FLOW_COUNT + 1;
                    RAISE;
                    FAILED("EXCEPTION NOT RERAISED (CASE 1; OUTER)");
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED (CASE 1)");
          END;

     EXCEPTION
          WHEN E1 =>
               FLOW_COUNT := FLOW_COUNT + 1;
          WHEN OTHERS =>
               FAILED("WRONG EXCEPTION PASSED (CASE 1)");
     END;

     -------------------------------------------------------

     BEGIN
          BEGIN
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 1;
                    RAISE E1;
                    FAILED("EXCEPTION NOT RAISED (CASE 2)");
               EXCEPTION
                    WHEN OTHERS =>
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE;
                         FAILED("EXCEPTION NOT RERAISED (CASE 2; " &
                                "INNER)");
               END;

          EXCEPTION
          -- A HANDLER FOR SEVERAL EXCEPTIONS INCLUDING THE ONE RAISED.
               WHEN CONSTRAINT_ERROR =>
                    FAILED("WRONG EXCEPTION RAISED (CONSTRAINT_ERROR)");
               WHEN E2 =>
                    FAILED("WRONG EXCEPTION RAISED (E2)");
               WHEN PROGRAM_ERROR | E1 | TASKING_ERROR =>
                    FLOW_COUNT := FLOW_COUNT + 1;
                    RAISE;
                    FAILED("EXCEPTION NOT RERAISED (CASE 2; OUTER)");
               WHEN STORAGE_ERROR =>
                    FAILED("WRONG EXCEPTION RAISED (STORAGE_ERROR)");
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED (OTHERS)");
          END;

     EXCEPTION
          WHEN E1 =>
               FLOW_COUNT := FLOW_COUNT + 1;
          WHEN OTHERS =>
               FAILED("WRONG EXCEPTION PASSED (CASE 2)");
     END;

     -------------------------------------------------------

     BEGIN
          BEGIN
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 1;
                    RAISE E1;
                    FAILED("EXCEPTION NOT RAISED (CASE 3)");
               EXCEPTION
                    WHEN OTHERS =>
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE;
                         FAILED("EXCEPTION NOT RERAISED (CASE 3; " &
                                "INNER)");
               END;

          EXCEPTION
          -- A NON-SPECIFIC HANDLER.
               WHEN CONSTRAINT_ERROR | E2 =>
                    FAILED("WRONG EXCEPTION RAISED " &
                           "(CONSTRAINT_ERROR | E2)");
               WHEN OTHERS =>
                    FLOW_COUNT := FLOW_COUNT + 1;
                    RAISE;
                    FAILED("EXCEPTION NOT RERAISED (CASE 3; OUTER)");
          END;

     EXCEPTION
          WHEN E1 =>
               FLOW_COUNT := FLOW_COUNT + 1;
          WHEN OTHERS =>
               FAILED("WRONG EXCEPTION PASSED (CASE 3)");
     END;

     -------------------------------------------------------

     IF FLOW_COUNT /= 12 THEN
          FAILED("INCORRECT FLOW_COUNT VALUE");
     END IF;

     RESULT;
END CB3003A;
