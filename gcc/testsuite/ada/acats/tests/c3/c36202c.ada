-- C36202C.ADA

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
-- CHECK THAT 'LENGTH DOES NOT RAISE AN EXCEPTION
-- WHEN APPLIED TO A NULL ARRAY A, EVEN IF A'LAST - A'FIRST 
-- WOULD RAISE CONSTRAINT_ERROR.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- L.BROWN  07/29/86
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;

PROCEDURE C36202C IS

     TYPE LRG_INT IS RANGE MIN_INT .. MAX_INT;

     BEGIN
          TEST("C36202C", "NO EXCEPTION IS RAISED FOR 'LENGTH "&
                          "WHEN APPLIED TO A NULL ARRAY");
              
          DECLARE
               TYPE LRG_ARR IS ARRAY
                              (LRG_INT RANGE MAX_INT .. MIN_INT)
                                     OF INTEGER;
               LRG_OBJ : LRG_ARR;

          BEGIN
               IF LRG_OBJ'LENGTH /= 0  THEN
                    FAILED("INCORRECT VALUE RETURNED BY 'LENGTH " &
                           "FOR ONE-DIM NULL ARRAY");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED("CONSTRAINT_ERROR WAS RAISED " &
                           "FOR ONE-DIM NULL ARRAY");
               WHEN OTHERS =>
                    FAILED("EXCEPTION RAISED FOR ONE-DIM " &
                           "NULL ARRAY");
          END;

          DECLARE
               TYPE LRG2_ARR IS ARRAY (LRG_INT RANGE 1 .. 3 ,
                                LRG_INT RANGE MAX_INT .. MIN_INT)
                                OF INTEGER;
          BEGIN
               IF LRG2_ARR'LENGTH(2) /= 0  THEN
                    FAILED("INCORRECT VALUE RETURNED BY 'LENGTH " &
                           "FOR TWO-DIM NULL ARRAY");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED("CONSTRAINT_ERROR WAS RAISED " &
                           "FOR TWO-DIM NULL ARRAY");
               WHEN OTHERS =>
                    FAILED("EXCEPTION RAISED FOR TWO-DIM " &
                           "NULL ARRAY");
          END;

     RESULT;

     END C36202C;
