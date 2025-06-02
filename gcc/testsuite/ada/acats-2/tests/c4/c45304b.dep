-- C45304B.DEP

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED BY
--     "+" AND "-" FOR PREDEFINED SHORT_INTEGER WHEN THE RESULT IS
--     OUTSIDE THE RANGE OF THE BASE TYPE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE IF THE IMPLEMENTATION HAS A
--     PREDEFINED TYPE SHORT_INTEGER.

--     IF SHORT_INTEGER IS NOT SUPPORTED, THEN THE DECLARATION OF
--     "TEST_VAR" MUST BE REJECTED.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- HISTORY:
--     TBN 10/07/86  CREATED ORIGINAL TEST.
--     JET 12/30/87  ADDED CODE TO PREVENT OPTIMIZATION.
--     JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

WITH REPORT; USE REPORT;
PROCEDURE C45304B IS

     TEST_VAR : SHORT_INTEGER;                 -- N/A => ERROR.

     -- THESE FUNCTIONS ARE TO PREVENT OPTIMIZATION.

     FUNCTION IDENT_SHORT (X : SHORT_INTEGER) RETURN SHORT_INTEGER IS
     BEGIN
          IF EQUAL (3, 3) THEN
               RETURN X;
          ELSE
               RETURN (0);
          END IF;
     END IDENT_SHORT;

     FUNCTION SHORT_OK (X : SHORT_INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN EQUAL (INTEGER(X),INTEGER(X));
     END SHORT_OK;

BEGIN
     TEST ("C45304B", "CHECK THAT CONSTRAINT_ERROR " &
                      "IS RAISED BY ""+"" AND ""-"" FOR PREDEFINED " &
                      "SHORT_INTEGER WHEN THE RESULT IS OUTSIDE THE " &
                      "RANGE OF THE BASE TYPE");

     DECLARE
          B : SHORT_INTEGER := SHORT_INTEGER'LAST;
     BEGIN
          IF SHORT_OK (B + IDENT_SHORT(1)) THEN
               FAILED ("NO EXCEPTION RAISED FOR ADDITION - " &
                       "SHORT_OK RETURNS TRUE");
          ELSE
               FAILED ("NO EXCEPTION RAISED FOR ADDITION - " &
                       "SHORT_OK RETURNS FALSE");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 1");
     END;

     DECLARE
          B : SHORT_INTEGER := SHORT_INTEGER'FIRST;
     BEGIN

          IF SHORT_OK (B - IDENT_SHORT(1)) THEN
               FAILED ("NO EXCEPTION RAISED FOR SUBTRACTION- " &
                       "SHORT_OK RETURNS TRUE");
          ELSE
               FAILED ("NO EXCEPTION RAISED FOR SUBTRACTION - " &
                       "SHORT_OK RETURNS FALSE");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
     END;

     RESULT;
END C45304B;
