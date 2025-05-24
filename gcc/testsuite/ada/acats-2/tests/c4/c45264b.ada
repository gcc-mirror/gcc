-- C45264B.ADA

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
-- CHECK THAT EQUALITY COMPARISONS YIELD CORRECT RESULTS FOR ONE
-- DIMENSIONAL AND MULTI-DIMENSIONAL ARRAY TYPES.
-- THIS TEST CHECKS THE CASE WHERE THE ARRAY HAS A BOUND THAT DEPENDS ON
-- A DISCRIMINANT WITH DEFAULTS.

-- JWC 11/18/85

WITH REPORT; USE REPORT;

PROCEDURE C45264B IS

BEGIN

     TEST("C45264B","CHECK THAT EQUALITY COMPARISONS YIELD CORRECT " &
                    "RESULTS FOR ONE DIMENSIONAL AND MULTI-"         &
                    "DIMENSIONAL ARRAY TYPES");

     DECLARE

          SUBTYPE SUBINT IS INTEGER RANGE 1 .. 5;
          TYPE REC (DISC : SUBINT := 1) IS
               RECORD
                    COMP : STRING(IDENT_INT(3) .. DISC);
               END RECORD;
          TYPE ARR IS ARRAY (1 .. 3) OF REC;

          A1, A2 : ARR;

     BEGIN

          IF A1 /= A2 THEN
               FAILED ("NULL ARRAYS, RESULT NOT EQUAL");
          END IF;

          A1(2) := (5, "ABC");

          IF A1 = A2 THEN
               FAILED ("NON-NULL ARRAY AND NULL ARRAY, RESULT EQUAL");
          END IF;

          A2(2) := (5, "ABD");

          IF A1 = A2 THEN
               FAILED ("ARRAYS DIFFER BY LAST ELEMENT, RESULT EQUAL");
          END IF;

          A2(2) := (4, "AB");

          IF A1 = A2 THEN
               FAILED ("ARRAYS OF DIFFERENT LENGTH, RESULT EQUAL");
          END IF;

          A1(2) := (4, "AB");

          IF A1 /= A2 THEN
               FAILED ("DISCRIMINANTS AND COMPONENTS ARE THE SAME, " &
                       "RESULT NOT EQUAL");
          END IF;

     END;

     RESULT;

END C45264B;
