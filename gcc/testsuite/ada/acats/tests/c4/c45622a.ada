-- C45622A.ADA

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
--     FOR EXPONENTIATION OF FLOATING POINT TYPES, CHECK THAT
--      CONSTRAINT_ERROR IS RAISED IF
--     MACHINE_OVERFLOWS IS TRUE AND THE RESULT IS OUTSIDE THE RANGE OF
--     THE BASE TYPE.  THIS TESTS DIGITS 5.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- HISTORY:
--     BCB 02/09/88  CREATED ORIGINAL TEST.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;

PROCEDURE C45622A IS

     TYPE FLT IS DIGITS 5;

     F : FLT;

     FUNCTION EQUAL_FLT (ONE, TWO : FLT) RETURN BOOLEAN IS
     BEGIN
          RETURN ONE = TWO * FLT (IDENT_INT(1));
     END EQUAL_FLT;

BEGIN
     TEST ("C45622A", "FOR EXPONENTIATION OF FLOATING POINT TYPES, " &
                      "CHECK THAT CONSTRAINT_ERROR " &
                      "IS RAISED IF MACHINE_OVERFLOWS IS TRUE AND " &
                      "THE RESULT IS OUTSIDE THE RANGE OF THE BASE " &
                      "TYPE.  THIS TESTS DIGITS 5");

     IF FLT'MACHINE_OVERFLOWS THEN
          BEGIN
               F := (FLT'BASE'LAST)**IDENT_INT (2);
               FAILED ("CONSTRAINT_ERROR WAS NOT RAISED FOR " &
                       "EXPONENTIATION");

               IF NOT EQUAL_FLT(F,F) THEN
                    COMMENT ("DON'T OPTIMIZE F");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    COMMENT ("CONSTRAINT_ERROR WAS RAISED FOR " &
                             "EXPONENTIATION");
               WHEN OTHERS =>
                    FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                            "WAS RAISED FOR EXPONENTIATION");
          END;
     ELSE
          NOT_APPLICABLE ("THIS TEST IS NOT APPLICABLE DUE TO " &
                          "MACHINE_OVERFLOWS BEING FALSE");
     END IF;

     RESULT;
END C45622A;
