-- C45322A.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED IF 
--     MACHINE_OVERFLOWS IS TRUE AND THE RESULT OF THE ADDITION OR 
--     SUBTRACTION LIES OUTSIDE OF THE RANGE OF THE BASE TYPE.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- HISTORY:
--     NPL 09/01/90  CREATED ORIGINAL TEST.
--     LDC 10/09/90  CHANGED THE STYLE OF THE TEST TO THE STANDARD
--                   ACVC FORMAT AND WRAPPED LINES WHICH WHERE LONGER
--                   THAN 71 CHARACTERS.
--     JRL 03/30/93  REMOVED NUMERIC_ERROR FROM TEST.

WITH REPORT; USE REPORT;

PROCEDURE C45322A IS

  TYPE FLOAT5 IS DIGITS 5;
  F5 : FLOAT5;

  FUNCTION IDENT (F : FLOAT5) RETURN FLOAT5 IS
  BEGIN
    RETURN F * FLOAT5(IDENT_INT(1));
  END IDENT;

  FUNCTION EQUAL (F,G : FLOAT5) RETURN BOOLEAN IS
  BEGIN
    RETURN F = G + FLOAT5(IDENT_INT(0));
  END EQUAL;

BEGIN
     TEST ("C45322A", "CHECK THAT CONSTRAINT_ERROR " &
                      "IS RAISED IF MACHINE_OVERFLOWS IS TRUE AND " &
                      "THE RESULT OF THE ADDITION OR SUBTRACTION " &
                      "LIES OUTSIDE OF THE RANGE OF THE BASE TYPE");

     IF NOT FLOAT5'MACHINE_OVERFLOWS THEN
          NOT_APPLICABLE("MACHINE_OVERFLOWS IS FALSE");
     ELSE

          BEGIN
               F5 := IDENT(FLOAT5'BASE'LAST) + FLOAT5'BASE'LAST;

               FAILED("NO EXCEPTION RAISED BY LARGE '+'");

               IF NOT EQUAL(F5, F5) THEN
                    COMMENT("DON'T OPTIMIZE F5");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED BY LARGE '+'");
          END;

          -- AS ABOVE BUT INTERCHANGING '+' AND '-'
          BEGIN
               F5 := IDENT(FLOAT5'BASE'LAST) - FLOAT5'BASE'LAST;

               IF NOT EQUAL(F5, F5) THEN
                    COMMENT("DON'T OPTIMIZE F5");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED("CONSTRAINT_ERROR " &
                           "RAISED BY INTERCHANGING LARGE '+'");
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED BY " &
                           "INTERCHANGING LARGE '+'");
          END;

          BEGIN
               F5 := IDENT(FLOAT5'BASE'FIRST) + FLOAT5'BASE'FIRST;

               FAILED("NO EXCEPTION RAISED BY SMALL '+'");

               IF NOT EQUAL(F5, F5) THEN
                    COMMENT("DON'T OPTIMIZE F5");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED BY SMALL '+'");
          END;

          -- AS ABOVE BUT INTERCHANGING '+' AND '-'
          BEGIN
               F5 := IDENT(FLOAT5'BASE'FIRST) - FLOAT5'BASE'FIRST;

               IF NOT EQUAL(F5, F5) THEN
                    COMMENT("DON'T OPTIMIZE F5");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED("CONSTRAINT_ERROR " &
                           "RAISED BY INTERCHANGING SMALL '+'");
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED BY " &
                           "INTERCHANGING SMALL '+'");
          END;

          BEGIN
               F5 := IDENT(FLOAT5'BASE'LAST) - FLOAT5'BASE'FIRST;

               FAILED("NO EXCEPTION RAISED BY LARGE '-'");

               IF NOT EQUAL(F5, F5) THEN
                    COMMENT("DON'T OPTIMIZE F5");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED BY LARGE '-'");
          END;

          -- AS ABOVE BUT INTERCHANGING '+' AND '-'
          BEGIN
               F5 := IDENT(FLOAT5'BASE'LAST) + FLOAT5'BASE'FIRST;

               IF NOT EQUAL(F5, F5) THEN
                    COMMENT("DON'T OPTIMIZE F5");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED("CONSTRAINT_ERROR " &
                           "RAISED BY INTERCHANGING LARGE '-'");
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED BY " &
                           "INTERCHANGING LARGE '-'");
          END;

          BEGIN
               F5 := IDENT(FLOAT5'BASE'FIRST) - FLOAT5'BASE'LAST;

               FAILED("NO EXCEPTION RAISED BY SMALL '-'");

               IF NOT EQUAL(F5, F5) THEN
                    COMMENT("DON'T OPTIMIZE F5");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED BY SMALL '-'");
          END;

          -- AS ABOVE BUT INTERCHANGING '+' AND '-'
          BEGIN
               F5 := IDENT(FLOAT5'BASE'FIRST) + FLOAT5'BASE'LAST;

               IF NOT EQUAL(F5, F5) THEN
                    COMMENT("DON'T OPTIMIZE F5");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED("CONSTRAINT_ERROR " &
                           "RAISED BY INTERCHANGING SMALL '-'");
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED BY " &
                           "INTERCHANGING SMALL '-'");
          END;

     END IF;

     RESULT;

END C45322A;
