-- C45534B.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN A
--     FIXED POINT VALUE IS DIVIDED BY ZERO (EITHER AN INTEGER ZERO OR
--     A FIXED POINT ZERO).


-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- HISTORY:
--     BCB 07/14/88  CREATED ORIGINAL TEST.
--     MRM 03/30/93  REMOVED NUMERIC ERROR FOR 9X CONSISTENCY

WITH REPORT; USE REPORT;

PROCEDURE C45534B IS

     TYPE FIX IS DELTA 2.0**(-1) RANGE -2.0 .. 2.0;
     TYPE FIX2 IS DELTA 2.0**(-1) RANGE -3.0 .. 3.0;

     A : FIX := 1.0;
     B : FIX;
     ZERO : FIX := 0.0;
     ZERO2 : FIX2 := 0.0;

     FUNCTION IDENT_FLT (ONE, TWO : FIX) RETURN BOOLEAN IS
     BEGIN
          RETURN ONE = FIX (TWO * FIX (IDENT_INT(1)));
     END IDENT_FLT;

BEGIN
     TEST ("C45534B", "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
                      "A FIXED POINT VALUE IS " &
                      "DIVIDED BY ZERO (EITHER AN INTEGER ZERO OR A " &
                      "FIXED POINT ZERO)");

     BEGIN
          B := A / IDENT_INT (0);
          FAILED ("NO EXCEPTION RAISED FOR DIVISION BY INTEGER ZERO");
          IF IDENT_FLT (B,B) THEN
               COMMENT ("DON'T OPTIMIZE B");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED");
     END;

     BEGIN
          B := FIX (A / ZERO);
          FAILED ("NO EXCEPTION RAISED FOR DIVISION BY FIXED POINT " &
                  "ZERO - 1");
          IF IDENT_FLT (B,B) THEN
               COMMENT ("DON'T OPTIMIZE B");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED");
      END;

     BEGIN
          B := FIX (A / ZERO2);
          FAILED ("NO EXCEPTION RAISED FOR DIVISION BY FIXED POINT " &
                  "ZERO - 2");
          IF IDENT_FLT (B,B) THEN
               COMMENT ("DON'T OPTIMIZE B");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED");
      END;

     RESULT;
END C45534B;
