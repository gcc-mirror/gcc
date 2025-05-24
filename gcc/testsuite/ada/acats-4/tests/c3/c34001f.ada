-- C34001F.ADA

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
-- FOR DERIVED BOOLEAN TYPES:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 8/20/86

WITH REPORT; USE REPORT;

PROCEDURE C34001F IS

     SUBTYPE PARENT IS BOOLEAN;

     TYPE T IS NEW PARENT RANGE
               PARENT'VAL (IDENT_INT (PARENT'POS (FALSE))) ..
               PARENT'VAL (IDENT_INT (PARENT'POS (FALSE)));

     SUBTYPE SUBPARENT IS PARENT RANGE TRUE .. TRUE;

     TYPE S IS NEW SUBPARENT;

     X : T;
     Y : S;

BEGIN
     TEST ("C34001F", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "BOOLEAN TYPES");

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     IF T'BASE'FIRST /= FALSE OR T'BASE'LAST /= TRUE OR
        S'BASE'FIRST /= FALSE OR S'BASE'LAST /= TRUE THEN
          FAILED ("INCORRECT 'BASE'FIRST OR 'BASE'LAST");
     END IF;

     IF T'PRED (TRUE) /= FALSE OR T'SUCC (FALSE) /= TRUE OR
        S'PRED (TRUE) /= FALSE OR S'SUCC (FALSE) /= TRUE THEN
          FAILED ("INCORRECT 'PRED OR 'SUCC");
     END IF;

     -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

     IF T'FIRST /= FALSE OR T'LAST /= FALSE OR
        S'FIRST /= TRUE  OR S'LAST /= TRUE  THEN
          FAILED ("INCORRECT 'FIRST OR 'LAST");
     END IF;

     BEGIN
          X := FALSE;
          Y := TRUE;
          IF NOT PARENT (X) /= PARENT (Y) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGNMENT");
     END;

     BEGIN
          X := TRUE;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- X := TRUE");
          IF X = TRUE THEN  -- USE X.
               COMMENT ("X ALTERED -- X := TRUE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- X := TRUE");
     END;

     BEGIN
          Y := FALSE;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- Y := FALSE");
          IF Y = FALSE THEN  -- USE Y.
               COMMENT ("Y ALTERED -- Y := FALSE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- Y := FALSE");
     END;

     RESULT;
END C34001F;
