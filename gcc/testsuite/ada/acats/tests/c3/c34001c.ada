-- C34001C.ADA

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
-- FOR DERIVED ENUMERATION TYPES, EXCLUDING BOOLEAN TYPES:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 8/20/86

WITH REPORT; USE REPORT;

PROCEDURE C34001C IS

     TYPE PARENT IS (E1, E2, E3, 'A', E4, E5, E6);

     TYPE T IS NEW PARENT RANGE
               PARENT'VAL (IDENT_INT (PARENT'POS (E3))) ..
               PARENT'VAL (IDENT_INT (PARENT'POS (E4)));

     SUBTYPE SUBPARENT IS PARENT RANGE E3 .. E4;

     TYPE S IS NEW SUBPARENT;

     X : T;
     Y : S;

BEGIN
     TEST ("C34001C", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "ENUMERATION TYPES, EXCLUDING BOOLEAN TYPES");

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     IF T'BASE'FIRST /= E1 OR T'BASE'LAST /= E6 OR
        S'BASE'FIRST /= E1 OR S'BASE'LAST /= E6 THEN
          FAILED ("INCORRECT 'BASE'FIRST OR 'BASE'LAST");
     END IF;

     IF T'PRED (E2) /= E1 OR T'SUCC (E1) /= E2 OR
        S'PRED (E2) /= E1 OR S'SUCC (E1) /= E2 THEN
          FAILED ("INCORRECT 'PRED OR 'SUCC");
     END IF;

     -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

     IF T'FIRST /= E3 OR T'LAST /= E4 OR
        S'FIRST /= E3 OR S'LAST /= E4 THEN
          FAILED ("INCORRECT 'FIRST OR 'LAST");
     END IF;

     BEGIN
          X := E3;
          Y := E3;
          IF PARENT (X) /= PARENT (Y) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT - 1");
          END IF;
          X := E4;
          Y := E4;
          IF PARENT (X) /= PARENT (Y) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT - 2");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGNMENT");
     END;

     BEGIN
          X := E2;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- X := E2");
          IF X = E2 THEN  -- USE X.
               COMMENT ("X ALTERED -- X := E2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- X := E2");
     END;

     BEGIN
          X := E5;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- X := E5");
          IF X = E5 THEN  -- USE X.
               COMMENT ("X ALTERED -- X := E5");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- X := E5");
     END;

     BEGIN
          Y := E2;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- Y := E2");
          IF Y = E2 THEN  -- USE Y.
               COMMENT ("Y ALTERED -- Y := E2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- Y := E2");
     END;

     BEGIN
          Y := E5;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- Y := E5");
          IF Y = E5 THEN  -- USE Y.
               COMMENT ("Y ALTERED -- Y := E5");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- Y := E5");
     END;

     RESULT;
END C34001C;
