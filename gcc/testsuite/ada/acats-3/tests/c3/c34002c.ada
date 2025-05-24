-- C34002C.ADA

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
-- FOR DERIVED INTEGER TYPES:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 8/21/86

WITH REPORT; USE REPORT;

PROCEDURE C34002C IS

     TYPE PARENT IS RANGE -100 .. 100;

     TYPE T IS NEW PARENT RANGE
               PARENT'VAL (IDENT_INT (-30)) ..
               PARENT'VAL (IDENT_INT ( 30));

     SUBTYPE SUBPARENT IS PARENT RANGE -30 .. 30;

     TYPE S IS NEW SUBPARENT;

     X : T;
     Y : S;

BEGIN
     TEST ("C34002C", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "INTEGER TYPES");

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     IF T'POS (T'BASE'FIRST) /= PARENT'POS (PARENT'BASE'FIRST) OR
        S'POS (S'BASE'FIRST) /= PARENT'POS (PARENT'BASE'FIRST) OR
        T'POS (T'BASE'LAST)  /= PARENT'POS (PARENT'BASE'LAST)  OR
        S'POS (S'BASE'LAST)  /= PARENT'POS (PARENT'BASE'LAST)  THEN
          FAILED ("INCORRECT 'BASE'FIRST OR 'BASE'LAST");
     END IF;

     IF T'PRED (100) /= 99 OR T'SUCC (99) /= 100 OR
        S'PRED (100) /= 99 OR S'SUCC (99) /= 100 THEN
          FAILED ("INCORRECT 'PRED OR 'SUCC");
     END IF;

     -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

     IF T'FIRST /= -30 OR T'LAST /= 30 OR
        S'FIRST /= -30 OR S'LAST /= 30 THEN
          FAILED ("INCORRECT 'FIRST OR 'LAST");
     END IF;

     BEGIN
          X := -30;
          Y := -30;
          IF PARENT (X) /= PARENT (Y) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT - 1");
          END IF;
          X := 30;
          Y := 30;
          IF PARENT (X) /= PARENT (Y) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT - 2");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGNMENT");
     END;

     BEGIN
          X := -31;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- X := -31");
          IF X = -31 THEN  -- USE X.
               COMMENT ("X ALTERED -- X := -31");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- X := -31");
     END;

     BEGIN
          X := 31;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- X := 31");
          IF X = 31 THEN  -- USE X.
               COMMENT ("X ALTERED -- X := 31");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- X := 31");
     END;

     BEGIN
          Y := -31;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- Y := -31");
          IF Y = -31 THEN -- USE Y.
               COMMENT ("Y ALTERED -- Y := -31");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- Y := -31");
     END;

     BEGIN
          Y := 31;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- Y := 31");
          IF Y = 31 THEN  -- USE Y.
               COMMENT ("Y ALTERED -- Y := 31");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- Y := 31");
     END;

     RESULT;
END C34002C;
