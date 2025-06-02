-- C34003C.ADA

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
-- FOR DERIVED FLOATING POINT TYPES:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 9/4/86
-- GJD 11/15/95  REMOVED USES OF OBSOLETE ADA 83 ATTRIBUTE (SAFE_LARGE).

WITH REPORT; USE REPORT;

PROCEDURE C34003C IS

     TYPE PARENT IS DIGITS 5;

     TYPE T IS NEW PARENT DIGITS 4 RANGE
               PARENT (IDENT_INT (-30)) ..
               PARENT (IDENT_INT ( 30));

     SUBTYPE SUBPARENT IS PARENT DIGITS 4 RANGE -30.0 .. 30.0;

     TYPE S IS NEW SUBPARENT;

     X : T;
     Y : S;

BEGIN
     TEST ("C34003C", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "FLOATING POINT TYPES");

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     IF T'BASE'DIGITS < 5 OR S'BASE'DIGITS < 5 THEN
          FAILED ("INCORRECT 'BASE'DIGITS");
     END IF;

     IF  12344.0 + T'(1.0) + 1.0 /=  12346.0 OR
         12344.0 + S'(1.0) + 1.0 /=  12346.0 OR
        -12344.0 - T'(1.0) - 1.0 /= -12346.0 OR
        -12344.0 - S'(1.0) - 1.0 /= -12346.0 THEN
          FAILED ("INCORRECT + OR -");
     END IF;

     -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

     IF T'DIGITS /= 4 OR S'DIGITS /= 4 THEN
          FAILED ("INCORRECT 'DIGITS");
     END IF;

     IF T'FIRST /= -30.0 OR T'LAST /= 30.0 OR
        S'FIRST /= -30.0 OR S'LAST /= 30.0 THEN
          FAILED ("INCORRECT 'FIRST OR 'LAST");
     END IF;

     BEGIN
          X := -30.0;
          Y := -30.0;
          IF PARENT (X) /= PARENT (Y) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT - 1");
          END IF;
          X := 30.0;
          Y := 30.0;
          IF PARENT (X) /= PARENT (Y) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT - 2");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGNMENT");
     END;

     BEGIN
          X := -31.0;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- X := -31.0");
          IF X = -31.0 THEN  -- USE X.
               COMMENT ("X ALTERED -- X := -31.0");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- X := -31.0");
     END;

     BEGIN
          X := 31.0;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- X := 31.0");
          IF X = 31.0 THEN  -- USE X.
               COMMENT ("X ALTERED -- X := 31.0");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- X := 31.0");
     END;

     BEGIN
          Y := -31.0;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- Y := -31.0");
          IF Y = -31.0 THEN  -- USE Y.
               COMMENT ("Y ALTERED -- Y := -31.0");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- Y := -31.0");
     END;

     BEGIN
          Y := 31.0;
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- Y := 31.0");
          IF Y = 31.0 THEN  -- USE Y.
               COMMENT ("Y ALTERED -- Y := 31.0");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- Y := 31.0");
     END;

     RESULT;
END C34003C;
