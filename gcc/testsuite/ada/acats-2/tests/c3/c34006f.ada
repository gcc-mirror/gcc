-- C34006F.ADA

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
--     FOR DERIVED RECORD TYPES WITH DISCRIMINANTS AND WITH NON-LIMITED
--     COMPONENT TYPES:
--     CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR
--     THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--     CONSTRAINED.
--     CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--     IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 9/22/86  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C34006F IS

     SUBTYPE COMPONENT IS INTEGER;

     PACKAGE PKG IS

          MAX_LEN : CONSTANT := 10;

          SUBTYPE LENGTH IS NATURAL RANGE 0 .. MAX_LEN;

          TYPE PARENT (B : BOOLEAN := TRUE; L : LENGTH := 1) IS
               RECORD
                    I : INTEGER;
                    CASE B IS
                         WHEN TRUE =>
                              S : STRING (1 .. L);
                              C : COMPONENT;
                         WHEN FALSE =>
                              F : FLOAT := 5.0;
                    END CASE;
               END RECORD;

          FUNCTION CREATE ( B : BOOLEAN;
                            L : LENGTH;
                            I : INTEGER;
                            S : STRING;
                            C : COMPONENT;
                            F : FLOAT;
                            X : PARENT  -- TO RESOLVE OVERLOADING.
                          ) RETURN PARENT;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT (IDENT_BOOL (TRUE), IDENT_INT (3));

     SUBTYPE SUBPARENT IS PARENT (TRUE, 3);

     TYPE S IS NEW SUBPARENT;

     X : T := (TRUE, 3, 2, "AAA", 2);
     Y : S := (TRUE, 3, 2, "AAA", 2);

     PACKAGE BODY PKG IS

          FUNCTION CREATE
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               S : STRING;
               C : COMPONENT;
               F : FLOAT;
               X : PARENT
             ) RETURN PARENT
          IS
          BEGIN
               CASE B IS
                    WHEN TRUE =>
                         RETURN (TRUE, L, I, S, C);
                    WHEN FALSE =>
                         RETURN (FALSE, L, I, F);
               END CASE;
          END CREATE;

     END PKG;

BEGIN
     TEST ("C34006F", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "RECORD TYPES WITH DISCRIMINANTS AND WITH " &
                      "NON-LIMITED COMPONENT TYPES");

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     BEGIN
          IF CREATE (FALSE, 2, 3, "ZZ", 5, 6.0, X) /=
             (FALSE, 2, 3, 6.0) OR
             CREATE (FALSE, 2, 3, "ZZ", 5, 6.0, Y) /=
             (FALSE, 2, 3, 6.0) THEN
               FAILED ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE " &
                       "SUBTYPE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 1");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 1");
     END;

     BEGIN
          IF CREATE (FALSE, 2, 3, "ZZ", 5, 6.0, X) IN T OR
             CREATE (FALSE, 2, 3, "ZZ", 5, 6.0, Y) IN S THEN
               FAILED ("INCORRECT ""IN""");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 2");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 2");
     END;

     -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

     IF X.B /= TRUE OR X.L /= 3 OR
        Y.B /= TRUE OR Y.L /= 3 THEN
          FAILED ("INCORRECT SELECTION OF DISCRIMINANT VALUES");
     END IF;

     IF NOT X'CONSTRAINED OR NOT Y'CONSTRAINED THEN
          FAILED ("INCORRECT 'CONSTRAINED");
     END IF;

     BEGIN
          X := (TRUE, 3, 1, "ABC", 4);
          Y := (TRUE, 3, 1, "ABC", 4);
          IF PARENT (X) /= PARENT (Y) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGNMENT");
     END;

     BEGIN
          X := (FALSE, 3, 2, 6.0);
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "X := (FALSE, 3, 2, 6.0)");
          IF X = (FALSE, 3, 2, 6.0) THEN  -- USE X.
               COMMENT ("X ALTERED -- X := (FALSE, 3, 2, 6.0)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "X := (FALSE, 3, 2, 6.0)");
     END;

     BEGIN
          X := (TRUE, 4, 2, "ZZZZ", 6);
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "X := (TRUE, 4, 2, ""ZZZZ"", 6)");
          IF X = (TRUE, 4, 2, "ZZZZ", 6) THEN  -- USE X.
               COMMENT ("X ALTERED -- X := (TRUE, 4, 2, ""ZZZZ"", 6)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "X := (TRUE, 4, 2, ""ZZZZ"", 6)");
     END;

     BEGIN
          Y := (FALSE, 3, 2, 6.0);
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "Y := (FALSE, 3, 2, 6.0)");
          IF Y = (FALSE, 3, 2, 6.0) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- Y := (FALSE, 3, 2, 6.0)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "Y := (FALSE, 3, 2, 6.0)");
     END;

     BEGIN
          Y := (TRUE, 4, 2, "ZZZZ", 6);
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "Y := (TRUE, 4, 2, ""ZZZZ"", 6)");
          IF Y = (TRUE, 4, 2, "ZZZZ", 6) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- Y := (TRUE, 4, 2, ""ZZZZ"", 6)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "Y := (TRUE, 4, 2, ""ZZZZ"", 6)");
     END;

     RESULT;
END C34006F;
