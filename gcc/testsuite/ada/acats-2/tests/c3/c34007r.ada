-- C34007R.ADA

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
-- FOR DERIVED ACCESS TYPES WHOSE DESIGNATED TYPE IS A RECORD TYPE
-- WITH DISCRIMINANTS:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 9/29/86

WITH REPORT; USE REPORT;

PROCEDURE C34007R IS

     SUBTYPE COMPONENT IS INTEGER;

     SUBTYPE LENGTH IS NATURAL RANGE 0 .. 10;

     TYPE DESIGNATED (B : BOOLEAN := TRUE; L : LENGTH := 1) IS
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

     PACKAGE PKG IS

          TYPE PARENT IS ACCESS DESIGNATED;

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

     X : T := NEW DESIGNATED'(TRUE, 3, 2, "AAA", 2);
     Y : S := NEW DESIGNATED'(TRUE, 3, 2, "AAA", 2);

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
                         RETURN NEW DESIGNATED'(TRUE, L, I, S, C);
                    WHEN FALSE =>
                         RETURN NEW DESIGNATED'(FALSE, L, I, F);
               END CASE;
          END CREATE;

     END PKG;

BEGIN
     TEST ("C34007R", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
                      "RECORD TYPE WITH DISCRIMINANTS");

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     IF CREATE (FALSE, 2, 3, "ZZ", 5, 6.0, X) . ALL /=
        (FALSE, 2, 3, 6.0) OR
        CREATE (FALSE, 2, 3, "ZZ", 5, 6.0, Y) . ALL /=
        (FALSE, 2, 3, 6.0) THEN
          FAILED ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE SUBTYPE");
     END IF;

     IF CREATE (FALSE, 2, 3, "ZZ", 5, 6.0, X) IN T OR
        CREATE (FALSE, 2, 3, "ZZ", 5, 6.0, Y) IN S THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

     IF X.B /= TRUE OR X.L /= 3 OR
        Y.B /= TRUE OR Y.L /= 3 THEN
          FAILED ("INCORRECT SELECTION OF DISCRIMINANT VALUES");
     END IF;

     BEGIN
          X := NEW DESIGNATED'(TRUE, 3, 1, "ABC", 4);
          Y := NEW DESIGNATED'(TRUE, 3, 1, "ABC", 4);
          IF PARENT (X) = PARENT (Y) OR  -- USE X AND Y.
             X.ALL /= Y.ALL THEN
               FAILED ("INCORRECT ALLOCATOR OR CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGNMENT");
     END;

     BEGIN
          X := NEW DESIGNATED'(FALSE, 3, 2, 6.0);
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "X := NEW DESIGNATED'(FALSE, 3, 2, 6.0)");
          IF X = NULL OR ELSE X.ALL = (FALSE, 3, 2, 6.0) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "X := NEW DESIGNATED'(FALSE, 3, 2, 6.0)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "X := NEW DESIGNATED'(FALSE, 3, 2, 6.0)");
     END;

     BEGIN
          X := NEW DESIGNATED'(TRUE, 4, 2, "ZZZZ", 6);
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "X := NEW DESIGNATED'(TRUE, 4, 2, ""ZZZZ"", 6)");
          IF X = NULL OR ELSE
             X.ALL = (TRUE, 4, 2, "ZZZZ", 6) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "X := NEW DESIGNATED'" &
                        "(TRUE, 4, 2, ""ZZZZ"", 6)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "X := NEW DESIGNATED'(TRUE, 4, 2, ""ZZZZ"", 6)");
     END;

     BEGIN
          Y := NEW DESIGNATED'(FALSE, 3, 2, 6.0);
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "Y := NEW DESIGNATED'(FALSE, 3, 2, 6.0)");
          IF Y = NULL OR ELSE Y.ALL = (FALSE, 3, 2, 6.0) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "Y := NEW DESIGNATED'(FALSE, 3, 2, 6.0)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "Y := NEW DESIGNATED'(FALSE, 3, 2, 6.0)");
     END;

     BEGIN
          Y := NEW DESIGNATED'(TRUE, 4, 2, "ZZZZ", 6);
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "Y := NEW DESIGNATED'(TRUE, 4, 2, ""ZZZZ"", 6)");
          IF Y = NULL OR ELSE
             Y.ALL = (TRUE, 4, 2, "ZZZZ", 6) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "Y := NEW DESIGNATED'" &
                        "(TRUE, 4, 2, ""ZZZZ"", 6)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "Y := NEW DESIGNATED'(TRUE, 4, 2, ""ZZZZ"", 6)");
     END;

     RESULT;
END C34007R;
