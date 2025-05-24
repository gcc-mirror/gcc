-- C34007U.ADA

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
-- FOR DERIVED ACCESS TYPES WHOSE DESIGNATED TYPE IS A PRIVATE TYPE
-- WITH DISCRIMINANTS:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 9/30/86

WITH REPORT; USE REPORT;

PROCEDURE C34007U IS

     SUBTYPE COMPONENT IS INTEGER;

     PACKAGE PKG_D IS

          SUBTYPE LENGTH IS NATURAL RANGE 0 .. 10;

          TYPE DESIGNATED (B : BOOLEAN := TRUE; L : LENGTH := 1) IS
                          PRIVATE;

          FUNCTION CREATE ( B : BOOLEAN;
                            L : LENGTH;
                            I : INTEGER;
                            S : STRING;
                            C : COMPONENT;
                            F : FLOAT
                          ) RETURN DESIGNATED;

     PRIVATE

          TYPE DESIGNATED (B : BOOLEAN := TRUE; L : LENGTH := 1) IS
               RECORD
                    I : INTEGER := 2;
                    CASE B IS
                         WHEN TRUE =>
                              S : STRING (1 .. L) := (1 .. L => 'A');
                              C : COMPONENT := 2;
                         WHEN FALSE =>
                              F : FLOAT := 5.0;
                    END CASE;
               END RECORD;

     END PKG_D;

     USE PKG_D;

     PACKAGE PKG_P IS

          TYPE PARENT IS ACCESS DESIGNATED;

          FUNCTION CREATE ( B : BOOLEAN;
                            L : LENGTH;
                            I : INTEGER;
                            S : STRING;
                            C : COMPONENT;
                            F : FLOAT;
                            X : PARENT  -- TO RESOLVE OVERLOADING.
                          ) RETURN PARENT;

     END PKG_P;

     USE PKG_P;

     TYPE T IS NEW PARENT (IDENT_BOOL (TRUE), IDENT_INT (3));

     SUBTYPE SUBPARENT IS PARENT (TRUE, 3);

     TYPE S IS NEW SUBPARENT;

     X : T := NEW DESIGNATED (TRUE, 3);
     Y : S := NEW DESIGNATED (TRUE, 3);

     PACKAGE BODY PKG_D IS

          FUNCTION CREATE
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               S : STRING;
               C : COMPONENT;
               F : FLOAT
             ) RETURN DESIGNATED
          IS
          BEGIN
               CASE B IS
                    WHEN TRUE =>
                         RETURN (TRUE, L, I, S, C);
                    WHEN FALSE =>
                         RETURN (FALSE, L, I, F);
               END CASE;
          END CREATE;

     END PKG_D;

     PACKAGE BODY PKG_P IS

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
               RETURN NEW DESIGNATED'(CREATE (B, L, I, S, C, F));
          END CREATE;

     END PKG_P;

BEGIN
     TEST ("C34007U", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
                      "PRIVATE TYPE WITH DISCRIMINANTS");

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     IF CREATE (FALSE, 2, 3, "WW", 5, 6.0, X) . ALL /=
        CREATE (FALSE, 2, 3, "ZZ", 7, 6.0) OR
        CREATE (FALSE, 2, 3, "WW", 5, 6.0, Y) . ALL /=
        CREATE (FALSE, 2, 3, "ZZ", 7, 6.0) THEN
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
          X := NEW DESIGNATED'(CREATE (TRUE, 3, 1, "ABC", 4, 1.0));
          Y := NEW DESIGNATED'(CREATE (TRUE, 3, 1, "ABC", 4, 1.0));
          IF PARENT (X) = PARENT (Y) OR  -- USE X AND Y.
             X.ALL /= Y.ALL THEN
               FAILED ("INCORRECT ALLOCATOR OR CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGNMENT");
     END;

     BEGIN
          X := NEW DESIGNATED'(CREATE (FALSE, 3, 2, "ZZZ", 5, 6.0));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "X := NEW DESIGNATED'" &
                  "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
          IF X = NULL OR ELSE
             X.ALL = CREATE (FALSE, 3, 2, "ZZZ", 5, 6.0) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "X := NEW DESIGNATED'" &
                        "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "X := NEW DESIGNATED'" &
                       "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
     END;

     BEGIN
          X := NEW DESIGNATED'(CREATE (TRUE, 4, 2, "ZZZZ", 6, 7.0));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "X := NEW DESIGNATED'" &
                  "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
          IF X = NULL OR ELSE
             X.ALL = CREATE (TRUE, 4, 2, "ZZZZ", 6, 7.0) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "X := NEW DESIGNATED'" &
                        "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "X := NEW DESIGNATED'" &
                       "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
     END;

     BEGIN
          Y := NEW DESIGNATED'(CREATE (FALSE, 3, 2, "ZZZ", 5, 6.0));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "Y := NEW DESIGNATED'" &
                  "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
          IF Y = NULL OR ELSE
             Y.ALL = CREATE (FALSE, 3, 2, "ZZZ", 5, 6.0) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "Y := NEW DESIGNATED'" &
                        "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "Y := NEW DESIGNATED'" &
                       "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
     END;

     BEGIN
          Y := NEW DESIGNATED'(CREATE (TRUE, 4, 2, "ZZZZ", 6, 7.0));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "Y := NEW DESIGNATED'" &
                  "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
          IF Y = NULL OR ELSE
             Y.ALL = CREATE (TRUE, 4, 2, "ZZZZ", 6, 7.0) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "Y := NEW DESIGNATED'" &
                        "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "Y := NEW DESIGNATED'" &
                       "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
     END;

     RESULT;
END C34007U;
