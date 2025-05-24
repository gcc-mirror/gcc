-- C34009L.ADA

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
--     FOR DERIVED LIMITED PRIVATE TYPES WITH DISCRIMINANTS:

--        CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT
--        FOR THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION
--        IS CONSTRAINED.

--        CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS
--        ALSO IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 09/01/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C34009L IS

     PACKAGE PKG IS

          MAX_LEN : CONSTANT := 10;

          SUBTYPE LENGTH IS NATURAL RANGE 0 .. MAX_LEN;

          TYPE PARENT (B : BOOLEAN := TRUE; L : LENGTH := 3) IS
                      LIMITED PRIVATE;

          FUNCTION CREATE ( B : BOOLEAN;
                            L : LENGTH;
                            I : INTEGER;
                            S : STRING;
                            J : INTEGER;
                            F : FLOAT;
                            X : PARENT  -- TO RESOLVE OVERLOADING.
                          ) RETURN PARENT;

          FUNCTION CON ( B : BOOLEAN;
                         L : LENGTH;
                         I : INTEGER;
                         S : STRING;
                         J : INTEGER
                       ) RETURN PARENT;

          FUNCTION CON ( B : BOOLEAN;
                         L : LENGTH;
                         I : INTEGER;
                         F : FLOAT
                       ) RETURN PARENT;

          FUNCTION EQUAL (X, Y : PARENT) RETURN BOOLEAN;

          PROCEDURE ASSIGN (X : OUT PARENT; Y : PARENT);

     PRIVATE

          TYPE PARENT (B : BOOLEAN := TRUE; L : LENGTH := 3) IS
               RECORD
                    I : INTEGER := 2;
                    CASE B IS
                         WHEN TRUE =>
                              S : STRING (1 .. L) := (1 .. L => 'A');
                              J : INTEGER := 2;
                         WHEN FALSE =>
                              F : FLOAT := 5.0;
                    END CASE;
               END RECORD;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT (IDENT_BOOL (TRUE), IDENT_INT (3));

     SUBTYPE SUBPARENT IS PARENT (TRUE, 3);

     TYPE S IS NEW SUBPARENT;

     X : T;
     Y : S;

     PACKAGE BODY PKG IS

          FUNCTION CREATE
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               S : STRING;
               J : INTEGER;
               F : FLOAT;
               X : PARENT
             ) RETURN PARENT
          IS
          BEGIN
               CASE B IS
                    WHEN TRUE =>
                         RETURN (TRUE, L, I, S, J);
                    WHEN FALSE =>
                         RETURN (FALSE, L, I, F);
               END CASE;
          END CREATE;

          FUNCTION CON
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               S : STRING;
               J : INTEGER
             ) RETURN PARENT
          IS
          BEGIN
               RETURN (TRUE, L, I, S, J);
          END CON;

          FUNCTION CON
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               F : FLOAT
             ) RETURN PARENT
          IS
          BEGIN
               RETURN (FALSE, L, I, F);
          END CON;

          FUNCTION EQUAL (X, Y : PARENT) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END EQUAL;

          PROCEDURE ASSIGN (X : OUT PARENT; Y : PARENT) IS
          BEGIN
               X := Y;
          END ASSIGN;

     END PKG;

BEGIN
     TEST ("C34009L", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "LIMITED PRIVATE TYPES WITH DISCRIMINANTS");

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     IF NOT EQUAL (CREATE (FALSE, 2, 3, "ZZ", 5, 6.0, X),
                   CON (FALSE, 2, 3, 6.0)) OR
        NOT EQUAL (CREATE (FALSE, 2, 3, "ZZ", 5, 6.0, Y),
                   CON (FALSE, 2, 3, 6.0)) THEN
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

     IF NOT X'CONSTRAINED OR NOT Y'CONSTRAINED THEN
          FAILED ("INCORRECT 'CONSTRAINED");
     END IF;

     BEGIN
          ASSIGN (X, CON (TRUE, 3, 1, "ABC", 4));
          ASSIGN (Y, CON (TRUE, 3, 1, "ABC", 4));
          IF NOT EQUAL (PARENT (X), PARENT (Y)) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGN CALL");
     END;

     BEGIN
          ASSIGN (X, CON (FALSE, 3, 2, 6.0));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (X, CON (FALSE, 3, 2, 6.0))");
          IF EQUAL (X, CON (FALSE, 3, 2, 6.0)) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "ASSIGN (X, CON (FALSE, 3, 2, 6.0))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (X, CON (FALSE, 3, 2, 6.0))");
     END;

     BEGIN
          ASSIGN (X, CON (TRUE, 4, 2, "ZZZZ", 6));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (X, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
          IF EQUAL (X, CON (TRUE, 4, 2, "ZZZZ", 6)) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "ASSIGN (X, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (X, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
     END;

     BEGIN
          ASSIGN (Y, CON (FALSE, 3, 2, 6.0));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (Y, CON (FALSE, 3, 2, 6.0))");
          IF EQUAL (Y, CON (FALSE, 3, 2, 6.0)) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "ASSIGN (Y, CON (FALSE, 3, 2, 6.0))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (Y, CON (FALSE, 3, 2, 6.0))");
     END;

     BEGIN
          ASSIGN (Y, CON (TRUE, 4, 2, "ZZZZ", 6));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (Y, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
          IF EQUAL (Y, CON (TRUE, 4, 2, "ZZZZ", 6)) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "ASSIGN (Y, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (Y, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
     END;

     RESULT;
END C34009L;
