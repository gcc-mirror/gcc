-- C34006L.ADA

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
--     FOR DERIVED RECORD TYPES WITH DISCRIMINANTS AND WITH A LIMITED
--     COMPONENT TYPE:

--        CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT
--        FOR THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION
--        IS CONSTRAINED.

--        CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS
--        ALSO IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 08/26/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C34006L IS

     PACKAGE PKG_L IS

          TYPE LP IS LIMITED PRIVATE;

          FUNCTION CREATE (X : INTEGER) RETURN LP;

          FUNCTION EQUAL (X, Y : LP) RETURN BOOLEAN;

          PROCEDURE ASSIGN (X : OUT LP; Y : LP);

          C2 : CONSTANT LP;
          C4 : CONSTANT LP;
          C5 : CONSTANT LP;
          C6 : CONSTANT LP;

     PRIVATE

          TYPE LP IS NEW INTEGER;

          C2 : CONSTANT LP := 2;
          C4 : CONSTANT LP := 4;
          C5 : CONSTANT LP := 5;
          C6 : CONSTANT LP := 6;

     END PKG_L;

     USE PKG_L;

     SUBTYPE COMPONENT IS LP;

     PACKAGE PKG_P IS

          MAX_LEN : CONSTANT := 10;

          SUBTYPE LENGTH IS NATURAL RANGE 0 .. MAX_LEN;

          TYPE PARENT (B : BOOLEAN := TRUE; L : LENGTH := 3) IS
               RECORD
                    I : INTEGER := 2;
                    CASE B IS
                         WHEN TRUE =>
                              S : STRING (1 .. L) := (1 .. L => 'A');
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

          FUNCTION EQUAL (X, Y : PARENT) RETURN BOOLEAN;

          FUNCTION AGGR ( B : BOOLEAN;
                          L : LENGTH;
                          I : INTEGER;
                          S : STRING;
                          C : COMPONENT
                        ) RETURN PARENT;

          FUNCTION AGGR ( B : BOOLEAN;
                          L : LENGTH;
                          I : INTEGER;
                          F : FLOAT
                        ) RETURN PARENT;

     END PKG_P;

     USE PKG_P;

     TYPE T IS NEW PARENT (IDENT_BOOL (TRUE), IDENT_INT (3));

     SUBTYPE SUBPARENT IS PARENT (TRUE, 3);

     TYPE S IS NEW SUBPARENT;

     X : T;
     Y : S;

     PACKAGE BODY PKG_L IS

          FUNCTION CREATE (X : INTEGER) RETURN LP IS
          BEGIN
               RETURN LP (IDENT_INT (X));
          END CREATE;

          FUNCTION EQUAL (X, Y : LP) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END EQUAL;

          PROCEDURE ASSIGN (X : OUT LP; Y : LP) IS
          BEGIN
               X := Y;
          END ASSIGN;

     END PKG_L;

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
               A : PARENT (B, L);
          BEGIN
               A.I := I;
               CASE B IS
                    WHEN TRUE =>
                         A.S := S;
                         ASSIGN (A.C, C);
                    WHEN FALSE =>
                         A.F := F;
               END CASE;
               RETURN A;
          END CREATE;

          FUNCTION EQUAL (X, Y : PARENT) RETURN BOOLEAN IS
          BEGIN
               IF X.B /= Y.B OR X.L /= Y.L OR X.I /= Y.I THEN
                    RETURN FALSE;
               END IF;
               CASE X.B IS
                    WHEN TRUE =>
                         RETURN X.S = Y.S AND EQUAL (X.C, Y.C);
                    WHEN FALSE =>
                         RETURN X.F = Y.F;
               END CASE;
          END EQUAL;

          FUNCTION AGGR
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               S : STRING;
               C : COMPONENT
             ) RETURN PARENT
          IS
               RESULT : PARENT (B, L);
          BEGIN
               RESULT.I := I;
               RESULT.S := S;
               ASSIGN (RESULT.C, C);
               RETURN RESULT;
          END AGGR;

          FUNCTION AGGR
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               F : FLOAT
             ) RETURN PARENT
          IS
               RESULT : PARENT (B, L);
          BEGIN
               RESULT.I := I;
               RESULT.F := F;
               RETURN RESULT;
          END AGGR;

     END PKG_P;

     PROCEDURE ASSIGN (X : IN OUT T; Y : T) IS
     BEGIN
          X.I := Y.I;
          X.S := Y.S;
          ASSIGN (X.C, Y.C);
     END ASSIGN;

     PROCEDURE ASSIGN (X : IN OUT S; Y : S) IS
     BEGIN
          X.I := Y.I;
          X.S := Y.S;
          ASSIGN (X.C, Y.C);
     END ASSIGN;

BEGIN
     TEST ("C34006L", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "RECORD TYPES WITH DISCRIMINANTS AND WITH A " &
                      "LIMITED COMPONENT TYPE");

     ASSIGN (X.C, CREATE (2));
     ASSIGN (Y.C, C2);

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     IF NOT EQUAL (CREATE (FALSE, 2, 3, "ZZ", C5, 6.0, X),
                   AGGR (FALSE, 2, 3, 6.0)) OR
        NOT EQUAL (CREATE (FALSE, 2, 3, "ZZ", C5, 6.0, Y),
                   AGGR (FALSE, 2, 3, 6.0)) THEN
          FAILED ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE SUBTYPE");
     END IF;

     IF CREATE (FALSE, 2, 3, "ZZ", C5, 6.0, X) IN T OR
        CREATE (FALSE, 2, 3, "ZZ", C5, 6.0, Y) IN S THEN
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
          ASSIGN (X, AGGR (TRUE, 3, 1, "ABC", C4));
          ASSIGN (Y, AGGR (TRUE, 3, 1, "ABC", C4));
          IF NOT EQUAL (PARENT (X), PARENT (Y)) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGN CALL");
     END;

     BEGIN
          ASSIGN (X, AGGR (FALSE, 3, 2, 6.0));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (X, AGGR (FALSE, 3, 2, 6.0))");
          IF EQUAL (X, AGGR (FALSE, 3, 2, 6.0)) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "ASSIGN (X, AGGR (FALSE, 3, 2, 6.0))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (X, AGGR (FALSE, 3, 2, 6.0))");
     END;

     BEGIN
          ASSIGN (X, AGGR (TRUE, 4, 2, "ZZZZ", C6));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (X, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
          IF EQUAL (X, AGGR (TRUE, 4, 2, "ZZZZ", C6)) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "ASSIGN (X, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (X, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
     END;

     BEGIN
          ASSIGN (Y, AGGR (FALSE, 3, 2, 6.0));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (Y, AGGR (FALSE, 3, 2, 6.0))");
          IF EQUAL (Y, AGGR (FALSE, 3, 2, 6.0)) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "ASSIGN (Y, AGGR (FALSE, 3, 2, 6.0))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (Y, AGGR (FALSE, 3, 2, 6.0))");
     END;

     BEGIN
          ASSIGN (Y, AGGR (TRUE, 4, 2, "ZZZZ", C6));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (Y, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
          IF EQUAL (Y, AGGR (TRUE, 4, 2, "ZZZZ", C6)) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "ASSIGN (Y, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (Y, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
     END;

     RESULT;
END C34006L;
