-- C34005U.ADA

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
--     FOR DERIVED MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT TYPE IS
--     A LIMITED TYPE:

--        CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT
--        FOR THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION
--        IS CONSTRAINED.

--        CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS
--        ALSO IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 08/21/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C34005U IS

     PACKAGE PKG_L IS

          TYPE LP IS LIMITED PRIVATE;

          FUNCTION CREATE (X : INTEGER) RETURN LP;

          FUNCTION VALUE (X : LP) RETURN INTEGER;

          FUNCTION EQUAL (X, Y : LP) RETURN BOOLEAN;

          PROCEDURE ASSIGN (X : OUT LP; Y : LP);

          C1  : CONSTANT LP;
          C2  : CONSTANT LP;
          C3  : CONSTANT LP;
          C4  : CONSTANT LP;
          C5  : CONSTANT LP;
          C6  : CONSTANT LP;
          C7  : CONSTANT LP;
          C8  : CONSTANT LP;

     PRIVATE

          TYPE LP IS NEW INTEGER;

          C1  : CONSTANT LP :=  1;
          C2  : CONSTANT LP :=  2;
          C3  : CONSTANT LP :=  3;
          C4  : CONSTANT LP :=  4;
          C5  : CONSTANT LP :=  5;
          C6  : CONSTANT LP :=  6;
          C7  : CONSTANT LP :=  7;
          C8  : CONSTANT LP :=  8;

     END PKG_L;

     USE PKG_L;

     SUBTYPE COMPONENT IS LP;

     PACKAGE PKG_P IS

          FIRST : CONSTANT := 0;
          LAST  : CONSTANT := 10;

          SUBTYPE INDEX IS INTEGER RANGE FIRST .. LAST;

          TYPE PARENT IS ARRAY (INDEX RANGE <>, INDEX RANGE <>) OF
                               COMPONENT;

          FUNCTION CREATE ( F1, L1 : INDEX;
                            F2, L2 : INDEX;
                            C      : COMPONENT;
                            DUMMY  : PARENT   -- TO RESOLVE OVERLOADING.
                          ) RETURN PARENT;

          FUNCTION EQUAL (X, Y : PARENT) RETURN BOOLEAN;

          FUNCTION AGGR (A, B, C, D, E, F, G, H : COMPONENT)
                        RETURN PARENT;

     END PKG_P;

     USE PKG_P;

     TYPE T IS NEW PARENT (IDENT_INT (4) .. IDENT_INT (5),
                           IDENT_INT (6) .. IDENT_INT (8));

     SUBTYPE SUBPARENT IS PARENT (4 .. 5, 6 .. 8);

     TYPE S IS NEW SUBPARENT;

     X : T;
     Y : S;

     PACKAGE BODY PKG_L IS

          FUNCTION CREATE (X : INTEGER) RETURN LP IS
          BEGIN
               RETURN LP (IDENT_INT (X));
          END CREATE;

          FUNCTION VALUE (X : LP) RETURN INTEGER IS
          BEGIN
               RETURN INTEGER (X);
          END VALUE;

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
             ( F1, L1 : INDEX;
               F2, L2 : INDEX;
               C      : COMPONENT;
               DUMMY  : PARENT
             ) RETURN PARENT
          IS
               A : PARENT (F1 .. L1, F2 .. L2);
               B : COMPONENT;
          BEGIN
               ASSIGN (B, C);
               FOR I IN F1 .. L1 LOOP
                    FOR J IN F2 .. L2 LOOP
                         ASSIGN (A (I, J), B);
                         ASSIGN (B, CREATE (VALUE (B) + 1));
                    END LOOP;
               END LOOP;
               RETURN A;
          END CREATE;

          FUNCTION EQUAL (X, Y : PARENT) RETURN BOOLEAN IS
          BEGIN
               IF X'LENGTH /= Y'LENGTH OR
                  X'LENGTH(2) /= Y'LENGTH(2) THEN
                    RETURN FALSE;
               ELSE FOR I IN X'RANGE LOOP
                         FOR J IN X'RANGE(2) LOOP
                              IF NOT EQUAL (X (I, J),
                                            Y (I - X'FIRST + Y'FIRST,
                                               J - X'FIRST(2) +
                                                   Y'FIRST(2))) THEN
                                   RETURN FALSE;
                              END IF;
                         END LOOP;
                    END LOOP;
               END IF;
               RETURN TRUE;
          END EQUAL;

          FUNCTION AGGR (A, B, C, D, E, F, G, H : COMPONENT)
                        RETURN PARENT IS
               X : PARENT (INDEX'FIRST .. INDEX'FIRST + 3,
                           INDEX'FIRST .. INDEX'FIRST + 1);
          BEGIN
               ASSIGN (X (INDEX'FIRST    , INDEX'FIRST    ), A);
               ASSIGN (X (INDEX'FIRST    , INDEX'FIRST + 1), B);
               ASSIGN (X (INDEX'FIRST + 1, INDEX'FIRST    ), C);
               ASSIGN (X (INDEX'FIRST + 1, INDEX'FIRST + 1), D);
               ASSIGN (X (INDEX'FIRST + 2, INDEX'FIRST    ), E);
               ASSIGN (X (INDEX'FIRST + 2, INDEX'FIRST + 1), F);
               ASSIGN (X (INDEX'FIRST + 3, INDEX'FIRST    ), G);
               ASSIGN (X (INDEX'FIRST + 3, INDEX'FIRST + 1), H);
               RETURN X;
          END AGGR;

     END PKG_P;

     PROCEDURE ASSIGN (X : IN OUT T; Y : T) IS
     BEGIN
          FOR I IN X'RANGE LOOP
               FOR J IN X'RANGE(2) LOOP
                    ASSIGN (X (I, J), Y (I, J));
               END LOOP;
          END LOOP;
     END ASSIGN;

     PROCEDURE ASSIGN (X : IN OUT S; Y : S) IS
     BEGIN
          FOR I IN X'RANGE LOOP
               FOR J IN X'RANGE(2) LOOP
                    ASSIGN (X (I, J), Y (I, J));
               END LOOP;
          END LOOP;
     END ASSIGN;

BEGIN
     TEST ("C34005U", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
                      "TYPE IS A LIMITED TYPE");

     FOR I IN X'RANGE LOOP
          FOR J IN X'RANGE(2) LOOP
               ASSIGN (X (I, J), C2);
               ASSIGN (Y (I, J), C2);
          END LOOP;
     END LOOP;

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.
     BEGIN
          IF NOT EQUAL (CREATE (6, 9, 2, 3, C1, X),
                        AGGR (C1, C2, C3, C4, C5, C6, C7, C8)) OR
             NOT EQUAL (CREATE (6, 9, 2, 3, C1, Y),
                        AGGR (C1, C2, C3, C4, C5, C6, C7, C8)) THEN
               FAILED ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE " &
                       "SUBTYPE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR WHEN TRYING TO CREATE BASE " &
                       "TYPE VALUES OUTSIDE THE SUBTYPE");
          WHEN OTHERS =>
               FAILED ("EXCEPTION WHEN TRYING TO CREATE BASE TYPE " &
                       "VALUES OUTSIDE THE SUBTYPE");
     END;

     IF AGGR (C1, C2, C3, C4, C5, C6, C7, C8) IN T OR
        AGGR (C1, C2, C3, C4, C5, C6, C7, C8) IN S THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

     IF T'FIRST /= 4 OR T'LAST /= 5 OR
        S'FIRST /= 4 OR S'LAST /= 5 OR
        T'FIRST (2) /= 6 OR T'LAST (2) /= 8 OR
        S'FIRST (2) /= 6 OR S'LAST (2) /= 8 THEN
          FAILED ("INCORRECT 'FIRST OR 'LAST");
     END IF;

     BEGIN
          ASSIGN (X, CREATE (4, 5, 6, 8, C1, X));
          ASSIGN (Y, CREATE (4, 5, 6, 8, C1, Y));
          IF NOT EQUAL (PARENT (X), PARENT (Y)) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGN CALL");
     END;

     BEGIN
          ASSIGN (X, CREATE (4, 4, 6, 8, C1, X));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (X, CREATE (4, 4, 6, 8, C1, X))");
          IF EQUAL (X, CREATE (4, 4, 6, 8, C1, X)) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "ASSIGN (X, CREATE (4, 4, 6, 8, C1, X))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (X, CREATE (4, 4, 6, 8, C1, X))");
     END;

     BEGIN
          ASSIGN (X, CREATE (4, 6, 6, 8, C1, X));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (X, CREATE (4, 6, 6, 8, C1, X))");
          IF EQUAL (X, CREATE (4, 6, 6, 8, C1, X)) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "ASSIGN (X, CREATE (4, 6, 6, 8, C1, X))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (X, CREATE (4, 6, 6, 8, C1, X))");
     END;

     BEGIN
          ASSIGN (X, CREATE (4, 5, 6, 7, C1, X));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (X, CREATE (4, 5, 6, 7, C1, X))");
          IF EQUAL (X, CREATE (4, 5, 6, 7, C1, X)) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "ASSIGN (X, CREATE (4, 5, 6, 7, C1, X))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (X, CREATE (4, 5, 6, 7, C1, X))");
     END;

     BEGIN
          ASSIGN (X, CREATE (4, 5, 6, 9, C1, X));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (X, CREATE (4, 5, 6, 9, C1, X))");
          IF EQUAL (X, CREATE (4, 5, 6, 9, C1, X)) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "ASSIGN (X, CREATE (4, 5, 6, 9, C1, X))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (X, CREATE (4, 5, 6, 9, C1, X))");
     END;

     BEGIN
          ASSIGN (Y, CREATE (4, 4, 6, 8, C1, Y));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (Y, CREATE (4, 4, 6, 8, C1, Y))");
          IF EQUAL (Y, CREATE (4, 4, 6, 8, C1, Y)) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "ASSIGN (Y, CREATE (4, 4, 6, 8, C1, Y))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (Y, CREATE (4, 4, 6, 8, C1, Y))");
     END;

     BEGIN
          ASSIGN (Y, CREATE (4, 6, 6, 8, C1, Y));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (Y, CREATE (4, 6, 6, 8, C1, Y))");
          IF EQUAL (Y, CREATE (4, 6, 6, 8, C1, Y)) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "ASSIGN (Y, CREATE (4, 6, 6, 8, C1, Y))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (Y, CREATE (4, 6, 6, 8, C1, Y))");
     END;

     BEGIN
          ASSIGN (Y, CREATE (4, 5, 6, 7, C1, Y));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (Y, CREATE (4, 5, 6, 7, C1, Y))");
          IF EQUAL (Y, CREATE (4, 5, 6, 7, C1, Y)) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "ASSIGN (Y, CREATE (4, 5, 6, 7, C1, Y))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (Y, CREATE (4, 5, 6, 7, C1, Y))");
     END;

     BEGIN
          ASSIGN (Y, CREATE (4, 5, 6, 9, C1, Y));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (Y, CREATE (4, 5, 6, 9, C1, Y))");
          IF EQUAL (Y, CREATE (4, 5, 6, 9, C1, Y)) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "ASSIGN (Y, CREATE (4, 5, 6, 9, C1, Y))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (Y, CREATE (4, 5, 6, 9, C1, Y))");
     END;

     RESULT;
END C34005U;
