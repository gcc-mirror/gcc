-- C34005R.ADA

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
--     FOR DERIVED ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT TYPE IS A
--     LIMITED TYPE:

--        CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT
--        FOR THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION
--        IS CONSTRAINED.

--        CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS
--        ALSO IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 08/19/87  CREATED ORIGINAL TEST.
--     VCL 07/01/88  ADDED EXCEPTION HANDLERS TO CATCH INCORRECT TYPE
--                   CONVERSIONS TO DERIVED SUBTYPES.

WITH REPORT; USE REPORT;

PROCEDURE C34005R IS

     PACKAGE PKG_L IS

          TYPE LP IS LIMITED PRIVATE;

          FUNCTION CREATE (X : INTEGER) RETURN LP;

          FUNCTION VALUE (X : LP) RETURN INTEGER;

          FUNCTION EQUAL (X, Y : LP) RETURN BOOLEAN;

          PROCEDURE ASSIGN (X : OUT LP; Y : LP);

          C1 : CONSTANT LP;
          C2 : CONSTANT LP;
          C3 : CONSTANT LP;
          C4 : CONSTANT LP;
          C5 : CONSTANT LP;

     PRIVATE

          TYPE LP IS NEW INTEGER;

          C1 : CONSTANT LP := 1;
          C2 : CONSTANT LP := 2;
          C3 : CONSTANT LP := 3;
          C4 : CONSTANT LP := 4;
          C5 : CONSTANT LP := 5;

     END PKG_L;

     USE PKG_L;

     SUBTYPE COMPONENT IS LP;

     PACKAGE PKG_P IS

          FIRST : CONSTANT := 0;
          LAST  : CONSTANT := 100;

          SUBTYPE INDEX IS INTEGER RANGE FIRST .. LAST;

          TYPE PARENT IS ARRAY (INDEX RANGE <>) OF COMPONENT;

          FUNCTION CREATE ( F, L  : INDEX;
                            C     : COMPONENT;
                            DUMMY : PARENT   -- TO RESOLVE OVERLOADING.
                          ) RETURN PARENT;

          FUNCTION EQUAL (X, Y : PARENT) RETURN BOOLEAN;

          FUNCTION AGGR (X, Y : COMPONENT) RETURN PARENT;

          FUNCTION AGGR (W, X, Y, Z : COMPONENT) RETURN PARENT;

     END PKG_P;

     USE PKG_P;

     TYPE T IS NEW PARENT (IDENT_INT (5) .. IDENT_INT (7));

     SUBTYPE SUBPARENT IS PARENT (5 .. 7);

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
             ( F, L  : INDEX;
               C     : COMPONENT;
               DUMMY : PARENT
             ) RETURN PARENT
          IS
               A : PARENT (F .. L);
               B : COMPONENT;
          BEGIN
               ASSIGN (B, C);
               FOR I IN F .. L LOOP
                    ASSIGN (A (I), B);
                    ASSIGN (B, CREATE (VALUE (B) + 1));
               END LOOP;
               RETURN A;
          END CREATE;

          FUNCTION EQUAL (X, Y : PARENT) RETURN BOOLEAN IS
          BEGIN
               IF X'LENGTH /= Y'LENGTH THEN
                    RETURN FALSE;
               ELSE FOR I IN X'RANGE LOOP
                         IF NOT EQUAL (X (I),
                                       Y (I - X'FIRST + Y'FIRST)) THEN
                              RETURN FALSE;
                         END IF;
                    END LOOP;
               END IF;
               RETURN TRUE;
          END EQUAL;

          FUNCTION AGGR (X, Y : COMPONENT) RETURN PARENT IS
               RESULT : PARENT (INDEX'FIRST .. INDEX'FIRST + 1);
          BEGIN
               ASSIGN (RESULT (INDEX'FIRST    ), X);
               ASSIGN (RESULT (INDEX'FIRST + 1), Y);
               RETURN RESULT;
          END AGGR;

          FUNCTION AGGR (W, X, Y, Z : COMPONENT) RETURN PARENT IS
               RESULT : PARENT (INDEX'FIRST .. INDEX'FIRST + 3);
          BEGIN
               ASSIGN (RESULT (INDEX'FIRST    ), W);
               ASSIGN (RESULT (INDEX'FIRST + 1), X);
               ASSIGN (RESULT (INDEX'FIRST + 2), Y);
               ASSIGN (RESULT (INDEX'FIRST + 3), Z);
               RETURN RESULT;
          END AGGR;

     END PKG_P;

     PROCEDURE ASSIGN (X : IN OUT T; Y : T) IS
     BEGIN
          FOR I IN X'RANGE LOOP
               ASSIGN (X (I), Y (I));
          END LOOP;
     END ASSIGN;

     PROCEDURE ASSIGN (X : IN OUT S; Y : S) IS
     BEGIN
          FOR I IN X'RANGE LOOP
               ASSIGN (X (I), Y (I));
          END LOOP;
     END ASSIGN;

BEGIN
     TEST ("C34005R", "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
                      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
                      "WHEN THE DERIVED TYPE DEFINITION IS " &
                      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
                      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
                      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
                      "ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
                      "TYPE IS A LIMITED TYPE");

     ASSIGN (X (IDENT_INT (5)), CREATE (2));
     ASSIGN (X (IDENT_INT (6)), CREATE (3));
     ASSIGN (X (IDENT_INT (7)), CREATE (4));

     ASSIGN (Y (5), C2);
     ASSIGN (Y (6), C3);
     ASSIGN (Y (7), C4);

     -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

     BEGIN
          IF NOT EQUAL (CREATE (2, 3, C4, X), AGGR (C4, C5)) THEN
               FAILED ("CANNOT CREATE BASE TYPE VALUES OUTSIDE " &
                       "OF THE SUBTYPE T");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED WHILE CHECKING BASE TYPE " &
                       "VALUES OUTSIDE OF THE SUBTYPE T");
     END;

     BEGIN
          IF NOT EQUAL (CREATE (2, 3, C4, Y), AGGR (C4, C5)) THEN
               FAILED ("CANNOT CREATE BASE TYPE VALUES OUTSIDE " &
                       "OF THE SUBTYPE S");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED WHILE CHECKING BASE TYPE " &
                       "VALUES OUTSIDE OF THE SUBTYPE S");
     END;

     BEGIN
          IF NOT EQUAL (X(IDENT_INT (6)..IDENT_INT (7)),
                        AGGR (C3, C4))                     THEN
               FAILED ("INCORRECT SLICE OF X (VALUE)");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED WHILE CHECKING SLICE OF X");
     END;

     BEGIN
          IF NOT EQUAL (AGGR (C3, C4),
                        Y(IDENT_INT (6)..IDENT_INT (7)))  THEN
               FAILED ("INCORRECT SLICE OF Y (VALUE)");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED WHILE CHECKING SLICE OF Y");
     END;

     -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

     IF T'FIRST /= 5 OR T'LAST /= 7 OR
        S'FIRST /= 5 OR S'LAST /= 7 THEN
          FAILED ("INCORRECT 'FIRST OR 'LAST");
     END IF;

     BEGIN
          ASSIGN (X, CREATE (5, 7, C1, X));
          ASSIGN (Y, CREATE (5, 7, C1, Y));
          IF NOT EQUAL (PARENT (X), PARENT (Y)) THEN  -- USE X AND Y.
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY OK ASSIGN CALL");
     END;

     BEGIN
          ASSIGN (X, AGGR (C1, C2));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (X, AGGR (C1, C2))");
          IF EQUAL (X, AGGR (C1, C2)) THEN  -- USE X.
               COMMENT ("X ALTERED -- ASSIGN (X, AGGR (C1, C2))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (X, AGGR (C1, C2))");
     END;

     BEGIN
          ASSIGN (X, AGGR (C1, C2, C3, C4));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (X, AGGR (C1, C2, C3, C4))");
          IF EQUAL (X, AGGR (C1, C2, C3, C4)) THEN  -- USE X.
               COMMENT ("X ALTERED -- " &
                        "ASSIGN (X, AGGR (C1, C2, C3, C4))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (X, AGGR (C1, C2, C3, C4))");
     END;

     BEGIN
          ASSIGN (Y, AGGR (C1, C2));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (Y, AGGR (C1, C2))");
          IF EQUAL (Y, AGGR (C1, C2)) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- ASSIGN (Y, AGGR (C1, C2))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (Y, AGGR (C1, C2))");
     END;

     BEGIN
          ASSIGN (Y, AGGR (C1, C2, C3, C4));
          FAILED ("CONSTRAINT_ERROR NOT RAISED -- " &
                  "ASSIGN (Y, AGGR (C1, C2, C3, C4))");
          IF EQUAL (Y, AGGR (C1, C2, C3, C4)) THEN  -- USE Y.
               COMMENT ("Y ALTERED -- " &
                        "ASSIGN (Y, AGGR (C1, C2, C3, C4))");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED -- " &
                       "ASSIGN (Y, AGGR (C1, C2, C3, C4))");
     END;

     RESULT;
END C34005R;
