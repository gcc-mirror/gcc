-- C34005V.ADA

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
--     CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
--     (IMPLICITLY) FOR DERIVED MULTI-DIMENSIONAL ARRAY TYPES WHOSE
--     COMPONENT TYPE IS A LIMITED TYPE.  THIS TEST IS PART 2 OF 2
--     TESTS WHICH COVER THE OBJECTIVE.  THE FIRST PART IS IN TEST
--     C34005S.

-- HISTORY:
--     BCB 04/12/90  CREATED ORIGINAL TEST FROM SPLIT OF C34005S.ADA.
--     RLB 10/03/02  REMOVED ILLEGAL (BY AI-246) TYPE CONVERSIONS AND
--                   SUPPORTING CODE.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34005V IS

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
          C9  : CONSTANT LP;
          C10 : CONSTANT LP;
          C11 : CONSTANT LP;
          C12 : CONSTANT LP;
          C13 : CONSTANT LP;
          C14 : CONSTANT LP;

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
          C9  : CONSTANT LP :=  9;
          C10 : CONSTANT LP := 10;
          C11 : CONSTANT LP := 11;
          C12 : CONSTANT LP := 12;
          C13 : CONSTANT LP := 13;
          C14 : CONSTANT LP := 14;

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

          FUNCTION AGGR (A, B, C, D : COMPONENT) RETURN PARENT;

          FUNCTION AGGR (A, B, C, D, E, F : COMPONENT) RETURN PARENT;

          FUNCTION AGGR (A, B, C, D, E, F, G, H : COMPONENT)
                        RETURN PARENT;

          FUNCTION AGGR (A, B, C, D, E, F, G, H, I : COMPONENT)
                        RETURN PARENT;

     END PKG_P;

     USE PKG_P;

     TYPE T IS NEW PARENT (IDENT_INT (4) .. IDENT_INT (5),
                           IDENT_INT (6) .. IDENT_INT (8));

     X : T;
     W : PARENT (4 .. 5, 6 .. 8);
     C : COMPONENT;
     B : BOOLEAN := FALSE;
     N : CONSTANT := 2;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     FUNCTION V RETURN T IS
          RESULT : T;
     BEGIN
          FOR I IN RESULT'RANGE LOOP
               FOR J IN RESULT'RANGE(2) LOOP
                    ASSIGN (RESULT (I, J), C);
               END LOOP;
          END LOOP;
          RETURN RESULT;
     END V;

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

          FUNCTION AGGR (A, B, C, D : COMPONENT) RETURN PARENT IS
               X : PARENT (INDEX'FIRST .. INDEX'FIRST + 1,
                           INDEX'FIRST .. INDEX'FIRST + 1);
          BEGIN
               ASSIGN (X (INDEX'FIRST    , INDEX'FIRST    ), A);
               ASSIGN (X (INDEX'FIRST    , INDEX'FIRST + 1), B);
               ASSIGN (X (INDEX'FIRST + 1, INDEX'FIRST    ), C);
               ASSIGN (X (INDEX'FIRST + 1, INDEX'FIRST + 1), D);
               RETURN X;
          END AGGR;

          FUNCTION AGGR (A, B, C, D, E, F : COMPONENT) RETURN PARENT IS
               X : PARENT (INDEX'FIRST .. INDEX'FIRST + 1,
                           INDEX'FIRST .. INDEX'FIRST + 2);
          BEGIN
               ASSIGN (X (INDEX'FIRST    , INDEX'FIRST    ), A);
               ASSIGN (X (INDEX'FIRST    , INDEX'FIRST + 1), B);
               ASSIGN (X (INDEX'FIRST    , INDEX'FIRST + 2), C);
               ASSIGN (X (INDEX'FIRST + 1, INDEX'FIRST    ), D);
               ASSIGN (X (INDEX'FIRST + 1, INDEX'FIRST + 1), E);
               ASSIGN (X (INDEX'FIRST + 1, INDEX'FIRST + 2), F);
               RETURN X;
          END AGGR;

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

          FUNCTION AGGR (A, B, C, D, E, F, G, H, I : COMPONENT)
                        RETURN PARENT IS
               X : PARENT (INDEX'FIRST .. INDEX'FIRST + 2,
                           INDEX'FIRST .. INDEX'FIRST + 2);
          BEGIN
               ASSIGN (X (INDEX'FIRST    , INDEX'FIRST    ), A);
               ASSIGN (X (INDEX'FIRST    , INDEX'FIRST + 1), B);
               ASSIGN (X (INDEX'FIRST    , INDEX'FIRST + 2), C);
               ASSIGN (X (INDEX'FIRST + 1, INDEX'FIRST    ), D);
               ASSIGN (X (INDEX'FIRST + 1, INDEX'FIRST + 1), E);
               ASSIGN (X (INDEX'FIRST + 1, INDEX'FIRST + 2), F);
               ASSIGN (X (INDEX'FIRST + 2, INDEX'FIRST    ), G);
               ASSIGN (X (INDEX'FIRST + 2, INDEX'FIRST + 1), H);
               ASSIGN (X (INDEX'FIRST + 2, INDEX'FIRST + 2), I);
               RETURN X;
          END AGGR;

     END PKG_P;

BEGIN
     TEST ("C34005V", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
                      "TYPE IS A LIMITED TYPE.  THIS TEST IS PART 2 " &
                      "OF 2 TESTS WHICH COVER THE OBJECTIVE.  THE " &
                      "FIRST PART IS IN TEST C34005S");

     ASSIGN (X (IDENT_INT (4), IDENT_INT (6)), CREATE (1));
     ASSIGN (X (IDENT_INT (4), IDENT_INT (7)), CREATE (2));
     ASSIGN (X (IDENT_INT (4), IDENT_INT (8)), CREATE (3));
     ASSIGN (X (IDENT_INT (5), IDENT_INT (6)), CREATE (4));
     ASSIGN (X (IDENT_INT (5), IDENT_INT (7)), CREATE (5));
     ASSIGN (X (IDENT_INT (5), IDENT_INT (8)), CREATE (6));

     ASSIGN (W (4, 6), CREATE (1));
     ASSIGN (W (4, 7), CREATE (2));
     ASSIGN (W (4, 8), CREATE (3));
     ASSIGN (W (5, 6), CREATE (4));
     ASSIGN (W (5, 7), CREATE (5));
     ASSIGN (W (5, 8), CREATE (6));

     ASSIGN (C, CREATE (2));

     IF NOT EQUAL (T'(X), AGGR (C1, C2, C3, C4, C5, C6)) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF NOT EQUAL (T (X), AGGR (C1, C2, C3, C4, C5, C6)) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF NOT EQUAL (T (W), AGGR (C1, C2, C3, C4, C5, C6)) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     BEGIN
          IF NOT EQUAL (PARENT (X), AGGR (C1, C2, C3, C4, C5, C6)) OR
             NOT EQUAL (PARENT (CREATE (6, 9, 2, 3, C4, X)),
                        AGGR (C4, C5, C6, C7, C8, C9, C10, C11)) THEN
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR WHEN PREPARING TO CONVERT " &
                       "TO PARENT");
          WHEN OTHERS =>
               FAILED ("EXCEPTION WHEN PREPARING TO CONVERT " &
                       "TO PARENT");
     END;

     IF NOT (X IN T) OR AGGR (C1, C2, C3, C4) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR
        NOT (AGGR (C1, C2, C3, C4, C5, C6, C7, C8, C9) NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     RESULT;
END C34005V;
