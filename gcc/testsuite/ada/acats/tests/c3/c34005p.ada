-- C34005P.ADA

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
--     (IMPLICITLY) FOR DERIVED ONE-DIMENSIONAL ARRAY TYPES WHOSE
--     COMPONENT TYPE IS A LIMITED TYPE.

-- HISTORY:
--     JRK 08/17/87  CREATED ORIGINAL TEST.
--     VCL 07/01/88  MODIFIED THE STATEMENTS INVOLVING THE 'SIZE
--                   ATTRIBUTE TO REMOVE ANY ASSUMPTIONS ABOUT THE
--                   SIZES.  ADDED EXCEPTION HANDLERS TO CATCH INCORRECT
--                   TYPE CONVERSIONS TO DERIVED SUBTYPES.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.
--     RLB 10/03/02  REMOVED ILLEGAL (BY AI-246) TYPE CONVERSIONS AND
--                   SUPPORTING CODE.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34005P IS

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
          C6 : CONSTANT LP;

     PRIVATE

          TYPE LP IS NEW INTEGER;

          C1 : CONSTANT LP := 1;
          C2 : CONSTANT LP := 2;
          C3 : CONSTANT LP := 3;
          C4 : CONSTANT LP := 4;
          C5 : CONSTANT LP := 5;
          C6 : CONSTANT LP := 6;

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

          FUNCTION AGGR (X, Y, Z : COMPONENT) RETURN PARENT;

     END PKG_P;

     USE PKG_P;

     TYPE T IS NEW PARENT (IDENT_INT (5) .. IDENT_INT (7));

     X : T;
     W : PARENT (5 .. 7);
     C : COMPONENT;
     B : BOOLEAN := FALSE;
     N : CONSTANT := 1;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     FUNCTION V RETURN T IS
          RESULT : T;
     BEGIN
          FOR I IN RESULT'RANGE LOOP
               ASSIGN (RESULT (I), C);
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

          FUNCTION AGGR (X, Y, Z : COMPONENT) RETURN PARENT IS
               RESULT : PARENT (INDEX'FIRST .. INDEX'FIRST + 2);
          BEGIN
               ASSIGN (RESULT (INDEX'FIRST    ), X);
               ASSIGN (RESULT (INDEX'FIRST + 1), Y);
               ASSIGN (RESULT (INDEX'FIRST + 2), Z);
               RETURN RESULT;
          END AGGR;

     END PKG_P;

BEGIN
     TEST ("C34005P", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
                      "TYPE IS A LIMITED TYPE");

     ASSIGN (X (IDENT_INT (5)), CREATE (1));
     ASSIGN (X (IDENT_INT (6)), CREATE (2));
     ASSIGN (X (IDENT_INT (7)), CREATE (3));

     ASSIGN (W (5), CREATE (1));
     ASSIGN (W (6), CREATE (2));
     ASSIGN (W (7), CREATE (3));

     ASSIGN (C, CREATE (2));

     IF NOT EQUAL (T'(X), AGGR (C1, C2, C3)) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF NOT EQUAL (T(X), AGGR (C1, C2, C3)) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF NOT EQUAL (T(W), AGGR (C1, C2, C3)) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF NOT EQUAL (PARENT(X), AGGR (C1, C2, C3)) THEN
          FAILED ("INCORRECT CONVERSION TO PARENT - 1");
     END IF;

     BEGIN
          IF NOT EQUAL (PARENT(CREATE (2, 3, C4, X)),
                        AGGR (C4, C5))                 THEN
               FAILED ("INCORRECT CONVERSION TO PARENT - 2");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED WHILE CHECKING BASE TYPE " &
                       "VALUES OUTSIDE OF THE SUBTYPE T - 1");
     END;

     IF NOT EQUAL (X(IDENT_INT (5)), C1) THEN
          FAILED ("INCORRECT INDEX (VALUE)");
     END IF;

     BEGIN
          IF NOT EQUAL (X(IDENT_INT (6)..IDENT_INT (7)),
                        AGGR (C2, C3))     OR
             NOT EQUAL (CREATE (1, 4, C4, X)(1..3),
                        AGGR (C4, C5, C6))           THEN
               FAILED ("INCORRECT SLICE (VALUE)");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED WHILE CHECKING SLICES");
     END;

     IF NOT (X IN T) OR AGGR (C1, C2) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT (AGGR (C1, C2) NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     B := FALSE;
     A (X'ADDRESS);
     IF NOT B THEN
          FAILED ("INCORRECT 'ADDRESS");
     END IF;

     IF T'FIRST /= 5 THEN
          FAILED ("INCORRECT TYPE'FIRST");
     END IF;

     IF X'FIRST /= 5 THEN
          FAILED ("INCORRECT OBJECT'FIRST");
     END IF;

     IF V'FIRST /= 5 THEN
          FAILED ("INCORRECT VALUE'FIRST");
     END IF;

     IF T'FIRST (N) /= 5 THEN
          FAILED ("INCORRECT TYPE'FIRST (N)");
     END IF;

     IF X'FIRST (N) /= 5 THEN
          FAILED ("INCORRECT OBJECT'FIRST (N)");
     END IF;

     IF V'FIRST (N) /= 5 THEN
          FAILED ("INCORRECT VALUE'FIRST (N)");
     END IF;

     IF T'LAST /= 7 THEN
          FAILED ("INCORRECT TYPE'LAST");
     END IF;

     IF X'LAST /= 7 THEN
          FAILED ("INCORRECT OBJECT'LAST");
     END IF;

     IF V'LAST /= 7 THEN
          FAILED ("INCORRECT VALUE'LAST");
     END IF;

     IF T'LAST (N) /= 7 THEN
          FAILED ("INCORRECT TYPE'LAST (N)");
     END IF;

     IF X'LAST (N) /= 7 THEN
          FAILED ("INCORRECT OBJECT'LAST (N)");
     END IF;

     IF V'LAST (N) /= 7 THEN
          FAILED ("INCORRECT VALUE'LAST (N)");
     END IF;

     IF T'LENGTH /= 3 THEN
          FAILED ("INCORRECT TYPE'LENGTH");
     END IF;

     IF X'LENGTH /= 3 THEN
          FAILED ("INCORRECT OBJECT'LENGTH");
     END IF;

     IF V'LENGTH /= 3 THEN
          FAILED ("INCORRECT VALUE'LENGTH");
     END IF;

     IF T'LENGTH (N) /= 3 THEN
          FAILED ("INCORRECT TYPE'LENGTH (N)");
     END IF;

     IF X'LENGTH (N) /= 3 THEN
          FAILED ("INCORRECT OBJECT'LENGTH (N)");
     END IF;

     IF V'LENGTH (N) /= 3 THEN
          FAILED ("INCORRECT VALUE'LENGTH (N)");
     END IF;

     DECLARE
          Y : PARENT (T'RANGE);
     BEGIN
          IF Y'FIRST /= 5 OR Y'LAST /= 7 THEN
               FAILED ("INCORRECT TYPE'RANGE");
          END IF;
     END;

     DECLARE
          Y : PARENT (X'RANGE);
     BEGIN
          IF Y'FIRST /= 5 OR Y'LAST /= 7 THEN
               FAILED ("INCORRECT OBJECT'RANGE");
          END IF;
     END;

     DECLARE
          Y : PARENT (V'RANGE);
     BEGIN
          IF Y'FIRST /= 5 OR Y'LAST /= 7 THEN
               FAILED ("INCORRECT VALUE'RANGE");
          END IF;
     END;

     DECLARE
          Y : PARENT (T'RANGE (N));
     BEGIN
          IF Y'FIRST /= 5 OR Y'LAST /= 7 THEN
               FAILED ("INCORRECT TYPE'RANGE (N)");
          END IF;
     END;

     DECLARE
          Y : PARENT (X'RANGE (N));
     BEGIN
          IF Y'FIRST /= 5 OR Y'LAST /= 7 THEN
               FAILED ("INCORRECT OBJECT'RANGE (N)");
          END IF;
     END;

     DECLARE
          Y : PARENT (V'RANGE (N));
     BEGIN
          IF Y'FIRST /= 5 OR Y'LAST /= 7 THEN
               FAILED ("INCORRECT VALUE'RANGE (N)");
          END IF;
     END;

     IF X'SIZE < T'SIZE THEN
          COMMENT ("X'SIZE < T'SIZE");
     ELSIF X'SIZE = T'SIZE THEN
          COMMENT ("X'SIZE = T'SIZE");
     ELSE
          COMMENT ("X'SIZE > T'SIZE");
     END IF;

     RESULT;
END C34005P;
