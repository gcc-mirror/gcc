-- C34005S.ADA

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
--     COMPONENT TYPE IS A LIMITED TYPE.  THIS TEST IS PART 1 OF 2
--     TESTS WHICH COVER THE OBJECTIVE.  THE SECOND PART IS IN TEST
--     C34005V.

-- HISTORY:
--     JRK 08/20/87  CREATED ORIGINAL TEST.
--     BCB 04/12/90  SPLIT ORIGINAL TEST INTO C34005S.ADA AND
--                   C34005V.ADA
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34005S IS

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

     END PKG_P;

     USE PKG_P;

     TYPE T IS NEW PARENT (IDENT_INT (4) .. IDENT_INT (5),
                           IDENT_INT (6) .. IDENT_INT (8));

     TYPE ARRT IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <>) OF
                        COMPONENT;

     SUBTYPE ARR IS ARRT (8 .. 9, 2 .. 4);

     X : T;
     W : PARENT (4 .. 5, 6 .. 8);
     C : COMPONENT;
     B : BOOLEAN := FALSE;
     U : ARR;
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

     END PKG_P;

     FUNCTION EQUAL (X, Y : ARRT) RETURN BOOLEAN IS
     BEGIN
          IF X'LENGTH /= Y'LENGTH OR X'LENGTH(2) /= Y'LENGTH(2) THEN
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

BEGIN
     TEST ("C34005S", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
                      "TYPE IS A LIMITED TYPE.    THIS TEST IS PART " &
                      "1 OF 2 TESTS WHICH COVER THE OBJECTIVE.  THE " &
                      "SECOND PART IS IN TEST C34005V");

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

     ASSIGN (U (8, 2), CREATE (1));
     ASSIGN (U (8, 3), CREATE (2));
     ASSIGN (U (8, 4), CREATE (3));
     ASSIGN (U (9, 2), CREATE (4));
     ASSIGN (U (9, 3), CREATE (5));
     ASSIGN (U (9, 4), CREATE (6));

     IF NOT EQUAL (X (IDENT_INT (4), IDENT_INT (6)), C1) OR
        NOT EQUAL (CREATE (6, 9, 2, 3, C4, X) (9, 3), C11) THEN
          FAILED ("INCORRECT INDEX (VALUE)");
     END IF;

     B := FALSE;
     A (X'ADDRESS);
     IF NOT B THEN
          FAILED ("INCORRECT 'ADDRESS");
     END IF;

     IF T'FIRST /= 4 THEN
          FAILED ("INCORRECT TYPE'FIRST");
     END IF;

     IF X'FIRST /= 4 THEN
          FAILED ("INCORRECT OBJECT'FIRST");
     END IF;

     IF V'FIRST /= 4 THEN
          FAILED ("INCORRECT VALUE'FIRST");
     END IF;

     IF T'FIRST (N) /= 6 THEN
          FAILED ("INCORRECT TYPE'FIRST (N)");
     END IF;

     IF X'FIRST (N) /= 6 THEN
          FAILED ("INCORRECT OBJECT'FIRST (N)");
     END IF;

     IF V'FIRST (N) /= 6 THEN
          FAILED ("INCORRECT VALUE'FIRST (N)");
     END IF;

     IF T'LAST /= 5 THEN
          FAILED ("INCORRECT TYPE'LAST");
     END IF;

     IF X'LAST /= 5 THEN
          FAILED ("INCORRECT OBJECT'LAST");
     END IF;

     IF V'LAST /= 5 THEN
          FAILED ("INCORRECT VALUE'LAST");
     END IF;

     IF T'LAST (N) /= 8 THEN
          FAILED ("INCORRECT TYPE'LAST (N)");
     END IF;

     IF X'LAST (N) /= 8 THEN
          FAILED ("INCORRECT OBJECT'LAST (N)");
     END IF;

     IF V'LAST (N) /= 8 THEN
          FAILED ("INCORRECT VALUE'LAST (N)");
     END IF;

     IF T'LENGTH /= 2 THEN
          FAILED ("INCORRECT TYPE'LENGTH");
     END IF;

     IF X'LENGTH /= 2 THEN
          FAILED ("INCORRECT OBJECT'LENGTH");
     END IF;

     IF V'LENGTH /= 2 THEN
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
          Y : PARENT (T'RANGE, 1 .. 3);
     BEGIN
          IF Y'FIRST /= 4 OR Y'LAST /= 5 THEN
               FAILED ("INCORRECT TYPE'RANGE");
          END IF;
     END;

     DECLARE
          Y : PARENT (X'RANGE, 1 .. 3);
     BEGIN
          IF Y'FIRST /= 4 OR Y'LAST /= 5 THEN
               FAILED ("INCORRECT OBJECT'RANGE");
          END IF;
     END;

     DECLARE
          Y : PARENT (V'RANGE, 1 .. 3);
     BEGIN
          IF Y'FIRST /= 4 OR Y'LAST /= 5 THEN
               FAILED ("INCORRECT VALUE'RANGE");
          END IF;
     END;

     DECLARE
          Y : PARENT (1 .. 2, T'RANGE (N));
     BEGIN
          IF Y'FIRST (N) /= 6 OR Y'LAST (N) /= 8 THEN
               FAILED ("INCORRECT TYPE'RANGE (N)");
          END IF;
     END;

     DECLARE
          Y : PARENT (1 .. 2, X'RANGE (N));
     BEGIN
          IF Y'FIRST (N) /= 6 OR Y'LAST (N) /= 8 THEN
               FAILED ("INCORRECT OBJECT'RANGE (N)");
          END IF;
     END;

     DECLARE
          Y : PARENT (1 .. 2, V'RANGE (N));
     BEGIN
          IF Y'FIRST (N) /= 6 OR Y'LAST (N) /= 8 THEN
               FAILED ("INCORRECT VALUE'RANGE (N)");
          END IF;
     END;

     IF T'SIZE < T'LENGTH * T'LENGTH (N) * COMPONENT'SIZE THEN
          FAILED ("INCORRECT TYPE'SIZE");
     END IF;

     IF X'SIZE < X'LENGTH * X'LENGTH (N) * COMPONENT'SIZE THEN
          FAILED ("INCORRECT OBJECT'SIZE");
     END IF;

     RESULT;
END C34005S;
