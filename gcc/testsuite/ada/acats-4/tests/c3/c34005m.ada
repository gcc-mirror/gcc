-- C34005M.ADA

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
--     COMPONENT TYPE IS A NON-LIMITED TYPE.

-- HISTORY:
--     JRK 9/17/86  CREATED ORIGINAL TEST.
--     PWN 11/30/94 REMOVED 'BASE USE ILLEGAL IN ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34005M IS

     SUBTYPE COMPONENT IS INTEGER;

     PACKAGE PKG IS

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

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT (IDENT_INT (4) .. IDENT_INT (5),
                           IDENT_INT (6) .. IDENT_INT (8));

     TYPE ARRT IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <>) OF
                        COMPONENT;

     SUBTYPE ARR IS ARRT (8 .. 9, 2 .. 4);

     X : T                       := (OTHERS => (OTHERS => 2));
     W : PARENT (4 .. 5, 6 .. 8) := (OTHERS => (OTHERS => 2));
     C : COMPONENT               := 1;
     B : BOOLEAN                 := FALSE;
     U : ARR                     := (OTHERS => (OTHERS => C));
     N : CONSTANT                := 2;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     FUNCTION V RETURN T IS
     BEGIN
          RETURN (OTHERS => (OTHERS => C));
     END V;

     PACKAGE BODY PKG IS

          FUNCTION CREATE
             ( F1, L1 : INDEX;
               F2, L2 : INDEX;
               C      : COMPONENT;
               DUMMY  : PARENT
             ) RETURN PARENT
          IS
               A : PARENT (F1 .. L1, F2 .. L2);
               B : COMPONENT := C;
          BEGIN
               FOR I IN F1 .. L1 LOOP
                    FOR J IN F2 .. L2 LOOP
                         A (I, J) := B;
                         B := B + 1;
                    END LOOP;
               END LOOP;
               RETURN A;
          END CREATE;

     END PKG;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF EQUAL (X'LENGTH, X'LENGTH) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN (OTHERS => (OTHERS => -1));
     END IDENT;

BEGIN
     TEST ("C34005M", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
                      "TYPE IS A NON-LIMITED TYPE");

     X := IDENT (((1, 2, 3), (4, 5, 6)));
     IF X /= ((1, 2, 3), (4, 5, 6)) THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= ((1, 2, 3), (4, 5, 6)) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= ((1, 2, 3), (4, 5, 6)) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := ((1, 2, 3), (4, 5, 6));
     END IF;
     IF T (W) /= ((1, 2, 3), (4, 5, 6)) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     BEGIN
          IF PARENT (X) /= ((1, 2, 3), (4, 5, 6)) OR
             PARENT (CREATE (6, 9, 2, 3, 4, X)) /=
             ((4, 5), (6, 7), (8, 9), (10, 11)) THEN
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 1");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 1");
     END;

     IF EQUAL (3, 3) THEN
          U := ((1, 2, 3), (4, 5, 6));
     END IF;
     IF T (U) /= ((1, 2, 3), (4, 5, 6)) THEN
          FAILED ("INCORRECT CONVERSION FROM ARRAY");
     END IF;

     BEGIN
          IF ARR (X) /= ((1, 2, 3), (4, 5, 6)) OR
             ARRT (CREATE (7, 9, 2, 5, 3, X)) /=
             ((3, 4, 5, 6), (7, 8, 9, 10), (11, 12, 13, 14)) THEN
               FAILED ("INCORRECT CONVERSION TO ARRAY");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 2");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 2");
     END;

     IF IDENT (((1, 2, 3), (4, 5, 6))) /= ((1, 2, 3), (4, 5, 6)) OR
        X = ((1, 2), (3, 4), (5, 6)) THEN
          FAILED ("INCORRECT AGGREGATE");
     END IF;

     BEGIN
          IF X (IDENT_INT (4), IDENT_INT (6)) /= 1 OR
             CREATE (6, 9, 2, 3, 4, X) (9, 3) /= 11 THEN
               FAILED ("INCORRECT INDEX (VALUE)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 3");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 3");
     END;

     X (IDENT_INT (5), IDENT_INT (8)) := 7;
     IF X /= ((1, 2, 3), (4, 5, 7)) THEN
          FAILED ("INCORRECT INDEX (ASSIGNMENT)");
     END IF;

     X := IDENT (((1, 2, 3), (4, 5, 6)));
     IF X = IDENT (((1, 2, 3), (4, 5, 7))) OR
        X = ((1, 2), (4, 5)) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= IDENT (((1, 2, 3), (4, 5, 6))) OR
        NOT (X /= ((1, 2, 3), (4, 5, 6), (7, 8, 9))) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF NOT (X IN T) OR ((1, 2), (3, 4)) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR
        NOT (((1, 2, 3), (4, 5, 6), (7, 8, 9)) NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
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
END C34005M;
