-- C34005D.ADA

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
--     (IMPLICITLY) FOR DERIVED ONE-DIMENSIONAL ARRAY TYPES
--     WHOSE COMPONENT TYPE IS A DISCRETE TYPE.

-- HISTORY:
--     JRK 9/12/86  CREATED ORIGINAL TEST.
--     PWN 11/30/94 REMOVED 'BASE USE ILLEGAL IN ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34005D IS

     SUBTYPE COMPONENT IS INTEGER;

     PACKAGE PKG IS

          FIRST : CONSTANT := 0;
          LAST  : CONSTANT := 100;

          SUBTYPE INDEX IS INTEGER RANGE FIRST .. LAST;

          TYPE PARENT IS ARRAY (INDEX RANGE <>) OF COMPONENT;

          FUNCTION CREATE ( F, L  : INDEX;
                            C     : COMPONENT;
                            DUMMY : PARENT   -- TO RESOLVE OVERLOADING.
                          ) RETURN PARENT;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT (IDENT_INT (5) .. IDENT_INT (7));

     TYPE ARRT IS ARRAY (INTEGER RANGE <>) OF COMPONENT;
     SUBTYPE ARR IS ARRT (2 .. 4);

     X : T               := (OTHERS => 2);
     W : PARENT (5 .. 7) := (OTHERS => 2);
     C : COMPONENT       := 1;
     B : BOOLEAN         := FALSE;
     U : ARR             := (OTHERS => C);
     N : CONSTANT        := 1;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     FUNCTION V RETURN T IS
     BEGIN
          RETURN (OTHERS => C);
     END V;

     PACKAGE BODY PKG IS

          FUNCTION CREATE
             ( F, L  : INDEX;
               C     : COMPONENT;
               DUMMY : PARENT
             ) RETURN PARENT
          IS
               A : PARENT (F .. L);
               B : COMPONENT := C;
          BEGIN
               FOR I IN F .. L LOOP
                    A (I) := B;
                    B := B + 1;
               END LOOP;
               RETURN A;
          END CREATE;

     END PKG;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF EQUAL (X'LENGTH, X'LENGTH) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN (OTHERS => -1);
     END IDENT;

BEGIN
     TEST ("C34005D", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
                      "TYPE IS A DISCRETE TYPE");

     X := IDENT ((1, 2, 3));
     IF X /= (1, 2, 3) THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= (1, 2, 3) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= (1, 2, 3) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := (1, 2, 3);
     END IF;
     IF T (W) /= (1, 2, 3) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     BEGIN
          IF PARENT (X) /= (1, 2, 3) OR
             PARENT (CREATE (2, 3, 4, X)) /= (4, 5) THEN
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 1");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 1");
     END;

     IF EQUAL (3, 3) THEN
          U := (1, 2, 3);
     END IF;
     IF T (U) /= (1, 2, 3) THEN
          FAILED ("INCORRECT CONVERSION FROM ARRAY");
     END IF;

     BEGIN
          IF ARR (X) /= (1, 2, 3) OR
             ARRT (CREATE (1, 2, 3, X)) /= (3, 4) THEN
               FAILED ("INCORRECT CONVERSION TO ARRAY");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 2");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 2");
     END;

     IF IDENT ((1, 2, 3)) /= (1, 2, 3) OR
        X = (1, 2) THEN
          FAILED ("INCORRECT AGGREGATE");
     END IF;

     BEGIN
          IF X (IDENT_INT (5)) /= 1 OR
             CREATE (2, 3, 4, X) (3) /= 5 THEN
               FAILED ("INCORRECT INDEX (VALUE)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 3");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 3");
     END;

     X (IDENT_INT (7)) := 4;
     IF X /= (1, 2, 4) THEN
          FAILED ("INCORRECT INDEX (ASSIGNMENT)");
     END IF;

     BEGIN
          X := IDENT ((1, 2, 3));
          IF X (IDENT_INT (6) .. IDENT_INT (7)) /= (2, 3) OR
             CREATE (1, 4, 4, X) (1 .. 3) /= (4, 5, 6) THEN
               FAILED ("INCORRECT SLICE (VALUE)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 4");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 4");
     END;

     X (IDENT_INT (5) .. IDENT_INT (6)) := (4, 5);
     IF X /= (4, 5, 3) THEN
          FAILED ("INCORRECT SLICE (ASSIGNMENT)");
     END IF;

     X := IDENT ((1, 2, 3));
     IF X = IDENT ((1, 2, 4)) OR X = (1, 2) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= IDENT ((1, 2, 3)) OR NOT (X /= (2, 3)) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF X < IDENT ((1, 2, 3)) OR X < (1, 2) THEN
          FAILED ("INCORRECT <");
     END IF;

     IF X > IDENT ((1, 2, 3)) OR X > (1, 3) THEN
          FAILED ("INCORRECT >");
     END IF;

     IF X <= IDENT ((1, 2, 2)) OR X <= (1, 2, 2, 4) THEN
          FAILED ("INCORRECT <=");
     END IF;

     IF X >= IDENT ((1, 2, 4)) OR X >= (1, 2, 3, 1) THEN
          FAILED ("INCORRECT >=");
     END IF;

     IF NOT (X IN T) OR (1, 2) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT ((1, 2) NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     BEGIN
          IF X & (4, 5, 6) /= (1, 2, 3, 4, 5, 6) OR
             CREATE (2, 3, 2, X) & (4, 5) /= (2, 3, 4, 5) THEN
               FAILED ("INCORRECT & (ARRAY, ARRAY)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 5");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 5");
     END;

     BEGIN
          IF X & 4 /= (1, 2, 3, 4) OR
             CREATE (2, 3, 2, X) & 4 /= (2, 3, 4) THEN
               FAILED ("INCORRECT & (ARRAY, COMPONENT)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 6");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 6");
     END;

     BEGIN
          IF 4 & X /= (4, 1, 2, 3) OR
             2 & CREATE (2, 3, 3, X) /= (2, 3, 4) THEN
               FAILED ("INCORRECT & (COMPONENT, ARRAY)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 7");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 7");
     END;

     IF EQUAL (3, 3) THEN
          C := 2;
     END IF;

     BEGIN
          IF C & 3 /= CREATE (2, 3, 2, X) THEN
               FAILED ("INCORRECT & (COMPONENT, COMPONENT)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 8");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 8");
     END;

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

     IF T'SIZE < T'LENGTH * COMPONENT'SIZE THEN
          FAILED ("INCORRECT TYPE'SIZE");
     END IF;

     IF X'SIZE < X'LENGTH * COMPONENT'SIZE THEN
          FAILED ("INCORRECT OBJECT'SIZE");
     END IF;

     RESULT;
END C34005D;
