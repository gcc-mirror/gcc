-- C34005J.ADA

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
--     WHOSE COMPONENT TYPE IS A BOOLEAN TYPE.

-- HISTORY:
--     JRK 9/16/86  CREATED ORIGINAL TEST.
--     RJW 8/21/89  MODIFIED CHECKS FOR TYPE AND OBJECT SIZES.
--     PWN 11/30/94 REMOVED 'BASE USE ILLEGAL IN ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34005J IS

     SUBTYPE COMPONENT IS BOOLEAN;

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

     X : T               := (OTHERS => TRUE);
     W : PARENT (5 .. 7) := (OTHERS => TRUE);
     C : COMPONENT       := FALSE;
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
                    B := NOT B;
               END LOOP;
               RETURN A;
          END CREATE;

     END PKG;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF EQUAL (X'LENGTH, X'LENGTH) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN (OTHERS => FALSE);
     END IDENT;

BEGIN
     TEST ("C34005J", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
                      "TYPE IS A BOOLEAN TYPE");

     X := IDENT ((TRUE, FALSE, TRUE));
     IF X /= (TRUE, FALSE, TRUE) THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= (TRUE, FALSE, TRUE) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= (TRUE, FALSE, TRUE) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := (TRUE, FALSE, TRUE);
     END IF;
     IF T (W) /= (TRUE, FALSE, TRUE) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     BEGIN
          IF PARENT (X) /= (TRUE, FALSE, TRUE) OR
             PARENT (CREATE (2, 3, FALSE, X)) /= (FALSE, TRUE) THEN
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 1");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 1");
     END;

     IF EQUAL (3, 3) THEN
          U := (TRUE, FALSE, TRUE);
     END IF;
     IF T (U) /= (TRUE, FALSE, TRUE) THEN
          FAILED ("INCORRECT CONVERSION FROM ARRAY");
     END IF;

     BEGIN
          IF ARR (X) /= (TRUE, FALSE, TRUE) OR
             ARRT (CREATE (1, 2, TRUE, X)) /= (TRUE, FALSE) THEN
               FAILED ("INCORRECT CONVERSION TO ARRAY");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 2");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 2");
     END;

     IF IDENT ((TRUE, FALSE, TRUE)) /= (TRUE, FALSE, TRUE) OR
        X = (TRUE, FALSE) THEN
          FAILED ("INCORRECT AGGREGATE");
     END IF;

     BEGIN
          IF X (IDENT_INT (5)) /= TRUE OR
             CREATE (2, 3, FALSE, X) (3) /= TRUE THEN
               FAILED ("INCORRECT INDEX (VALUE)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 3");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 3");
     END;

     X (IDENT_INT (7)) := FALSE;
     IF X /= (TRUE, FALSE, FALSE) THEN
          FAILED ("INCORRECT INDEX (ASSIGNMENT)");
     END IF;

     BEGIN
          X := IDENT ((TRUE, FALSE, TRUE));
          IF X (IDENT_INT (6) .. IDENT_INT (7)) /= (FALSE, TRUE) OR
             CREATE (1, 4, FALSE, X) (1 .. 3) /=
             (FALSE, TRUE, FALSE) THEN
               FAILED ("INCORRECT SLICE (VALUE)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 4");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 4");
     END;

     X (IDENT_INT (5) .. IDENT_INT (6)) := (FALSE, TRUE);
     IF X /= (FALSE, TRUE, TRUE) THEN
          FAILED ("INCORRECT SLICE (ASSIGNMENT)");
     END IF;

     BEGIN
          X := IDENT ((TRUE, FALSE, TRUE));
          IF NOT X /= (FALSE, TRUE, FALSE) OR
             NOT CREATE (2, 3, FALSE, X) /= (TRUE, FALSE) THEN
               FAILED ("INCORRECT ""NOT""");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 5");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 5");
     END;

     BEGIN
          IF (X AND IDENT ((TRUE, TRUE, FALSE))) /=
             (TRUE, FALSE, FALSE) OR
             (CREATE (1, 4, FALSE, X) AND
             (FALSE, FALSE, TRUE, TRUE)) /=
             (FALSE, FALSE, FALSE, TRUE) THEN
               FAILED ("INCORRECT ""AND""");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 6");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 6");
     END;

     BEGIN
          IF (X OR IDENT ((TRUE, FALSE, FALSE))) /=
             (TRUE, FALSE, TRUE) OR
             (CREATE (1, 4, FALSE, X) OR (FALSE, FALSE, TRUE, TRUE)) /=
             (FALSE, TRUE, TRUE, TRUE) THEN
               FAILED ("INCORRECT ""OR""");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 7");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 7");
     END;

     BEGIN
          IF (X XOR IDENT ((TRUE, TRUE, FALSE))) /=
             (FALSE, TRUE, TRUE) OR
             (CREATE (1, 4, FALSE, X) XOR
             (FALSE, FALSE, TRUE, TRUE)) /=
             (FALSE, TRUE, TRUE, FALSE) THEN
               FAILED ("INCORRECT ""XOR""");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 8");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 8");
     END;

     IF X = IDENT ((TRUE, FALSE, FALSE)) OR X = (TRUE, FALSE) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= IDENT ((TRUE, FALSE, TRUE)) OR
        NOT (X /= (FALSE, TRUE)) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF X < IDENT ((TRUE, FALSE, TRUE)) OR X < (TRUE, FALSE) THEN
          FAILED ("INCORRECT <");
     END IF;

     IF X > IDENT ((TRUE, FALSE, TRUE)) OR X > (TRUE, TRUE) THEN
          FAILED ("INCORRECT >");
     END IF;

     IF X <= IDENT ((TRUE, FALSE, FALSE)) OR
        X <= (TRUE, FALSE, FALSE, TRUE) THEN
          FAILED ("INCORRECT <=");
     END IF;

     IF X >= IDENT ((TRUE, TRUE, FALSE)) OR
        X >= (TRUE, FALSE, TRUE, FALSE) THEN
          FAILED ("INCORRECT >=");
     END IF;

     IF NOT (X IN T) OR (TRUE, FALSE) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT ((TRUE, FALSE) NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     BEGIN
          IF X & (FALSE, TRUE, FALSE) /=
             (TRUE, FALSE, TRUE, FALSE, TRUE, FALSE) OR
             CREATE (2, 3, FALSE, X) & (FALSE, TRUE) /=
             (FALSE, TRUE, FALSE, TRUE) THEN
               FAILED ("INCORRECT & (ARRAY, ARRAY)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 9");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 9");
     END;

     BEGIN
          IF X & FALSE /= (TRUE, FALSE, TRUE, FALSE) OR
             CREATE (2, 3, FALSE, X) & FALSE /=
             (FALSE, TRUE, FALSE) THEN
               FAILED ("INCORRECT & (ARRAY, COMPONENT)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 10");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 10");
     END;

     BEGIN
          IF FALSE & X /= (FALSE, TRUE, FALSE, TRUE) OR
             FALSE & CREATE (2, 3, TRUE, X) /=
             (FALSE, TRUE, FALSE) THEN
               FAILED ("INCORRECT & (COMPONENT, ARRAY)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 11");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 11");
     END;

     IF EQUAL (3, 3) THEN
          C := FALSE;
     END IF;

     BEGIN
          IF C & TRUE /= CREATE (2, 3, FALSE, X) THEN
               FAILED ("INCORRECT & (COMPONENT, COMPONENT)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 12");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 12");
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

     RESULT;
END C34005J;
