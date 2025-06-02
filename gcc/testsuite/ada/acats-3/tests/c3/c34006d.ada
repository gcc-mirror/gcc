-- C34006D.ADA

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
--     (IMPLICITLY) FOR DERIVED RECORD TYPES WITH DISCRIMINANTS AND WITH
--     NON-LIMITED COMPONENT TYPES.

-- HISTORY:
--     JRK 09/22/86  CREATED ORIGINAL TEST.
--     BCB 11/13/87  CHANGED TEST SO AN OBJECT'S SIZE MAY BE LESS THAN
--                   THAT OF ITS TYPE.
--     RJW 08/21/89  MODIFIED CHECKS FOR SIZE.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34006D IS

     SUBTYPE COMPONENT IS INTEGER;

     PACKAGE PKG IS

          MAX_LEN : CONSTANT := 10;

          SUBTYPE LENGTH IS NATURAL RANGE 0 .. MAX_LEN;

          TYPE PARENT (B : BOOLEAN := TRUE; L : LENGTH := 1) IS
               RECORD
                    I : INTEGER;
                    CASE B IS
                         WHEN TRUE =>
                              S : STRING (1 .. L);
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

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT (IDENT_BOOL (TRUE), IDENT_INT (3));

     X : T         := (TRUE, 3, 2, "AAA", 2);
     W : PARENT    := (TRUE, 3, 2, "AAA", 2);
     C : COMPONENT := 1;
     B : BOOLEAN   := FALSE;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     PACKAGE BODY PKG IS

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
          BEGIN
               CASE B IS
                    WHEN TRUE =>
                         RETURN (TRUE, L, I, S, C);
                    WHEN FALSE =>
                         RETURN (FALSE, L, I, F);
               END CASE;
          END CREATE;

     END PKG;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF EQUAL (X.I, X.I) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN (TRUE, 3, -1, "---", -1);
     END IDENT;

BEGIN
     TEST ("C34006D", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "RECORD TYPES WITH DISCRIMINANTS AND WITH " &
                      "NON-LIMITED COMPONENT TYPES");

     X := IDENT ((TRUE, 3, 1, "ABC", 4));
     IF X /= (TRUE, 3, 1, "ABC", 4) THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= (TRUE, 3, 1, "ABC", 4) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= (TRUE, 3, 1, "ABC", 4) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := (TRUE, 3, 1, "ABC", 4);
     END IF;
     IF T (W) /= (TRUE, 3, 1, "ABC", 4) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     BEGIN
          IF PARENT (X) /= (TRUE, 3, 1, "ABC", 4) OR
             PARENT (CREATE (FALSE, 2, 3, "XX", 5, 6.0, X)) /=
             (FALSE, 2, 3, 6.0) THEN
               FAILED ("INCORRECT CONVERSION TO PARENT");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 1");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 1");
     END;

     IF IDENT ((TRUE, 3, 1, "ABC", 4)) /= (TRUE, 3, 1, "ABC", 4) OR
        X = (FALSE, 3, 1, 4.0) THEN
          FAILED ("INCORRECT AGGREGATE");
     END IF;

     BEGIN
          IF X.B /= TRUE OR X.L /= 3 OR
             CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . B /= FALSE OR
             CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . L /= 2 THEN
               FAILED ("INCORRECT SELECTION (DISCRIMINANT)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 2");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 2");
     END;

     BEGIN
          IF X.I /= 1 OR X.S /= "ABC" OR X.C /= 4 OR
             CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . I /= 3 OR
             CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . F /= 6.0 THEN
               FAILED ("INCORRECT SELECTION (VALUE)");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 3");
          WHEN OTHERS =>
               FAILED ("CALL TO CREATE RAISED EXCEPTION - 3");
     END;

     X.I := IDENT_INT (7);
     X.S := IDENT_STR ("XYZ");
     X.C := IDENT_INT (9);
     IF X /= (TRUE, 3, 7, "XYZ", 9) THEN
          FAILED ("INCORRECT SELECTION (ASSIGNMENT)");
     END IF;

     X := IDENT ((TRUE, 3, 1, "ABC", 4));
     IF X = IDENT ((TRUE, 3, 1, "ABC", 5)) OR
        X = (FALSE, 2, 3, 6.0) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= IDENT ((TRUE, 3, 1, "ABC", 4)) OR
        NOT (X /= (FALSE, 2, 3, 6.0)) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF NOT (X IN T) OR (FALSE, 2, 3, 6.0) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT ((FALSE, 2, 3, 6.0) NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     B := FALSE;
     A (X'ADDRESS);
     IF NOT B THEN
          FAILED ("INCORRECT 'ADDRESS");
     END IF;

     IF NOT X'CONSTRAINED THEN
          FAILED ("INCORRECT 'CONSTRAINED");
     END IF;

     IF X.C'FIRST_BIT < 0 THEN
          FAILED ("INCORRECT 'FIRST_BIT");
     END IF;

     IF X.C'LAST_BIT < 0 OR
        X.C'LAST_BIT - X.C'FIRST_BIT + 1 /= X.C'SIZE THEN
          FAILED ("INCORRECT 'LAST_BIT");
     END IF;

     IF X.C'POSITION < 0 THEN
          FAILED ("INCORRECT 'POSITION");
     END IF;

     RESULT;
END C34006D;
