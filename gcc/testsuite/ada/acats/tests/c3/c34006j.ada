-- C34006J.ADA

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
--     A LIMITED COMPONENT TYPE.

-- HISTORY:
--     JRK 08/25/87  CREATED ORIGINAL TEST.
--     VCL 06/28/88  MODIFIED THE STATEMENTS INVOLVING THE 'SIZE
--                   ATTRIBUTE TO REMOVE ANY ASSUMPTIONS ABOUT THE
--                   SIZES.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34006J IS

     PACKAGE PKG_L IS

          TYPE LP IS LIMITED PRIVATE;

          FUNCTION CREATE (X : INTEGER) RETURN LP;

          FUNCTION EQUAL (X, Y : LP) RETURN BOOLEAN;

          PROCEDURE ASSIGN (X : OUT LP; Y : LP);

          C4 : CONSTANT LP;
          C5 : CONSTANT LP;

     PRIVATE

          TYPE LP IS NEW INTEGER;

          C4 : CONSTANT LP := 4;
          C5 : CONSTANT LP := 5;

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

     X : T;
     W : PARENT;
     B : BOOLEAN := FALSE;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

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

BEGIN
     TEST ("C34006J", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "RECORD TYPES WITH DISCRIMINANTS AND WITH A " &
                      "LIMITED COMPONENT TYPE");

     X.I := IDENT_INT (1);
     X.S := IDENT_STR ("ABC");
     ASSIGN (X.C, CREATE (4));

     W.I := IDENT_INT (1);
     W.S := IDENT_STR ("ABC");
     ASSIGN (W.C, CREATE (4));

     IF NOT EQUAL (T'(X), AGGR (TRUE, 3, 1, "ABC", C4)) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF NOT EQUAL (T(X), AGGR (TRUE, 3, 1, "ABC", C4)) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF NOT EQUAL (T(W), AGGR (TRUE, 3, 1, "ABC", C4)) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF NOT EQUAL (PARENT(X), AGGR (TRUE, 3, 1, "ABC", C4))   OR
        NOT EQUAL (PARENT(CREATE (FALSE, 2, 3, "XX", C5, 6.0, X)),
                   AGGR (FALSE, 2, 3, 6.0))   THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     IF X.B /= TRUE OR X.L /= 3 OR
        CREATE (FALSE, 2, 3, "XX", C5, 6.0, X).B /= FALSE OR
        CREATE (FALSE, 2, 3, "XX", C5, 6.0, X).L /= 2 THEN
          FAILED ("INCORRECT SELECTION (DISCRIMINANT)");
     END IF;

     IF X.I /= 1 OR X.S /= "ABC" OR NOT EQUAL (X.C, C4) OR
        CREATE (FALSE, 2, 3, "XX", C5, 6.0, X).I /= 3 OR
        CREATE (FALSE, 2, 3, "XX", C5, 6.0, X).F /= 6.0 THEN
          FAILED ("INCORRECT SELECTION (VALUE)");
     END IF;

     X.I := IDENT_INT (7);
     X.S := IDENT_STR ("XYZ");
     IF NOT EQUAL (X, AGGR (TRUE, 3, 7, "XYZ", C4)) THEN
          FAILED ("INCORRECT SELECTION (ASSIGNMENT)");
     END IF;

     X.I := IDENT_INT (1);
     X.S := IDENT_STR ("ABC");
     IF NOT (X IN T) OR AGGR (FALSE, 2, 3, 6.0) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT (AGGR (FALSE, 2, 3, 6.0) NOT IN T) THEN
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

     IF X'SIZE < T'SIZE THEN
          COMMENT ("X'SIZE < T'SIZE");
     ELSIF X'SIZE = T'SIZE THEN
          COMMENT ("X'SIZE = T'SIZE");
     ELSE
          COMMENT ("X'SIZE > T'SIZE");
     END IF;

     RESULT;
EXCEPTION
     WHEN OTHERS =>
          FAILED ("UNEXPECTED EXCEPTION RAISED WHILE CHECKING BASIC " &
                  "OPERATIONS");
          RESULT;
END C34006J;
