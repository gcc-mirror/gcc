-- C34006G.ADA

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
--     (IMPLICITLY) FOR DERIVED RECORD TYPES WITHOUT DISCRIMINANTS AND
--     WITH A LIMITED COMPONENT TYPE.

-- HISTORY:
--     JRK 08/24/87  CREATED ORIGINAL TEST.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34006G IS

     PACKAGE PKG_L IS

          TYPE LP IS LIMITED PRIVATE;

          FUNCTION CREATE (X : INTEGER) RETURN LP;

          FUNCTION EQUAL (X, Y : LP) RETURN BOOLEAN;

          PROCEDURE ASSIGN (X : OUT LP; Y : LP);

          C1 : CONSTANT LP;

     PRIVATE

          TYPE LP IS NEW INTEGER;

          C1 : CONSTANT LP := 1;

     END PKG_L;

     USE PKG_L;

     SUBTYPE COMPONENT IS LP;

     PACKAGE PKG_P IS

          TYPE PARENT IS
               RECORD
                    C : COMPONENT;
                    B : BOOLEAN := TRUE;
               END RECORD;

          FUNCTION EQUAL (X, Y : PARENT) RETURN BOOLEAN;

          FUNCTION AGGR (C : COMPONENT; B : BOOLEAN) RETURN PARENT;

     END PKG_P;

     USE PKG_P;

     TYPE T IS NEW PARENT;

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

          FUNCTION EQUAL (X, Y : PARENT) RETURN BOOLEAN IS
          BEGIN
               RETURN EQUAL (X.C, Y.C) AND X.B = Y.B;
          END EQUAL;

          FUNCTION AGGR (C : COMPONENT; B : BOOLEAN) RETURN PARENT IS
               RESULT : PARENT;
          BEGIN
               ASSIGN (RESULT.C, C);
               RESULT.B := B;
               RETURN RESULT;
          END AGGR;

     END PKG_P;

BEGIN
     TEST ("C34006G", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "RECORD TYPES WITHOUT DISCRIMINANTS AND WITH A " &
                      "LIMITED COMPONENT TYPE");

     ASSIGN (X.C, CREATE (1));
     X.B := IDENT_BOOL (TRUE);

     ASSIGN (W.C, CREATE (1));
     W.B := IDENT_BOOL (TRUE);

     IF NOT EQUAL (T'(X), AGGR (C1, TRUE)) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF NOT EQUAL (T (X), AGGR (C1, TRUE)) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF NOT EQUAL (T (W), AGGR (C1, TRUE)) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF NOT EQUAL (PARENT (X), AGGR (C1, TRUE)) THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     IF NOT EQUAL (X.C, C1) OR X.B /= TRUE THEN
          FAILED ("INCORRECT SELECTION (VALUE)");
     END IF;

     X.B := IDENT_BOOL (FALSE);
     IF NOT EQUAL (X, AGGR (C1, FALSE)) THEN
          FAILED ("INCORRECT SELECTION (ASSIGNMENT)");
     END IF;

     X.B := IDENT_BOOL (TRUE);
     IF NOT (X IN T) THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     B := FALSE;
     A (X'ADDRESS);
     IF NOT B THEN
          FAILED ("INCORRECT 'ADDRESS");
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

     IF X'SIZE   < T'SIZE OR
        X.C'SIZE < COMPONENT'SIZE OR
        X.B'SIZE < BOOLEAN'SIZE THEN
          FAILED ("INCORRECT OBJECT'SIZE");
     END IF;

     RESULT;
END C34006G;
