-- C34001D.ADA

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
-- CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
-- (IMPLICITLY) FOR DERIVED BOOLEAN TYPES.

-- JRK 8/20/86

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34001D IS

     SUBTYPE PARENT IS BOOLEAN;

     SUBTYPE SUBPARENT IS PARENT RANGE
               PARENT'VAL (IDENT_INT (PARENT'POS (FALSE))) ..
               PARENT'VAL (IDENT_INT (PARENT'POS (TRUE)));

     TYPE T IS NEW SUBPARENT RANGE
               PARENT'VAL (IDENT_INT (PARENT'POS (TRUE))) ..
               PARENT'VAL (IDENT_INT (PARENT'POS (TRUE)));

     X : T       := TRUE;
     W : PARENT  := FALSE;
     B : BOOLEAN := FALSE;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF EQUAL (T'POS (X), T'POS (X)) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN T'FIRST;
     END IDENT;

BEGIN
     TEST ("C34001D", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "BOOLEAN TYPES");

     X := IDENT (TRUE);
     IF X /= TRUE THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= TRUE THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= TRUE THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := TRUE;
     END IF;
     IF T (W) /= TRUE THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF PARENT (X) /= TRUE OR PARENT (T'VAL (0)) /= FALSE THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     IF IDENT (TRUE) /= TRUE OR IDENT (TRUE) = FALSE THEN
          FAILED ("INCORRECT ENUMERATION LITERAL");
     END IF;

     IF NOT X /= FALSE OR NOT FALSE /= X THEN
          FAILED ("INCORRECT ""NOT""");
     END IF;

     IF (X AND IDENT (TRUE)) /= TRUE OR (X AND FALSE) /= FALSE THEN
          FAILED ("INCORRECT ""AND""");
     END IF;

     IF (X OR IDENT (TRUE)) /= TRUE OR (FALSE OR X) /= TRUE THEN
          FAILED ("INCORRECT ""OR""");
     END IF;

     IF (X XOR IDENT (TRUE)) /= FALSE OR (X XOR FALSE) /= TRUE THEN
          FAILED ("INCORRECT ""XOR""");
     END IF;

     IF (X AND THEN IDENT (TRUE)) /= TRUE OR
        (X AND THEN FALSE) /= FALSE THEN
          FAILED ("INCORRECT ""AND THEN""");
     END IF;

     IF (X OR ELSE IDENT (TRUE)) /= TRUE OR
        (FALSE OR ELSE X) /= TRUE THEN
          FAILED ("INCORRECT ""OR ELSE""");
     END IF;

     IF NOT (X = IDENT (TRUE)) OR X = FALSE THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= IDENT (TRUE) OR NOT (X /= FALSE) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF X < IDENT (TRUE) OR X < FALSE THEN
          FAILED ("INCORRECT <");
     END IF;

     IF X > IDENT (TRUE) OR FALSE > X THEN
          FAILED ("INCORRECT >");
     END IF;

     IF NOT (X <= IDENT (TRUE)) OR X <= FALSE THEN
          FAILED ("INCORRECT <=");
     END IF;

     IF NOT (X >= IDENT (TRUE)) OR FALSE >= X THEN
          FAILED ("INCORRECT >=");
     END IF;

     IF NOT (X IN T) OR FALSE IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT (FALSE NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     B := FALSE;
     A (X'ADDRESS);
     IF NOT B THEN
          FAILED ("INCORRECT 'ADDRESS");
     END IF;

     IF T'BASE'SIZE < 1 THEN
          FAILED ("INCORRECT 'BASE'SIZE");
     END IF;

     IF T'FIRST /= TRUE OR T'BASE'FIRST /= FALSE THEN
          FAILED ("INCORRECT 'FIRST");
     END IF;

     IF T'IMAGE (X) /= "TRUE" OR T'IMAGE (FALSE) /= "FALSE" THEN
          FAILED ("INCORRECT 'IMAGE");
     END IF;

     IF T'LAST /= TRUE OR T'BASE'LAST /= TRUE THEN
          FAILED ("INCORRECT 'LAST");
     END IF;

     IF T'POS (X) /= 1 OR T'POS (FALSE) /= 0 THEN
          FAILED ("INCORRECT 'POS");
     END IF;

     IF T'PRED (X) /= FALSE THEN
          FAILED ("INCORRECT 'PRED");
     END IF;

     IF T'SIZE < 1 THEN
          FAILED ("INCORRECT TYPE'SIZE");
     END IF;

     IF X'SIZE < 1 THEN
          FAILED ("INCORRECT OBJECT'SIZE");
     END IF;

     IF T'SUCC (T'VAL (IDENT_INT (0))) /= X THEN
          FAILED ("INCORRECT 'SUCC");
     END IF;

     IF T'VAL (IDENT_INT (1)) /= X OR T'VAL (0) /= FALSE THEN
          FAILED ("INCORRECT 'VAL");
     END IF;

     IF T'VALUE (IDENT_STR ("TRUE")) /= X OR
        T'VALUE ("FALSE") /= FALSE THEN
          FAILED ("INCORRECT 'VALUE");
     END IF;

     IF T'WIDTH /= 4 OR T'BASE'WIDTH /= 5 THEN
          FAILED ("INCORRECT 'WIDTH");
     END IF;

     RESULT;
END C34001D;
