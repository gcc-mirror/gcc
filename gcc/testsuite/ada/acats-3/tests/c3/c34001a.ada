-- C34001A.ADA

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
-- (IMPLICITLY) FOR DERIVED ENUMERATION TYPES, EXCLUDING BOOLEAN TYPES.

-- JRK 8/20/86

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34001A IS

     TYPE PARENT IS (E1, E2, E3, 'A', E4, E5, E6);

     SUBTYPE SUBPARENT IS PARENT RANGE
               PARENT'VAL (IDENT_INT (PARENT'POS (E2))) ..
               PARENT'VAL (IDENT_INT (PARENT'POS (E5)));

     TYPE T IS NEW SUBPARENT RANGE
               PARENT'VAL (IDENT_INT (PARENT'POS (E3))) ..
               PARENT'VAL (IDENT_INT (PARENT'POS (E4)));

     X : T       := E3;
     W : PARENT  := E1;
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
     TEST ("C34001A", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "ENUMERATION TYPES, EXCLUDING BOOLEAN TYPES");

     X := IDENT (E4);
     IF X /= E4 THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= E4 THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= E4 THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := E3;
     END IF;
     IF T (W) /= E3 THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF PARENT (X) /= E4 OR PARENT (T'VAL (0)) /= E1 THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     IF IDENT ('A') /= 'A' THEN
          FAILED ("INCORRECT 'A'");
     END IF;

     IF IDENT (E3) /= E3 OR IDENT (E4) = E1 THEN
          FAILED ("INCORRECT ENUMERATION LITERAL");
     END IF;

     IF X = IDENT ('A') OR X = E1 THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= IDENT (E4) OR NOT (X /= E1) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF X < IDENT (E4) OR X < E1 THEN
          FAILED ("INCORRECT <");
     END IF;

     IF X > IDENT (E4) OR X > E6 THEN
          FAILED ("INCORRECT >");
     END IF;

     IF X <= IDENT ('A') OR X <= E1 THEN
          FAILED ("INCORRECT <=");
     END IF;

     IF IDENT ('A') >= X OR X >= E6 THEN
          FAILED ("INCORRECT >=");
     END IF;

     IF NOT (X IN T) OR E1 IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT (E1 NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     B := FALSE;
     A (X'ADDRESS);
     IF NOT B THEN
          FAILED ("INCORRECT 'ADDRESS");
     END IF;

     IF T'BASE'SIZE < 3 THEN
          FAILED ("INCORRECT 'BASE'SIZE");
     END IF;

     IF T'FIRST /= E3 OR T'BASE'FIRST /= E1 THEN
          FAILED ("INCORRECT 'FIRST");
     END IF;

     IF T'IMAGE (X) /= "E4" OR T'IMAGE (E1) /= "E1" THEN
          FAILED ("INCORRECT 'IMAGE");
     END IF;

     IF T'LAST /= E4 OR T'BASE'LAST /= E6 THEN
          FAILED ("INCORRECT 'LAST");
     END IF;

     IF T'POS (X) /= 4 OR T'POS (E1) /= 0 THEN
          FAILED ("INCORRECT 'POS");
     END IF;

     IF T'PRED (X) /= 'A' OR T'PRED (E2) /= E1 THEN
          FAILED ("INCORRECT 'PRED");
     END IF;

     IF T'SIZE < 2 THEN
          FAILED ("INCORRECT TYPE'SIZE");
     END IF;

     IF X'SIZE < 2 THEN
          FAILED ("INCORRECT OBJECT'SIZE");
     END IF;

     IF T'SUCC (IDENT ('A')) /= X OR T'SUCC (E1) /= E2 THEN
          FAILED ("INCORRECT 'SUCC");
     END IF;

     IF T'VAL (IDENT_INT (4)) /= X OR T'VAL (0) /= E1 THEN
          FAILED ("INCORRECT 'VAL");
     END IF;

     IF T'VALUE (IDENT_STR ("E4")) /= X OR T'VALUE ("E1") /= E1 THEN
          FAILED ("INCORRECT 'VALUE");
     END IF;

     IF T'WIDTH /= 3 OR T'BASE'WIDTH /= 3 THEN
          FAILED ("INCORRECT 'WIDTH");
     END IF;

     RESULT;
END C34001A;
