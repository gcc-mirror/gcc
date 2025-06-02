-- C34002A.ADA

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
-- (IMPLICITLY) FOR DERIVED INTEGER TYPES.

-- JRK 8/21/86

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34002A IS

     TYPE PARENT IS RANGE -100 .. 100;

     SUBTYPE SUBPARENT IS PARENT RANGE
               PARENT'VAL (IDENT_INT (-50)) ..
               PARENT'VAL (IDENT_INT ( 50));

     TYPE T IS NEW SUBPARENT RANGE
               PARENT'VAL (IDENT_INT (-30)) ..
               PARENT'VAL (IDENT_INT ( 30));

     TYPE FIXED IS DELTA 0.1 RANGE -1000.0 .. 1000.0;

     X : T        := -30;
     W : PARENT   := -100;
     N : CONSTANT := 1;
     M : CONSTANT := 100;
     B : BOOLEAN  := FALSE;
     F : FLOAT    := 0.0;
     G : FIXED    := 0.0;

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
     TEST ("C34002A", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "INTEGER TYPES");

     X := IDENT (30);
     IF X /= 30 THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= 30 THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= 30 THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := -30;
     END IF;
     IF T (W) /= -30 THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF PARENT (X) /= 30 OR PARENT (T'VAL (-100)) /= -100 THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     IF T (IDENT_INT (-30)) /= -30 THEN
          FAILED ("INCORRECT CONVERSION FROM INTEGER");
     END IF;

     IF INTEGER (X) /= 30 OR INTEGER (T'VAL (-100)) /= -100 THEN
          FAILED ("INCORRECT CONVERSION TO INTEGER");
     END IF;

     IF EQUAL (3, 3) THEN
          F := -30.0;
     END IF;
     IF T (F) /= -30 THEN
          FAILED ("INCORRECT CONVERSION FROM FLOAT");
     END IF;

     IF FLOAT (X) /= 30.0 OR FLOAT (T'VAL (-100)) /= -100.0 THEN
          FAILED ("INCORRECT CONVERSION TO FLOAT");
     END IF;

     IF EQUAL (3, 3) THEN
          G := -30.0;
     END IF;
     IF T (G) /= -30 THEN
          FAILED ("INCORRECT CONVERSION FROM FIXED");
     END IF;

     IF FIXED (X) /= 30.0 OR FIXED (T'VAL (-100)) /= -100.0 THEN
          FAILED ("INCORRECT CONVERSION TO FIXED");
     END IF;

     IF IDENT (N) /= 1 OR X = M THEN
          FAILED ("INCORRECT IMPLICIT CONVERSION");
     END IF;

     IF IDENT (30) /= 30 OR X = 100 THEN
          FAILED ("INCORRECT INTEGER LITERAL");
     END IF;

     IF X = IDENT (0) OR X = 100 THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= IDENT (30) OR NOT (X /= 100) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF X < IDENT (30) OR 100 < X THEN
          FAILED ("INCORRECT <");
     END IF;

     IF X > IDENT (30) OR X > 100 THEN
          FAILED ("INCORRECT >");
     END IF;

     IF X <= IDENT (0) OR 100 <= X THEN
          FAILED ("INCORRECT <=");
     END IF;

     IF IDENT (0) >= X OR X >= 100 THEN
          FAILED ("INCORRECT >=");
     END IF;

     IF NOT (X IN T) OR 100 IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT (100 NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     IF +X /= 30 OR +T'VAL(-100) /= -100 THEN
          FAILED ("INCORRECT UNARY +");
     END IF;

     IF -X /= 0 - 30 OR -T'VAL(-100) /= 100 THEN
          FAILED ("INCORRECT UNARY -");
     END IF;

     IF ABS X /= 30 OR ABS T'VAL (-100) /= 100 THEN
          FAILED ("INCORRECT ABS");
     END IF;

     IF X + IDENT (-1) /= 29 OR X + 70 /= 100 THEN
          FAILED ("INCORRECT BINARY +");
     END IF;

     IF X - IDENT (30) /= 0 OR X - 100 /= -70 THEN
          FAILED ("INCORRECT BINARY -");
     END IF;

     IF X * IDENT (-1) /= -30 OR IDENT (2) * 50 /= 100 THEN
          FAILED ("INCORRECT *");
     END IF;

     IF X / IDENT (3) /= 10 OR 90 / X /= 3 THEN
          FAILED ("INCORRECT /");
     END IF;

     IF X MOD IDENT (7) /= 2 OR 100 MOD X /= 10 THEN
          FAILED ("INCORRECT MOD");
     END IF;

     IF X REM IDENT (7) /= 2 OR 100 REM X /= 10 THEN
          FAILED ("INCORRECT REM");
     END IF;

     IF X ** IDENT_INT (1) /= 30 OR
        T'VAL (100) ** IDENT_INT (1) /= 100 THEN
          FAILED ("INCORRECT **");
     END IF;

     B := FALSE;
     A (X'ADDRESS);
     IF NOT B THEN
          FAILED ("INCORRECT 'ADDRESS");
     END IF;

     IF T'BASE'SIZE < 8 THEN
          FAILED ("INCORRECT 'BASE'SIZE");
     END IF;

     IF T'FIRST /= -30 OR
        T'POS (T'BASE'FIRST) /= PARENT'POS (PARENT'BASE'FIRST) THEN
          FAILED ("INCORRECT 'FIRST");
     END IF;

     IF T'IMAGE (X) /= " 30" OR T'IMAGE (-100) /= "-100" THEN
          FAILED ("INCORRECT 'IMAGE");
     END IF;

     IF T'LAST /= 30 OR
        T'POS (T'BASE'LAST) /= PARENT'POS (PARENT'BASE'LAST) THEN
          FAILED ("INCORRECT 'LAST");
     END IF;

     IF T'POS (X) /= 30 OR T'POS (-100) /= -100 THEN
          FAILED ("INCORRECT 'POS");
     END IF;

     IF T'PRED (X) /= 29 OR T'PRED (100) /= 99 THEN
          FAILED ("INCORRECT 'PRED");
     END IF;

     IF T'SIZE < 6 THEN
          FAILED ("INCORRECT TYPE'SIZE");
     END IF;

     IF X'SIZE < 6 THEN
          FAILED ("INCORRECT OBJECT'SIZE");
     END IF;

     IF T'SUCC (IDENT (29)) /= X OR T'SUCC (99) /= 100 THEN
          FAILED ("INCORRECT 'SUCC");
     END IF;

     IF T'VAL (IDENT_INT (30)) /= X OR T'VAL (100) /= 100 THEN
          FAILED ("INCORRECT 'VAL");
     END IF;

     IF T'VALUE (IDENT_STR ("30")) /= X OR T'VALUE ("100") /= 100 THEN
          FAILED ("INCORRECT 'VALUE");
     END IF;

     IF T'WIDTH /= 3 OR T'BASE'WIDTH < 4 THEN
          FAILED ("INCORRECT 'WIDTH");
     END IF;

     RESULT;
END C34002A;
