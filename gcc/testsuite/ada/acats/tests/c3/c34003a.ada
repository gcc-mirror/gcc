-- C34003A.ADA

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
-- (IMPLICITLY) FOR DERIVED FLOATING POINT TYPES.

-- JRK 9/4/86
-- GJD 11/14/95  REMOVED USES OF OBSOLETE ADA 83 ATTRIBUTES.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34003A IS

     TYPE PARENT IS DIGITS 5;

     SUBTYPE SUBPARENT IS PARENT RANGE
               PARENT (IDENT_INT (-50)) ..
               PARENT (IDENT_INT ( 50));

     TYPE T IS NEW SUBPARENT DIGITS 4 RANGE
               PARENT (IDENT_INT (-30)) ..
               PARENT (IDENT_INT ( 30));

     TYPE FIXED IS DELTA 0.1 RANGE -1000.0 .. 1000.0;

     X : T        := -30.0;
     W : PARENT   := -100.0;
     R : CONSTANT := 1.0;
     M : CONSTANT := 100.0;
     B : BOOLEAN  := FALSE;
     F : FLOAT    := 0.0;
     G : FIXED    := 0.0;

     Z : CONSTANT T := 0.0;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF EQUAL (3, 3) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN T'FIRST;
     END IDENT;

BEGIN
     TEST ("C34003A", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "FLOATING POINT TYPES");

     X := IDENT (30.0);
     IF X /= 30.0 THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= 30.0 THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= 30.0 THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := -30.0;
     END IF;
     IF T (W) /= -30.0 THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF PARENT (X) /= 30.0 OR PARENT (Z - 100.0) /= -100.0 THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     IF T (IDENT_INT (-30)) /= -30.0 THEN
          FAILED ("INCORRECT CONVERSION FROM INTEGER");
     END IF;

     IF INTEGER (X) /= 30 OR INTEGER (Z - 100.0) /= -100 THEN
          FAILED ("INCORRECT CONVERSION TO INTEGER");
     END IF;

     IF EQUAL (3, 3) THEN
          F := -30.0;
     END IF;
     IF T (F) /= -30.0 THEN
          FAILED ("INCORRECT CONVERSION FROM FLOAT");
     END IF;

     IF FLOAT (X) /= 30.0 OR FLOAT (Z - 100.0) /= -100.0 THEN
          FAILED ("INCORRECT CONVERSION TO FLOAT");
     END IF;

     IF EQUAL (3, 3) THEN
          G := -30.0;
     END IF;
     IF T (G) /= -30.0 THEN
          FAILED ("INCORRECT CONVERSION FROM FIXED");
     END IF;

     IF FIXED (X) /= 30.0 OR FIXED (Z - 100.0) /= -100.0 THEN
          FAILED ("INCORRECT CONVERSION TO FIXED");
     END IF;

     IF IDENT (R) /= 1.0 OR X = M THEN
          FAILED ("INCORRECT IMPLICIT CONVERSION");
     END IF;

     IF IDENT (30.0) /= 30.0 OR X = 100.0 THEN
          FAILED ("INCORRECT REAL LITERAL");
     END IF;

     IF X = IDENT (0.0) OR X = 100.0 THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= IDENT (30.0) OR NOT (X /= 100.0) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF X < IDENT (30.0) OR 100.0 < X THEN
          FAILED ("INCORRECT <");
     END IF;

     IF X > IDENT (30.0) OR X > 100.0 THEN
          FAILED ("INCORRECT >");
     END IF;

     IF X <= IDENT (0.0) OR 100.0 <= X THEN
          FAILED ("INCORRECT <=");
     END IF;

     IF IDENT (0.0) >= X OR X >= 100.0 THEN
          FAILED ("INCORRECT >=");
     END IF;

     IF NOT (X IN T) OR 100.0 IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT (100.0 NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     IF +X /= 30.0 OR +(Z - 100.0) /= -100.0 THEN
          FAILED ("INCORRECT UNARY +");
     END IF;

     IF -X /= 0.0 - 30.0 OR -(Z - 100.0) /= 100.0 THEN
          FAILED ("INCORRECT UNARY -");
     END IF;

     IF ABS X /= 30.0 OR ABS (Z - 100.0) /= 100.0 THEN
          FAILED ("INCORRECT ABS");
     END IF;

     IF X + IDENT (-1.0) /= 29.0 OR X + 70.0 /= 100.0 THEN
          FAILED ("INCORRECT BINARY +");
     END IF;

     IF X - IDENT (30.0) /= 0.0 OR X - 100.0 /= -70.0 THEN
          FAILED ("INCORRECT BINARY -");
     END IF;

     IF X * IDENT (-1.0) /= -30.0 OR IDENT (2.0) * 50.0 /= 100.0 THEN
          FAILED ("INCORRECT *");
     END IF;

     IF X / IDENT (3.0) /= 10.0 OR 90.0 / X /= 3.0 THEN
          FAILED ("INCORRECT /");
     END IF;

     IF X ** IDENT_INT (1) /= 30.0 OR
        (Z + 100.0) ** IDENT_INT (1) /= 100.0 THEN
          FAILED ("INCORRECT **");
     END IF;

     B := FALSE;
     A (X'ADDRESS);
     IF NOT B THEN
          FAILED ("INCORRECT 'ADDRESS");
     END IF;

     IF T'BASE'SIZE < 27 THEN
          FAILED ("INCORRECT 'BASE'SIZE");
     END IF;

     IF T'DIGITS /= 4 OR T'BASE'DIGITS < 5 THEN
          FAILED ("INCORRECT 'DIGITS");
     END IF;

     IF T'FIRST /= -30.0 THEN
          FAILED ("INCORRECT 'FIRST");
     END IF;

     IF T'LAST /= 30.0 THEN
          FAILED ("INCORRECT 'LAST");
     END IF;

     IF T'MACHINE_EMAX < 1 OR T'BASE'MACHINE_EMAX /= T'MACHINE_EMAX THEN
          FAILED ("INCORRECT 'MACHINE_EMAX");
     END IF;

     IF T'MACHINE_EMIN > -1 OR T'BASE'MACHINE_EMIN /= T'MACHINE_EMIN THEN
          FAILED ("INCORRECT 'MACHINE_EMIN");
     END IF;

     IF T'MACHINE_MANTISSA < 1 OR
        T'BASE'MACHINE_MANTISSA /= T'MACHINE_MANTISSA THEN
          FAILED ("INCORRECT 'MACHINE_MANTISSA");
     END IF;

     IF T'MACHINE_OVERFLOWS /= T'BASE'MACHINE_OVERFLOWS THEN
          FAILED ("INCORRECT 'MACHINE_OVERFLOWS");
     END IF;

     IF T'MACHINE_RADIX < 2 OR
        T'BASE'MACHINE_RADIX /= T'MACHINE_RADIX THEN
          FAILED ("INCORRECT 'MACHINE_RADIX");
     END IF;

     IF T'MACHINE_ROUNDS /= T'BASE'MACHINE_ROUNDS THEN
          FAILED ("INCORRECT 'MACHINE_ROUNDS");
     END IF;

     IF T'SIZE < 23 THEN
          FAILED ("INCORRECT TYPE'SIZE");
     END IF;

     IF X'SIZE < 23 THEN
          FAILED ("INCORRECT OBJECT'SIZE");
     END IF;

     RESULT;
END C34003A;
