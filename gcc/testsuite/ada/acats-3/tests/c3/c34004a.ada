-- C34004A.ADA

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
--      CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
--      (IMPLICITLY) FOR DERIVED FIXED POINT TYPES.

-- HISTORY:
--      JRK 09/08/86  CREATED ORIGINAL TEST.
--      JET 08/06/87  FIXED BUGS IN DELTAS AND RANGE ERROR.
--      JET 09/22/88  CHANGED USAGE OF X'SIZE.
--      RDH 04/16/90  ADDED TEST FOR REAL VARIABLE VALUES.
--      THS 09/25/90  REMOVED ALL REFERENCES TO B, MODIFIED CHECK OF
--                    '=', INITIALIZED Z NON-STATICALLY, MOVED BINARY
--                    CHECKS.
--      DTN 11/30/95  REMOVED NON ADA95 ATTRIBUTES.               
--      KAS 03/04/96  REMOVED COMPARISON OF T'SMALL TO T'BASE'SMALL

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34004A IS

     TYPE PARENT IS DELTA 2.0 ** (-7) RANGE -100.0 .. 100.0;

     SUBTYPE SUBPARENT IS PARENT RANGE
               IDENT_INT (1) * (-50.0) ..
               IDENT_INT (1) * ( 50.0);

     TYPE T IS NEW SUBPARENT DELTA 2.0 ** (-4) RANGE
               IDENT_INT (1) * (-30.0) ..
               IDENT_INT (1) * ( 30.0);

     TYPE FIXED IS DELTA 2.0 ** (-4) RANGE -1000.0 .. 1000.0;

     X : T        := -30.0;
     I : INTEGER  := X'SIZE;  --CHECK FOR THE AVAILABILITY OF 'SIZE.
     W : PARENT   := -100.0;
     R : CONSTANT := 1.0;
     M : CONSTANT := 100.0;
     F : FLOAT    := 0.0;
     G : FIXED    := 0.0;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          NULL;
     END A;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF EQUAL (3, 3) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN T'FIRST;
     END IDENT;

BEGIN

     DECLARE
          Z : CONSTANT T := IDENT(0.0);
     BEGIN
          TEST ("C34004A", "CHECK THAT THE REQUIRED PREDEFINED " &
                           "OPERATIONS ARE DECLARED (IMPLICITLY) " &
                           "FOR DERIVED FIXED POINT TYPES");

          X := IDENT (30.0);
          IF X /= 30.0 THEN
               FAILED ("INCORRECT :=");
          END IF;

          IF X + IDENT (-1.0) /= 29.0 OR X + 70.0 /= 100.0 THEN
               FAILED ("INCORRECT BINARY +");
          END IF;

          IF X - IDENT (30.0) /= 0.0 OR X - 100.0 /= -70.0 THEN
               FAILED ("INCORRECT BINARY -");
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

          IF NOT (X = IDENT (30.0)) THEN
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

          IF T (X * IDENT (-1.0)) /= -30.0 OR
             T (IDENT (2.0) * (Z + 15.0)) /= 30.0 THEN
               FAILED ("INCORRECT * (FIXED, FIXED)");
          END IF;

          IF X * IDENT_INT (-1) /= -30.0 OR
             (Z + 50.0) * 2 /= 100.0 THEN
               FAILED ("INCORRECT * (FIXED, INTEGER)");
          END IF;

          IF IDENT_INT (-1) * X /= -30.0 OR
             2 * (Z + 50.0) /= 100.0 THEN
               FAILED ("INCORRECT * (INTEGER, FIXED)");
          END IF;

          IF T (X / IDENT (3.0)) /= 10.0 OR
             T ((Z + 90.0) / X) /= 3.0 THEN
               FAILED ("INCORRECT / (FIXED, FIXED)");
          END IF;

          IF X / IDENT_INT (3) /= 10.0 OR (Z + 90.0) / 30 /= 3.0 THEN
               FAILED ("INCORRECT / (FIXED, INTEGER)");
          END IF;

          A (X'ADDRESS);

          IF T'AFT /= 2 OR T'BASE'AFT < 3 THEN
               FAILED ("INCORRECT 'AFT");
          END IF;

          IF T'BASE'SIZE < 15 THEN
               FAILED ("INCORRECT 'BASE'SIZE");
          END IF;

          IF T'DELTA /= 2.0 ** (-4) OR T'BASE'DELTA > 2.0 ** (-7) THEN
               FAILED ("INCORRECT 'DELTA");
          END IF;


          IF T'FORE /= 3 OR T'BASE'FORE < 4 THEN
               FAILED ("INCORRECT 'FORE");
          END IF;



          IF T'MACHINE_OVERFLOWS /= T'BASE'MACHINE_OVERFLOWS THEN
               FAILED ("INCORRECT 'MACHINE_OVERFLOWS");
          END IF;

          IF T'MACHINE_ROUNDS /= T'BASE'MACHINE_ROUNDS THEN
               FAILED ("INCORRECT 'MACHINE_ROUNDS");
          END IF;




          IF T'SIZE < 10 THEN
               FAILED ("INCORRECT TYPE'SIZE");
          END IF;

          IF T'SMALL > 2.0 ** (-4) OR T'BASE'SMALL > 2.0 ** (-7) THEN
               FAILED ("INCORRECT 'SMALL");
          END IF;
     END;

     RESULT;
END C34004A;
