-- C74306A.ADA

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
--     AFTER THE FULL DECLARATION OF A DEFERRED CONSTANT, THE VALUE OF
--     THE CONSTANT MAY BE USED IN ANY EXPRESSION, PARTICULARLY
--     EXPRESSIONS IN WHICH THE USE WOULD BE ILLEGAL BEFORE THE FULL
--     DECLARATION.

-- HISTORY:
--     BCB 03/14/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C74306A IS

     GENERIC
          TYPE GENERAL_PURPOSE IS LIMITED PRIVATE;
          Y : IN OUT GENERAL_PURPOSE;
     FUNCTION IDENT (X : GENERAL_PURPOSE) RETURN GENERAL_PURPOSE;

     FUNCTION IDENT (X : GENERAL_PURPOSE) RETURN GENERAL_PURPOSE IS
     BEGIN
          IF EQUAL(3,3) THEN
               RETURN X;
          END IF;
          RETURN Y;
     END IDENT;

     PACKAGE P IS
          TYPE T IS PRIVATE;
          C : CONSTANT T;
     PRIVATE
          TYPE T IS RANGE 1 .. 100;

          TYPE A IS ARRAY(1..2) OF T;

          TYPE B IS ARRAY(INTEGER RANGE <>) OF T;

          TYPE D (DISC : T) IS RECORD
               NULL;
          END RECORD;

          C : CONSTANT T := 50;

          PARAM : T := 99;

          FUNCTION IDENT_T IS NEW IDENT (T, PARAM);

          FUNCTION F (X : T := C) RETURN T;

          SUBTYPE RAN IS T RANGE 1 .. C;

          SUBTYPE IND IS B(1..INTEGER(C));

          SUBTYPE DIS IS D (DISC => C);

          OBJ : T := C;

          CON : CONSTANT T := C;

          ARR : A := (5, C);

          PAR : T := IDENT_T (C);

          RANOBJ : T RANGE 1 .. C := C;

          INDOBJ : B(1..INTEGER(C));

          DIS_VAL : DIS;

          REN : T RENAMES C;

          GENERIC
               FOR_PAR : T := C;
          PACKAGE GENPACK IS
               VAL : T;
          END GENPACK;

          GENERIC
               IN_PAR : IN T;
          PACKAGE NEWPACK IS
               IN_VAL : T;
          END NEWPACK;
     END P;

     USE P;

     PACKAGE BODY P IS
          TYPE A1 IS ARRAY(1..2) OF T;

          TYPE B1 IS ARRAY(INTEGER RANGE <>) OF T;

          TYPE D1 (DISC1 : T) IS RECORD
               NULL;
          END RECORD;

          SUBTYPE RAN1 IS T RANGE 1 .. C;

          SUBTYPE IND1 IS B1(1..INTEGER(C));

          SUBTYPE DIS1 IS D1 (DISC1 => C);

          OBJ1 : T := C;

          FUNCVAR : T;

          CON1 : CONSTANT T := C;

          ARR1 : A1 := (5, C);

          PAR1 : T := IDENT_T (C);

          RANOBJ1 : T RANGE 1 .. C := C;

          INDOBJ1 : B1(1..INTEGER(C));

          DIS_VAL1 : DIS1;

          REN1 : T RENAMES C;

          FUNCTION F (X : T := C) RETURN T IS
          BEGIN
               RETURN C;
          END F;

          PACKAGE BODY GENPACK IS
          BEGIN
               VAL := FOR_PAR;
          END GENPACK;

          PACKAGE BODY NEWPACK IS
          BEGIN
               IN_VAL := IN_PAR;
          END NEWPACK;

          PACKAGE PACK IS NEW GENPACK (FOR_PAR => C);

          PACKAGE NPACK IS NEW NEWPACK (IN_PAR => C);
     BEGIN
          TEST ("C74306A", "AFTER THE FULL DECLARATION OF A DEFERRED " &
                           "CONSTANT, THE VALUE OF THE CONSTANT MAY " &
                           "BE USED IN ANY EXPRESSION, PARTICULARLY " &
                           "EXPRESSIONS IN WHICH THE USE WOULD BE " &
                           "ILLEGAL BEFORE THE FULL DECLARATION");

          IF OBJ /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR OBJ");
          END IF;

          IF CON /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR CON");
          END IF;

          IF ARR /= (IDENT_T(5), IDENT_T(50)) THEN
               FAILED ("IMPROPER VALUES FOR ARR");
          END IF;

          IF PAR /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR PAR");
          END IF;

          IF OBJ1 /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR OBJ1");
          END IF;

          IF CON1 /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR CON1");
          END IF;

          IF ARR1 /= (IDENT_T(5), IDENT_T(50)) THEN
               FAILED ("IMPROPER VALUES FOR ARR1");
          END IF;

          IF PAR1 /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR PAR1");
          END IF;

          IF PACK.VAL /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR PACK.VAL");
          END IF;

          IF NPACK.IN_VAL /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR NPACK.IN_VAL");
          END IF;

          IF RAN'LAST /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR RAN'LAST");
          END IF;

          IF RANOBJ /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR RANOBJ");
          END IF;

          IF IND'LAST /= IDENT_INT(50) THEN
               FAILED ("IMPROPER VALUE FOR IND'LAST");
          END IF;

          IF INDOBJ'LAST /= IDENT_INT(50) THEN
               FAILED ("IMPROPER VALUE FOR INDOBJ'LAST");
          END IF;

          IF DIS_VAL.DISC /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR DIS_VAL.DISC");
          END IF;

          IF REN /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR REN");
          END IF;

          IF RAN1'LAST /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR RAN1'LAST");
          END IF;

          IF RANOBJ1 /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR RANOBJ1");
          END IF;

          IF IND1'LAST /= IDENT_INT(50) THEN
               FAILED ("IMPROPER VALUE FOR IND1'LAST");
          END IF;

          IF INDOBJ1'LAST /= IDENT_INT(50) THEN
               FAILED ("IMPROPER VALUE FOR INDOBJ1'LAST");
          END IF;

          IF DIS_VAL1.DISC1 /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR DIS_VAL1.DISC1");
          END IF;

          IF REN1 /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR REN1");
          END IF;

          FUNCVAR := F(C);

          IF FUNCVAR /= IDENT_T(50) THEN
               FAILED ("IMPROPER VALUE FOR FUNCVAR");
          END IF;

          RESULT;
     END P;

BEGIN
     DECLARE
          TYPE ARR IS ARRAY(1..2) OF T;

          VAL1 : T := C;

          VAL2 : ARR := (C, C);

          VAL3 : T RENAMES C;
     BEGIN
          NULL;
     END;

     NULL;
END C74306A;
