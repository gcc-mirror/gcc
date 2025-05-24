-- C58005H.ADA

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
-- CHECK THAT CONSTRAINTS ON THE RETURN VALUE OF A FUNCTION ARE
-- SATISIFIED WHEN THE FUNCTION RETURNS CONTROL TO ITS INVOKER.

-- THIS TESTS CHECKS FOR CONSTRAINTS ON CONSTRAINED ACCESS TYPES WITH
-- RECORD, ARRAY, PRIVATE AND LIMITED PRIVATE DESIGNATED TYPES.

-- SPS 3/10/83
-- RLB 6/29/01 - Repaired test to work in the face of aggressive optimizations.
--               The objects must be used, and must be tied somehow to the
--               calls to Failed.

WITH REPORT;
USE REPORT;
PROCEDURE C58005H IS

     PACKAGE PACK IS
          TYPE PV (D : NATURAL) IS PRIVATE;
          TYPE LP (D : NATURAL) IS LIMITED PRIVATE;
     PRIVATE
          TYPE PV (D : NATURAL) IS RECORD
               NULL;
          END RECORD;
          TYPE LP (D : NATURAL) IS RECORD
               NULL;
          END RECORD;
     END PACK;

     USE PACK;

     TYPE ARR IS ARRAY (NATURAL RANGE <>) OF NATURAL;
     TYPE REC (D : NATURAL) IS RECORD
          NULL;
     END RECORD;

     TYPE ACC_REC IS ACCESS REC;
     TYPE ACC_ARR IS ACCESS ARR;
     TYPE ACC_PV IS ACCESS PV;
     TYPE ACC_LP IS ACCESS LP;

     SUBTYPE ACC_REC1 IS ACC_REC (D => 1);
     SUBTYPE ACC_REC2 IS ACC_REC (D => 2);

     SUBTYPE ACC_ARR1 IS ACC_ARR (1 .. 10);
     SUBTYPE ACC_ARR2 IS ACC_ARR (2 .. 5);

     SUBTYPE ACC_PV1 IS ACC_PV (D => 1);
     SUBTYPE ACC_PV2 IS ACC_PV (D => 2);

     SUBTYPE ACC_LP1 IS ACC_LP (D => 1);
     SUBTYPE ACC_LP2 IS ACC_LP (D => 2);

     VAR1 : ACC_REC1 := NEW REC(1);
     VAR2 : ACC_REC2 := NEW REC(2);
     VAA1 : ACC_ARR1 := NEW ARR(1 .. 10);
     VAA2 : ACC_ARR2 := NEW ARR(2 .. 5);
     VAP1 : ACC_PV1 := NEW PV(1);
     VAP2 : ACC_PV2 := NEW PV(2);
     VAL1 : ACC_LP1 := NEW LP(1);
     VAL2 : ACC_LP2 := NEW LP(2);

     FUNCTION FREC ( X : ACC_REC1) RETURN ACC_REC2 IS
     BEGIN
          RETURN X;
     END FREC;

     FUNCTION FARR ( X : ACC_ARR1) RETURN ACC_ARR2 IS
     BEGIN
          RETURN X;
     END FARR;

     FUNCTION FPV ( X : ACC_PV1) RETURN ACC_PV2 IS
     BEGIN
          RETURN X;
     END FPV;

     FUNCTION FLP ( X : ACC_LP1) RETURN ACC_LP2 IS
     BEGIN
          RETURN X;
     END FLP;

     PACKAGE BODY PACK IS
          FUNCTION LF (X : LP) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT(3);
          END LF;
     BEGIN
          NULL;
     END PACK;

BEGIN

     TEST ("C58005H", "CHECK ACCESS CONSTRAINTS ON RETURN VALUES " &
                      "OF FUNCTIONS");

     BEGIN
          VAR2 := FREC (VAR1);
          IF VAR2.D /= REPORT.IDENT_INT(2) THEN
              FAILED ("CONSTRAINT_ERROR NOT RAISED - REC 1");
          ELSE
              FAILED ("CONSTRAINT_ERROR NOT RAISED - REC 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - REC");
     END;

     BEGIN
          VAA2 := FARR (VAA1);
          IF VAA2'FIRST /= REPORT.IDENT_INT(2) THEN
              FAILED ("CONSTRAINT_ERROR NOT RAISED - ARR 1");
          ELSE
              FAILED ("CONSTRAINT_ERROR NOT RAISED - ARR 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - ARR");
     END;

     BEGIN
          VAP2 := FPV (VAP1);
          IF VAP2.D /= REPORT.IDENT_INT(2) THEN
              FAILED ("CONSTRAINT_ERROR NOT RAISED - PV 1");
          ELSE
              FAILED ("CONSTRAINT_ERROR NOT RAISED - PV 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - PV");
     END;

     BEGIN
          VAL2 := FLP (VAL1);
          IF VAL2.D /= REPORT.IDENT_INT(2) THEN
              FAILED ("CONSTRAINT_ERROR NOT RAISED - LP 1");
          ELSE
              FAILED ("CONSTRAINT_ERROR NOT RAISED - LP 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - LP");
     END;

     RESULT;
END C58005H;
