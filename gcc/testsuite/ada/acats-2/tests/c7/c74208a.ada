-- C74208A.ADA

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
--     CHECK THAT 'SIZE AND 'ADDRESS FOR OBJECTS OF LIMITED AND
--     NON-LIMITED TYPES ARE AVAILABLE BOTH INSIDE AND OUTSIDE THE
--     PACKAGE DECLARING THE TYPES.

-- HISTORY:
--     BCB 03/14/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;

PROCEDURE C74208A IS

     PACKAGE P IS
          TYPE T IS PRIVATE;
          TYPE U IS LIMITED PRIVATE;
     PRIVATE
          TYPE T IS RANGE 1 .. 100;
          TYPE U IS RANGE 1 .. 100;
     END P;

     A : P.T;
     B : P.U;
     ASIZE, BSIZE : INTEGER;
     AADDRESS, BADDRESS : ADDRESS;

     FUNCTION IDENT_ADR(X : ADDRESS) RETURN ADDRESS IS
          Y : P.T;
     BEGIN
          IF EQUAL(3,3) THEN
               RETURN X;
          END IF;
          RETURN Y'ADDRESS;
     END IDENT_ADR;

     PACKAGE BODY P IS
          X : T;
          Y : U;
          XSIZE, YSIZE : INTEGER;
          XADDRESS, YADDRESS : ADDRESS;
     BEGIN
          TEST ("C74208A", "CHECK THAT 'SIZE AND 'ADDRESS FOR " &
                           "OBJECTS OF LIMITED AND NON-LIMITED TYPES " &
                           "ARE AVAILABLE BOTH INSIDE AND OUTSIDE " &
                           "THE PACKAGE DECLARING THE TYPES");

          XSIZE := X'SIZE;
          YSIZE := Y'SIZE;
          XADDRESS := X'ADDRESS;
          YADDRESS := Y'ADDRESS;

          IF NOT EQUAL(XSIZE,X'SIZE) THEN
               FAILED ("IMPROPER VALUE FOR X'SIZE");
          END IF;

          IF XADDRESS /= IDENT_ADR(X'ADDRESS) THEN
               FAILED ("IMPROPER VALUE FOR X'ADDRESS");
          END IF;

          IF NOT EQUAL(YSIZE,Y'SIZE) THEN
               FAILED ("IMPROPER VALUE FOR Y'SIZE");
          END IF;

          IF YADDRESS /= IDENT_ADR(Y'ADDRESS) THEN
               FAILED ("IMPROPER VALUE FOR Y'ADDRESS");
          END IF;
     END P;

BEGIN
     ASIZE := A'SIZE;
     BSIZE := B'SIZE;
     AADDRESS := A'ADDRESS;
     BADDRESS := B'ADDRESS;

     IF NOT EQUAL(ASIZE,A'SIZE) THEN
          FAILED ("IMPROPER VALUE FOR A'SIZE");
     END IF;

     IF AADDRESS /= IDENT_ADR(A'ADDRESS) THEN
          FAILED ("IMPROPER VALUE FOR A'ADDRESS");
     END IF;

     IF NOT EQUAL(BSIZE,B'SIZE) THEN
          FAILED ("IMPROPER VALUE FOR B'SIZE");
     END IF;

     IF BADDRESS /= IDENT_ADR(B'ADDRESS) THEN
          FAILED ("IMPROPER VALUE FOR B'ADDRESS");
     END IF;

     RESULT;
END C74208A;
