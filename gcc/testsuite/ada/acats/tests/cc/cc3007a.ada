-- CC3007A.ADA

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
-- CHECK THAT NAMES IN A GENERIC DECLARATIONS ARE STATICALLY BOUND.

-- DAT 9/18/81
-- SPS 2/7/83

WITH REPORT; USE REPORT;

PROCEDURE CC3007A IS
BEGIN
     TEST ("CC3007A", "NAMES IN GENERICS ARE STATICALLY BOUND");

     DECLARE
          I : INTEGER := 1;
          EX : EXCEPTION;
          IA : INTEGER := I'SIZE;

          FUNCTION F (X : INTEGER) RETURN INTEGER;

          PACKAGE P IS
               Q : INTEGER := 1;
          END P;

          GENERIC
               J : IN OUT INTEGER;
               WITH FUNCTION FP (X : INTEGER) RETURN INTEGER IS F;
          PACKAGE GP IS
               V1 : INTEGER := F(I);
               V2 : INTEGER := FP(I);
          END GP;

          GENERIC
               TYPE T IS RANGE <> ;
               WITH FUNCTION F1 (X : INTEGER) RETURN INTEGER IS F;
               INP : IN T := T (I'SIZE);
          FUNCTION F1 (X : T) RETURN T;

          FUNCTION F1 (X : T) RETURN T IS
          BEGIN
               IF INP /= T(IA) THEN
                    FAILED ("INCORRECT GENERIC BINDING 2");
               END IF;
               I := I + 1;
               RETURN 2 * T (F1 (F (INTEGER (X) + I + P.Q)));
          END F1;

          PACKAGE BODY GP IS
               PACKAGE P IS
                    Q : INTEGER := I + 1;
               END P;
               I : INTEGER := 1000;
               FUNCTION F IS NEW F1 (INTEGER);
               FUNCTION F2 IS NEW F1 (INTEGER);
          BEGIN
               P.Q := F2 (J + P.Q + V1 + 2 * V2);
               J := P.Q;
               RAISE EX;
          END GP;

          FUNCTION F (X : INTEGER) RETURN INTEGER IS
          BEGIN
               I := I + 2;
               RETURN X + I;
          END;
     BEGIN
          DECLARE
               I : INTEGER := 1000;
               EX : EXCEPTION;
               FUNCTION F IS NEW F1 (INTEGER);
               V : INTEGER := F (3);
          BEGIN
               BEGIN
                    DECLARE
                         PACKAGE P IS NEW GP (V);
                    BEGIN
                         FAILED ("EX NOT RAISED");
                    END;
               EXCEPTION
                    WHEN EX =>
                         FAILED ("WRONG EXCEPTION RAISED");
                    WHEN OTHERS =>
                         IF V /= 266 THEN
                              FAILED ("WRONG BINDING IN GENERICS");
                         END IF;
                         RAISE;
               END;

          END;
     EXCEPTION
          WHEN EX => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 2");
     END;

     RESULT;
END CC3007A;
