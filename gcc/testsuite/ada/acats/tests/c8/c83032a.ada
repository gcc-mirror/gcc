-- C83032A.ADA

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
--     CHECK THAT AN IMPLICIT DECLARATION OF A PREDEFINED OPERATOR OR
--     AN ENUMERATION LITERAL IS HIDDEN BY A DERIVED SUBPROGRAM
--     HOMOGRAPH.

-- HISTORY:
--     VCL  08/10/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C83032A IS
BEGIN
     TEST ("C83032A", "AN IMPLICIT DECLARATION OF A PREDEFINED " &
                      "OPERATOR OR AN ENUMERATION LITERAL IS HIDDEN " &
                      "BY A DERIVED SUBPROGRAM HOMOGRAPH");

     DECLARE             -- CHECK PREDEFINED OPERATOR.
          PACKAGE P IS
               TYPE INT IS RANGE -20 .. 20;
               FUNCTION "ABS" (X : INT) RETURN INT;
          END P;
          USE P;

          TYPE NINT IS NEW INT;

          I2 : NINT := -5;

          PACKAGE BODY P IS
               I1 : NINT := 5;

               FUNCTION "ABS" (X : INT) RETURN INT IS
               BEGIN
                    RETURN INT (- (ABS (INTEGER (X))));
               END "ABS";

          BEGIN
               IF "ABS"(I1) /= -5 THEN
                    FAILED ("INCORRECT VALUE FOR 'I1' AFTER CALL " &
                            "TO DERIVED ""ABS"" - 1");
               END IF;

               I1 := ABS (-10);
               IF ABS I1 /= NINT(IDENT_INT (-10)) THEN
                    FAILED ("INCORRECT VALUE FOR 'I1' AFTER CALL " &
                            "TO DERIVED ""ABS"" - 2");
               END IF;
          END P;
     BEGIN
          IF "ABS"(I2) /= -5 THEN
               FAILED ("INCORRECT VALUE FOR 'I2' AFTER CALL " &
                    "TO DERIVED ""ABS"" - 1");
          END IF;

          I2 := ABS (10);
          IF ABS I2 /= NINT (IDENT_INT (-10)) THEN
               FAILED ("INCORRECT VALUE FOR 'I1' AFTER CALL " &
                        "TO DERIVED ""ABS"" - 2");
          END IF;
     END;

     DECLARE   -- CHECK ENUMERATION LITERALS.

          PACKAGE P1 IS
               TYPE ENUM1 IS (E11, E12, E13);
               TYPE PRIV1 IS PRIVATE;
               FUNCTION E11 RETURN PRIV1;
          PRIVATE
               TYPE PRIV1 IS NEW ENUM1;
               TYPE NPRIV1 IS NEW PRIV1;
          END P1;
          USE P1;

          PACKAGE BODY P1 IS
               FUNCTION E11 RETURN PRIV1 IS
               BEGIN
                    RETURN E13;
               END E11;
          BEGIN
               IF NPRIV1'(E11) /= E13 THEN
                    FAILED ("INCORRECT VALUE FOR E11");
               END IF;
          END P1;

     BEGIN
          NULL;
     END;
     RESULT;
END C83032A;
