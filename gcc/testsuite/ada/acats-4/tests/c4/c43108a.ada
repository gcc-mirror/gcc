-- C43108A.ADA

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
--     CHECK THAT IN A RECORD AGGREGATE THE VALUE OF A DISCRIMINANT IS
--     USED TO RESOLVE THE TYPE OF A COMPONENT THAT DEPENDS ON THE
--     DISCRIMINANT.

-- HISTORY:
--     DHH 09/08/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C43108A IS

BEGIN
     TEST ("C43108A", "CHECK THAT IN A RECORD AGGREGATE THE VALUE OF " &
                      "A DISCRIMINANT IS USED TO RESOLVE THE TYPE OF " &
                      "A COMPONENT THAT DEPENDS ON THE DISCRIMINANT");

     DECLARE
          A : INTEGER;

          TYPE DIS(A : BOOLEAN) IS
               RECORD
                    CASE A IS
                         WHEN TRUE =>
                              B : BOOLEAN;
                              C : INTEGER;
                         WHEN FALSE =>
                              D : INTEGER;
                    END CASE;
               END RECORD;

          FUNCTION DIFF(PARAM : DIS) RETURN INTEGER IS
          BEGIN
               IF PARAM.B THEN
                    RETURN PARAM.C;
               ELSE
                    RETURN PARAM.D;
               END IF;
          END DIFF;

     BEGIN
          A := DIFF((C => 3, OTHERS => TRUE));

          IF A /= IDENT_INT(3) THEN
               FAILED("STATIC OTHERS NOT DECIDED CORRECTLY");
          END IF;
     END;

     DECLARE
          GLOBAL : INTEGER := 0;
          TYPE INT IS NEW INTEGER;

          TYPE DIS(A : BOOLEAN) IS
               RECORD
                    CASE A IS
                         WHEN TRUE =>
                              I1 : INT;
                         WHEN FALSE =>
                              I2 : INTEGER;
                    END CASE;
               END RECORD;
          FUNCTION F RETURN INT;
          FUNCTION F RETURN INTEGER;

          A : DIS(TRUE);

          FUNCTION F RETURN INT IS
          BEGIN
               GLOBAL := 1;
               RETURN 5;
          END F;

          FUNCTION F RETURN INTEGER IS
          BEGIN
               GLOBAL := 2;
               RETURN 5;
          END F;

     BEGIN
          A := (TRUE, OTHERS => F);

          IF GLOBAL /= 1 THEN
               FAILED("NON_STATIC OTHERS NOT DECIDED CORRECTLY");
          END IF;
     END;

     RESULT;
END C43108A;
