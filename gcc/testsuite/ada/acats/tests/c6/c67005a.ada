-- C67005A.ADA

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
-- CHECK IF A RENAMING DECLARATION DECLARES AN EQUALITY OPERATOR, THE
-- TYPES OF THE PARAMETERS NEED NOT BE LIMITED TYPES. 

-- JBG 9/28/83

WITH REPORT; USE REPORT;
PROCEDURE C67005A IS
BEGIN
     TEST ("C67005A", "CHECK THAT AN EQUALITY OPERATOR DECLARED BY " &
                      "A RENAMING DECLARATION NEED NOT HAVE " &
                      "PARAMETERS OF A LIMITED TYPE");
     DECLARE
          GENERIC
               TYPE LP IS LIMITED PRIVATE;
               WITH FUNCTION EQUAL (L, R : LP) RETURN BOOLEAN;
          PACKAGE EQUALITY_OPERATOR IS
               FUNCTION "=" (L, R : LP) RETURN BOOLEAN;
          END EQUALITY_OPERATOR;

          PACKAGE BODY EQUALITY_OPERATOR IS
               FUNCTION "=" (L, R : LP) RETURN BOOLEAN IS
               BEGIN
                    RETURN EQUAL(L, R);
               END "=";
          END EQUALITY_OPERATOR;

          PACKAGE POLAR_COORDINATES IS
               TYPE POLAR_COORD IS
                    RECORD
                         R     : INTEGER;
                         THETA : INTEGER;
                    END RECORD;
               FUNCTION EQUAL (L, R : POLAR_COORD) RETURN BOOLEAN;
               PACKAGE POLAR_EQUAL IS NEW EQUALITY_OPERATOR
                         (POLAR_COORD, EQUAL);
               FUNCTION "=" (L, R : POLAR_COORD) RETURN BOOLEAN
                         RENAMES POLAR_EQUAL."=";
          END POLAR_COORDINATES;

          PACKAGE BODY POLAR_COORDINATES IS
               FUNCTION EQUAL (L, R : POLAR_COORD) RETURN BOOLEAN IS
               BEGIN
                    RETURN (L.THETA MOD 360) = (R.THETA MOD 360) AND
                           L.R = R.R;
               END EQUAL;
          END POLAR_COORDINATES;

          USE POLAR_COORDINATES;

          PACKAGE VARIABLES IS
               P270 : POLAR_COORD := (R => 3, THETA => 270);
               P360 : POLAR_COORD := (R => 3, THETA => IDENT_INT(360));
          END VARIABLES;

          USE VARIABLES;

     BEGIN

          IF P270 /= (3, -90) THEN
               FAILED ("INCORRECT INEQUALITY OPERATOR");
          END IF;

          IF P360 = (3, 0) THEN
               NULL;
          ELSE
               FAILED ("INCORRECT EQUALITY OPERATOR");
          END IF;

          RESULT;

     END;
END C67005A;
