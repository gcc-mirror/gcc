-- C67005B.ADA

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
-- CHECK THAT IF EQUALITY IS REDEFINED FOR A SCALAR TYPE, CASE
-- STATEMENTS STILL USE THE PREDEFINED EQUALITY OPERATION.

-- JBG 9/28/83

WITH REPORT; USE REPORT;
PROCEDURE C67005B IS

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

BEGIN
     TEST ("C67005B", "CHECK THAT REDEFINING EQUALITY FOR A " &
                      "SCALAR TYPE DOES NOT AFFECT CASE STATEMENTS");

     DECLARE
          TYPE MY IS NEW INTEGER;
          CHECK : MY;

          VAR : INTEGER RANGE 1..3 := 3;

          PACKAGE INTEGER_EQUALS IS
               FUNCTION EQUAL (L, R : INTEGER) RETURN BOOLEAN;
               PACKAGE INTEGER_EQUAL IS NEW EQUALITY_OPERATOR
                         (INTEGER, EQUAL);
          END INTEGER_EQUALS;

          FUNCTION "=" (L, R : INTEGER) RETURN BOOLEAN RENAMES
                       INTEGER_EQUALS.INTEGER_EQUAL."=";

          PACKAGE BODY INTEGER_EQUALS IS
               FUNCTION EQUAL (L, R : INTEGER) RETURN BOOLEAN IS
               BEGIN
                    RETURN FALSE;
               END EQUAL;
          END INTEGER_EQUALS;

     BEGIN

          IF VAR = 3 THEN
               FAILED ("DID NOT USE REDEFINED '=' - 1");
          END IF;

          IF VAR /= 3 THEN
               NULL;
          ELSE
               FAILED ("DID NOT USE REDEFINED '/=' - 1");
          END IF;

          IF VAR = IDENT_INT(3) THEN
               FAILED ("DID NOT USE REDEFINED '=' - 2");
          END IF;

          IF VAR /= IDENT_INT(3) THEN
               NULL;
          ELSE
               FAILED ("DID NOT USE REDEFINED '/=' - 2");
          END IF;

          CHECK := MY(IDENT_INT(0));
          IF CHECK /= 0 THEN
               FAILED ("USING WRONG EQUALITY FOR DERIVED TYPE");
          END IF;

          CASE VAR IS
               WHEN 1..3 => CHECK := MY(IDENT_INT(1));
               WHEN OTHERS => NULL;
          END CASE;

          IF CHECK /= 1 THEN
               FAILED ("DID NOT USE PREDEFINED EQUALS IN CASE - 1");
          END IF;

          CASE IDENT_INT(VAR) IS
               WHEN 1 => CHECK := 4;
               WHEN 2 => CHECK := 5;
               WHEN 3 => CHECK := 6;
               WHEN OTHERS => CHECK := 7;
          END CASE;

          IF CHECK /= 6 THEN
               FAILED ("DID NOT USE PREDEFINED EQUALS IN CASE - 2");
          END IF;

     END;

     RESULT;

END C67005B;
