-- C83031A.ADA

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
--     AN ENUMERATION LITERAL IS HIDDEN BY A SUBPROGRAM DECLARATION OR
--     A RENAMING DECLARATION WHICH DECLARES A HOMOGRAPH OF THE
--     OPERATOR OR LITERAL.

-- HISTORY:
--     VCL  08/10/88  CREATED ORIGINAL TEST.
--     JRL  03/20/92  ELIMINATED REDUNDANT TESTING.

WITH REPORT;  USE REPORT;
PROCEDURE C83031A IS
BEGIN
     TEST ("C83031A", "AN IMPLICIT DECLARATION OF A PREDEFINED " &
                      "OPERATOR OR AN ENUMERATION LITERAL IS HIDDEN " &
                      "BY A SUBPROGRAM DECLARATION OR A RENAMING " &
                      "DECLARATION WHICH DECLARES A HOMOGRAPH OF THE " &
                      "OPERATOR OR LITERAL");

     DECLARE             -- CHECK SUBPROGRAM DECLARATIONS OF OPERATORS
          PACKAGE P IS
               TYPE INT IS RANGE -20 .. 20;

               M : INT := 3 * INT(IDENT_INT(3));
               N : INT := 4 + INT(IDENT_INT(4));

               FUNCTION "*" (LEFT, RIGHT : INT) RETURN INT;
               TYPE INT2 IS PRIVATE;
               FUNCTION "+" (LEFT, RIGHT : INT2) RETURN INT2;
          PRIVATE
               FUNCTION "+" (LEFT, RIGHT : INT) RETURN INT
                            RENAMES "/" ;

               TYPE INT2 IS RANGE -20 .. 20;
          END P;
          USE P;

          PACKAGE BODY P IS
               FUNCTION "*" (LEFT, RIGHT : INT) RETURN INT IS
               BEGIN
                    RETURN LEFT / RIGHT;
               END "*";

               FUNCTION "+" (LEFT, RIGHT : INT2) RETURN INT2 IS
               BEGIN
                    RETURN LEFT - RIGHT;
               END "+";

          BEGIN
               IF 2 * INT(IDENT_INT(2)) /= 1 THEN
                    FAILED ("INCORRECT VALUE RETURNED IN CALL TO " &
                            "EXPLICIT '*' OPERATOR - 1");
               END IF;

               IF N /= 8 THEN
                    FAILED ("INCORRECT INITIAL VALUE FOR N - 1");
               END IF;
               N := 2 + 2;
               IF N /= INT(IDENT_INT (1)) THEN
               FAILED ("INCORRECT VALUE FOR N AFTER CALL TO " &
                       "EXPLICIT '+' OPERATOR - 1");
               END IF;

               DECLARE
                    Q : INT2 := 8 + 9;
               BEGIN
                    IF Q /= -1 THEN
                         FAILED ("INCORRECT VALUE FOR Q");
                    END IF;
               END;
          END P;
     BEGIN
          IF M /= 9 THEN
               FAILED ("INCORRECT INITIAL VALUE FOR M - 2");
          END IF;
          IF 2 * INT(IDENT_INT(2)) /= 1 THEN
               FAILED ("INCORRECT VALUE RETURNED IN CALL TO " &
                       "EXPLICIT '*' OPERATOR - 2");
          END IF;

          N := 2 + 2;
          IF N /= INT(IDENT_INT (4)) THEN
               FAILED ("INCORRECT VALUE FOR N AFTER CALL TO " &
                       "IMPLICIT '+' OPERATOR - 2");
          END IF;

     END;

     DECLARE   -- CHECK SUBPROGRAM DECLARATIONS OF ENUMERATION LITERALS.

          PACKAGE P1 IS
               TYPE ENUM1 IS (E11, E12, E13);
               TYPE PRIV1 IS PRIVATE;
               FUNCTION E11 RETURN PRIV1;
          PRIVATE
               TYPE PRIV1 IS NEW ENUM1;
               FUNCTION E12 RETURN PRIV1 RENAMES E13;
          END P1;
          USE P1;

          E13 : INTEGER := IDENT_INT (5);

          FUNCTION E12 RETURN ENUM1 RENAMES E11 ;

          FUNCTION CHECK (E: ENUM1) RETURN INTEGER IS
          BEGIN
               RETURN ENUM1'POS (E);
          END CHECK;

          FUNCTION CHECK (E: INTEGER) RETURN INTEGER IS
          BEGIN
               RETURN INTEGER'POS (E);
          END CHECK;

          PACKAGE BODY P1 IS
               FUNCTION E11 RETURN PRIV1 IS
               BEGIN
                    RETURN E13;
               END E11;
          BEGIN
               IF PRIV1'(E11) /= E13 THEN
                    FAILED ("INCORRECT VALUE FOR E11");
               END IF;

               IF E12 /= PRIV1'LAST THEN
                    FAILED ("INCORRECT VALUE FOR E12 - 1");
               END IF;
          END P1;
     BEGIN
          IF E12 /= ENUM1'FIRST THEN
               FAILED ("INCORRECT VALUE FOR E12 - 2");
          END IF;

          IF CHECK (E13) /= 5 THEN
               FAILED ("INCORRECT VALUE FOR E13");
          END IF;
     END;
     RESULT;
END C83031A;
