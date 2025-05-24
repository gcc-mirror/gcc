-- C83031C.ADA

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
--     ENUMERATION LITERAL IS HIDDEN BY A GENERIC INSTANTIATION WHICH
--     DECLARES A HOMOGRAPH OF THE OPERATOR OR LITERAL.

-- HISTORY:
--     BCB 09/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C83031C IS

BEGIN
     TEST ("C83031C", "CHECK THAT AN IMPLICIT DECLARATION OF A " &
                      "PREDEFINED OPERATOR OR ENUMERATION LITERAL IS " &
                      "HIDDEN BY A GENERIC INSTANTIATION WHICH " &
                      "DECLARES A HOMOGRAPH OF THE OPERATOR OR " &
                      "LITERAL");

     DECLARE             -- CHECK SUBPROGRAM DECLARATIONS OF OPERATORS
          PACKAGE P IS
               TYPE INT IS RANGE -20 .. 20;

               GENERIC
                    TYPE X IS RANGE <>;
               FUNCTION GEN_FUN (LEFT, RIGHT : X) RETURN X;
          END P;
          USE P;

          PACKAGE BODY P IS
               FUNCTION GEN_FUN (LEFT, RIGHT : X) RETURN X IS
               BEGIN
                    RETURN LEFT / RIGHT;
               END GEN_FUN;

               FUNCTION "*" IS NEW GEN_FUN (INT);
          BEGIN
               IF 2 * INT(IDENT_INT(2)) /= 1 THEN
                    FAILED ("INCORRECT VALUE RETURNED IN CALL TO " &
                            "EXPLICIT '*' OPERATOR - 1");
               END IF;
          END P;
     BEGIN
          NULL;
     END;

     DECLARE   -- CHECK SUBPROGRAM DECLARATIONS OF ENUMERATION LITERALS.

          PACKAGE P1 IS
               TYPE ENUM1 IS (E11, E12, E13);
               TYPE PRIV1 IS PRIVATE;

               GENERIC
                    TYPE X IS (<>);
               FUNCTION GEN_FUN RETURN X;
          PRIVATE
               TYPE PRIV1 IS NEW ENUM1;
          END P1;
          USE P1;

          PACKAGE BODY P1 IS
               FUNCTION GEN_FUN RETURN X IS
               BEGIN
                    RETURN X'LAST;
               END GEN_FUN;

               FUNCTION E11 IS NEW GEN_FUN (PRIV1);
          BEGIN
               IF PRIV1'(E11) /= E13 THEN
                    FAILED ("INCORRECT VALUE FOR E11");
               END IF;
          END P1;
     BEGIN
          NULL;
     END;

     RESULT;
END C83031C;
