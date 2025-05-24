-- C83031E.ADA

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
--     CHECK THAT AN IMPLICIT DECLARATION OF A PREDEFINED OPERATOR IS
--     HIDDEN BY A GENERIC FORMAL SUBPROGRAM DECLARATION WHICH DECLARES
--     A HOMOGRAPH OF THE OPERATOR.

-- HISTORY:
--     BCB 09/19/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C83031E IS

BEGIN
     TEST ("C83031E", "CHECK THAT AN IMPLICIT DECLARATION OF A " &
                      "PREDEFINED OPERATOR IS HIDDEN BY A GENERIC " &
                      "FORMAL SUBPROGRAM DECLARATION WHICH DECLARES " &
                      "A HOMOGRAPH OF THE OPERATOR");

     DECLARE             -- CHECK SUBPROGRAM DECLARATIONS OF OPERATORS
          TYPE INT IS RANGE -20 .. 20;

          GENERIC
               WITH FUNCTION "*" (LEFT, RIGHT : INT) RETURN INT;
          PACKAGE P IS
          END P;

          PACKAGE BODY P IS
          BEGIN
               IF 2 * INT(IDENT_INT(2)) /= 1 THEN
                    FAILED ("INCORRECT VALUE RETURNED IN CALL TO " &
                            "EXPLICIT '*' OPERATOR - 1");
               END IF;
          END P;

          FUNCTION MULT (X, Y : INT) RETURN INT IS
          BEGIN
               RETURN X / Y;
          END MULT;

          PACKAGE NEW_P IS NEW P (MULT);
     BEGIN
          NULL;
     END;

     RESULT;
END C83031E;
