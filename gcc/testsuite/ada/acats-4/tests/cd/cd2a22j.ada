-- CD2A22J.ADA

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
--     CHECK THAT WHEN A SIZE SPECIFICATION IS GIVEN FOR AN
--     ENUMERATION TYPE, THEN SUCH A TYPE OF THE SMALLEST APPROPRIATE
--     UNSIGNED SIZE CAN BE PASSED AS AN ACTUAL PARAMETER TO A GENERIC
--     PROCEDURE.

-- HISTORY:
--     JET 08/13/87 CREATED ORIGINAL TEST.
--     DHH 04/17/89 CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                  OPERATORS ON 'SIZE TESTS, AND ADDED CHECK ON
--                  REPRESENTATION CLAUSE.
--     JRL 03/27/92 ELIMINATED REDUNDANT TESTING.

WITH REPORT; USE REPORT;
WITH LENGTH_CHECK;                      -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD2A22J IS

     TYPE BASIC_ENUM IS (ZERO, ONE, TWO);
     BASIC_SIZE : CONSTANT := 2;

     FOR BASIC_ENUM'SIZE USE BASIC_SIZE;

BEGIN
     TEST ("CD2A22J", "CHECK THAT WHEN A SIZE SPECIFICATION IS GIVEN " &
                      "FOR AN ENUMERATION TYPE, THEN SUCH A TYPE OF " &
                      "THE SMALLEST APPROPRIATE UNSIGNED SIZE CAN BE " &
                      "PASSED AS AN ACTUAL PARAMETER TO A GENERIC " &
                      "PROCEDURE");

     DECLARE -- TYPE DECLARATION GIVEN WITHIN GENERIC PROCEDURE.

          GENERIC
               TYPE GPARM IS (<>);
          PROCEDURE GENPROC (C0, C1, C2: GPARM);

          PROCEDURE GENPROC (C0, C1, C2: GPARM) IS

               SUBTYPE CHECK_TYPE IS GPARM;

               FUNCTION IDENT (CH : CHECK_TYPE) RETURN CHECK_TYPE IS
               BEGIN
                    IF EQUAL (3, 3) THEN
                         RETURN CH;
                    ELSE
                         RETURN C1;
                    END IF;
               END IDENT;

               PROCEDURE CHECK_1 IS NEW LENGTH_CHECK (CHECK_TYPE);

          BEGIN -- GENPROC.
               CHECK_1 (C0, BASIC_SIZE, "CHECK_TYPE");

               IF CHECK_TYPE'SIZE /= IDENT_INT (BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'SIZE");
               END IF;

               IF C0'SIZE < IDENT_INT (BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR C0'SIZE");
               END IF;

               IF NOT ((C0 <  IDENT (C1)) AND
                       (IDENT (C2)  > IDENT (C1)) AND
                       (C1 <= IDENT (C1)) AND (IDENT (C2) = C2)) THEN
                    FAILED ("INCORRECT RESULTS FOR RELATIONAL " &
                            "OPERATORS");
               END IF;

               IF CHECK_TYPE'FIRST /= IDENT (C0) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'FIRST");
               END IF;

               IF CHECK_TYPE'POS (C0) /= IDENT_INT (0) OR
                  CHECK_TYPE'POS (C1) /= IDENT_INT (1) OR
                  CHECK_TYPE'POS (C2) /= IDENT_INT (2) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'POS");
               END IF;

               IF CHECK_TYPE'SUCC (C0) /= IDENT (C1) OR
                  CHECK_TYPE'SUCC (C1) /= IDENT (C2) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'SUCC");
               END IF;

               IF CHECK_TYPE'IMAGE (C0) /= IDENT_STR ("ZERO") OR
                  CHECK_TYPE'IMAGE (C1) /= IDENT_STR ("ONE")  OR
                  CHECK_TYPE'IMAGE (C2) /= IDENT_STR ("TWO")  THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'IMAGE");
               END IF;

          END GENPROC;

          PROCEDURE NEWPROC IS NEW GENPROC (BASIC_ENUM);

     BEGIN

          NEWPROC (ZERO, ONE, TWO);

     END;

     RESULT;
END CD2A22J;
