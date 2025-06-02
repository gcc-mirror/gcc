-- CD2A21E.ADA

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
--     ENUMERATION TYPE, THEN SUCH A TYPE CAN
--     BE PASSED AS AN ACTUAL PARAMETER TO A GENERIC PROCEDURE.

-- HISTORY:
--     JET 08/18/87 CREATED ORIGINAL TEST.
--     DHH 04/17/89 CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                  OPERATORS ON 'SIZE TESTS, AND ADDED CHECK ON
--                  REPRESENTATION CLAUSE.
--     BCB 03/05/90 ADDED CALL TO LENGTH_CHECK TO VERIFY THAT THE SIZE
--                  SPECIFICATION IS OBEYED.
--     LDC 10/03/90 ADDED CASES FOR >=, /=, ASSIGNMENT, QUALIFICATION, 
--                  AND EXPLICIT CONVERSION.
--     JRL 03/26/92 ELIMINATED REDUNDANT TESTING.

WITH REPORT; USE REPORT;
WITH LENGTH_CHECK;                      -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD2A21E IS

     TYPE BASIC_ENUM IS (ZERO, ONE, TWO);
     BASIC_SIZE : CONSTANT := INTEGER'SIZE / 2;

     FOR BASIC_ENUM'SIZE USE BASIC_SIZE;

BEGIN
     TEST ("CD2A21E", "CHECK THAT WHEN A SIZE SPECIFICATION IS " &
                      "GIVEN FOR AN ENUMERATION TYPE, " &
                      "THEN SUCH A TYPE CAN BE " &
                      "PASSED AS AN ACTUAL PARAMETER TO A GENERIC " &
                      "PROCEDURE");

     DECLARE -- TYPE DECLARATION GIVEN WITHIN GENERIC PROCEDURE.

          GENERIC
               TYPE GPARM IS (<>);
          PROCEDURE GENPROC (C0, C1, C2: GPARM);

          PROCEDURE GENPROC (C0, C1, C2: GPARM) IS

               SUBTYPE CHECK_TYPE IS GPARM;

               C3 : GPARM;

               CHECKVAR : CHECK_TYPE;

               PROCEDURE CHECK_1 IS NEW LENGTH_CHECK (CHECK_TYPE);

               FUNCTION IDENT (CH : CHECK_TYPE) RETURN CHECK_TYPE IS
               BEGIN
                    IF EQUAL (3, 3) THEN
                         RETURN CH;
                    ELSE
                         RETURN C1;
                    END IF;
               END IDENT;

          BEGIN -- GENPROC.

               CHECKVAR := IDENT (C0);

               CHECK_1 (CHECKVAR, CHECK_TYPE'SIZE, "CHECK_TYPE");

               IF CHECK_TYPE'SIZE /= IDENT_INT (BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'SIZE");
               END IF;

               IF C0'SIZE < IDENT_INT (BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR C0'SIZE");
               END IF;

               IF NOT ((IDENT (C1) IN C1 .. C2)       AND
                       (IDENT(C0) NOT IN IDENT (C1) .. C2)) THEN
                    FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                            "OPERATORS");
               END IF;

               IF CHECK_TYPE'LAST /= IDENT (C2) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'LAST");
               END IF;

               IF CHECK_TYPE'VAL (0) /= IDENT (C0) OR
                  CHECK_TYPE'VAL (1) /= IDENT (C1) OR
                  CHECK_TYPE'VAL (2) /= IDENT (C2) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'VAL");
               END IF;

               IF CHECK_TYPE'PRED (C1) /= IDENT (C0) OR
                  CHECK_TYPE'PRED (C2) /= IDENT (C1) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'PRED");
               END IF;

               IF CHECK_TYPE'VALUE ("ZERO") /= IDENT (C0)  OR
                  CHECK_TYPE'VALUE ("ONE")  /=  IDENT (C1) OR
                  CHECK_TYPE'VALUE ("TWO")  /=  IDENT (C2) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'VALUE");
               END IF;

               CHECKVAR := CHECK_TYPE'VALUE ("ONE");
               C3 := GPARM(CHECKVAR);
               IF C3 /= IDENT(C1) THEN 
                    FAILED ("INCORRECT VALUE FOR CONVERSION");
               END IF;

               CHECK_1 (IDENT(C0), BASIC_SIZE, "CHECK_ENUM");


               IF CHECK_TYPE'(C2) /= IDENT(C2) THEN 
                    FAILED ("INCORRECT VALUE FOR QUALIFICATION");
               END IF;

               C3 := CHECK_TYPE'VALUE ("TWO");
               IF C3 /= IDENT(C2) THEN 
                    FAILED ("INCORRECT VALUE FOR ASSIGNMENT");
               END IF;

          END GENPROC;

          PROCEDURE NEWPROC IS NEW GENPROC (BASIC_ENUM);

     BEGIN

          NEWPROC (ZERO, ONE, TWO);

     END;

     RESULT;

END CD2A21E;
