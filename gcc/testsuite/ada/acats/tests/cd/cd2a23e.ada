-- CD2A23E.ADA

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
--     CHECK THAT WHEN A SIZE SPECIFICATION AND AN ENUMERATION
--     REPRESENTATION CLAUSE ARE GIVEN FOR AN ENUMERATION TYPE,
--     THEN SUCH A TYPE CAN BE PASSED AS AN ACTUAL PARAMETER TO A
--     GENERIC PROCEDURE.

-- HISTORY:
--     JET 08/18/87 CREATED ORIGINAL TEST.
--     DHH 04/18/89 CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                  OPERATORS ON 'SIZE TESTS, AND ADDED CHECK ON
--                  REPRESENTATION CLAUSE.
--     BCB 03/05/90 ADDED CALL TO LENGTH_CHECK TO VERIFY THAT THE SIZE
--                  SPECIFICATION IS OBEYED.
--     LDC 10/03/90 ADDED EXCEPTION HANDER FOR CHECK OF 'SUCC, 'PRED,
--                  ADDED CASES FOR >=, /=, ASSIGNMENT, QUALIFICATION, 
--                  AND EXPLICIT CONVERSION.
--     WMC 03/27/92 ELIMINATED TEST REDUNDANCIES.


WITH REPORT; USE REPORT;
WITH LENGTH_CHECK;                      -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD2A23E IS

     TYPE BASIC_ENUM IS (ZERO, ONE, TWO);
     BASIC_SIZE : CONSTANT := 8;

     FOR BASIC_ENUM USE (ZERO => 3, ONE => 4, TWO => 5);
     FOR BASIC_ENUM'SIZE USE BASIC_SIZE;

BEGIN
     TEST ("CD2A23E", "CHECK THAT WHEN A SIZE SPECIFICATION AND AN " &
                      "ENUMERATION REPRESENTATION CLAUSE ARE " &
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

               CHECKVAR := IDENT (C0);

               CHECK_1 (CHECKVAR, CHECK_TYPE'SIZE, "CHECK_TYPE");

               IF CHECK_TYPE'SIZE /= IDENT_INT (BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'SIZE");
               END IF;

               IF C0'SIZE < IDENT_INT (BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR C0'SIZE");
               END IF;

               IF NOT ((IDENT(C0) <  IDENT (C1)) AND
                       (IDENT(C2) >  IDENT (C1)) AND
                       (IDENT(C1) <= IDENT (C1)) AND 
                       (IDENT(C2) =  IDENT (C2))) THEN
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

               BEGIN
                    IF CHECK_TYPE'SUCC (IDENT(C2)) /= IDENT (C1) THEN
                         FAILED ("CONSTRAINT ERROR NOT RAISED FOR " &
                                 "CHECK_TYPE'SUCC");
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         IF 3 /= IDENT_INT(3) THEN
                              COMMENT ("DON'T OPTIMIZE EXCEPTION -1");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED FOR " &
                                 "CHECK_TYPE'SUCC");
               END;

               BEGIN
                    IF CHECK_TYPE'PRED(IDENT(C0)) /= IDENT (C1) THEN
                        FAILED ("CONSTRAINT ERROR NOT RAISED FOR " &
                                 "CHECK_TYPE'PRED");
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         IF 3 /= IDENT_INT(3) THEN
                              COMMENT ("DON'T OPTIMIZE EXCEPTION -2");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED FOR " &
                                 "CHECK_TYPE'PRED");
               END;

               IF CHECK_TYPE'PRED (C1) /= IDENT (C0) OR
                  CHECK_TYPE'PRED (C2) /= IDENT (C1) THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'PRED");
               END IF;

               IF CHECK_TYPE'IMAGE (C0) /= IDENT_STR ("ZERO") OR
                  CHECK_TYPE'IMAGE (C1) /= IDENT_STR ("ONE")  OR
                  CHECK_TYPE'IMAGE (C2) /= IDENT_STR ("TWO")  THEN
                    FAILED ("INCORRECT VALUE FOR CHECK_TYPE'IMAGE");
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

END CD2A23E;
