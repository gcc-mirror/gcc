-- CD2A32J.ADA

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
--     CHECK THAT WHEN A SIZE SPECIFICATION OF THE SMALLEST APPROPRIATE
--     UNSIGNED SIZE IS GIVEN FOR AN INTEGER TYPE, THE TYPE CAN BE
--     PASSED AS AN ACTUAL PARAMETER TO GENERIC PROCEDURES.

-- HISTORY:
--     JET 08/12/87  CREATED ORIGINAL TEST.
--     DHH 04/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   SIZE CLAUSE VALUE TO 7, AND CHANGED OPERATOR ON
--                   'SIZE CHECKS.
--     JRL 03/27/92  ELIMINATED REDUNDANT TESTING.

WITH REPORT;  USE REPORT;

PROCEDURE CD2A32J IS

     TYPE BASIC_INT IS RANGE 0 .. 126;
     BASIC_SIZE : CONSTANT := 7;

     FOR BASIC_INT'SIZE USE BASIC_SIZE;

BEGIN

     TEST ("CD2A32J", "CHECK THAT WHEN A SIZE SPECIFICATION " &
                      "OF THE SMALLEST APPROPRIATE UNSIGNED SIZE " &
                      "IS GIVEN FOR AN INTEGER TYPE, THE TYPE " &
                      "CAN BE PASSED AS AN ACTUAL PARAMETER TO " &
                      "GENERIC PROCEDURES");

     DECLARE -- TYPE DECLARATION WITHIN GENERIC PROCEDURE.

          GENERIC
               TYPE GPARM IS RANGE <>;
          PROCEDURE GENPROC;

          PROCEDURE GENPROC IS

          SUBTYPE INT IS GPARM;

          I0 : INT :=   0;
          I1 : INT :=  63;
          I2 : INT := 126;

          FUNCTION IDENT (I : INT) RETURN INT IS
          BEGIN
               IF EQUAL (0,0) THEN
                    RETURN I;
               ELSE
                    RETURN 0;
               END IF;
          END IDENT;

          BEGIN -- GENPROC.

               IF INT'SIZE /= IDENT_INT (BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR INT'SIZE");
               END IF;

               IF I0'SIZE < IDENT_INT (BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR I0'SIZE");
               END IF;

               IF NOT ((I0 < IDENT (1))          AND
                       (IDENT (I2) > IDENT (I1)) AND
                       (I1 <= IDENT (63))       AND
                       (IDENT (126) = I2))       THEN
                    FAILED ("INCORRECT RESULTS FOR RELATIONAL " &
                            "OPERATORS");
               END IF;

               IF NOT (((I0 + I2)  = I2)          AND
                       ((I2 - I1)  = I1)          AND
                       ((I1 * IDENT (2)) = I2)    AND
                       ((I2 / I1)  = IDENT (2))   AND
                       ((I1 ** 1)  = IDENT (63)) AND
                       ((I2 REM 10) = IDENT (6))   AND
                       ((I1 MOD 10) = IDENT (3)))  THEN
                    FAILED ("INCORRECT RESULTS FOR BINARY ARITHMETIC " &
                            "OPERATORS");
               END IF;

               IF INT'POS (I0) /= IDENT_INT (0)   OR
                  INT'POS (I1) /= IDENT_INT (63) OR
                  INT'POS (I2) /= IDENT_INT (126) THEN
                    FAILED ("INCORRECT VALUE FOR INT'POS");
               END IF;

               IF INT'SUCC (I0) /= IDENT (1)   OR
                  INT'SUCC (I1) /= IDENT (64)  THEN
                    FAILED ("INCORRECT VALUE FOR INT'SUCC");
               END IF;

               IF INT'IMAGE (I0) /= IDENT_STR (" 0")   OR
                  INT'IMAGE (I1) /= IDENT_STR (" 63") OR
                  INT'IMAGE (I2) /= IDENT_STR (" 126") THEN
                    FAILED ("INCORRECT VALUE FOR INT'IMAGE");
               END IF;

          END GENPROC;

          PROCEDURE NEWPROC IS NEW GENPROC (BASIC_INT);

     BEGIN

          NEWPROC;

     END;

     RESULT;

END CD2A32J;
