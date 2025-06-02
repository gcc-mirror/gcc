-- CD2A31E.ADA

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
--     INTEGER TYPE, THEN SUCH A TYPE CAN BE PASSED AS AN ACTUAL
--     PARAMETER TO GENERIC PROCEDURES.

-- HISTORY:
--     JET 08/12/87  CREATED ORIGINAL TEST.
--     BCB 10/18/88  MODIFIED HEADER AND ENTERED IN ACVC.
--     DHH 04/06/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   SIZE CLAUSE VALUE TO 9, AND CHANGED 'SIZE CLAUSE
--                   CHECKS.
--     JRL 03/27/92  ELIMINATED REDUNDANT TESTING.

WITH REPORT;  USE REPORT;

PROCEDURE CD2A31E IS

     TYPE BASIC_INT IS RANGE -100 .. 100;
     BASIC_SIZE : CONSTANT := 9;

     FOR BASIC_INT'SIZE USE BASIC_SIZE;

BEGIN

     TEST ("CD2A31E", "CHECK THAT WHEN A SIZE SPECIFICATION IS " &
                      "GIVEN FOR AN INTEGER TYPE, THEN SUCH A TYPE " &
                      "CAN BE PASSED AS AN ACTUAL PARAMETER TO " &
                      "GENERIC PACKAGES AND PROCEDURES");

     DECLARE -- TYPE DECLARATION WITHIN GENERIC PROCEDURE.

          GENERIC
               TYPE GPARM IS RANGE <>;
          PROCEDURE GENPROC;

          PROCEDURE GENPROC IS

          SUBTYPE INT IS GPARM;

          I1 : INT := -100;
          I2 : INT :=    0;
          I3 : INT :=  100;

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

               IF I1'SIZE < IDENT_INT (BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR I1'SIZE");
               END IF;

               IF NOT ((I1 < IDENT (0))           AND
                       (IDENT (I3) > IDENT (I2))  AND
                       (I2 <= IDENT (0))          AND
                       (IDENT (100) = I3))        THEN
                    FAILED ("INCORRECT RESULTS FOR RELATIONAL " &
                            "OPERATORS");
               END IF;

               IF NOT (((I1 + I3)  = I2)         AND
                       ((I2 - I3)  = I1)         AND
                       ((I3 * I2)  = I2)         AND
                       ((I2 / I1)  = I2)         AND
                       ((I1 ** 1)  = I1)         AND
                       ((I1 REM 9) = IDENT (-1)) AND
                       ((I3 MOD 9) = IDENT (1))) THEN
                    FAILED ("INCORRECT RESULTS FOR BINARY ARITHMETIC " &
                            "OPERATORS");
               END IF;

               IF INT'LAST /= IDENT (100) THEN
                    FAILED ("INCORRECT VALUE FOR INT'LAST");
               END IF;

               IF INT'VAL (-100) /= IDENT (I1) OR
                  INT'VAL (0)    /= IDENT (I2) OR
                  INT'VAL (100)  /= IDENT (I3) THEN
                    FAILED ("INCORRECT VALUE FOR INT'VAL");
               END IF;

               IF INT'PRED (I2) /= IDENT (-1) OR
                  INT'PRED (I3) /= IDENT (99) THEN
                    FAILED ("INCORRECT VALUE FOR INT'PRED");
               END IF;

               IF INT'VALUE ("-100") /= IDENT (I1) OR
                  INT'VALUE (" 0")   /= IDENT (I2) OR
                  INT'VALUE (" 100") /= IDENT (I3) THEN
                         FAILED ("INCORRECT VALUE FOR INT'VALUE");
               END IF;

          END GENPROC;

          PROCEDURE NEWPROC IS NEW GENPROC (BASIC_INT);

     BEGIN

          NEWPROC;

     END;

     RESULT;

END CD2A31E;
