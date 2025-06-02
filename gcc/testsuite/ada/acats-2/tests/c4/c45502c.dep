-- C45502C.DEP

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
--     CHECK THAT MULTIPLICATION AND DIVISION YIELD CORRECT RESULTS WHEN
--     THE OPERANDS ARE OF PREDEFINED TYPE LONG_INTEGER.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO THOSE IMPLEMENTATIONS WHICH SUPPORT
--     LONG_INTEGER.

--     IF "LONG_INTEGER" IS NOT SUPPORTED, THEN THE DECLARATION OF
--     "CHECK_LONG" MUST BE REJECTED.

-- HISTORY:
--     RJW 09/01/86
--     DHH 01/13/88 ADDED APPLICABILITY CRITERIA AND STANDARD HEADER.

WITH REPORT; USE REPORT;
PROCEDURE C45502C IS

     CHECK_LONG : LONG_INTEGER;                      -- N/A => ERROR.

     FUNCTION IDENT (S : LONG_INTEGER) RETURN LONG_INTEGER IS
     BEGIN
          IF EQUAL (3, 3) THEN
               RETURN S;
          ELSE
               RETURN 0;
          END IF;
     END IDENT;

BEGIN
     TEST ( "C45502C", "CHECK THAT MULTIPLICATION AND DIVISION " &
                       "YIELD CORRECT RESULTS WHEN THE OPERANDS " &
                       "ARE OF PREDEFINED TYPE LONG_INTEGER" );

     DECLARE
          I0  : LONG_INTEGER := 0;
          I1  : LONG_INTEGER := 1;
          I2  : LONG_INTEGER := 2;
          I3  : LONG_INTEGER := 3;
          I5  : LONG_INTEGER := 5;
          I10 : LONG_INTEGER := 10;
          I11 : LONG_INTEGER := 11;
          I12 : LONG_INTEGER := 12;
          I13 : LONG_INTEGER := 13;
          I14 : LONG_INTEGER := 14;
          N1  : LONG_INTEGER := -1;
          N2  : LONG_INTEGER := -2;
          N5  : LONG_INTEGER := -5;
          N10 : LONG_INTEGER := -10;
          N11 : LONG_INTEGER := -11;
          N12 : LONG_INTEGER := -12;
          N13 : LONG_INTEGER := -13;
          N14 : LONG_INTEGER := -14;
          N50 : LONG_INTEGER := -50;

     BEGIN
          IF I0 * LONG_INTEGER'FIRST /= 0 THEN
               FAILED ( "INCORRECT RESULT FOR I0 * " &
                        "LONG_INTEGER'FIRST" );
          END IF;

          IF I0 * LONG_INTEGER'LAST /= 0 THEN
               FAILED ( "INCORRECT RESULT FOR I0 * " &
                        "LONG_INTEGER'LAST" );
          END IF;

          IF N1 * LONG_INTEGER'LAST + LONG_INTEGER'LAST /= 0 THEN
               FAILED ( "INCORRECT RESULT FOR N1 * " &
                        "LONG_INTEGER'LAST" );
          END IF;

          IF I3 * I1 /= I3 THEN
               FAILED ( "INCORRECT RESULT FOR I3 * I1" );
          END IF;

          IF IDENT (I3) * IDENT (I1) /= I3 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (I3) * " &
                        "IDENT (I1)" );
          END IF;

          IF I2 * N1 /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR I2 * N1" );
          END IF;

          IF "*" (LEFT => I2, RIGHT => N1) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR ""*"" (LEFT => I2, " &
                        "RIGHT => N1)" );
          END IF;

          IF IDENT (I2) * IDENT (N1) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (I2) * " &
                        "IDENT (N1)" );
          END IF;

          IF I5 * I2 * N5 /= N50 THEN
               FAILED ( "INCORRECT RESULT FOR I5 * I2 * N5" );
          END IF;

          IF IDENT (N1) * IDENT (N5) /= I5 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (N1) * " &
                        "IDENT (N5)" );
          END IF;

          IF "*" (LEFT => IDENT (N1), RIGHT => IDENT (N5)) /=
                 I5 THEN
               FAILED ( "INCORRECT RESULT FOR ""*"" (LEFT => " &
                        "IDENT (N1), RIGHT => IDENT (N5))" );
          END IF;

          IF IDENT (N1) * IDENT (I2) * IDENT (N5) /= I10
             THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (N1) * " &
                        "IDENT (I2) * IDENT (N5)" );
          END IF;

          IF (-IDENT (I0)) * IDENT (I10) /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR (-IDENT (I0)) * " &
                        "IDENT (I10)" );
          END IF;

          IF I0 * I10 /= (-I0) THEN
               FAILED ( "INCORRECT RESULT FOR I0 * I10" );
          END IF;

          IF "*" (LEFT => I0, RIGHT => I10) /= (-I0) THEN
               FAILED ( "INCORRECT RESULT FOR ""*"" (LEFT => I0, " &
                        "RIGHT => I10)" );
          END IF;

          IF IDENT (I10) / IDENT (I5) /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (I10) " &
                        "/ IDENT (I5)" );
          END IF;

          IF I11 / I5 /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR I11 / I5" );
          END IF;

          IF IDENT (I12) / IDENT (I5) /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (I12) " &
                        "/ IDENT (I5)" );
          END IF;

          IF "/" (LEFT => IDENT (I12), RIGHT => IDENT (I5)) /=
                 I2 THEN
               FAILED ( "INCORRECT RESULT FOR ""/"" (LEFT => " &
                        "IDENT (I12), RIGHT => IDENT (I5))" );
          END IF;

          IF I13 / I5 /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR I13 / I5" );
          END IF;

          IF IDENT (I14) / IDENT (I5) /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (I14) " &
                        "/ IDENT (I5)" );
          END IF;

          IF I10 / N5 /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR I10 / N5" );
          END IF;

          IF "/" (LEFT => I10, RIGHT => N5) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR ""/"" (LEFT => I10, " &
                        "RIGHT => N5)" );
          END IF;

          IF IDENT (I11) / IDENT (N5) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (I11) " &
                        "/ IDENT (N5)" );
          END IF;

          IF I12 / N5 /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR I12 / N5" );
          END IF;

          IF IDENT (I13) / IDENT (N5) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (I13) " &
                        "/ IDENT (N5)" );
          END IF;

          IF I14 / N5 /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR I14 / N5" );
          END IF;

          IF IDENT (N10) / IDENT (I5) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (N10) " &
                        "/ IDENT (I5)" );
          END IF;

          IF "/" (LEFT => IDENT (N10), RIGHT => IDENT (I5)) /=
                 N2 THEN
               FAILED ( "INCORRECT RESULT FOR ""/"" (LEFT => " &
                        "IDENT (N10), RIGHT => IDENT (I5))" );
          END IF;

          IF N11 / I5 /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR N11 / I5" );
          END IF;

          IF IDENT (N12) / IDENT (I5) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (N12) " &
                        "/ IDENT (I5)" );
          END IF;

          IF N13 / I5 /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR N13 / I5" );
          END IF;

          IF "/" (LEFT => N13, RIGHT => I5) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR ""/"" (LEFT => N13, " &
                        "RIGHT => I5)" );
          END IF;

          IF IDENT (N14) / IDENT (I5) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (N14) " &
                        "/ IDENT (I5)" );
          END IF;

          IF N10 / N5 /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR N10 / N5" );
          END IF;

          IF IDENT (N11) / IDENT (N5) /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (N11) " &
                        "/ IDENT (N5)" );
          END IF;

          IF "/" (LEFT => IDENT (N11), RIGHT => IDENT (N5)) /=
                 I2 THEN
               FAILED ( "INCORRECT RESULT FOR ""/"" (LEFT => " &
                        "IDENT (N11), RIGHT => IDENT (N5))" );
          END IF;

          IF N12 / N5 /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR N12 / N5" );
          END IF;


          IF IDENT (N13) / IDENT (N5) /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT (N13) " &
                        "/ IDENT (N5)" );
          END IF;

          IF N14 / N5 /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR N14 / N5" );
          END IF;

          IF "/" (LEFT => N14, RIGHT => N5) /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR ""/"" (LEFT => N14, " &
                        "RIGHT => N5)" );
          END IF;

          IF I0 / I5 /= (-I0) THEN
               FAILED ( "INCORRECT RESULT FOR I0 / I5" );
          END IF;

          IF "/" (LEFT => I0, RIGHT => I5) /= (-I0) THEN
               FAILED ( "INCORRECT RESULT FOR ""/"" (LEFT => I0, " &
                        "RIGHT => I5)" );
          END IF;

          IF (-IDENT (I0)) / IDENT (I5) /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR (-IDENT (I0)) / " &
                        "IDENT (I5)" );
          END IF;

     END;

     RESULT;
END C45502C;
