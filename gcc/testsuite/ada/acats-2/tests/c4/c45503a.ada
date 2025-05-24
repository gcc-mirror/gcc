-- C45503A.ADA

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
-- CHECK THAT 'REM' AND 'MOD' YIELD CORRECT RESULTS WHEN THE OPERANDS 
-- ARE OF PREDEFINED TYPE INTEGER.

-- R.WILLIAMS 9/1/86

WITH REPORT; USE REPORT;
PROCEDURE C45503A IS

BEGIN
     TEST ( "C45503A", "CHECK THAT 'REM' AND 'MOD' YIELD CORRECT " &
                       "RESULTS WHEN THE OPERANDS ARE OF PREDEFINED " &
                       "TYPE INTEGER" );

     DECLARE
          I0  : INTEGER := 0;
          I1  : INTEGER := 1;
          I2  : INTEGER := 2;
          I3  : INTEGER := 3;
          I4  : INTEGER := 4;
          I5  : INTEGER := 5;    
          I10 : INTEGER := 10;
          I11 : INTEGER := 11;
          I12 : INTEGER := 12;
          I13 : INTEGER := 13;
          I14 : INTEGER := 14;
          N1  : INTEGER := -1;
          N2  : INTEGER := -2;
          N3  : INTEGER := -3;
          N4  : INTEGER := -4;
          N5  : INTEGER := -5;
          N10 : INTEGER := -10;
          N11 : INTEGER := -11;
          N12 : INTEGER := -12;
          N13 : INTEGER := -13;
          N14 : INTEGER := -14;

     BEGIN
          IF I10 REM I5 /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR I10 REM I5" );
          END IF;

          IF IDENT_INT (I11) REM IDENT_INT (I5) /= I1 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (I11) REM " &
                        "IDENT_INT (I5)" );
          END IF;
 
          IF I12 REM I5 /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR I12 REM I5" );
          END IF;

          IF "REM" (LEFT => I12, RIGHT => I5) /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR ""REM"" (LEFT => I12, " &
                        "RIGHT => I5)" );
          END IF;

          IF IDENT_INT (I13) REM IDENT_INT (I5) /= I3 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (I13) REM " &
                        "IDENT_INT (I5)" );
          END IF;

          IF I14 REM I5 /= I4 THEN
               FAILED ( "INCORRECT RESULT FOR I14 REM I5" );
          END IF;

          IF IDENT_INT (I10) REM IDENT_INT (N5) /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (I10) REM " &
                        "IDENT_INT (N5)" );
          END IF;

          IF "REM" (LEFT => IDENT_INT (I10), RIGHT => IDENT_INT (N5)) 
                   /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR ""REM"" (LEFT => " &
                        "IDENT_INT (I10), RIGHT => IDENT_INT (N5))" );
          END IF;

          IF I11 REM N5 /= I1 THEN
               FAILED ( "INCORRECT RESULT FOR I11 REM N5" );
          END IF;

          IF IDENT_INT (I12) REM IDENT_INT (N5) /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (I12) REM " &
                        "IDENT_INT (N5)" );
          END IF;

          IF I13 REM N5 /= I3 THEN
               FAILED ( "INCORRECT RESULT FOR I13 REM N5" );
          END IF;

          IF "REM" (LEFT => I13, RIGHT => N5) /= I3 THEN
               FAILED ( "INCORRECT RESULT FOR ""REM"" (LEFT => I13, " &
                        "RIGHT => N5)" );
          END IF;

          IF IDENT_INT (I14) REM IDENT_INT (N5) /= I4 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (I14) REM " &
                        "IDENT_INT (N5)" );
          END IF;

          IF N10 REM I5 /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR N10 REM I5" );
          END IF;

          IF IDENT_INT (N11) REM IDENT_INT (I5) /= N1 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (N11) REM " &
                        "IDENT_INT (I5)" );
          END IF;

          IF "REM" (LEFT => IDENT_INT (N11), RIGHT => IDENT_INT (I5)) 
                   /= N1 THEN
               FAILED ( "INCORRECT RESULT FOR ""REM"" (LEFT => " &
                        "IDENT_INT (N11), RIGHT => IDENT_INT (I5))" );
          END IF;

          IF N12 REM I5 /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR N12 REM I5" );
          END IF;

          IF IDENT_INT (N13) REM IDENT_INT (I5) /= N3 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (N13) REM " &
                        "IDENT_INT (I5)" );
          END IF;

          IF N14 REM I5 /= N4 THEN
               FAILED ( "INCORRECT RESULT FOR N14 REM I5" );
          END IF;

          IF "REM" (LEFT => N14, RIGHT => I5) /= N4 THEN
               FAILED ( "INCORRECT RESULT FOR ""REM"" (LEFT => N14, " &
                        "RIGHT => I5)" );
          END IF;

          IF IDENT_INT (N10) REM IDENT_INT (N5) /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (N10) REM " &
                        "IDENT_INT (N5)" );
          END IF;

          IF N11 REM N5 /= N1 THEN
               FAILED ( "INCORRECT RESULT FOR N11 REM N5" );
          END IF;

          IF IDENT_INT (N12) REM IDENT_INT (N5) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (N12) REM " &
                        "IDENT_INT (N5)" );
          END IF;

          IF "REM" (LEFT => IDENT_INT (N12), RIGHT => IDENT_INT (N5)) 
                   /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR ""REM"" (LEFT => " &
                        "IDENT_INT (N12), RIGHT => IDENT_INT (N5))" );
          END IF;

          IF N13 REM N5 /= N3 THEN
               FAILED ( "INCORRECT RESULT FOR N13 REM N5" );
          END IF;

          IF IDENT_INT (N14) REM IDENT_INT (N5) /= N4 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (N14) REM " &
                        "IDENT_INT (N5)" );
          END IF;
          
          IF I10 MOD I5 /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR I10 MOD I5" );
          END IF;

          IF IDENT_INT (I11) MOD IDENT_INT (I5) /= I1 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (I11) MOD " &
                        "IDENT_INT (I5)" );
          END IF;
 
          IF I12 MOD I5 /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR I12 MOD I5" );
          END IF;

          IF "MOD" (LEFT => I12, RIGHT => I5) /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR ""MOD"" (LEFT => I12, " &
                        "RIGHT => I5)" );
          END IF;

          IF IDENT_INT (I13) MOD IDENT_INT (I5) /= I3 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (I13) MOD " &
                        "IDENT_INT (I5)" );
          END IF;

          IF I14 MOD I5 /= I4 THEN
               FAILED ( "INCORRECT RESULT FOR I14 MOD I5" );
          END IF;

          IF IDENT_INT (I10) MOD IDENT_INT (N5) /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (I10) MOD " &
                        "IDENT_INT (N5)" );
          END IF;

          IF "MOD" (LEFT => IDENT_INT (I10), RIGHT => IDENT_INT (N5)) 
                   /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR ""MOD"" (LEFT => " &
                        "IDENT_INT (I10), RIGHT => IDENT_INT (N5))" );
          END IF;

          IF I11 MOD N5 /= N4 THEN
               FAILED ( "INCORRECT RESULT FOR I11 MOD N5" );
          END IF;

          IF IDENT_INT (I12) MOD IDENT_INT (N5) /= N3 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (I12) MOD " &
                        "IDENT_INT (N5)" );
          END IF;

          IF I13 MOD N5 /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR I13 MOD N5" );
          END IF;

          IF "MOD" (LEFT => I13, RIGHT => N5) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR ""MOD"" (LEFT => I13, " &
                        "RIGHT => N5)" );
          END IF;

          IF IDENT_INT (I14) MOD IDENT_INT (N5) /=  N1 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (I14) MOD " &
                        "IDENT_INT (N5)" );
          END IF;

          IF N10 MOD I5 /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR N10 MOD I5" );
          END IF;

          IF IDENT_INT (N11) MOD IDENT_INT (I5) /= I4 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (N11) MOD " &
                        "IDENT_INT (I5)" );
          END IF;

          IF "MOD" (LEFT => IDENT_INT (N11), RIGHT => IDENT_INT (I5)) 
                   /= I4 THEN
               FAILED ( "INCORRECT RESULT FOR ""MOD"" (LEFT => " &
                        "IDENT_INT (N11), RIGHT => IDENT_INT (I5))" );
          END IF;

          IF N12 MOD I5 /= I3 THEN
               FAILED ( "INCORRECT RESULT FOR N12 MOD I5" );
          END IF;

          IF IDENT_INT (N13) MOD IDENT_INT (I5) /= I2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (N13) MOD " &
                        "IDENT_INT (I5)" );
          END IF;

          IF N14 MOD I5 /= I1 THEN
               FAILED ( "INCORRECT RESULT FOR N14 MOD I5" );
          END IF;

          IF "MOD" (LEFT => N14, RIGHT => I5) /= I1 THEN
               FAILED ( "INCORRECT RESULT FOR ""MOD"" (LEFT => I14, " &
                        "RIGHT => I5)" );
          END IF;

          IF IDENT_INT (N10) MOD IDENT_INT (N5) /= I0 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (N10) MOD " &
                        "IDENT_INT (N5)" );
          END IF;

          IF N11 MOD N5 /= N1 THEN
               FAILED ( "INCORRECT RESULT FOR N11 MOD N5" );
          END IF;

          IF IDENT_INT (N12) MOD IDENT_INT (N5) /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (N12) MOD " &
                        "IDENT_INT (N5)" );
          END IF;

          IF "MOD" (LEFT => IDENT_INT (N12), RIGHT => IDENT_INT (N5)) 
                   /= N2 THEN
               FAILED ( "INCORRECT RESULT FOR ""MOD"" (LEFT => " &
                        "IDENT_INT (N12), RIGHT => IDENT_INT (N5))" );
          END IF;

          IF N13 MOD N5 /= N3 THEN
               FAILED ( "INCORRECT RESULT FOR N13 MOD N5" );
          END IF;

          IF IDENT_INT (N14) MOD IDENT_INT (N5) /= N4 THEN
               FAILED ( "INCORRECT RESULT FOR IDENT_INT (N14) MOD " &
                        "IDENT_INT (N5)" );
          END IF;
     END;
          
     RESULT;
END C45503A;
