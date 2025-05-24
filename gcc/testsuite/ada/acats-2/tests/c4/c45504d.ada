-- C45504D.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE SECOND
-- OPERAND OF '/', 'MOD', OR 'REM' EQUALS ZERO, IF THE OPERANDS ARE OF 
-- PREDEFINED TYPE INTEGER.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- R.WILLIAMS 9/1/86
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

WITH REPORT; USE REPORT;
PROCEDURE C45504D IS

     I0 : INTEGER := IDENT_INT (0);
     I5 : INTEGER := IDENT_INT (5);
     N5 : INTEGER := IDENT_INT (-5);

BEGIN
     TEST ( "C45504D", "CHECK THAT CONSTRAINT_ERROR " &
                       "IS RAISED WHEN THE SECOND OPERAND OF '/', " &
                       "'MOD', OR 'REM' EQUALS ZERO, IF THE " & 
                       "OPERANDS ARE OF PREDEFINED TYPE INTEGER" );

     BEGIN
          IF I5 / I0 = 0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'I5 / I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'I5 / I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'I5 / I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'I5 / I0'" );
     END;

     BEGIN
          IF N5 / I0 = 0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'N5 / I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'N5 / I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'N5 / I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'N5 / I0'" );
     END;

     BEGIN
          IF I0 / I0  = 0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'I0 / I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'I0 / I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'I0 / I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'I0 / I0'" );
     END;

     BEGIN
          IF I5 / I0 * I0  = 0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'I5 / I0 * I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'I5 / I0 * I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'I5 / I0 * I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'I5 / I0 * I0'" );
     END;

     BEGIN
          IF I5 MOD I0 = 0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'I5 MOD I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'I5 MOD I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'I5 MOD I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'I5 MOD I0'" );
     END;

     BEGIN
          IF N5 MOD I0 = 0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'N5 MOD I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'N5 MOD I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'N5 MOD I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'N5 MOD I0'" );
     END;

     BEGIN
          IF I0 MOD I0  = 0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'I0 MOD I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'I0 MOD I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'I0 MOD I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'I0 MOD I0'" );
     END;

     BEGIN
          IF I5 MOD I0 = (I5 + I0) MOD I0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'I5 MOD I0 = " &
                        "(I5 + I0) MOD I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'I5 MOD I0 = " &
                        "(I5 + I0) MOD I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'I5 MOD I0 = " &
                         "(I5 + I0) MOD I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'I5 MOD I0 = " &
                         "(I5 + I0) MOD I0'" );
     END;
          
     BEGIN
          IF I5 REM I0 = 0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'I5 REM I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'I5 REM I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'I5 REM I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'I5 REM I0'" );
     END;

     BEGIN
          IF N5 REM I0 = 0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'N5 REM I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'N5 REM I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'N5 REM I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'N5 REM I0'" );
     END;

     BEGIN
          IF I0 REM I0  = 0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'I0 REM I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'I0 REM I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'I0 REM I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'I0 REM I0'" );
     END;

     BEGIN
          IF I5 REM (-I0) = I5 REM I0 THEN
               FAILED ( "NO EXCEPTION RAISED BY 'I5 REM (-I0) = " &
                        "I5 REM I0' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED BY 'I5 REM (-I0) = " &
                        "I5 REM I0' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED BY 'I5 REM (-I0) " &
                         "= I5 REM I0'" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED BY 'I5 REM (-I0) = " &
                         "I5 REM I0'" );
     END;
          
     RESULT;
END C45504D;
