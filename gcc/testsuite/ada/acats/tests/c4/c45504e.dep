-- C45504E.DEP

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE
--     SECOND OPERAND OF '/', 'MOD', OR 'REM' EQUALS ZERO, IF THE
--     OPERANDS ARE OF PREDEFINED TYPE SHORT_INTEGER.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO THOSE IMPLEMENTATIONS WHICH SUPPORT
--     SHORT_INTEGER.

--     IF "SHORT_INTEGER" IS NOT SUPPORTED, THEN THE DECLARATION OF
--     "CHECK_SHORT" MUST BE REJECTED.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- HISTORY:
--     RJW 09/01/86 CREATED ORIGINAL TEST.
--     DHH 01/13/88 ADDED APPLICABILITY CRITERIA AND STANDARD HEADER.
--     JRL 03/11/93 INITIALIZED VARIABLES TO DEFEAT OPTIMIZATION.
--     JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

WITH REPORT; USE REPORT;
PROCEDURE C45504E IS

     CHECK_SHORT : SHORT_INTEGER;                   -- N/A => ERROR.

     I0 : SHORT_INTEGER := 1;
     I5 : SHORT_INTEGER := 2;
     N5 : SHORT_INTEGER := 3;

BEGIN
     TEST ( "C45504E", "CHECK THAT CONSTRAINT_ERROR " &
                       "IS RAISED WHEN THE SECOND OPERAND OF '/', " &
                       "'MOD', OR 'REM' EQUALS ZERO, IF THE " &
                       "OPERANDS ARE OF PREDEFINED TYPE " &
                       "SHORT_INTEGER" );

     IF EQUAL (3, 3) THEN
          I0 := 0;
          I5 := 5;
          N5 := -5;
     END IF;

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
END C45504E;
