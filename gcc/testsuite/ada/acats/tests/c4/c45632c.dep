-- C45632C.DEP

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
--     CHECK THAT FOR PREDEFINED TYPE LONG_INTEGER,
--     CONSTRAINT_ERROR IS RAISED FOR ABS (LONG_INTEGER'FIRST)
--     IF -LONG_INTEGER'LAST > LONG_INTEGER'FIRST.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS THAT SUPPORT
--     THE USE OF "LONG_INTEGER" AS A PREDEFINED DATA TYPE.

--     IF SUCH A TYPE IS NOT SUPPORTED, THEN THE DECLARATION OF THE
--     VARIABLE "TEST_VAR" MUST BE REJECTED.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- HISTORY:
--     RJW 02/20/86  CREATED ORIGINAL TEST.
--     JET 12/30/87  UPDATED HEADER FORMAT, ADDED CODE TO DEFEAT
--                   OPTIMIZATION.
--     MRM 03/30/93  REMOVED NUMERIC ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;

PROCEDURE C45632C IS

     TEST_VAR : LONG_INTEGER;                  -- N/A => ERROR.

     FUNCTION IDENT_LONG (A : LONG_INTEGER) RETURN LONG_INTEGER IS
     BEGIN
          IF EQUAL (3,3) THEN
               RETURN A;
          ELSE
               RETURN 0;
          END IF;
     END IDENT_LONG;

BEGIN

     TEST ( "C45632C", "CHECK THAT FOR PREDEFINED TYPE " &
                       "LONG_INTEGER CONSTRAINT_ERROR IS RAISED FOR " &
                       "ABS (LONG_INTEGER'FIRST) IF " &
                       "-LONG_INTEGER'LAST > LONG_INTEGER'FIRST" );

     BEGIN
          IF - LONG_INTEGER'LAST > LONG_INTEGER'FIRST THEN
               DECLARE
                    I : LONG_INTEGER := IDENT_LONG(LONG_INTEGER'FIRST);
               BEGIN
                    IF IDENT_LONG(ABS I) = IDENT_LONG(I) THEN
                         FAILED ("NO EXCEPTION -- EQUALITY TRUE");
                    ELSE
                         FAILED ("NO EXCEPTION -- EQUALITY FALSE");
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         COMMENT ( "CONSTRAINT_ERROR RAISED" );
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED" );
               END;
          ELSE
               COMMENT ( "-LONG_INTEGER'LAST <= " &
                         "LONG_INTEGER'FIRST" );
          END IF;
     END;

     RESULT;

END C45632C;
