-- C97116A.ADA

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
--     CHECK THAT THE GUARD CONDITIONS IN A SELECTIVE WAIT STATEMENT
--     ARE NOT RE-EVALUATED DURING THE WAIT.

-- HISTORY:
--     WRG 7/10/86  CREATED ORIGINAL TEST.
--     RJW 5/15/90  REMOVED SHARED VARIABLES.

with Impdef;
WITH REPORT; USE REPORT;
PROCEDURE C97116A IS

     GUARD_EVALUATIONS : NATURAL := 0;

     FUNCTION GUARD RETURN BOOLEAN IS
     BEGIN
          GUARD_EVALUATIONS := GUARD_EVALUATIONS + 1;
          RETURN FALSE;
     END GUARD;

     FUNCTION SO_LONG RETURN DURATION IS
     BEGIN
          RETURN 20.0;
     END SO_LONG;

BEGIN

     TEST ("C97116A", "CHECK THAT THE GUARD CONDITIONS IN A " &
                      "SELECTIVE WAIT STATEMENT ARE NOT RE-EVALUATED " &
                      "DURING THE WAIT");

     DECLARE

          TASK T IS
               ENTRY E;
          END T;

          TASK BODY T IS
          BEGIN
               SELECT
                    ACCEPT E;
                    FAILED ("ACCEPTED NONEXISTENT CALL TO E");
               OR WHEN GUARD     =>
                    DELAY 0.0;
                    FAILED ("EXECUTED ALTERNATIVE CLOSED BY FALSE " &
                            "GUARD FUNCTION" );
               OR
                    DELAY SO_LONG * Impdef.One_Second;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED");
          END T;

          TASK GET_CPU;

          TASK BODY GET_CPU IS
          BEGIN
               WHILE NOT T'TERMINATED LOOP
                    DELAY 1.0 * Impdef.One_Second;
               END LOOP;

          END GET_CPU;

     BEGIN

          NULL;

     END;

     IF GUARD_EVALUATIONS /= 1 THEN
          FAILED ("GUARD EVALUATED" &
                  NATURAL'IMAGE(GUARD_EVALUATIONS) & " TIMES");
     END IF;

     RESULT;

END C97116A;
