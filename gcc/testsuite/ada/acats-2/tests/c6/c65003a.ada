-- C65003A.ADA

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
-- CHECK THAT IF NO RETURN STATEMENT IS EXECUTED, A FUNCTION RAISES
-- PROGRAM_ERROR.  DETERMINE WHERE THE EXCEPTION IS RAISED.

-- THIS LACK OF AN EXECUTABLE RETURN IS DETECTABLE AT COMPILE TIME IN
-- THIS TEST.

-- JBG 10/14/83
-- SPS 2/22/84

WITH REPORT; USE REPORT;
PROCEDURE C65003A IS

     EXCEPTION_RAISED : BOOLEAN := FALSE;
     FUNCTION RETURN_IN_EXCEPTION RETURN INTEGER IS
     BEGIN
          IF FALSE THEN
               RETURN 5;
          END IF;
     EXCEPTION
          WHEN PROGRAM_ERROR =>
               COMMENT ("PROGRAM_ERROR RAISED IN FUNCTION BODY - " &
                        "RETURN_IN_EXCEPTION");
               EXCEPTION_RAISED := TRUE;
               RETURN 5;
     END RETURN_IN_EXCEPTION;

     FUNCTION NO_RETURN RETURN INTEGER IS
          NO_RETURN_EXCEPTION : EXCEPTION;
     BEGIN
          RAISE NO_RETURN_EXCEPTION;
          RETURN 5;
     EXCEPTION
          WHEN NO_RETURN_EXCEPTION =>
               NULL;
     END NO_RETURN;

BEGIN

     TEST ("C65003A", "CHECK THAT PROGRAM_ERROR IS RAISED IF A " &
                      "FUNCTION RETURNS WITHOUT EXECUTING A RETURN " &
                      "STATEMENT");

     BEGIN

          IF RETURN_IN_EXCEPTION = RETURN_IN_EXCEPTION THEN
               IF NOT EXCEPTION_RAISED THEN
                    FAILED ("PROGRAM_ERROR NOT RAISED - " &
                            "RETURN_IN_EXCEPTION");
               END IF;
          END IF;

     EXCEPTION
     
          WHEN PROGRAM_ERROR =>
               COMMENT ("PROGRAM_ERROR RAISED AT POINT OF CALL " &
                        "- RETURN_IN_EXCEPTION");

     END;


     BEGIN

          IF NO_RETURN = NO_RETURN THEN
               FAILED ("PROGRAM_ERROR NOT RAISED - NO_RETURN");
          END IF;

     EXCEPTION
     
          WHEN PROGRAM_ERROR =>
               COMMENT ("PROGRAM_ERROR RAISED WHEN NO RETURN IN " &
                        "EXCEPTION HANDLER");
     END;

     RESULT;

END C65003A;
