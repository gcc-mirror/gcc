-- C95080B.ADA

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
-- CHECK THAT PARAMETERLESS ENTRIES CAN BE CALLED WITH THE APPROPRIATE
-- NOTATION.

-- JWC 7/15/85
-- JRK 8/21/85

WITH REPORT; USE REPORT;
PROCEDURE C95080B IS

     I : INTEGER := 1;

     TASK T IS
          ENTRY E;
          ENTRY EF (1..3);
     END T;

     TASK BODY T IS
     BEGIN
          ACCEPT E DO
               I := 15;
          END E;
          ACCEPT EF (2) DO
               I := 20;
          END EF;
     END T;

BEGIN

     TEST ("C95080B", "CHECK THAT PARAMETERLESS ENTRIES CAN BE " &
                      "CALLED");

     T.E;
     IF I /= 15 THEN
          FAILED ("PARAMETERLESS ENTRY CALL YIELDS INCORRECT " &
                  "RESULT");
     END IF;

     I := 0;
     T.EF (2);
     IF I /= 20 THEN
          FAILED ("PARAMETERLESS ENTRY FAMILY CALL YIELDS " &
                  "INCORRECT RESULT");
     END IF;

     RESULT;

END C95080B;
