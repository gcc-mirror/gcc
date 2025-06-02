-- C85009A.ADA

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
--     CHECK THAT PREDEFINED AND USER-DEFINED EXCEPTIONS CAN BE RENAMED
--     AND THAT HANDLERS REFERRING TO EITHER NAME ARE INVOKED WHEN THE
--     EXCEPTION IS RAISED, EVEN BY AN EXPLICIT 'RAISE' STATEMENT
--     REFERRING TO THE OTHER NAME.

-- HISTORY:
--     JET 03/24/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85009A IS

     MY_EXCEPTION : EXCEPTION;

     MY_EXCEPTION2 : EXCEPTION RENAMES MY_EXCEPTION;

     CONSTRAINT_ERROR2 : EXCEPTION RENAMES CONSTRAINT_ERROR;

     I : INTEGER := 1;

BEGIN
     TEST ("C85009A", "CHECK THAT PREDEFINED AND USER-DEFINED " &
                      "EXCEPTIONS CAN BE RENAMED AND THAT HANDLERS " &
                      "REFERRING TO EITHER NAME ARE INVOKED WHEN " &
                      "THE EXCEPTION IS RAISED, EVEN BY AN EXPLICIT " &
                      "'RAISE' STATEMENT REFERRING TO THE OTHER NAME");

     BEGIN
          RAISE MY_EXCEPTION;
          FAILED ("MY_EXCEPTION NOT RAISED");
     EXCEPTION
          WHEN MY_EXCEPTION2 =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED BY MY_EXCEPTION");
     END;

     BEGIN
          RAISE MY_EXCEPTION2;
          FAILED ("MY_EXCEPTION2 NOT RAISED");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED BY MY_EXCEPTION2");
     END;

     DECLARE
          TYPE COLORS IS (RED, BLUE, YELLOW);
          E : COLORS := RED;
     BEGIN
          E := COLORS'PRED(E);
          IF NOT EQUAL(COLORS'POS(E),COLORS'POS(E)) THEN
               COMMENT("DON'T OPTIMIZE E");
          END IF;
          FAILED ("CONSTRAINT_ERROR NOT RAISED BY PRED(RED)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR2 =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED BY PRED(RED)");
     END;

     BEGIN
          RAISE CONSTRAINT_ERROR;
          FAILED ("CONSTRAINT_ERROR NOT RAISED");
     EXCEPTION
          WHEN CONSTRAINT_ERROR2 =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED BY CONSTRAINT_ERROR");
     END;

     BEGIN
          RAISE CONSTRAINT_ERROR2;
          FAILED ("CONSTRAINT_ERROR2 NOT RAISED");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED BY CONSTRAINT_ERROR2");
     END;

     RESULT;
END C85009A;
