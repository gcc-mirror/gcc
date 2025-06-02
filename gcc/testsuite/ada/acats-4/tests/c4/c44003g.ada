-- C44003G.ADA

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
--     CHECK FOR CORRECT PRECEDENCE OF PRE-DEFINED AND OVERLOADED
--     OPERATIONS ON BOOLEAN TYPES AND ONE-DIMENSIONAL ARRAYS WITH
--     COMPONENTS OF TYPE BOOLEAN.

-- HISTORY:
--     RJW 10/13/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C44003G IS

BEGIN
     TEST ("C44003G", "CHECK FOR CORRECT PRECEDENCE OF PRE-DEFINED " &
                      "AND OVERLOADED OPERATIONS ON BOOLEAN TYPES " &
                      "AND ONE-DIMENSIONAL ARRAYS WITH COMPONENTS OF " &
                      "TYPE BOOLEAN");

----- PREDEFINED BOOLEAN:

     DECLARE
          T : BOOLEAN := TRUE;
          F : BOOLEAN := FALSE;

          FUNCTION "AND" (LEFT, RIGHT : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               RETURN FALSE;
          END "AND";

          FUNCTION "<" (LEFT, RIGHT : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               RETURN TRUE;
          END "<";

          FUNCTION "-" (LEFT, RIGHT : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               RETURN TRUE;
          END "-";

          FUNCTION "+" (RIGHT : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               RETURN NOT RIGHT;
          END "+";

          FUNCTION "*" (LEFT, RIGHT : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               RETURN FALSE;
          END "*";

          FUNCTION "**" (LEFT, RIGHT : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               RETURN TRUE;
          END "**";

     BEGIN
          IF NOT (+T = F) OR T /= +F OR (TRUE AND FALSE ** TRUE) OR
             NOT (+T < F) OR NOT (T - F * T) OR (NOT T - F XOR + F - F)
             THEN
               FAILED ("INCORRECT RESULT - 1");
          END IF;

     END;

----- ARRAYS:

     DECLARE
          TYPE ARR IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;

          SUBTYPE SARR IS ARR (1 .. 3);

          T : SARR := (OTHERS => TRUE);
          F : SARR := (OTHERS => FALSE);

          FUNCTION "XOR" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => FALSE);
          END "XOR";

          FUNCTION "<=" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => TRUE);
          END "<=";

          FUNCTION "+" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => FALSE);
          END "+";

          FUNCTION "MOD" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => TRUE);
          END "MOD";

          FUNCTION "**" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => FALSE);
          END "**";
     BEGIN
          IF (F ** T <= F + T MOD T XOR T) /= (1 .. 3 => FALSE)
             THEN
               FAILED ("INCORRECT RESULT - 2");
          END IF;

          IF F ** T & T /= NOT T & T OR
             (T MOD F <= T) /= (1 .. 3 => TRUE) THEN
               FAILED ("INCORRECT RESULT - 3");
          END IF;
     END;

     RESULT;
END C44003G;
