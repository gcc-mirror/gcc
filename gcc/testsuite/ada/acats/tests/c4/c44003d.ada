-- C44003D.ADA

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
--     CHECK FOR CORRECT PRECEDENCE OF PREDEFINED AND OVERLOADED
--     OPERATIONS ON PREDEFINED TYPE FLOAT, USER-DEFINED TYPES, AND
--     ONE-DIMENSIONAL ARRAYS WITH COMPONENTS OF TYPE FLOAT.

-- HISTORY:
--     RJW 10/13/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C44003D IS

BEGIN
     TEST ("C44003D", "CHECK FOR CORRECT PRECEDENCE OF PREDEFINED " &
                      "AND OVERLOADED OPERATIONS ON PREDEFINED TYPE " &
                      "FLOAT, USER-DEFINED TYPES, AND ONE-DIMEN" &
                      "SIONAL ARRAYS WITH COMPONENTS OF TYPE FLOAT");

----- PREDEFINED FLOAT:

     DECLARE
          F1 : FLOAT := 1.0;
          F2 : FLOAT := 2.0;
          F5 : FLOAT := 5.0;

          FUNCTION "OR" (LEFT, RIGHT : FLOAT) RETURN FLOAT IS
          BEGIN
               RETURN 4.5;
          END "OR";

          FUNCTION "<" (LEFT, RIGHT : FLOAT) RETURN FLOAT IS
          BEGIN
               RETURN 5.5;
          END "<";

          FUNCTION "-" (LEFT, RIGHT : FLOAT) RETURN FLOAT IS
          BEGIN
               RETURN 6.5;
          END "-";

          FUNCTION "+" (RIGHT : FLOAT) RETURN FLOAT IS
          BEGIN
               RETURN 7.5;
          END "+";

          FUNCTION "*" (LEFT, RIGHT : FLOAT) RETURN FLOAT IS
          BEGIN
               RETURN 8.5;
          END "*";

          FUNCTION "NOT" (RIGHT : FLOAT) RETURN FLOAT IS
          BEGIN
               RETURN 9.5;
          END "NOT";

     BEGIN
          IF NOT (-ABS F1 + F2 / F1 + F5 ** 2 =  26.0 AND
                  F1 > 0.0 AND
                  - F2 * F2 ** 3 = -8.5) THEN
               FAILED ("INCORRECT RESULT - 1");
          END IF;

          IF (F1 OR NOT F2 < F1 - F5 * F5 ** 3) /= 4.5 THEN
               FAILED ("INCORRECT RESULT - 2");
          END IF;
     END;

----- USER-DEFINED TYPE:

     DECLARE
          TYPE USR IS DIGITS 5;

          F1 : USR := 1.0;
          F2 : USR := 2.0;
          F5 : USR := 5.0;

          FUNCTION "AND" (LEFT, RIGHT : USR) RETURN USR IS
          BEGIN
               RETURN 4.5;
          END "AND";

          FUNCTION ">=" (LEFT, RIGHT : USR) RETURN USR IS
          BEGIN
               RETURN 5.5;
          END ">=";

          FUNCTION "+" (LEFT, RIGHT : USR) RETURN USR IS
          BEGIN
               RETURN 6.5;
          END "+";

          FUNCTION "-" (RIGHT : USR) RETURN USR IS
          BEGIN
               RETURN 7.5;
          END "-";

          FUNCTION "/" (LEFT, RIGHT : USR) RETURN USR IS
          BEGIN
               RETURN 8.5;
          END "/";

          FUNCTION "**" (LEFT, RIGHT : USR) RETURN USR IS
          BEGIN
               RETURN 9.5;
          END "**";
     BEGIN
          IF +F5 - F2 * F1 ** 2 /= 3.0 OR
             ABS F1 <= 0.0 OR
             - F2 * F2 ** 3.0 /= 7.5 THEN
               FAILED ("INCORRECT RESULT - 3");
          END IF;

          IF (F1 AND F2 >= F1 + F5 / F5 ** 3) /= 4.5 THEN
               FAILED ("INCORRECT RESULT - 4");
          END IF;
     END;

----- ARRAYS:

     DECLARE
          TYPE ARR IS ARRAY (INTEGER RANGE <>) OF FLOAT;

          SUBTYPE SARR IS ARR (1 .. 3);

          F1 : SARR := (OTHERS => 1.0);
          F2 : SARR := (OTHERS => 2.0);
          F5 : SARR := (OTHERS => 5.0);

          FUNCTION "XOR" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => 4.5);
          END "XOR";

          FUNCTION "<=" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => 5.5);
          END "<=";

          FUNCTION "&" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => 6.5);
          END "&";

          FUNCTION "MOD" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => 8.5);
          END "MOD";

          FUNCTION "ABS" (RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => 9.5);
          END "ABS";
     BEGIN
          IF (ABS F1 <= F2 & F5 MOD F1 XOR F1) /= (1 .. 3 => 4.5) THEN
               FAILED ("INCORRECT RESULT - 5");
          END IF;

          IF (ABS F1 & F2) /= (1 .. 3 => 6.5) OR
             (F1 MOD F2 <= F5) /= (1 .. 3 => 5.5) THEN
               FAILED ("INCORRECT RESULT - 6");
          END IF;
     END;

     RESULT;
END C44003D;
