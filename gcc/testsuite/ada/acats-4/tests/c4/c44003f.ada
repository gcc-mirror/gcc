-- C44003F.ADA

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
--     OPERATIONS ON ENUMERATION TYPES OTHER THAN BOOLEAN OR CHARACTER
--     AND ONE-DIMENSIONAL ARRAYS WITH COMPONENTS OF SUCH TYPES.

-- HISTORY:
--     RJW 10/13/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C44003F IS

     TYPE ENUM IS (ZERO, ONE, TWO, THREE, FOUR, FIVE);

BEGIN
     TEST ("C44003F", "CHECK FOR CORRECT PRECEDENCE OF PRE-DEFINED " &
                      "AND OVERLOADED OPERATIONS ON ENUMERATION " &
                      "TYPES OTHER THAN BOOLEAN OR CHARACTER AND " &
                      "ONE-DIMENSIONAL ARRAYS WITH COMPONENTS OF " &
                      "SUCH TYPES");


----- ENUMERATION TYPE:

     DECLARE
          E1 : ENUM := ONE;
          E2 : ENUM := TWO;
          E5 : ENUM := FIVE;

          FUNCTION "AND" (LEFT, RIGHT : ENUM) RETURN ENUM IS
          BEGIN
               RETURN ZERO;
          END "AND";

          FUNCTION "<" (LEFT, RIGHT : ENUM) RETURN ENUM IS
          BEGIN
               RETURN THREE;
          END "<";

          FUNCTION "-" (LEFT, RIGHT : ENUM) RETURN ENUM IS
          BEGIN
               RETURN ENUM'VAL (ENUM'POS (LEFT) - ENUM'POS (RIGHT));
          END "-";

          FUNCTION "+" (RIGHT : ENUM) RETURN ENUM IS
          BEGIN
               RETURN RIGHT;
          END "+";

          FUNCTION "*" (LEFT, RIGHT : ENUM) RETURN ENUM IS
          BEGIN
               RETURN ENUM'VAL (ENUM'POS (LEFT) * ENUM'POS (RIGHT));
          END "*";

          FUNCTION "**" (LEFT, RIGHT : ENUM) RETURN ENUM IS
          BEGIN
               RETURN ENUM'VAL (ENUM'POS (LEFT) ** ENUM'POS (RIGHT));
          END "**";

     BEGIN
          IF NOT (+E1 < E2) OR NOT (E2 >= +E2) OR NOT (E5 = +FIVE) THEN
               FAILED ("INCORRECT RESULT - 1");
          END IF;

          IF (E5 ** E1 AND E2) /= (E5 - E1 * E5 ** E1) THEN
               FAILED ("INCORRECT RESULT - 2");
          END IF;

     END;

----- ARRAYS:

     DECLARE
          TYPE ARR IS ARRAY (INTEGER RANGE <>) OF ENUM;

          SUBTYPE SARR IS ARR (1 .. 3);

          E1 : SARR := (OTHERS => ONE);
          E2 : SARR := (OTHERS => TWO);
          E5 : SARR := (OTHERS => FIVE);

          FUNCTION "XOR" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => ZERO);
          END "XOR";

          FUNCTION "<=" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => THREE);
          END "<=";

          FUNCTION "+" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => ZERO);
          END "+";

          FUNCTION "MOD" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => THREE);
          END "MOD";

          FUNCTION "**" (LEFT, RIGHT : ARR) RETURN ARR IS
          BEGIN
               RETURN (1 .. 3 => FOUR);
          END "**";
     BEGIN
          IF (E5 ** E1 <= E2 + E5 MOD E1 XOR E1) /= (1 .. 3 => ZERO)
             THEN
               FAILED ("INCORRECT RESULT - 3");
          END IF;

          IF (E5 ** E1 & E2) /= (FOUR, FOUR, FOUR, TWO, TWO, TWO) OR
             (E1 MOD E2 <= E5) /= (1 .. 3 => THREE) THEN
               FAILED ("INCORRECT RESULT - 4");
          END IF;
     END;

     RESULT;

END C44003F;
