-- C46031A.ADA

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
--     CHECK CONVERSIONS TO FIXED POINT TYPES WHEN THE OPERAND TYPE
--     IS AN INTEGER TYPE.

-- HISTORY:
--     JET 07/11/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C46031A IS

     TYPE FIX1 IS DELTA 2#0.01# RANGE -16#20.0# .. 16#20.0#;
     TYPE FIX2 IS DELTA 2#0.0001# RANGE -16#80.0# .. 16#80.0#;
     TYPE FIX3 IS DELTA 2#0.000001# RANGE -16#200.0# .. 16#200.0#;

     TYPE NEW_INT IS NEW INTEGER RANGE -16#200# .. 16#200#;

     I : INTEGER;
     J : NEW_INT;

     FUNCTION IDENT_NEW (X : NEW_INT) RETURN NEW_INT IS
     BEGIN
          RETURN X * NEW_INT(IDENT_INT(1));
     END IDENT_NEW;

BEGIN
     TEST ("C46031A", "CHECK CONVERSIONS TO FIXED POINT TYPES WHEN " &
                      "THE OPERAND TYPE IS AN INTEGER TYPE");

     I := IDENT_INT(-16#1F#);
     IF FIX1(I) /= -16#1F.0# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (1)");
     END IF;

     J := IDENT_NEW(0);
     IF FIX1(J) /= 0.0 THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (2)");
     END IF;

     I := IDENT_INT(16#7F#);
     IF FIX2(I) /= 16#7F.0# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (3)");
     END IF;

     J := IDENT_NEW(16#1#);
     IF FIX2(J) /= 16#1.0# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (4)");
     END IF;

     I := IDENT_INT(-16#55#);
     IF FIX3(I) /= -16#55.0# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (5)");
     END IF;

     J := IDENT_NEW(-16#1#);
     IF FIX3(J) /= -16#1.0# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (6)");
     END IF;

     RESULT;

END C46031A;
