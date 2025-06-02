-- C46033A.ADA

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
--     IS ANOTHER FIXED POINT TYPE.

-- HISTORY:
--     JET 07/12/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C46033A IS

     TYPE FIX1 IS DELTA 2#0.01# RANGE -16#20.0# .. 16#20.0#;
     TYPE FIX2 IS DELTA 2#0.0001# RANGE -16#80.0# .. 16#80.0#;
     TYPE FIX3 IS DELTA 2#0.000001# RANGE -16#200.0# .. 16#200.0#;

     F1 : FIX1;
     F2 : FIX2;
     F3 : FIX3;

     GENERIC
          TYPE F IS DELTA <>;
     FUNCTION IDENT_G (X : F) RETURN F;

     FUNCTION IDENT_G (X : F) RETURN F IS
     BEGIN
          RETURN X + F(IDENT_INT(0));
     END IDENT_G;

     FUNCTION IDENT IS NEW IDENT_G(FIX1);
     FUNCTION IDENT IS NEW IDENT_G(FIX2);
     FUNCTION IDENT IS NEW IDENT_G(FIX3);

BEGIN
     TEST ("C46033A", "CHECK CONVERSIONS TO FIXED POINT TYPES WHEN " &
                      "THE OPERAND TYPE IS ANOTHER FIXED POINT TYPE");

     F1 := IDENT(-16#1F.C#);
     IF FIX1(F1) /= -16#1F.C# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (1)");
     END IF;

     F1 := IDENT(16#0.4#);
     IF FIX2(F1) /= 16#0.4# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (2)");
     END IF;

     F1 := IDENT(-16#10.4#);
     IF FIX3(F1) /= -16#10.4# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (3)");
     END IF;

     F2 := IDENT(16#3.3#);
     IF FIX1(F2) < 16#3.0# OR
        FIX1(F2) > 16#3.4# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (4)");
     END IF;

     F2 := IDENT(-16#40.1#);
     IF FIX2(F2) /= -16#40.1# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (5)");
     END IF;

     F2 := IDENT(16#0.0#);
     IF FIX3(F2) /= 16#0.0# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (6)");
     END IF;

     F3 := IDENT(-16#0.04#);
     IF FIX1(F3) < -16#0.4# OR
        FIX1(F3) > -16#0.0# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (7)");
     END IF;

     F3 := -IDENT(16#55.A8#);
     IF FIX2(F3) < -16#55.B# OR
        FIX2(F3) > -16#55.A# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (8)");
     END IF;

     F3 := IDENT(16#101.84#);
     IF FIX3(F3) /= 16#101.84# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (9)");
     END IF;

     RESULT;

END C46033A;
