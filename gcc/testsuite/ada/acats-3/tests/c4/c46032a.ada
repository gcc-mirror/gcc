-- C46032A.ADA

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
--     IS A FLOATING POINT TYPE OF 5 DIGITS PRECISION.

-- HISTORY:
--     JET 07/11/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C46032A IS

     TYPE FIX1 IS DELTA 2#0.01# RANGE -16#20.0# .. 16#20.0#;
     TYPE FIX2 IS DELTA 2#0.0001# RANGE -16#80.0# .. 16#80.0#;
     TYPE FIX3 IS DELTA 2#0.000001# RANGE -16#200.0# .. 16#200.0#;

     TYPE FLOAT5 IS DIGITS 5;

     F5 : FLOAT5;

     FUNCTION IDENT5 (X : FLOAT5) RETURN FLOAT5 IS
     BEGIN
          RETURN X * FLOAT5(IDENT_INT(1));
     END IDENT5;

BEGIN
     TEST ("C46032A", "CHECK CONVERSIONS TO FIXED POINT TYPES WHEN " &
                      "THE OPERAND TYPE IS A FLOATING POINT TYPE " &
                      "OF 5 DIGITS PRECISION");

     F5 := IDENT5(2#0.1100_0000_0000_0000_00#E0);
     IF FIX1(F5) /= 16#0.C# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (1)");
     END IF;

     F5 := IDENT5(2#0.1111_1110_0000_0000_00#E5);
     IF FIX1(F5) /= 16#1F.C# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (2)");
     END IF;

     F5 := IDENT5(-2#0.1010_1010_1010_1010_10#E2);
     IF FIX1(F5) < -16#2.C# OR
        FIX1(F5) > -16#2.8# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (3)");
     END IF;

     F5 := IDENT5(2#0.1111_0000_0000_0000_00#E0);
     IF FIX2(F5) /= 16#0.F# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (4)");
     END IF;

     F5 := IDENT5(-2#0.1111_1110_0000_0000_00#E7);
     IF FIX2(F5) /= -16#7F.0# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (5)");
     END IF;

     F5 := IDENT5(2#0.1111_1111_1101_0000_00#E7);
     IF FIX2(F5) < 16#7F.E# OR
        FIX2(F5) > 16#7F.F# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (6)");
     END IF;

     F5 := IDENT5(2#0.1000_0000_0000_0000_00#E-5);
     IF FIX3(F5) /= 16#0.04# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (7)");
     END IF;

     F5 := -IDENT5(2#0.1010_1010_1010_1010_00#E9);
     IF FIX3(F5) /= -16#155.54# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (8)");
     END IF;

     F5 := IDENT5(2#0.1000_0000_0000_0010_11#E9);
     IF FIX3(F5) < 16#100.04# OR
        FIX3(F5) > 16#100.08# THEN
          FAILED ("INCORRECT RESULT FROM CONVERSION (9)");
     END IF;

     RESULT;

END C46032A;
