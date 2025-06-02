-- C49025A.ADA

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
-- CHECK THAT CERTAIN ATTRIBUTES CAN BE USED IN STATIC EXPRESSIONS
-- SUCH AS: 'SUCC, 'PRED, 'POS, 'VAL, 'AFT, 'DELTA, 'DIGITS, 'FIRST,
--'FORE, 'LAST, 'MACHINE_EMAX, 'MACHINE_EMIN, 'MACHINE_MANTISSA, 
--'MACHINE_OVERFLOWS, 'MACHINE_RADIX, 'MACHINE_ROUNDS, 'SIZE, 'SMALL, 'WIDTH.

-- L.BROWN  10/07/86
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;

PROCEDURE  C49025A  IS

     TYPE ENUM IS (RED,BLUE,GREEN,BLACK);
     TYPE FIX IS DELTA 0.125 RANGE 0.0 .. 20.0;
     TYPE FLT IS DIGITS 3 RANGE 0.0 .. 25.0;
     TYPE INT IS RANGE 1 .. 100;
     TYPE TINT1 IS RANGE 1 .. ENUM'POS(BLUE);
     TYPE TFLT IS DIGITS FIX'AFT RANGE 0.0 .. 10.0;
     TYPE TFIX2 IS DELTA FIX'DELTA RANGE 0.0 .. 5.0;
     TYPE TFLT1 IS DIGITS FLT'DIGITS;
     TYPE ITN IS RANGE 0 .. INT'FIRST;
     TYPE TINT2 IS RANGE 1 .. FIX'FORE;
     TYPE TFLT3 IS DIGITS 3 RANGE 5.0 .. FLT'LAST;
     CON3 : CONSTANT := FLT'MACHINE_EMAX;
     TYPE TINT3 IS RANGE FLT'MACHINE_EMIN .. 1;
     CON4 : CONSTANT := FLT'MACHINE_MANTISSA;
     TYPE TINT4 IS RANGE 1 .. FLT'MACHINE_RADIX;
     CON6 : CONSTANT := INT'SIZE;
     TYPE TFIX5 IS DELTA 0.125 RANGE 0.0 .. FIX'SMALL;
     TYPE TINT6 IS RANGE 1 .. ENUM'WIDTH;
     OBJ1 : INTEGER := 1;
     CAS_OBJ : BOOLEAN := TRUE;

BEGIN

     TEST("C49025A","CHECK THAT CERTAIN ATTRIBUTES CAN "&
                    "BE USED IN STATIC EXPRESSIONS.");

     CASE CAS_OBJ IS
          WHEN (ENUM'PRED(BLUE) = ENUM'(RED)) =>
               OBJ1 := 2;
          WHEN OTHERS =>
               FAILED("INCORRECT VALUE RETURNED FOR ATTRIBUTE 1");
     END CASE;
     CAS_OBJ := TRUE;

     CASE CAS_OBJ IS
          WHEN (ENUM'SUCC(RED) = ENUM'(BLUE)) =>
               OBJ1 := 3;
          WHEN OTHERS =>
               FAILED("INCORRECT VALUE RETURNED FOR ATTRIBUTE 2");
     END CASE;
     CAS_OBJ := TRUE;

     CASE CAS_OBJ IS
          WHEN (ENUM'VAL(3) = ENUM'(BLACK)) =>
               OBJ1 := 4;
          WHEN OTHERS =>
               FAILED("INCORRECT VALUE RETURNED FOR ATTRIBUTE 3");
     END CASE;
     CAS_OBJ := TRUE;

     CASE CAS_OBJ IS
          WHEN (TRUE OR FLT'MACHINE_OVERFLOWS) =>
               OBJ1 := 5;
          WHEN OTHERS =>
               FAILED("INCORRECT VALUE RETURNED FOR ATTRIBUTE 4");
     END CASE;
     CAS_OBJ := FALSE;

     CASE CAS_OBJ IS
          WHEN (FALSE AND FIX'MACHINE_ROUNDS) =>
               OBJ1 := 6;
          WHEN OTHERS =>
               FAILED("INCORRECT VALUE RETURNED FOR ATTRIBUTE 5");
     END CASE;

     RESULT;

END C49025A;
