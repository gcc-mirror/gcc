-- C49026A.ADA

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
-- CHECK THAT A QUALIFIED EXPRESSION CAN APPEAR IN A STATIC EXPRESSION.

-- L.BROWN  10/07/86

WITH REPORT; USE REPORT;

PROCEDURE  C49026A  IS

     TYPE ENUM IS (RED,GREEN,BLUE,YELLOW);
     TYPE INT1 IS RANGE 1 .. 50;
     TYPE FLT1 IS DIGITS 3 RANGE 1.0 .. 5.0;
     TYPE FIX1 IS DELTA 0.125 RANGE 0.0 .. 10.0;
     TYPE INT2 IS RANGE 1 .. INT1'(25);
     TYPE FLT2 IS DIGITS 3 RANGE 1.0 .. FLT1'(2.0);
     TYPE FIX2 IS DELTA 0.125 RANGE 0.0 .. FIX1'(5.0);
     TYPE FLT3 IS DIGITS INT1'(3);
     TYPE FIX3 IS DELTA FIX1'(0.125) RANGE 0.0 .. 5.0;
     OBJ1 : INTEGER := 2;
     CAS_OBJ : ENUM := GREEN;

BEGIN

     TEST("C49026A","QUALIFIED EXPRESSIONS CAN APPEAR IN STATIC "&
                    "EXPRESSIONS");

     CASE CAS_OBJ IS
          WHEN ENUM'(GREEN) =>
               OBJ1 := 3;
          WHEN OTHERS =>
               FAILED("INCORRECT VALUE FOR QUALIFIED EXPRESSION 1");
     END CASE;

     RESULT;

END C49026A;
