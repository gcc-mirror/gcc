-- C45343A.ADA

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
-- CHECK THAT CATENATION OF NULL OPERANDS YIELDS THE CORRECT RESULT,
-- WITH THE CORRECT BOUNDS.

-- BHS 6/29/84

WITH REPORT;
PROCEDURE C45343A IS

     USE REPORT;

     TYPE ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;
     SUBTYPE ARR_8 IS ARR (1..8);
     A1, A2 : ARR_8;

     PROCEDURE CAT (A : ARR; I1,I2 : INTEGER; NUM : CHARACTER) IS
     BEGIN
          IF A'FIRST /= I1 OR A'LAST /= I2 THEN
               FAILED ("INCORRECT CATENATION - " & NUM);
          END IF;
     END CAT;

BEGIN

     TEST ("C45343A", "CATENATION OF NULL OPERANDS"); 


     A1 := (1,2,3,4,5,6,7,8);
     A2 := A1(1..0) & A1(6..5) & A1(1..8);
     IF A2 /= (1,2,3,4,5,6,7,8) THEN
          FAILED ("INCORRECT CATENATION RESULT - 1");
     END IF;

     A1 := (1,2,3,4,5,6,7,8);
     A2 := A1(2..8) & A1(1..0) & 9;
     IF A2 /= (2,3,4,5,6,7,8,9) THEN
          FAILED ("INCORRECT CATENATION RESULT - 2");
     END IF;


     CAT ( A1(1..0) & A1(IDENT_INT(2)..0), 2, 0, '3' );
     CAT ( A1(IDENT_INT(1)..0) & A2(2..0), 2, 0, '4' );

     CAT ( A1(1..0) & A1(6..5) & A1(2..8), 2, 8, '5' );
     CAT ( A1(2..8) & A1(1..0), 2, 8, '6' );

     CAT ( A2(1..0) & A2(6..5) & A2(IDENT_INT(2)..8), 2, 8, '7' );
     CAT ( A2(IDENT_INT(2)..8) & A2(1..0), 2, 8, '8' );

     RESULT;

END C45343A;
