-- D4A004A.ADA

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
-- 32 BIT INTEGERS IN NUMBER DECLARATIONS. UNLIKE TEST D4A002A,
-- NO CANCELLATION IS INVOLVED.

-- A COMPILER MAY REFUSE TO COMPILE THIS TEST BECAUSE THE NUMBERS
-- INVOLVED ARE TOO BIG.

-- BAW 29 SEPT 80
-- JBG 12/6/84

WITH REPORT;
PROCEDURE D4A004A IS

     USE REPORT;

     X : CONSTANT := 511_111_111 + 501_111_111;
     Y : CONSTANT := -599_999_999 - 411_111_112;
     Z : CONSTANT := 10 * (10 ** 8);
     D : CONSTANT := 2 ** 30 / 1;
     E : CONSTANT := ( 2 ** 29 - 1) REM 233;
     F : CONSTANT := ABS(( - 2 ** 27 + 1) MOD 511);

BEGIN TEST("D4A004A","LARGE INTEGER VALUES IN NUMBER DECLARATIONS; " &
           "LONGEST INTEGER IS 32 BITS ");

      IF X /= 1_012_222_222 OR Y /= -1_011_111_111
      THEN FAILED("ADDITION OR SUBTRACTION NOT EXACT");
      END IF;

      IF Z /= 1_000_000_000  OR D /= 1_073_741_824 OR E /= 0 OR F /= 0
      THEN FAILED("INTEGER ** IS NOT EXACT");
      END IF;

      RESULT;

END D4A004A;
