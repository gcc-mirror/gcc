-- D4A002B.ADA

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
-- LARGER LITERALS IN NUMBER DECLARATIONS, BUT WITH RESULTING
-- SMALLER VALUE OBTAINED BY SUBTRACTION. THIS TEST LIMITS VALUES
-- TO 64 BINARY PLACES.

-- BAW 29 SEPT 80
-- JBG 05/02/85     RENAMED TO -B.  REVISED SO THAT ALL RESULTS FIT IN
--                  16 BITS.

WITH REPORT;
PROCEDURE D4A002B IS

     USE REPORT;

     X : CONSTANT := 4123456789012345678 - 4123456789012345679;
     Y : CONSTANT := 4 * (10 ** 18) - 3999999999999999999;
     Z : CONSTANT := (1024 ** 6) - (2 ** 60);
     D : CONSTANT := 9_223_372_036_854_775_807 / 994_862_694_084_217;
     E : CONSTANT := 36_028_790_976_242_271 REM 17_600_175_361;
     F : CONSTANT := ( - 2 ** 51 ) MOD ( - 131_071 );

BEGIN TEST("D4A002B","LARGE INTEGER RANGE (WITH CANCELLATION) IN " &
           "NUMBER DECLARATIONS; LONGEST INTEGER IS 64 BITS ");

      IF X /= -1 OR Y /= 1 OR Z /= 0
         OR D /= 9271 OR E /= 1 OR F /= -1
      THEN FAILED("EXPRESSIONS WITH A LARGE INTEGER RANGE (WITH " &
                  "CANCELLATION) ARE NOT EXACT ");
      END IF;

      RESULT;

END D4A002B;
