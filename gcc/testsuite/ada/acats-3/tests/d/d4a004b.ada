-- D4A004B.ADA

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
-- INTEGERS TO 64 BITS IN NUMBER DECLARATIONS. UNLIKE TEST C4A002B,
-- NO CANCELLATION IS INVOLVED.

-- BAW 29 SEPT 80
-- JWC 7/8/85   RENAMED TO -AB

WITH REPORT;
PROCEDURE D4A004B IS

     USE REPORT;

     X : CONSTANT := 2200000000000000000 + 2199999999999999999;
     Y : CONSTANT := -2200000000000000001 - 2199999999999999998;
     Z : CONSTANT := 4 * (10 ** 18);
     D : CONSTANT := 2 ** 63 / 1;
     E : CONSTANT := ( 2 ** 63 - 1 ) REM 454_279;
     F : CONSTANT := ABS(( -2 ** 55 + 1 ) MOD 2047 );

BEGIN TEST("D4A004B","LARGE INTEGER VALUES IN NUMBER DECLARATIONS; " &
           "LONGEST INTEGER IS 64 BITS ");

      IF X /= 4399999999999999999 THEN
          FAILED ("ERROR X");
      END IF;

      IF Y /= -4399999999999999999 THEN
          FAILED ("ERROR Y");
      END IF;

      IF Z /= 4000000000000000000 THEN
          FAILED ("ERROR Z");
      END IF;

      IF E /= 0 THEN
          FAILED ("ERROR E");
      END IF;

      IF F /= 0 THEN
          FAILED ("ERROR F");
      END IF;

      IF D /= 9_223_372_036_854_775_808 THEN
          FAILED ("ERROR D");
      END IF;

      RESULT;

END D4A004B;
