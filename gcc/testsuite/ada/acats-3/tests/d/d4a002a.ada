-- D4A002A.ADA

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
-- LARGE LITERALS IN NUMBER DECLARATIONS, BUT WITH RESULTING
-- SMALLER VALUE OBTAINED BY SUBTRACTION. THIS TEST LIMITS VALUES
-- TO 32 BINARY PLACES.

-- BAW 29 SEPT 80
-- JBG 12/6/84

WITH REPORT;
PROCEDURE D4A002A IS

     USE REPORT;

     X : CONSTANT := 1_034_567_890 - 1_034_567_891;
     Y : CONSTANT := 107 * (10 ** 7) - 1_069_999_999;
     Z : CONSTANT := (1024 ** 3) - (2 ** 30);
     D : CONSTANT := 1_073_741_823 / 32_769;
     E : CONSTANT := 536_870_912 REM 2_304_167;
     F : CONSTANT := (-134_217_728) MOD (-262_657);

BEGIN TEST("D4A002A","LARGE INTEGER RANGE (WITH CANCELLATION) IN " &
           "NUMBER DECLARATIONS; LONGEST INTEGER IS 32 BITS");

      IF X /= -1 OR Y /= 1 OR Z /= 0 OR D /= 32_767 OR E /= 1 OR F /= -1
      THEN FAILED("EXPRESSIONS WITH A LARGE INTEGER RANGE (WITH " &
                  "CANCELLATION) ARE NOT EXACT ");
      END IF;

      RESULT;

END D4A002A;
