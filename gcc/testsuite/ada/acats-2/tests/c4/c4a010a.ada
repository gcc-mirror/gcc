-- C4A010A.ADA

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
-- CHECK THAT STATIC UNIVERSAL_REAL EXPRESSIONS ARE EVALUATED EXACTLY.

-- SMALL RATIONAL NUMBERS ARE USED IN THIS TEST.

-- JBG 5/3/85

WITH REPORT; USE REPORT;
PROCEDURE C4A010A IS

     C13 : CONSTANT := 1.0/3.0;
     C47 : CONSTANT := 4.0/7.0;
     C112: CONSTANT := 13.0/12.0;
     HALF: CONSTANT := 3.5/7.0;

BEGIN

     TEST ("C4A010A", "CHECK STATIC UNIVERSAL_REAL ACCURACY FOR " &
                      "SMALL RATIONAL NUMBERS");

     IF C13 - C47 /= -5.0/21.0 THEN
          FAILED ("REAL SUBTRACTION RESULT INCORRECT");
     END IF;

     IF C47 + C112 = 1.0 + 55.0/84.0 THEN
          NULL;
     ELSE
          FAILED ("REAL ADDITION RESULT INCORRECT");
     END IF;

     IF C112 - C13 /= 6.0/8.0 THEN
          FAILED ("LCD NOT FOUND");
     END IF;

     IF 0.1 * 0.1 /= 0.01 THEN
          FAILED ("REAL MULTIPLICATION RESULT INCORRECT");
     END IF;

     IF C112/C13 /= 13.0/4 THEN
          FAILED ("REAL QUOTIENT RESULT INCORRECT");
     END IF;

     IF 0.1 ** 4 /= 0.0001 THEN
          FAILED ("POSITIVE EXPONENTIATION RESULT INCORRECT");
     END IF;

     IF C13 ** (-3) /= 27.0 * 0.5 * 2 THEN
          FAILED ("NEGATIVE EXPONENTIATION RESULT INCORRECT");
     END IF;

     IF HALF /= 0.1/0.2 THEN
          FAILED ("FRACTIONAL NUMERATOR AND DENOMINATOR");
     END IF;

     RESULT;

END C4A010A;
