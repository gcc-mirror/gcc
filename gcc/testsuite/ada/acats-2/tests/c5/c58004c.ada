-- C58004C.ADA

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
-- CHECK THAT THE RETURN STATEMENT WORKS FOR RECURSIVE SUBPROGRAMS,
--    BOTH FUNCTIONS AND PROCEDURES.

-- DCB 2/8/80
-- SPS 3/7/83
-- JBG 5/17/83

WITH REPORT;
PROCEDURE C58004C IS

     USE REPORT;

     I1, I2 : INTEGER := 0;   -- INITIAL VALUE IS IMMATERIAL

     PROCEDURE FACTORIALP (IP1 : IN INTEGER; IP2 : IN OUT INTEGER) IS

     BEGIN
          IF IP1 = 1 THEN
               IP2 := 1;
               RETURN;
          ELSE FACTORIALP (IP1 - 1, IP2);
               IP2 := IP1 * IP2;
               RETURN;
          END IF;

          IP2 := 0;

     END FACTORIALP;

     FUNCTION FACTORIALF (IF1 : INTEGER) RETURN INTEGER IS

     BEGIN
          IF IF1 = 1 THEN RETURN (1);
          END IF;

          RETURN (IF1 * FACTORIALF(IF1 - 1) );

     END FACTORIALF;

BEGIN
     TEST ("C58004C", "CHECK THAT THE RETURN STATEMENT WORKS FOR" &
           " RECURSIVE FUNCTIONS AND PROCEDURES");

     I1 := FACTORIALF (5);

     IF I1 /= 120 THEN
           FAILED ("RETURN STATEMENT IN RECURSIVE FUNCTION NOT " &
                   "WORKING");
     END IF;

     FACTORIALP (5, I2);

     IF I2 = 0 THEN
          FAILED ("RETURN STATEMENT IN RECURSIVE PROCEDURE NOT " &
                  "WORKING");
     ELSIF I2 /= 120 THEN
          FAILED
          ("RETURN STMT IN RECURSIVE PROCEDURE NOT WORKING CORRECTLY");
     END IF;

     RESULT;
END C58004C;
