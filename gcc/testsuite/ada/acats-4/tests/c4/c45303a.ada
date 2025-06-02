-- C45303A.ADA

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
-- CHECK THAT ADDITION AND SUBTRACTION YIELD RESULTS BELONGING TO THE
-- BASE TYPE.

-- JBG 2/24/84
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.
-- JRL 10/13/96 Fixed static expressions which contained values outside
--              the base range.

WITH REPORT; USE REPORT;
PROCEDURE C45303A IS

     TYPE INT IS RANGE 1..10;

     X, Y : INT := INT(IDENT_INT(9));

BEGIN

     TEST ("C45303A", "CHECK SUBTYPE OF INTEGER ADDITION/SUBTRACTION");

     BEGIN

          IF X + Y - 10 /= INT(IDENT_INT(8)) THEN
               FAILED ("INCORRECT RESULT - ADDITION");
          END IF;

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>
               IF INT'POS(INT'BASE'LAST) >= 18 THEN
                    FAILED ("ADDITION DOES NOT YIELD RESULT " &
                            "BELONGING TO THE BASE TYPE");
               ELSE
                    COMMENT ("BASE TYPE HAS RANGE LESS THAN 18 - ADD");
               END IF;
     END;

     BEGIN

          IF 2 - X - INT(IDENT_INT(1)) /= INT'VAL(IDENT_INT(-8)) THEN
               FAILED ("INCORRECT RESULT - SUBTRACTION");
          END IF;

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>
               IF INT'POS(INT'BASE'FIRST) <= -8 THEN
                    FAILED ("SUBTRACTION DOES NOT YIELD RESULT " &
                            "BELONGING TO THE BASE TYPE");
               ELSE
                    COMMENT ("BASE TYPE HAS RANGE GREATER THAN -8 - SUB");
               END IF;
     END;

     RESULT;

END C45303A;
