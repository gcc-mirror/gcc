-- C35504B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED FOR I'SUCC, I'PRED,
-- I'POS, I'VAL, I'IMAGE, AND I'VALUE FOR INTEGER ARGUMENTS
-- OUTSIDE THE RANGE OF I.

-- DAT 3/30/81
-- SPS 01/13/83

WITH REPORT;
USE REPORT;

PROCEDURE C35504B IS

     SUBTYPE I IS INTEGER RANGE 0 .. 0;

BEGIN
     TEST ("C35504B", "CONSTRAINT_ERROR IS NOT RAISED FOR"
          & " INTEGER SUBTYPE ATTRIBUTES 'SUCC, 'PRED, 'POS, 'VAL,"
          & " 'IMAGE, AND 'VALUE WHOSE ARGUMENTS ARE OUTSIDE THE"
          & " SUBTYPE");

     BEGIN
          IF I'SUCC(-1) /= I'PRED(1)
          THEN
               FAILED ("WRONG ATTRIBUTE VALUE - 1");
          END IF;

          IF I'SUCC (100) /= 101
          THEN
               FAILED ("WRONG ATTRIBUTE VALUE - 2");
          END IF;

          IF I'PRED (100) /= 99
          THEN
               FAILED ("WRONG ATTRIBUTE VALUE - 3");
          END IF;

          IF I'POS (-100) /= -100
          THEN
               FAILED ("WRONG ATTRIBUTE VALUE - 4");
          END IF;

          IF I'VAL(-100) /= -100
          THEN
               FAILED ("WRONG ATTRIBUTE VALUE - 5");
          END IF;

          IF I'IMAGE(1234) /= " 1234"
          THEN
               FAILED ("WRONG ATTRIBUTE VALUE - 6");
          END IF;

          IF I'VALUE("999") /= 999
          THEN
               FAILED ("WRONG ATTRIBUTE VALUE - 7");
          END IF;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED");
     END;

     RESULT;
END C35504B;
