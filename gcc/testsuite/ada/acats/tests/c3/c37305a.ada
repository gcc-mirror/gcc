-- C37305A.ADA

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
-- CHECK THAT CHOICES DENOTING A NULL RANGE OF VALUES ARE PERMITTED,
-- AND THAT FOR CHOICES CONSISTING OF A SUBTYPE NAME FOLLOWED BY A
-- RANGE CONSTRAINT WHERE THE LOWER BOUND IS GREATER THAN THE UPPER
-- BOUND, THE BOUNDS NEED NOT BE IN THE RANGE OF THE SUBTYPE VALUES.

-- CHECK THAT AN OTHERS ALTERNATIVE CAN BE PROVIDED EVEN IF ALL VALUES
-- OF THE CASE EXPRESSION HAVE BEEN COVERED BY PRECEDING ALTERNATIVES.

-- ASL 7/14/81
-- JWC 6/28/85   RENAMED TO -AB

WITH REPORT;
PROCEDURE C37305A IS

     USE REPORT;

BEGIN
     TEST ("C37305A","NULL RANGES ALLOWED IN CHOICES FOR VARIANT " &
           "PARTS.  OTHERS ALTERNATIVE ALLOWED AFTER ALL VALUES " &
           "PREVIOUSLY COVERED");

     DECLARE
          SUBTYPE ST IS INTEGER RANGE 1..10;

          TYPE REC(DISC : ST := 1) IS
               RECORD
                    CASE DISC IS
                         WHEN 0..-1 => NULL;
                         WHEN 1..-3 => NULL;
                         WHEN 6..5 =>
                              COMP : INTEGER;
                         WHEN 11..10 => NULL;
                         WHEN 15..12 => NULL;
                         WHEN 11..0 => NULL;
                         WHEN 1..10 => NULL;
                         WHEN OTHERS => NULL;
                    END CASE;
               END RECORD;

          R : REC;
     BEGIN
          R := (DISC => 4);

          IF EQUAL(3,4) THEN
               R := (DISC => 7);
          END IF;

          IF R.DISC /= 4 THEN
               FAILED ("ASSIGNMENT FAILED");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED");
     END;

     RESULT;

END C37305A;
