-- C54A22A.ADA

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
-- CHECK ALL FORMS OF CHOICE IN CASE CHOICES.

-- DAT 1/29/81
-- SPS 1/21/83

WITH REPORT;
PROCEDURE C54A22A IS

     USE REPORT;

     TYPE T IS RANGE 1 .. 10;
     C5 : CONSTANT T := 5;
     SUBTYPE S1 IS T RANGE 1 .. 5;
     SUBTYPE S2 IS T RANGE C5 + 1 .. 7;
     SUBTYPE SN IS T RANGE C5 + 4 .. C5 - 4 + 7;  -- NULL RANGE.
     SUBTYPE S10 IS T RANGE C5 + 5 .. T'LAST;

BEGIN
     TEST ("C54A22A", "CHECK ALL FORMS OF CASE CHOICES");

     CASE T'(C5 + 3) IS
          WHEN SN                       -- 9..8
          | S1 RANGE 1 .. 0             -- 1..0
          | S2 RANGE C5 + 2 .. C5 + 1   -- 7..6
          | 3 .. 2                      -- 3..2
               => FAILED ("WRONG CASE 1");

          WHEN  S1 RANGE 4 .. C5        -- 4..5
          | S1 RANGE C5 - 4 .. C5 / 2   -- 1..2
          | 3 .. 1 + C5 MOD 3           -- 3..3
          | SN                          -- 9..8
          | S1 RANGE 5 .. C5 - 1        -- 5..4
          | 6 .. 7                      -- 6..7
          | S10                         -- 10..10
          | 9                           -- 9
          | S10 RANGE 10 .. 9 =>        -- 10..9
               FAILED ("WRONG CASE 2");

          WHEN C5 + C5 - 2 .. 8         -- 8
               => NULL;
     END CASE;

     RESULT;
END C54A22A;
