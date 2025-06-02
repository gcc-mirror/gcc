-- C37306A.ADA

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
-- CHECK THAT IN A VARIANT PART OF A RECORD THE CHOICES WITHIN AND
-- BETWEEN ALTERNATIVES CAN APPEAR IN NON-MONOTONIC ORDER.

-- ASL 7/13/81
-- JWC 6/28/85   RENAMED TO -AB

WITH REPORT;
PROCEDURE C37306A IS

     USE REPORT;

BEGIN
     TEST ("C37306A","NON-MONOTONIC ORDER OF CHOICES IN VARIANT PARTS");

     DECLARE
          TYPE COLOR IS (WHITE,RED,ORANGE,YELLOW,GREEN,AQUA,BLUE,BLACK);

          TYPE REC(DISC : COLOR := BLUE) IS
               RECORD
                    CASE DISC IS
                         WHEN ORANGE => NULL;
                         WHEN GREEN | WHITE | BLACK => NULL;
                         WHEN YELLOW => NULL;
                         WHEN BLUE | RED => NULL;
                         WHEN OTHERS => NULL;
                    END CASE;
               END RECORD;

          R : REC;
     BEGIN
          R := (DISC => WHITE);

          IF EQUAL(3,4) THEN
               R := (DISC => RED);
          END IF;

          IF R.DISC /= WHITE THEN
               FAILED ("ASSIGNMENT FAILED");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED");
     END;

     RESULT;
END C37306A;
