-- CE3907A.ADA

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
-- CHECK THAT PUT FOR ENUMERATION TYPES CAN BE APPLIED TO A STRING.
-- CHECK THAT IT RAISES LAYOUT_ERROR WHEN THE ENUMERATION LITERAL TO BE
-- PLACED IN THE STRING IS LONGER THAN THE STRING.

-- SPS 10/11/82
-- JBG 2/22/84  CHANGED TO .ADA TEST 

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3907A IS
BEGIN

     TEST ("CE3907A", "CHECK THAT ENUMERATION_IO PUT OPERATES ON " &
                      "STRINGS CORRECTLY");

     DECLARE
          TYPE COLOR IS (RED, BLUE, GREEN);
          ST : STRING (1..4);
          PACKAGE COLOR_IO IS NEW ENUMERATION_IO (COLOR);
          USE COLOR_IO;
          CRAYON : COLOR := GREEN;
     BEGIN
          PUT (ST, RED);
          IF ST /= "RED " THEN
               FAILED ("PUT TO STRING, LENGTH LESS THAN STRING " &
                       "INCORRECT");
          END IF;

          PUT (ST, BLUE);
          IF ST /= "BLUE" THEN
               FAILED ("PUT TO STRING, LENGTH EQUAL TO STRING " &
                       "INCORRECT");
          END IF;

          BEGIN
               PUT (ST, CRAYON);
               FAILED ("LAYOUT_ERROR NOT RAISED");
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED");
          END;

     END;

     RESULT;
END CE3907A;
