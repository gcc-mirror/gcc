-- C85019A.ADA

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
-- CHECK THAT A CHARACTER OR OTHER ENUMERATION LITERAL MAY BE RENAMED 
-- AS A FUNCTION.

-- RJW 6/4/86

WITH REPORT; USE REPORT;

PROCEDURE C85019A IS

BEGIN

     TEST( "C85019A", "CHECK THAT A CHARACTER OR OTHER ENUMERATION " &
                      "LITERAL MAY BE RENAMED AS A FUNCTION" );

     DECLARE
          FUNCTION SEA RETURN CHARACTER RENAMES 'C';

          TYPE COLOR IS (RED, YELLOW, BLUE, GREEN);
          
          FUNCTION TEAL RETURN COLOR RENAMES BLUE;

     BEGIN
          IF SEA /= 'C' THEN
               FAILED ( "SEA IS NOT EQUAL TO 'C'" );
          END IF;

          IF TEAL /= BLUE THEN
               FAILED ( "TEAL IS NOT EQUAL TO BLUE" );
          END IF;

     END;

     RESULT;

END C85019A;
