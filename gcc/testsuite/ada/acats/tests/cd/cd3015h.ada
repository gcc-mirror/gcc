-- CD3015H.ADA

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
-- OBJECTIVE:
--     CHECK THAT AN ENUMERATION REPRESENTATION CLAUSE FOR A DERIVED
--     TYPE CAN BE GIVEN IN THE VISIBLE OR PRIVATE PART OF A PACKAGE
--     FOR A DERIVED TYPE DECLARED IN THE VISIBLE PART, WHERE AN
--     ENUMERATION CLAUSE HAS BEEN GIVEN FOR THE PARENT.

-- HISTORY
--     DHH  10/01/87  CREATED ORIGINAL TEST
--     DHH  03/29/89  CHANGE FROM 'A' TEST TO 'C' TEST AND FROM '.DEP'
--                    '.ADA'.  ADDED CHECK ON REPRESENTATION CLAUSES.

WITH REPORT; USE REPORT;
WITH ENUM_CHECK;                      -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD3015H IS

BEGIN

     TEST ("CD3015H", "CHECK THAT AN ENUMERATION " &
                      "REPRESENTATION CLAUSE FOR A DERIVED TYPE CAN " &
                      "BE GIVEN IN THE VISIBLE OR PRIVATE PART OF A " &
                      "PACKAGE FOR A DERIVED TYPE DECLARED IN THE " &
                      "VISIBLE PART, WHERE AN ENUMERATION CLAUSE HAS " &
                      "BEEN GIVEN FOR THE PARENT");

     DECLARE
          PACKAGE PACK IS

               TYPE MAIN IS (RED,BLUE,YELLOW);
               FOR MAIN USE (RED => 1, BLUE => 2, YELLOW => 3);

               TYPE HUE IS NEW MAIN;
               TYPE NEWHUE IS NEW MAIN;

               FOR HUE USE
                         (RED => 8, BLUE => 9, YELLOW => 10);

          PRIVATE

               FOR NEWHUE USE (RED => 6, BLUE => 11, YELLOW => 18);

               TYPE INT1 IS RANGE 8 .. 10;
               FOR INT1'SIZE USE HUE'SIZE;

               TYPE INT2 IS RANGE 6 .. 18;
               FOR INT2'SIZE USE NEWHUE'SIZE;

               PROCEDURE CHECK_1 IS NEW ENUM_CHECK(HUE, INT1);
               PROCEDURE CHECK_2 IS NEW ENUM_CHECK(NEWHUE, INT2);

          END PACK;

          PACKAGE BODY PACK IS
          BEGIN
               CHECK_1 (RED, 8, "HUE");
               CHECK_2 (YELLOW, 18, "NEWHUE");
          END PACK;
     BEGIN
          NULL;
     END;

     RESULT;
END CD3015H;
