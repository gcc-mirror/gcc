-- CD3014F.ADA

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
--     CHECK THAT AN ENUMERATION REPRESENTATION CLAUSE CAN BE GIVEN
--     IN THE VISIBLE OR PRIVATE PART OF A GENERIC PACKAGE FOR A
--     TYPE DECLARED IN THE VISIBLE PART.

-- HISTORY
--     DHH 09/30/87  CREATED ORIGINAL TEST
--     DHH 03/29/89  CHANGED FROM 'A' TEST TO 'C' TEST AND FROM '.DEP'
--                   '.ADA'.  ADDED CHECK ON REPRESENTATION CLAUSES.
--     RJW 09/18/89  REMOVED THE COMMENT "-- N/A => ERROR.".

WITH REPORT; USE REPORT;
WITH ENUM_CHECK;                      -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD3014F IS

BEGIN

     TEST ("CD3014F", "CHECK THAT AN ENUMERATION REPRESENTATION " &
                      "CLAUSE CAN BE GIVEN IN THE VISIBLE " &
                      "OR PRIVATE PART OF A GENERIC PACKAGE FOR " &
                      "A TYPE DECLARED IN THE VISIBLE PART");

     DECLARE

          GENERIC
          PACKAGE GENPACK IS

               TYPE HUE IS (RED,BLUE,YELLOW,'R','B','Y');
               TYPE NEWHUE IS (RED,BLUE,YELLOW,'R','B','Y');

               FOR HUE USE (RED => 8, BLUE => 9, YELLOW => 10,
                   'R' => 11, 'B' => 12, 'Y' => 13);
               A : HUE := BLUE;

               TYPE INT1 IS RANGE 8 .. 13;
               FOR INT1'SIZE USE HUE'SIZE;

          PRIVATE

               FOR NEWHUE USE (RED => 2, BLUE => 4, YELLOW => 6,
                   'R' => 8, 'B' => 10, 'Y' => 12);

               B : NEWHUE := RED;
               TYPE INT2 IS RANGE 2 .. 12;
               FOR INT2'SIZE USE NEWHUE'SIZE;

               PROCEDURE CHECK_1 IS NEW ENUM_CHECK(HUE, INT1);
               PROCEDURE CHECK_2 IS NEW ENUM_CHECK(NEWHUE, INT2);
          END GENPACK;

          PACKAGE BODY GENPACK IS
          BEGIN
               CHECK_1 ('B', 12, "HUE");
               CHECK_2 ('B', 10, "NEWHUE");
          END GENPACK;

          PACKAGE P IS NEW GENPACK;

     BEGIN
          NULL;
     END;

     RESULT;
END CD3014F;
