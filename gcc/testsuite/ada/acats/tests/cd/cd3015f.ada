-- CD3015F.ADA

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
--     TYPE CAN BE GIVEN IN THE VISIBLE OR PRIVATE PART OF A GENERIC
--     PACKAGE FOR A DERIVED TYPE DECLARED IN THE VISIBLE PART, WHERE
--     NO ENUMERATION CLAUSE HAS BEEN GIVEN FOR THE PARENT.

-- HISTORY
--     DHH 10/01/87 CREATED ORIGINAL TEST
--     DHH 03/27/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA',CHANGED
--                   FROM 'A' TEST TO 'C' TEST AND ADDED CHECK FOR
--                   REPRESENTATION CLAUSE.

WITH REPORT; USE REPORT;
WITH ENUM_CHECK;            -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD3015F IS

BEGIN

     TEST ("CD3015F", "CHECK THAT AN " &
                      "ENUMERATION REPRESENTATION CLAUSE FOR A " &
                      "DERIVED TYPE CAN BE GIVEN IN THE VISIBLE OR " &
                      "PRIVATE PART OF A GENERIC PACKAGE FOR A " &
                      "DERIVED TYPE DECLARED IN THE VISIBLE PART, " &
                      "WHERE NO ENUMERATION CLAUSE HAS BEEN GIVEN " &
                      "FOR THE PARENT");

     DECLARE

          GENERIC
          PACKAGE GENPACK IS

               TYPE MAIN IS (RED,BLUE,YELLOW,'R','B','Y');

               TYPE HUE IS NEW MAIN;
               TYPE NEWHUE IS NEW MAIN;

               FOR HUE USE (RED => 8, BLUE => 9, YELLOW => 10,
                     'R' => 11, 'B' => 12, 'Y' => 13);

          PRIVATE
               FOR NEWHUE USE (RED => 8, BLUE => 9, YELLOW => 10,
                     'R' => 11, 'B' => 12, 'Y' => 13);

               TYPE INT_HUE IS RANGE 8 .. 13;
               FOR INT_HUE'SIZE USE HUE'SIZE;

               TYPE INT_NEW IS RANGE 8 .. 13;
               FOR INT_NEW'SIZE USE NEWHUE'SIZE;

               PROCEDURE CHECK_HUE IS NEW ENUM_CHECK(HUE, INT_HUE);
               PROCEDURE CHECK_NEW IS NEW ENUM_CHECK(NEWHUE, INT_NEW);

          END GENPACK;

          PACKAGE BODY GENPACK IS

          BEGIN
               CHECK_HUE (RED, 8, "HUE");
               CHECK_HUE ('R', 11, "NEWHUE");
          END GENPACK;

          PACKAGE P IS NEW GENPACK;

     BEGIN
          NULL;
     END;

     RESULT;
END CD3015F;
