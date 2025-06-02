-- CD3014C.ADA

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
--     CHECK THAT AN ENUMERATION REPRESENTATION CLAUSE CAN BE GIVEN IN
--     THE VISIBLE OR PRIVATE PART OF A PACKAGE FOR A TYPE DECLARED IN
--     THE VISIBLE PART.

-- HISTORY
--     DHH 09/30/87 CREATED ORIGINAL TEST
--     DHH 03/27/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA',CHANGED
--                   FROM 'A' TEST TO 'C' TEST AND ADDED CHECK FOR
--                   REPRESENTATION CLAUSE.

WITH REPORT; USE REPORT;
WITH ENUM_CHECK;        -- CONTAINS CALL TO 'FAILED'
PROCEDURE CD3014C IS

BEGIN

     TEST ("CD3014C", "CHECK THAT AN ENUMERATION " &
                      "REPRESENTATION CLAUSE CAN BE GIVEN IN THE " &
                      "VISIBLE OR PRIVATE PART OF A PACKAGE FOR " &
                      "A TYPE DECLARED IN THE VISIBLE PART");

     DECLARE
          PACKAGE PACK IS

               TYPE HUE IS (RED,BLUE,YELLOW);
               TYPE NEWHUE IS (RED,BLUE,YELLOW);

               FOR HUE USE
                         (RED => 8, BLUE => 16,
                                      YELLOW => 32);
               A : HUE := BLUE;
          PRIVATE

               FOR NEWHUE USE (RED => 8, BLUE => 16, YELLOW => 32);

               B : NEWHUE := RED;

               TYPE INT_HUE IS RANGE 8 .. 32;
               FOR INT_HUE'SIZE USE HUE'SIZE;

               TYPE INT_NEW IS RANGE 8 .. 32;
               FOR INT_NEW'SIZE USE NEWHUE'SIZE;

               PROCEDURE CHECK_HUE IS NEW ENUM_CHECK(HUE, INT_HUE);
               PROCEDURE CHECK_NEW IS NEW ENUM_CHECK(NEWHUE, INT_NEW);

          END PACK;

          PACKAGE BODY PACK IS
          BEGIN
               CHECK_HUE (RED, 8, "HUE");
               CHECK_NEW (YELLOW, 32, "NEWHUE");
          END PACK;

     BEGIN
          NULL;
     END;

     RESULT;
END CD3014C;
