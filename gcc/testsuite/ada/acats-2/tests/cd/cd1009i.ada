-- CD1009I.ADA

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
--     CHECK THAT A 'SIZE' CLAUSE MAY BE GIVEN IN THE PRIVATE
--     PART OF A PACKAGE FOR A LIMITED-PRIVATE TYPE DECLARED IN THE
--     VISIBLE PART OF THE SAME PACKAGE.

-- HISTORY:
--     VCL  09/18/87 CREATED ORIGINAL TEST.
--     DHH  03/30/89 CHANGED SPECIFIED_SIZE TO 5, ADDED CHECK FOR
--                   REPRESENTATION CLAUSES AND CHANGED THE TEST
--                   EXTENSION FROM '.DEP' TO '.ADA'.

WITH REPORT; USE REPORT;
WITH LENGTH_CHECK;                      -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD1009I IS
BEGIN
     TEST ("CD1009I", "A 'SIZE' CLAUSE MAY BE GIVEN IN THE " &
                      "PRIVATE PART OF A PACKAGE FOR A LIMITED-" &
                      "PRIVATE TYPE DECLARED IN THE VISIBLE PART " &
                      "OF THE SAME PACKAGE");
     DECLARE
          PACKAGE PACK IS
               SPECIFIED_SIZE : CONSTANT := 5;

               TYPE CHECK_TYPE_1 IS LIMITED PRIVATE;
          PRIVATE
               TYPE CHECK_TYPE_1 IS RANGE -8 .. 7;
               FOR CHECK_TYPE_1'SIZE USE SPECIFIED_SIZE;
               OBJ_CHECK : CHECK_TYPE_1 := -7;
               PROCEDURE CHECK_1 IS NEW LENGTH_CHECK (CHECK_TYPE_1);
          END PACK;

          PACKAGE BODY PACK IS
          BEGIN
               CHECK_1 (OBJ_CHECK, 5, "CHECK_TYPE_1");
          END PACK;

          USE PACK;
     BEGIN
          IF CHECK_TYPE_1'SIZE /= SPECIFIED_SIZE THEN
               FAILED ("CHECK_TYPE_1'SIZE /= SPECIFIED_SIZE");
          END IF;
     END;

     RESULT;
END CD1009I;
