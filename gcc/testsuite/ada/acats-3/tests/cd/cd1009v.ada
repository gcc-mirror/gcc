-- CD1009V.ADA

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
--     CHECK THAT AN ENUMERATION REPRESENTATION CLAUSE MAY BE GIVEN IN
--     THE PRIVATE PART OF A PACKAGE FOR AN INCOMPLETE TYPE, WHOSE
--     FULL TYPE DECLARATION IS AN ENUMERATION TYPE DECLARED IN THE
--     VISIBLE PART OF THE SAME PACKAGE.

-- HISTORY:
--     VCL  10/21/87  CREATED ORIGINAL TEST.
--     DHH  03/29/89  CHANGE FROM 'A' TEST TO 'C' TEST AND FROM '.DEP'
--                    '.ADA'.  ADDED CHECK ON REPRESENTATION CLAUSES.

WITH REPORT; USE REPORT;
WITH ENUM_CHECK;                        -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD1009V IS
BEGIN
     TEST ("CD1009V", "AN ENUMERATION REPRESENTATION CLAUSE MAY BE " &
                      "GIVEN IN THE PRIVATE PART OF A " &
                      "PACKAGE FOR AN INCOMPLETE TYPE, WHOSE FULL " &
                      "TYPE DECLARATION IS AN ENUMERATION TYPE, " &
                      "DECLARED IN THE VISIBLE PART OF THE SAME " &
                      "PACKAGE");
     DECLARE
          PACKAGE PACK IS
               TYPE CHECK_TYPE_1;
               TYPE ACC IS ACCESS CHECK_TYPE_1;

               TYPE CHECK_TYPE_1 IS (A0, A2, A4, A8);
          PRIVATE

               FOR CHECK_TYPE_1 USE (A0 => 9,
                                     A2 => 13,
                                     A4 => 15,
                                     A8 => 18);
               TYPE INT1 IS RANGE 9 .. 18;
               FOR INT1'SIZE USE CHECK_TYPE_1'SIZE;

               PROCEDURE CHECK_1 IS NEW ENUM_CHECK(CHECK_TYPE_1, INT1);

          END PACK;

          PACKAGE BODY PACK IS
          BEGIN
               CHECK_1 (A2, 13, "CHECK_TYPE_1");
          END PACK;

          USE PACK;
     BEGIN
          NULL;
     END;

     RESULT;
END CD1009V;
