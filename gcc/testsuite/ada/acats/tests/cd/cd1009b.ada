-- CD1009B.ADA

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
--     CHECK THAT A 'SIZE CLAUSE MAY BE GIVEN IN THE VISIBLE
--     OR PRIVATE PART OF A PACKAGE FOR AN ENUMERATION TYPE DECLARED
--     IN THE VISIBLE PART OF THE SAME PACKAGE.

-- HISTORY:
--     PWB 03/25/89  MODIFIED METHOD OF CHECKING OBJECT SIZE AGAINST
--                   TYPE SIZE; CHANGED EXTENSION FROM '.ADA' TO '.DEP'.
--     VCL 10/07/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE CD1009B IS
BEGIN
     TEST ("CD1009B", "A 'SIZE CLAUSE MAY BE GIVEN IN THE VISIBLE " &
                      "OR PRIVATE PART OF A PACKAGE FOR AN " &
                      "ENUMERATION TYPE DECLARED IN THE VISIBLE " &
                      "PART OF THE SAME PACKAGE");
     DECLARE
          PACKAGE PACK IS
               SPECIFIED_SIZE : CONSTANT := INTEGER'SIZE / 2;

               TYPE CHECK_TYPE_1 IS (A0, A1, A2, A3);
               FOR CHECK_TYPE_1'SIZE
                              USE SPECIFIED_SIZE;

               TYPE CHECK_TYPE_2 IS (A0, A1, A2, A3);
          PRIVATE
               FOR CHECK_TYPE_2'SIZE USE SPECIFIED_SIZE;
          END PACK;

          USE PACK;
          X : CHECK_TYPE_1 := A0;
          Y : CHECK_TYPE_2 := A2;
     BEGIN
          IF CHECK_TYPE_1'SIZE /= SPECIFIED_SIZE THEN
               FAILED ("CHECK_TYPE_1'SIZE IS INCORRECT");
          END IF;

          IF CHECK_TYPE_2'SIZE /= SPECIFIED_SIZE THEN
               FAILED ("CHECK_TYPE_2'SIZE IS INCORRECT");
          END IF;

          IF X'SIZE < SPECIFIED_SIZE THEN
               FAILED ("OBJECT'SIZE IS TOO SMALL --" &
                       CHECK_TYPE_1'IMAGE(X));
          END IF;

          IF Y'SIZE < SPECIFIED_SIZE THEN
               FAILED ("OBJECT'SIZE IS TOO SMALL --" &
                       CHECK_TYPE_2'IMAGE(Y));
          END IF;

     END;

     RESULT;
END CD1009B;
