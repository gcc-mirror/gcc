-- CD1009U.TST

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
--     CHECK THAT A 'STORAGE_SIZE' CLAUSE MAY BE GIVEN IN THE PRIVATE
--     PART OF A PACKAGE FOR A LIMITED PRIVATE TYPE, WHOSE FULL TYPE
--     DECLARATION IS A TASK TYPE, DECLARED IN THE VISIBLE PART OF THE
--     SAME PACKAGE.

-- MACRO SUBSTITUTION:
--     $TASK_STORAGE_SIZE IS THE NUMBER OF STORAGE_UNITS REQUIRED FOR
--     THE ACTIVATION OF A TASK.

-- HISTORY:
--     VCL  10/09/87  CREATED ORIGINAL TEST.
--     DHH  03/30/89  CHANGED SPECIFIED_SIZE TO A MACRO VALUE AND CHANGED
--                    EXTENSION FROM '.DEP' TO '.TST'.

WITH REPORT; USE REPORT;
PROCEDURE CD1009U IS
BEGIN
     TEST ("CD1009U", "A 'STORAGE_SIZE' CLAUSE MAY BE GIVEN IN THE " &
                      "PRIVATE PART OF A PACKAGE FOR A LIMITED " &
                      "PRIVATE TYPE, WHOSE FULL TYPE DECLARATION IS " &
                      "A TASK TYPE, DECLARED IN THE VISIBLE PART OF " &
                      "THE SAME PACKAGE");
     DECLARE
          PACKAGE PACK IS
               SPECIFIED_SIZE : CONSTANT := $TASK_STORAGE_SIZE;

               TYPE CHECK_TYPE_1 IS LIMITED PRIVATE;

               PROCEDURE P;
          PRIVATE
               TASK TYPE CHECK_TYPE_1 IS
               END CHECK_TYPE_1;

               FOR CHECK_TYPE_1'STORAGE_SIZE USE SPECIFIED_SIZE;
          END PACK;

          PACKAGE BODY PACK IS
               PROCEDURE P IS
               BEGIN
                    IF CHECK_TYPE_1'STORAGE_SIZE < SPECIFIED_SIZE THEN
                         FAILED ("CHECK_TYPE_1'STORAGE_SIZE IS TOO " &
                                 "SMALL");
                    END IF;
               END P;

               TASK BODY CHECK_TYPE_1 IS
                    I : INTEGER;
               BEGIN
                    NULL;
               END CHECK_TYPE_1;
          END PACK;

          USE PACK;
     BEGIN
          P;
     END;

     RESULT;
END CD1009U;
