-- C35502G.ADA

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
-- CHECK THAT 'PRED' AND 'SUCC' YIELD THE CORRECT RESULTS WHEN 
-- THE PREFIX IS AN ENUMERATION TYPE OTHER THAN A BOOLEAN OR A 
-- CHARACTER TYPE.

-- RJW 5/27/86

WITH REPORT; USE REPORT;

PROCEDURE C35502G IS

          TYPE ENUM IS (A, BC, ABC, A_B_C, ABCD);
          SUBTYPE SUBENUM IS ENUM RANGE A .. BC;

          TYPE NEWENUM IS NEW ENUM;
          SUBTYPE SUBNEW IS NEWENUM RANGE A .. BC;

BEGIN
     TEST ("C35502G", "CHECK THAT 'PRED' AND 'SUCC' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS AN " &
                      "ENUMERATION TYPE OTHER THAN A CHARACTER " &
                      "OR A BOOLEAN TYPE" );

     BEGIN
          FOR I IN ENUM'VAL (1) .. ENUM'VAL (4) LOOP
               IF SUBENUM'PRED (I) /= 
                  ENUM'VAL (ENUM'POS (I) - 1) THEN
                    FAILED ("INCORRECT SUBENUM'PRED(" &
                             ENUM'IMAGE (I) & ")" );
               END IF;
          END LOOP;

          FOR I IN ENUM'VAL (0) .. ENUM'VAL (3) LOOP
               IF SUBENUM'SUCC (I) /= 
                  ENUM'VAL (ENUM'POS (I) + 1) THEN
                    FAILED ("INCORRECT SUBENUM'SUCC(" &
                             ENUM'IMAGE (I) & ")" );
               END IF;
          END LOOP;
     END;

     BEGIN
          FOR I IN NEWENUM'VAL (1) .. NEWENUM'VAL (4) LOOP
               IF SUBNEW'PRED (I) /= 
                  NEWENUM'VAL (NEWENUM'POS (I) - 1) THEN
                    FAILED ("INCORRECT SUBNEW'PRED(" &
                             NEWENUM'IMAGE (I) & ")" );
               END IF;
          END LOOP;

          FOR I IN NEWENUM'VAL (0) .. NEWENUM'VAL (3) LOOP
               IF SUBNEW'SUCC (I) /= 
                  NEWENUM'VAL (NEWENUM'POS (I) + 1) THEN
                    FAILED ("INCORRECT SUBNEW'SUCC(" &
                             NEWENUM'IMAGE (I) & ")" );
               END IF;
          END LOOP;
     END;

     RESULT;
END C35502G;
