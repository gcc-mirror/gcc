-- CC3236A.ADA

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
--      CHECK THAT A FORMAL PRIVATE AND LIMITED PRIVATE TYPE DENOTES ITS
--      ACTUAL PARAMETER, AND OPERATIONS OF THE FORMAL TYPE ARE
--      IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE ACTUAL TYPE
--      WHEN THE ACTUAL PARAMETER IS A TYPE WITH DISCRIMINANTS.

-- HISTORY:
--      DHH 10/24/88 CREATED ORIGINAL TEST.
--      PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;
PROCEDURE CC3236A IS

     GENERIC
          TYPE T IS PRIVATE;
     PACKAGE P IS
          SUBTYPE SUB_T IS T;
          PAC_VAR : T;
     END P;

     GENERIC
          TYPE T IS LIMITED PRIVATE;
     PACKAGE LP IS
          SUBTYPE SUB_T IS T;
          PAC_VAR : T;
     END LP;

BEGIN
     TEST ("CC3236A", "CHECK THAT A FORMAL PRIVATE OR LIMITED " &
                      "PRIVATE TYPE DENOTES ITS ACTUAL PARAMETER AND " &
                      "OPERATIONS OF THE FORMAL TYPE ARE IDENTIFIED " &
                      "WITH CORRESPONDING OPERATIONS OF THE ACTUAL " &
                      "TYPE, WHEN THE ACTUAL PARAMETER IS A TYPE " &
                      "WITH DISCRIMINANTS");

     DECLARE
          TYPE REC(X : INTEGER := 5) IS
               RECORD
                    NULL;
               END RECORD;
          OBJ_REC : REC(4);

          PACKAGE P2 IS NEW P (REC);
          USE P2;

          TYPE NEW_T IS NEW SUB_T;
          OBJ_NEWT : NEW_T(4);
     BEGIN
          PAC_VAR := SUB_T'((X => 4));
          IF PAC_VAR /= OBJ_REC THEN
               FAILED ("INCORRECT RESULTS - 1");
          END IF;
          IF PAC_VAR NOT IN REC THEN
               FAILED ("INCORRECT RESULTS - 2");
          END IF;
          IF OBJ_REC NOT IN SUB_T THEN
               FAILED ("INCORRECT RESULTS - 3");
          END IF;
          IF PAC_VAR.X /= OBJ_NEWT.X THEN
               FAILED ("INCORRECT RESULTS - 4");
          END IF;
     END;

     DECLARE
          TYPE REC(X : INTEGER := 5) IS
               RECORD
                    NULL;
               END RECORD;
          OBJ_REC : REC(4);

          PACKAGE P2 IS NEW LP (REC);
          USE P2;

          TYPE NEW_T IS NEW SUB_T;
          OBJ_NEWT : NEW_T(4);
     BEGIN
          PAC_VAR := SUB_T'(X => 4);
          IF PAC_VAR /= OBJ_REC THEN
               FAILED ("INCORRECT RESULTS - 7");
          END IF;
          IF PAC_VAR NOT IN REC THEN
               FAILED ("INCORRECT RESULTS - 8");
          END IF;
          IF OBJ_REC NOT IN SUB_T THEN
               FAILED ("INCORRECT RESULTS - 9");
          END IF;
          IF PAC_VAR.X /= OBJ_NEWT.X THEN
               FAILED ("INCORRECT RESULTS - 10");
          END IF;
     END;

     RESULT;
END CC3236A;
