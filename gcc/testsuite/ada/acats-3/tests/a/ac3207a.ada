-- AC3207A.ADA

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
--     CHECK THAT AN INSTANTIATION IS LEGAL IF A FORMAL PARAMETER
--     HAVING A LIMITED PRIVATE TYPE WITHOUT DISCRIMINANTS IS USED TO
--     DECLARE AN OBJECT IN A BLOCK THAT CONTAINS A SELECTIVE WAIT
--     WITH A TERMINATE ALTERNATIVE, AND THE ACTUAL PARAMETER'S BASE
--     TYPE IS A TASK TYPE OR A TYPE WITH A SUBCOMPONENT OF A TASK TYPE.

-- HISTORY:
--     DHH 09/16/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE AC3207A IS

     GENERIC
          TYPE PRIV IS LIMITED PRIVATE;
     PACKAGE GEN_P IS
          TASK T1 IS
               ENTRY E;
          END T1;
     END GEN_P;

     TASK TYPE TASK_T IS
     END TASK_T;

     TYPE REC IS
          RECORD
               OBJ : TASK_T;
          END RECORD;

     PACKAGE BODY GEN_P IS
          TASK BODY T1 IS
          BEGIN
               DECLARE
                    OBJ : PRIV;
               BEGIN
                    SELECT
                         ACCEPT E;
                    OR
                         TERMINATE;
                    END SELECT;
               END;
          END T1;
     END GEN_P;

     TASK BODY TASK_T IS
     BEGIN
          NULL;
     END;

     PACKAGE P IS NEW GEN_P(TASK_T);
     PACKAGE NEW_P IS NEW GEN_P(REC);

BEGIN
     TEST ("AC3207A", "CHECK THAT AN INSTANTIATION IS LEGAL IF A " &
                      "FORMAL PARAMETER HAVING A LIMITED PRIVATE " &
                      "TYPE WITHOUT DISCRIMINANTS IS USED TO " &
                      "DECLARE AN OBJECT IN A BLOCK THAT CONTAINS " &
                      "A SELECTIVE WAIT WITH A TERMINATE " &
                      "ALTERNATIVE, AND THE ACTUAL PARAMETER'S BASE " &
                      "TYPE IS A TASK TYPE OR A TYPE WITH A " &
                      "SUBCOMPONENT OF A TASK TYPE");

     P.T1.E;

     NEW_P.T1.E;

     RESULT;
END AC3207A;
