-- C39008B.ADA

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
-- CHECK THAT IF THE ACTIVATION OF A TASK IS ATTEMPTED BEFORE THE
-- ELABORATION OF THE CORRESPONDING BODY IS FINISHED, THE EXCEPTION
-- PROGRAM_ERROR IS RAISED, NOT TASKING_ERROR (SEE AI-00149).

-- WEI  3/04/82
-- JBG  2/17/84
-- EG  11/02/84
-- JBG 5/23/85
-- JWC 6/28/85   RENAMED FROM C93007B-B.ADA

WITH REPORT;
 USE REPORT;

PROCEDURE C39008B IS

BEGIN

     TEST ("C39008B", "PROGRAM_ERROR AFTER ATTEMPT OF ACTIVATION " &
                      "BEFORE ELABORATION");
BLOCK1:
     BEGIN
BLOCK2:
          DECLARE
               TASK TYPE TT1;

               TYPE ATT1 IS ACCESS TT1;

               POINTER_TT1 : ATT1 := NEW TT1;  -- ACCESSING TASK BODY
                                               -- BEFORE ITS ELABORATION

               TASK BODY TT1 IS
               BEGIN
                    FAILED ("TT1 ACTIVATED");
               END TT1;

          BEGIN

               FAILED ("TT1 ACTIVATED - 2");

          END BLOCK2;

     EXCEPTION
          WHEN TASKING_ERROR =>
               FAILED ("TASKING_ERROR RAISED");
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED");
     END BLOCK1;

     RESULT;

END C39008B;
