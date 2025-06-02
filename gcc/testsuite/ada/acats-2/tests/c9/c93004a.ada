-- C93004A.ADA

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
-- CHECK THAT A TASK BECOMES COMPLETED WHEN AN EXCEPTION OCCURS DURING
-- ITS ACTIVATION.

-- WEI  3/ 4/82

WITH REPORT;
 USE REPORT;
PROCEDURE C93004A IS
BEGIN

     TEST ("C93004A", "TASK COMPLETION CAUSED BY EXCEPTION");

BLOCK:
     DECLARE
          TYPE I0 IS RANGE 0..1;

          TASK T1 IS
               ENTRY BYE;
          END T1;

          TASK BODY T1 IS
               SUBTYPE I1 IS I0 RANGE 0 .. 2;     -- CONSTRAINT ERROR.
          BEGIN
               ACCEPT BYE;
          END T1;
     BEGIN
          FAILED ("NO EXCEPTION RAISED");
          IF NOT T1'TERMINATED THEN 
               FAILED ("TASK NOT TERMINATED");
               T1.BYE;
          END IF;
     EXCEPTION
          WHEN TASKING_ERROR =>
               NULL;
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED");
     END BLOCK;

     RESULT;

END C93004A;
