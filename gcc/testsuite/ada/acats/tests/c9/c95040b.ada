-- C95040B.ADA

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
-- CHECK THAT THE EXCEPTION TASKING_ERROR IS RAISED BY A TASK IF THE
-- TASK BECOMES COMPLETED OR ABNORMAL BEFORE ACCEPTING THE CALL.

-- WEI  3/ 4/82
-- TLB 10/30/87  RENAMED FROM C950CHC.ADA.

with Impdef;
WITH REPORT;
 USE REPORT;
PROCEDURE C95040B IS

     TASK T1 IS
          ENTRY E1;
     END T1;

     TASK BODY T1 IS
     BEGIN
          DELAY 1.0 * Impdef.One_Second;
          IF EQUAL (1, 1) THEN
               ABORT T1;
          END IF;
          ACCEPT E1;
     END T1;

BEGIN

     TEST ("C95040B", "TASK COMPLETION BEFORE ACCEPTING AN ENTRY CALL");

     T1.E1;

     FAILED ("NO EXCEPTION TASKING_ERROR RAISED");

     RESULT;

EXCEPTION
     WHEN TASKING_ERROR =>
          RESULT;

END C95040B;
