-- CD5011G.ADA

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
--     CHECK THAT AN ADDRESS CLAUSE CAN BE GIVEN FOR A VARIABLE OF A
--     FIXED POINT TYPE IN THE DECLARATIVE PART OF A SUBPROGRAM.

-- HISTORY:
--     JET 09/11/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
WITH SPPRT13;

PROCEDURE CD5011G IS

     TYPE FIX_TYPE IS DELTA 0.125 RANGE 0.0 .. 10.0;

     PROCEDURE CD5011G_PROC IS

          FP : FIX_TYPE := 2.0;
          FOR FP USE
               AT SPPRT13.VARIABLE_ADDRESS;

     BEGIN
          IF EQUAL (3, 3) THEN
               FP := 3.0;
          END IF;

          IF FP /= 3.0 THEN
               FAILED ("INCORRECT VALUE FOR VARIABLE IN PROCEDURE");
          END IF;

          IF FP'ADDRESS /= SPPRT13.VARIABLE_ADDRESS THEN
               FAILED ("INCORRECT ADDRESS FOR VARIABLE IN PROCEDURE");
          END IF;

     END CD5011G_PROC;

BEGIN
     TEST ("CD5011G", "AN ADDRESS CLAUSE CAN BE " &
                      "GIVEN FOR A VARIABLE OF A FIXED POINT " &
                      "TYPE IN THE DECLARATIVE PART OF A " &
                      "SUBPROGRAM");

     CD5011G_PROC;

     RESULT;

END CD5011G;
