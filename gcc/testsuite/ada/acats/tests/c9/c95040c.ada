-- C95040C.ADA

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
-- CHECKS THAT A TASK COMPLETED, BUT NOT TERMINATED (I.E. WAITING
-- FOR TERMINATION OF A DEPENDENT TASK) IS NEITHER 'TERMINATED NOR
-- 'CALLABLE.  CALLS TO ENTRIES BELONGING TO SUCH A TASK RAISE
-- TASKING_ERROR.

-- J.P. ROSEN, ADA PROJECT, NYU
-- JBG 6/1/84
-- JWC 6/28/85   RENAMED FROM C9A009A-B.ADA
-- PWN 9/11/94   REMOVED PRAGMA PRIORITY FOR ADA 9X

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C95040C IS
BEGIN

     TEST ("C95040C", "TASKING_ERROR RAISED WHEN CALLING COMPLETED " &
                      "BUT UNTERMINATED TASK");

     DECLARE

          TASK T1 IS
               ENTRY E;
          END T1;

          TASK BODY T1 IS

               TASK T2 IS
               END T2;

               TASK BODY T2 IS
               BEGIN
                    COMMENT ("BEGIN T2");
                    T1.E;          -- T1 WILL COMPLETE BEFORE THIS CALL
                                   -- OR WHILE WAITING FOR THIS CALL TO
                                   -- BE ACCEPTED.  WILL DEADLOCK IF
                                   -- TASKING_ERROR IS NOT RAISED.
                    FAILED ("NO TASKING_ERROR RAISED");
               EXCEPTION
                    WHEN TASKING_ERROR =>
                         IF T1'CALLABLE THEN
                              FAILED ("T1 STILL CALLABLE");
                         END IF;

                         IF T1'TERMINATED THEN    -- T1 CAN'T TERMINATE
                                                  -- UNTIL T2 HAS
                                                  -- TERMINATED.
                              FAILED ("T1 TERMINATED");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION");
               END T2;
          BEGIN
               NULL;
          END;

     BEGIN
          NULL;
     END;

     RESULT;

END C95040C;
