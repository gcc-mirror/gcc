-- C9A009H.ADA

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
-- CHECK THAT A TASK ABORTED DURING A RENDEVOUS IS NEITHER CALLABLE NOR
-- TERMINATED BEFORE THE END OF THE RENDEVOUS.

-- J.P ROSEN, ADA PROJECT, NYU
-- JBG 6/1/84

WITH REPORT; USE REPORT;
PROCEDURE C9A009H IS
BEGIN
     TEST ("C9A009H", "TASK ABORTED IN RENDEVOUS IS NOT CALLABLE OR " &
                      "TERMINATED");

     DECLARE

          TASK T1 IS
               ENTRY E1;
          END T1;

          TASK T2 IS
          END T2;

          TASK BODY T2 IS
          BEGIN
               T1.E1;
               FAILED ("T2 NOT ABORTED");
          EXCEPTION
               WHEN TASKING_ERROR =>
                    FAILED ("TASKING_ERROR RAISED IN ABORTED TASK");
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED");
          END T2;

          TASK BODY T1 IS
          BEGIN
               ACCEPT E1 DO
                    ABORT T2;
                    IF T2'CALLABLE THEN
                         FAILED ("T2 STILL CALLABLE");
                    END IF;

                    IF T2'TERMINATED THEN
                         FAILED ("T2 TERMINATED");
                    END IF;
               END E1;
          END T1;

     BEGIN
          NULL;
     END;

     RESULT;

END C9A009H;
