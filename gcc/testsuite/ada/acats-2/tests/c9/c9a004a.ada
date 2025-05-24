-- C9A004A.ADA

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
-- CHECK THAT IF A TASK IS ABORTED BEFORE BEING ACTIVATED, THE TASK IS
--     TERMINATED.


-- RM 5/21/82
-- SPS 11/21/82
-- JBG 6/3/85
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE  C9A004A  IS

BEGIN


     -------------------------------------------------------------------


     TEST ("C9A004A", "CHECK THAT IF A TASK IS ABORTED"  &
                      " BEFORE BEING ACTIVATED,"         &
                      "  THE TASK IS TERMINATED"         );

  
     DECLARE


          TASK TYPE  T_TYPE  IS


               ENTRY  E ;

          END  T_TYPE ;


          T_OBJECT1 : T_TYPE ;


          TASK BODY  T_TYPE  IS
               BUSY : BOOLEAN := FALSE ;
          BEGIN

               NULL;

          END  T_TYPE ;


          PACKAGE  P  IS
               X : INTEGER := 0 ;
          END  P ;


          PACKAGE BODY  P  IS
          BEGIN

               IF      T_OBJECT1'TERMINATED  OR
                   NOT T_OBJECT1'CALLABLE
               THEN
                    FAILED( "WRONG VALUES FOR ATTRIBUTES" );
               END IF;

               ABORT  T_OBJECT1 ;  -- ELABORATED BUT NOT YET ACTIVATED.

          END  P ;


     BEGIN


          IF NOT  T_OBJECT1'TERMINATED  THEN
               FAILED(  "ABORTED (BEFORE ACTIVATION) TASK"  &
                        "  NOT TERMINATED"  );
          END IF;

     EXCEPTION

          WHEN TASKING_ERROR =>
               FAILED ("TASKING_ERROR RAISED");

     END;

     RESULT;

END C9A004A;
