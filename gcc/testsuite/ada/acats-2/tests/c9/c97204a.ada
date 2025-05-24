-- C97204A.ADA

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
-- CHECK THAT THE EXCEPTION  TASKING_ERROR  WILL BE RAISED IF THE CALLED
--     TASK HAS ALREADY COMPLETED ITS EXECUTION AT THE TIME OF THE
--     CONDITIONAL_ENTRY_CALL.


-- RM 5/28/82
-- SPS 11/21/82
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE  C97204A  IS

     -- THE TASK WILL HAVE HIGHER PRIORITY ( PRIORITY'LAST )

BEGIN


     -------------------------------------------------------------------


     TEST ("C97204A", "CHECK THAT THE EXCEPTION  TASKING_ERROR  WILL" &
                      " BE RAISED IF THE CALLED TASK HAS ALREADY"     &
                      " COMPLETED ITS EXECUTION AT THE TIME OF THE"   &
                      " CONDITIONAL_ENTRY_CALL"                       );

  
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


     BEGIN


          FOR  I  IN  1..5  LOOP
               EXIT WHEN  T_OBJECT1'TERMINATED ;
               DELAY 10.0 * Impdef.One_Second;
          END LOOP;


          IF NOT  T_OBJECT1'TERMINATED  THEN
               COMMENT( "TASK NOT YET TERMINATED (AFTER 50 S.)" );
          END IF;


          BEGIN

               SELECT
                    T_OBJECT1.E ;
                    FAILED( "CALL WAS NOT DISOBEYED" );
               ELSE
                    FAILED( "'ELSE' BRANCH TAKEN INSTEAD OF TSKG_ERR" );
               END SELECT;

               FAILED( "EXCEPTION NOT RAISED" );

          EXCEPTION

               WHEN  TASKING_ERROR  =>
                    NULL ;

               WHEN  OTHERS  =>
                    FAILED(  "WRONG EXCEPTION RAISED"  );

          END ;


     END ;


     -------------------------------------------------------------------



     RESULT;


END  C97204A ;
