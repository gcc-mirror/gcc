-- C97303A.ADA

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
-- CHECK THAT A TIMED_ENTRY_CALL CAN APPEAR IN PLACES WHERE A
--     SELECTIVE_WAIT  CANNOT.

-- PART 1: PACKAGE BODY EMBEDDED IN TASK BODY.


-- RM 4/06/1982

with Impdef;
WITH REPORT;
USE REPORT;
PROCEDURE  C97303A  IS


BEGIN


     TEST ( "C97303A" , "CHECK THAT A  TIMED_ENTRY_CALL  CAN" &
                        " APPEAR WHERE A  SELECTIVE_WAIT  CANNOT" );


     -------------------------------------------------------------------


     DECLARE


          TASK  TT  IS
               ENTRY  A ( AUTHORIZED : IN BOOLEAN );
          END  TT ;


          TASK BODY  TT  IS

               PACKAGE  WITHIN_TASK_BODY  IS
                    -- NOTHING HERE
               END  WITHIN_TASK_BODY ;


               PACKAGE BODY  WITHIN_TASK_BODY  IS
               BEGIN

                    SELECT  -- NOT A SELECTIVE_WAIT
                         A ( FALSE ) ;  -- CALLING (OWN) ENTRY
                    OR
                         DELAY 1.0 * Impdef.One_Second;
                         COMMENT( "ALTERNATIVE BRANCH TAKEN" );
                    END SELECT;
                    
               END  WITHIN_TASK_BODY ;


          BEGIN

               ACCEPT  A ( AUTHORIZED : IN BOOLEAN )  DO

                    IF  AUTHORIZED  THEN
                         COMMENT(  "AUTHORIZED ENTRY_CALL" );
                    ELSE
                         FAILED( "UNAUTHORIZED ENTRY_CALL" );
                    END IF;

               END  A ;
   
          END  TT ;


          PACKAGE  OUTSIDE_TASK_BODY  IS
               -- NOTHING HERE
          END  OUTSIDE_TASK_BODY ;


          PACKAGE BODY  OUTSIDE_TASK_BODY  IS
          BEGIN

               SELECT  -- NOT A SELECTIVE_WAIT
                    TT.A ( FALSE ) ;  -- UNBORN
               OR 
                    DELAY  2.0 * Impdef.One_Second;
                    COMMENT( "(OUT:) ALTERNATIVE BRANCH TAKEN" );
               END SELECT;

          END  OUTSIDE_TASK_BODY ;


     BEGIN

          TT.A ( TRUE );

     EXCEPTION

          WHEN  TASKING_ERROR  =>
               FAILED( "TASKING ERROR" );

     END  ;


     -------------------------------------------------------------------


     RESULT ;


END  C97303A ;
