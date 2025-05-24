-- C97203B.ADA

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
-- CHECK THAT A CONDITIONAL_ENTRY_CALL CAN APPEAR IN PLACES WHERE A
--     SELECTIVE_WAIT  CANNOT.

-- PART 2: PROCEDURE BODY EMBEDDED IN TASK BODY.


-- RM 4/09/1982


WITH REPORT;
USE REPORT;
PROCEDURE  C97203B  IS


BEGIN


     TEST ( "C97203B" , "CHECK THAT A  CONDITIONAL_ENTRY_CALL  CAN" &
                        " APPEAR WHERE A  SELECTIVE_WAIT  CANNOT" );


     -------------------------------------------------------------------


     DECLARE


          TASK  TT  IS
               ENTRY  A ( AUTHORIZED : IN BOOLEAN );
          END  TT ;


          TASK BODY  TT  IS


               PROCEDURE  WITHIN_TASK_BODY ;


               PROCEDURE  WITHIN_TASK_BODY  IS
               BEGIN

                    SELECT  -- NOT A SELECTIVE_WAIT
                         A ( FALSE ) ;  -- CALLING (OWN) ENTRY
                    ELSE 
                         COMMENT( "ALTERNATIVE BRANCH TAKEN" );
                    END SELECT;
                    
               END  WITHIN_TASK_BODY ;


          BEGIN


               -- CALL THE INNER PROC. TO FORCE EXEC. OF COND_E_CALL
               WITHIN_TASK_BODY ;


               ACCEPT  A ( AUTHORIZED : IN BOOLEAN )  DO

                    IF  AUTHORIZED  THEN
                         COMMENT(  "AUTHORIZED ENTRY_CALL" );
                    ELSE
                         FAILED( "UNAUTHORIZED ENTRY_CALL" );
                    END IF;

               END  A ;

          END  TT ;


          PROCEDURE  OUTSIDE_TASK_BODY  IS
          BEGIN

               SELECT  -- NOT A SELECTIVE_WAIT
                    TT.A ( FALSE ) ;  -- UNBORN
               ELSE 
                    COMMENT( "(OUT:) ALTERNATIVE BRANCH TAKEN" );
               END SELECT;

          END  OUTSIDE_TASK_BODY ;


          PACKAGE       CREATE_OPPORTUNITY_TO_CALL           IS END;   
          PACKAGE BODY  CREATE_OPPORTUNITY_TO_CALL  IS
          BEGIN
               -- CALL THE OTHER PROC. TO FORCE EXEC. OF COND_E_CALL
               OUTSIDE_TASK_BODY ;
          END  CREATE_OPPORTUNITY_TO_CALL ;


     BEGIN

          TT.A ( TRUE );

     EXCEPTION

          WHEN  TASKING_ERROR  =>
               FAILED( "TASKING ERROR" );

     END  ;

     -------------------------------------------------------------------

     RESULT ;


END  C97203B ;
