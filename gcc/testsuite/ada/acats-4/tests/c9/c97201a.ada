-- C97201A.ADA

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
-- CHECK THAT A RENDEZVOUS REQUESTED BY A CONDITIONAL_ENTRY_CALL
--     IS PERFORMED ONLY IF IMMEDIATELY POSSIBLE.

-- CASE  A:  THE TASK TO BE CALLED IS NOT YET ACTIVE AS OF THE
--     MOMENT OF CALL (CONDITIONAL_ENTRY_CALL),
--     AND THIS FACT CAN BE DETERMINED STATICALLY.


-- RM 4/20/82


WITH REPORT; USE REPORT;
PROCEDURE C97201A IS

     ELSE_BRANCH_TAKEN    :  INTEGER  :=  3 ;

BEGIN


     TEST ("C97201A", "CHECK THAT NO RENDEZVOUS REQUESTED BY"      &
                      " A CONDITIONAL_ENTRY_CALL CAN OCCUR  WHILE" &
                      " THE CALLED TASK IS NOT YET ACTIVE"         );


     -------------------------------------------------------------------


     DECLARE


          TASK  T  IS
               ENTRY  DO_IT_NOW_ORELSE ( AUTHORIZED : IN BOOLEAN ) ;
          END  T ;


          TASK BODY  T  IS

               PACKAGE       SECOND_ATTEMPT  IS    END  SECOND_ATTEMPT ;
               PACKAGE BODY  SECOND_ATTEMPT  IS
               BEGIN

                    SELECT
                         DO_IT_NOW_ORELSE (FALSE) ;--CALLING (OWN) ENTRY
                    ELSE    -- (I.E. CALLER ADOPTS A NO-WAIT POLICY)
                            --      THEREFORE THIS BRANCH MUST BE CHOSEN
                         ELSE_BRANCH_TAKEN := 2 * ELSE_BRANCH_TAKEN ;
                         COMMENT( "ELSE_BRANCH  TAKEN  (#2)" );
                    END SELECT;

               END  SECOND_ATTEMPT ;

          BEGIN

               ACCEPT DO_IT_NOW_ORELSE ( AUTHORIZED : IN BOOLEAN )  DO

                    IF  AUTHORIZED  THEN
                         COMMENT(  "AUTHORIZED ENTRY_CALL" );
                    ELSE
                         FAILED( "UNAUTHORIZED ENTRY_CALL" );
                    END IF;

               END DO_IT_NOW_ORELSE ;


          END  T ;


          PACKAGE       FIRST_ATTEMPT  IS      END  FIRST_ATTEMPT ;
          PACKAGE BODY  FIRST_ATTEMPT  IS
          BEGIN
               SELECT
                    T.DO_IT_NOW_ORELSE (FALSE) ;
               ELSE        -- (I.E. CALLER ADOPTS A NO-WAIT POLICY)
                            --      THEREFORE THIS BRANCH MUST BE CHOSEN
                    ELSE_BRANCH_TAKEN := 1 + ELSE_BRANCH_TAKEN ;
                    COMMENT( "ELSE_BRANCH  TAKEN  (#1)" );
               END SELECT;

          END  FIRST_ATTEMPT ;


     BEGIN

          T.DO_IT_NOW_ORELSE ( TRUE );   -- TO SATISFY THE SERVER'S
                                          --     WAIT FOR SUCH A CALL

     EXCEPTION

          WHEN  TASKING_ERROR  =>
               FAILED( "TASKING ERROR" );

     END  ;


     -------------------------------------------------------------------


     -- BY NOW, THE TASK IS TERMINATED  (AND THE NONLOCALS UPDATED)


     CASE  ELSE_BRANCH_TAKEN  IS

          WHEN  3  =>
               FAILED( "NO 'ELSE'; BOTH (?) RENDEZVOUS ATTEMPTED?" );

          WHEN  4  =>
               FAILED( "'ELSE' #1 ONLY; RENDEZVOUS (#2) ATTEMPTED?" );

          WHEN  6  =>
               FAILED( "'ELSE' #2 ONLY; RENDEZVOUS (#1) ATTEMPTED?" );

          WHEN  7  =>
               FAILED( "WRONG ORDER FOR 'ELSE':  #2,#1 " );

          WHEN  8  =>
               NULL ;

          WHEN  OTHERS  =>
               FAILED( "WRONG CASE_VALUE" );

     END CASE;


     RESULT;


END  C97201A ; 
