-- C97301E.ADA

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
-- CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT LEAST THE SPECIFIED 
-- AMOUNT OF TIME IF A RENDEZVOUS IS NOT POSSIBLE.

-- CASE  E:  THE BODY OF THE TASK CONTAINING THE CALLED ENTRY
--           DOES NOT CONTAIN AN ACCEPT_STATEMENT FOR THAT ENTRY  -
--           (THE ENTRY BELONGS TO AN ENTRY FAMILY; SOME FAMILY MEMBERS
--           ARE "ACCEPTABLE", BUT NOT THE CALLED ONE.)

-- RJW 3/31/86

with Impdef;
WITH REPORT; USE REPORT;
WITH CALENDAR; USE CALENDAR;
PROCEDURE C97301E IS
     
     OR_BRANCH_TAKEN : BOOLEAN := FALSE;
     
BEGIN

     TEST ("C97301E", "CHECK THAT A TIMED_ENTRY_CALL DELAYS FOR AT " &
                      "LEAST THE SPECIFIED AMOUNT OF TIME " &
                      "IN THE ABSENCE OF A CORRESPONDING " &
                      "ACCEPT_STATEMENT " );

     DECLARE
          
          WAIT_TIME : DURATION :=  3.0 * Impdef.One_Second;
          
          START_TIME : TIME;
         
          STOP_TIME : TIME;

          SUBTYPE  SHORT  IS  INTEGER RANGE 10..20 ;

          KEEP_ALIVE  :  INTEGER  := 15 ;

          TASK  T  IS
               ENTRY  DO_IT_NOW_OR_WAIT ( SHORT ) ;
          END  T ;
          
          TASK BODY  T  IS
          BEGIN

               -- NO ACCEPT_STATEMENT FOR THE ENTRY_CALL BEING TESTED.
               ACCEPT  DO_IT_NOW_OR_WAIT ( IDENT_INT(15) );

                                     -- THIS ALSO PREVENTS THIS SERVER
                                     --     TASK FROM TERMINATING IF
                                     --     UPON ACTIVATION
                                     --     IT GETS TO RUN    
                                     --     AHEAD OF THE CALLER (WHICH
                                     --     WOULD LEAD TO A SUBSEQUENT
                                     --     TASKING_ERROR AT THE TIME 
                                     --     OF THE NO-WAIT CALL).

          END ;


     BEGIN
          START_TIME := CLOCK;
          SELECT
               T.DO_IT_NOW_OR_WAIT (10) ;  -- ACCEPT_STATEMENT HAS 15.
          OR              
                            --   THEREFORE THIS BRANCH MUST BE CHOSEN.
               DELAY WAIT_TIME;
               STOP_TIME := CLOCK;
               IF STOP_TIME >= (WAIT_TIME + START_TIME) THEN
                    NULL;
               ELSE
                    FAILED ( "INSUFFICIENT DELAY" );
               END IF;
               OR_BRANCH_TAKEN := TRUE ;
               COMMENT( "OR_BRANCH TAKEN" );
          END SELECT;

          T.DO_IT_NOW_OR_WAIT (KEEP_ALIVE) ;

     EXCEPTION
          WHEN TASKING_ERROR =>
               FAILED ( "TASKING ERROR" );

     END;   -- END OF BLOCK CONTAINING THE TIMED ENTRY CALL.

     -- BY NOW, TASK T IS TERMINATED.

     IF  OR_BRANCH_TAKEN  THEN
          NULL ;
     ELSE
          FAILED( "RENDEZVOUS ATTEMPTED" );
     END IF;

     RESULT;

END  C97301E ; 
