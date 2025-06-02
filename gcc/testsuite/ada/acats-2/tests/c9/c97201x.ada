-- C97201X.ADA

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
-- CHECK THAT NO RENDEZVOUS CAN EVER OCCUR IF BOTH PARTNERS REFUSE TO
--     WAIT (THAT IS, IF THE ENTRY CALL IS ISSUED BY A
--     "CONDITIONAL_ENTRY_CALL" AND THUS FOLLOWS A NO-WAIT POLICY
--     (DEMANDING UNCONDITIONALLY THAT "YOU DO IT   N O W  , OR ELSE"),
--     WHILE THE CALLEE IS ALSO COMMITTED TO A NO-WAIT POLICY,
--     BY VIRTUE OF A SELECTIVE_WAIT STATEMENT OF THE THIRD KIND
--     (WITH AN "ELSE" PART) IN WHICH THE CORRESPONDING ACCEPT_STATEMENT
--     IS EMBEDDED).
--     ("CLOSE ENCOUNTERS OF THE THIRD KIND"  --  ARE THEY POSSIBLE?)


-- THE SEMANTICS OF THIS ENTRY CALL REQUIRES THAT THE CALLING TASK
--     N O T   ENTER ITSELF ON ANY QUEUE BUT RATHER ATTEMPT AN IMMEDIATE
--     RENDEZVOUS WHICH IS TO TAKE PLACE IF AND ONLY IF THE CALLED TASK
--     HAS REACHED A POINT WHERE IT IS READY TO ACCEPT THE CALL (I.E.
--     IT IS EITHER WAITING AT AN ACCEPT STATEMENT FOR THE CORRESPONDING
--     ENTRY OR IT IS WAITING AT A SELECTIVE_WAIT STATEMENT WITH AN OPEN
--     ALTERNATIVE STARTING WITH SUCH AN ACCEPT STATEMENT).  IT ALSO
--     REQUIRES THAT THE ENTRY CALL BE CANCELLED IF THE CALLED TASK 
--     IS NOT AT SUCH A POINT.  ON THE OTHER HAND, THE SEMANTICS OF THE
--     SELECTIVE_WAIT STATEMENT WITH AN  'ELSE'  PART  SPECIFIES THAT
--     THE  'ELSE'  PART  MUST BE SELECTED IF NO 'ACCEPT' ALTERNATIVE
--     CAN BE IMMEDIATELY SELECTED,  AND THAT SUCH AN ALTERNATIVE
--     IS DEEMED TO BE IMMEDIATELY SELECTABLE ("SELECTION OF ONE SUCH
--     ALTERNATIVE OCCURS IMMEDIATELY"), AND A CORRESPONDING RENDEZVOUS
--     POSSIBLE,  IF AND ONLY IF THERE IS A CORRESPONDING ENTRY CALL
--     W A I T I N G   TO BE ACCCEPTED.  A "CONDITIONAL ENTRY CALL"
--     NEVER WAITS, AND IS NEVER ENTERED IN WAIT QUEUES; IT TAKES
--     THE 'ELSE' PART INSTEAD.


-- NOTE: IF THIS TEST PROGRAM HANGS UP, THE COMPILER WILL BE DEEMED
--     TO HAVE FAILED.


-- RM 3/19/82


WITH REPORT; USE REPORT;
PROCEDURE C97201X IS

     RENDEZVOUS_OCCURRED        :  BOOLEAN  :=  FALSE ;

     CALLER_TAKES_WRONG_BRANCH  :  BOOLEAN  :=  TRUE  ;
     SERVER_TAKES_WRONG_BRANCH  :  BOOLEAN  :=  TRUE  ;
     QUEUE_NOT_EMPTY            :  BOOLEAN  :=  FALSE ;

BEGIN


     TEST ("C97201X", "CHECK THAT NO RENDEZVOUS CAN EVER OCCUR IF" &
                      " BOTH PARTNERS REFUSE TO WAIT" );


     DECLARE


          TASK  T  IS
               ENTRY  SYNCHRONIZE ;
               ENTRY  DO_IT_NOW_ORELSE( DID_YOU_DO_IT : IN OUT BOOLEAN);
               ENTRY  KEEP_ALIVE  ;
          END  T ;
          

          TASK BODY  T  IS
          BEGIN


               ACCEPT  SYNCHRONIZE ;


               IF  DO_IT_NOW_ORELSE'COUNT /= 0  THEN
                    QUEUE_NOT_EMPTY := TRUE ;
               END IF;


               SELECT
                         ACCEPT  DO_IT_NOW_ORELSE
                                      ( DID_YOU_DO_IT : IN OUT BOOLEAN )
                         DO
                              DID_YOU_DO_IT  :=  TRUE ;
                         END ;
               ELSE         -- (I.E. TASK ADOPTS NO-WAIT POLICY)
                            -- 'ELSE' BRANCH MUST THEREFORE BE CHOSEN
                    SERVER_TAKES_WRONG_BRANCH := FALSE ;
               END SELECT;


               IF  DO_IT_NOW_ORELSE'COUNT /= 0  THEN
                    QUEUE_NOT_EMPTY := TRUE ;
               END IF;


               ACCEPT  KEEP_ALIVE ;  -- TO PREVENT THIS SERVER TASK FROM
                                     --     TERMINATING IF IT GETS TO
                                     --     THE NO-WAIT MEETING-PLACE
                                     --     AHEAD OF THE CALLER (WHICH
                                     --     WOULD LEAD TO A SUBSEQUENT
                                     --     TASKING_ERROR AT THE TIME OF
                                     --     THE NO-WAIT CALL).


          END  T ;


     BEGIN


          T.SYNCHRONIZE ;  -- TO MINIMIZE THE   N E E D   TO WAIT


          SELECT
               T.DO_IT_NOW_ORELSE ( RENDEZVOUS_OCCURRED );
          ELSE              -- (I.E. CALLER TOO ADOPTS A NO-WAIT POLICY)
                                    -- MUST THEREFORE CHOOSE THIS BRANCH
               CALLER_TAKES_WRONG_BRANCH := FALSE ;
          END SELECT;


          T.KEEP_ALIVE ;    -- THIS ALSO UPDATES THE NONLOCALS


     END;   -- END OF BLOCK CONTAINING THE NO-WAIT ENTRY CALL


     IF  RENDEZVOUS_OCCURRED
     THEN
          FAILED( "RENDEZVOUS OCCURRED" );
     END IF;

     IF  CALLER_TAKES_WRONG_BRANCH  OR
         SERVER_TAKES_WRONG_BRANCH
     THEN
          FAILED( "WRONG BRANCH TAKEN" );
     END IF;

     IF  QUEUE_NOT_EMPTY
     THEN
          FAILED( "ENTRY QUEUE NOT EMPTY" );
     END IF;


     RESULT;


END  C97201X ; 
