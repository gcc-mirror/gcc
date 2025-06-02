-- C97201G.ADA

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

-- CASE  G:  THE CORRESPONDING ACCEPT_STATEMENT IS CLOSED
--     AND THIS FACT IS STATICALLY DETERMINABLE.


-- RM 4/21/82


WITH REPORT; USE REPORT;
PROCEDURE C97201G IS

     ELSE_BRANCH_TAKEN    :  BOOLEAN  :=  FALSE ;
     RENDEZVOUS_OCCURRED  :  BOOLEAN  :=  FALSE ;
     QUEUE_NOT_EMPTY      :  BOOLEAN  :=  FALSE ;
     X                    :  INTEGER  :=  17 ;

BEGIN


     TEST ("C97201G", "CHECK THAT NO RENDEZVOUS REQUESTED BY"      &
                      " A CONDITIONAL_ENTRY_CALL CAN EVER OCCUR"   &
                      " IF THE CORRESPONDING ACCEPT_STATEMENT IS"  &
                      " CLOSED"                                    );


     -------------------------------------------------------------------


     DECLARE


          TASK  T  IS
               ENTRY  DO_IT_NOW_ORELSE( DID_YOU_DO_IT : IN OUT BOOLEAN);
               ENTRY  KEEP_ALIVE ;
          END  T ;
          

          TASK BODY  T  IS
          BEGIN

               IF  DO_IT_NOW_ORELSE'COUNT /= 0  THEN
                    QUEUE_NOT_EMPTY := TRUE ;
               END IF;


               SELECT
                    WHEN  3 = 5  =>
                         ACCEPT  DO_IT_NOW_ORELSE
                                      ( DID_YOU_DO_IT : IN OUT BOOLEAN)
                         DO
                              DID_YOU_DO_IT := TRUE ;
                         END;
               OR
                         ACCEPT  KEEP_ALIVE ; -- TO PREVENT SELECT_ERROR
               END SELECT;


               IF  DO_IT_NOW_ORELSE'COUNT /= 0  THEN
                    QUEUE_NOT_EMPTY := TRUE ;
               END IF;


          END  T ;


     BEGIN

          COMMENT( "PERMANENTLY CLOSED" );

          SELECT
               T.DO_IT_NOW_ORELSE( RENDEZVOUS_OCCURRED );
          ELSE              -- (I.E. CALLER ADOPTS A NO-WAIT POLICY)
                            --      THEREFORE THIS BRANCH MUST BE CHOSEN
               ELSE_BRANCH_TAKEN := TRUE ;
               COMMENT( "ELSE_BRANCH  TAKEN" );
          END SELECT;

          T.KEEP_ALIVE ;    -- THIS ALSO UPDATES THE NONLOCALS

     END;   -- END OF BLOCK CONTAINING THE ENTRY CALL


     -------------------------------------------------------------------


     -- BY NOW, THE TASK IS TERMINATED

     IF  RENDEZVOUS_OCCURRED
     THEN
          FAILED( "RENDEZVOUS OCCURRED" );
     END IF;

     IF  QUEUE_NOT_EMPTY
     THEN
          FAILED( "ENTRY QUEUE NOT EMPTY" );
     END IF;

     IF  ELSE_BRANCH_TAKEN  THEN
          NULL ;
     ELSE
          FAILED( "RENDEZVOUS ATTEMPTED?" );
     END IF;

     RESULT;


END  C97201G ; 
