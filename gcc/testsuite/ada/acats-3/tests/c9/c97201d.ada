-- C97201D.ADA

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

-- CASE  D:  THE BODY OF THE TASK CONTAINING THE CALLED ENTRY
--     DOES NOT CONTAIN AN ACCEPT_STATEMENT FOR THAT ENTRY  -
--     AND THIS FACT IS DETERMINED STATICALLY.


-- RM 4/12/82


WITH REPORT; USE REPORT;
PROCEDURE C97201D IS

     ELSE_BRANCH_TAKEN    :  BOOLEAN  :=  FALSE ;

BEGIN


     TEST ("C97201D", "CHECK THAT NO RENDEZVOUS REQUESTED BY"      &
                      " A CONDITIONAL_ENTRY_CALL CAN EVER OCCUR"   &
                      " IN THE ABSENCE OF A CORRESPONDING "        &
                      " ACCEPT_STATEMENT "                         );


     DECLARE


          TASK  T  IS
               ENTRY  DO_IT_NOW_ORELSE ;
               ENTRY  KEEP_ALIVE ;
          END  T ;
          

          TASK BODY  T  IS
          BEGIN

               -- NO ACCEPT_STATEMENT FOR THE ENTRY_CALL BEING TESTED

               ACCEPT  KEEP_ALIVE ;  -- TO PREVENT THIS SERVER TASK FROM
                                     --     TERMINATING IF
                                     --     UPON ACTIVATION
                                     --     IT GETS TO RUN    
                                     --     AHEAD OF THE CALLER (WHICH
                                     --     WOULD LEAD TO A SUBSEQUENT
                                     --     TASKING_ERROR AT THE TIME OF
                                     --     THE NO-WAIT CALL).

          END ;


     BEGIN

          SELECT
               T.DO_IT_NOW_ORELSE ;
          ELSE              -- (I.E. CALLER ADOPTS A NO-WAIT POLICY)
                            --      THEREFORE THIS BRANCH MUST BE CHOSEN
               ELSE_BRANCH_TAKEN := TRUE ;
               COMMENT( "ELSE_BRANCH  TAKEN" );
          END SELECT;

          T.KEEP_ALIVE ;    -- THIS ALSO UPDATES THE NONLOCALS

     END;   -- END OF BLOCK CONTAINING THE ENTRY CALL


     -- BY NOW, THE TASK IS TERMINATED

     IF  ELSE_BRANCH_TAKEN  THEN
          NULL ;
     ELSE
          FAILED( "RENDEZVOUS ATTEMPTED?" );
     END IF;

     RESULT;


END  C97201D ; 
