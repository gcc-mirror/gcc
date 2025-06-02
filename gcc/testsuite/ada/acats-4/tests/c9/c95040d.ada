-- C95040D.ADA

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
-- CHECK THAT TASKING_ERROR IS RAISED IN A CALLING
-- TASK WHEN THE TASK OWNING THE ENTRY TERMINATES BEFORE RENDEZVOUS
-- CAN OCCUR.

-- CHECK THAT RE-RAISING TASKING_ERROR, ONCE TRAPPED IN THE CALLER,
-- DOES NOT PROPAGATE OUTSIDE THE TASK BODY.

-- GOM 11/29/84
-- JWC 05/14/85
-- PWB 02/11/86  CORRECTED CALL TO TEST TO SHOW CORRECT TEST NAME.
-- RLB 12/15/99  REMOVED POTENTIALLY ERRONEOUS CALLS TO REPORT.COMMENT.

WITH REPORT;
USE REPORT;

PROCEDURE C95040D IS

     PROCEDURE DRIVER IS

          TASK NEST IS
               ENTRY OUTER;
               ENTRY INNER;
          END NEST;

          TASK SLAVE;

          TASK BODY NEST IS
          BEGIN
               --COMMENT("AT TOP OF 'NEST' TASK WAITING ON 'OUTER' " &
               --        "RENDEZVOUS");

               ACCEPT OUTER DO
                    --COMMENT("IN 'OUTER' RENDEZVOUS OF 'NEST' TASK " &
                    --        "ABOUT TO 'RETURN'");

                    RETURN;  -- CAUSES 'INNER' RENDEZVOUS TO BE SKIPPED.

                    ACCEPT INNER DO
                         FAILED("'INNER' RENDEZVOUS OF 'NEST' TASK " &
                                "SHOULD NEVER BE PERFORMED");
                    END INNER;
               END OUTER;

               --COMMENT("'OUTER' RENDEZVOUS COMPLETED IN 'NEST' TASK " &
               --        "AND NOW TERMINATING");
          END NEST;

          TASK BODY SLAVE IS
          BEGIN
               --COMMENT("AT TOP OF 'SLAVE' TASK. CALLING 'INNER' " &
               --        "RENDEZVOUS");

               NEST.INNER;

               FAILED("SHOULD HAVE RAISED 'TASKING_ERROR' IN 'SLAVE' " &
                      "TASK");
          EXCEPTION
               WHEN TASKING_ERROR =>
                    --COMMENT("'SLAVE' TASK CORRECTLY TRAPPING " &
                    --        "'TASKING_ERROR' AND RE-RAISING IT (BUT " &
                    --        "SHOULD NOT BE PROPAGATED)");
                    RAISE;
          END SLAVE;

     BEGIN  -- START OF DRIVER PROCEDURE.

          --COMMENT("AT TOP OF 'DRIVER'. CALLING 'OUTER' ENTRY OF " &
          --        "'NEST' TASK");

          NEST.OUTER;

          --COMMENT("'OUTER' RENDEZVOUS COMPLETED. 'DRIVER' AWAITING " &
          --        "TERMINATION OF 'NEST' AND 'SLAVE' TASKS");

     EXCEPTION
          WHEN TASKING_ERROR =>
               FAILED("'TASKING_ERROR' CAUGHT IN 'DRIVER' WHEN IT " &
                      "SHOULD HAVE BEEN CAUGHT IN 'SLAVE' TASK, OR " &
                      "'TASKING_ERROR' WAS INCORRECTLY PROPAGATED BY " &
                      "'SLAVE' TASK");
     END DRIVER;

BEGIN  -- START OF MAIN PROGRAM.

     TEST("C95040D","CHECK THAT 'TASKING_ERROR' IS RAISED IN A " &
                    "CALLER TASK WHEN TASK OWNING THE ENTRY CANNOT " &
                    "PERFORM RENDEZVOUS. ALSO CHECK THAT " &
                    "'TASKING_ERROR', ONCE RAISED, IS NOT PROPAGATED " &
                    "OUTSIDE THE TASK BODY");

     --COMMENT("MAIN PROGRAM CALLING 'DRIVER' PROCEDURE");

     DRIVER;

     --COMMENT("MAIN PROGRAM NOW TERMINATING");

     RESULT;
END C95040D;
