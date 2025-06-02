-- C97203C.ADA

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
-- CHECK THAT A CONDITIONAL ENTRY CALL CAN APPEAR IN PLACES WHERE A
-- SELECTIVE WAIT IS NOT ALLOWED.

-- PART 3: TASK BODY NESTED WITHIN A TASK.

-- WRG 7/15/86

WITH REPORT; USE REPORT;
PROCEDURE C97203C IS

BEGIN

     TEST ("C97203C", "CHECK THAT A CONDITIONAL ENTRY CALL CAN " &
                      "APPEAR IN PLACES WHERE A SELECTIVE WAIT " &
                      "IS NOT ALLOWED; CASE: TASK BODY NESTED " &
                      "WITHIN A TASK");

     DECLARE

          TASK T IS
               ENTRY E;
               ENTRY SYNCH;
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT SYNCH;
               ACCEPT SYNCH;
               ACCEPT SYNCH;
               ACCEPT E;
          END T;

          TASK OUTER IS
               ENTRY E;
               ENTRY SYNCH;
          END OUTER;

          TASK BODY OUTER IS

               TASK TYPE INNER;

               INNER1 : INNER;

               TASK BODY INNER IS
               BEGIN
                    SELECT
                         T.E;
                         FAILED ("CONDITIONAL ENTRY CALL ACCEPTED - " &
                                 "INNER (1)");
                    ELSE
                         T.SYNCH;
                    END SELECT;

                    SELECT
                         OUTER.E;
                         FAILED ("CONDITIONAL ENTRY CALL ACCEPTED - " &
                                 "INNER (2)");
                    ELSE
                         OUTER.SYNCH;
                    END SELECT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED - INNER");
               END INNER;

               PACKAGE DUMMY IS
                    TYPE ACC_INNER IS ACCESS INNER;
                    INNER2 : ACC_INNER := NEW INNER;
               END DUMMY;

          BEGIN

               SELECT
                    T.E;
                    FAILED ("CONDITIONAL ENTRY CALL ACCEPTED - OUTER");
               ELSE
                    T.SYNCH;
               END SELECT;

               ACCEPT SYNCH;
               ACCEPT SYNCH;
               ACCEPT E;

          EXCEPTION

               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - OUTER");

          END OUTER;

     BEGIN

          T.E;
          OUTER.E;

     END;

     RESULT;

END C97203C;
