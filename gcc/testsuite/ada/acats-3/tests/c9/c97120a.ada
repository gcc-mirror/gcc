-- C97120A.ADA

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
-- CHECK THAT A SELECTIVE WAIT DELAYS AT LEAST AS LONG AS IS SPECIFIED
-- IN A DELAY ALTERNATIVE.

-- WRG 7/11/86

with Impdef;
WITH REPORT;   USE REPORT;
WITH CALENDAR; USE CALENDAR;
PROCEDURE C97120A IS

BEGIN

     TEST ("C97120A", "CHECK THAT A SELECTIVE WAIT DELAYS AT LEAST " &
                      "AS LONG AS IS SPECIFIED IN A DELAY ALTERNATIVE");

     DECLARE

          TASK T IS
               ENTRY NO_GO;
               ENTRY SYNCH;
          END T;

          TASK BODY T IS
               BEFORE, AFTER : TIME;
          BEGIN
               -- ENSURE THAT SYNCH HAS BEEN CALLED BEFORE PROCEEDING:
               WHILE SYNCH'COUNT = 0 LOOP
                    DELAY 1.0 * Impdef.One_Second;
               END LOOP;

               BEFORE := CLOCK;
               SELECT
                    ACCEPT NO_GO;
                    FAILED ("ACCEPTED NONEXISTENT ENTRY CALL");
               OR
                    DELAY 10.0 * Impdef.One_Second;
                    AFTER := CLOCK;
                    IF AFTER - BEFORE < 10.0 * Impdef.One_Second THEN
                         FAILED ("INSUFFICIENT DELAY");
                    END IF;
               END SELECT;

               ACCEPT SYNCH;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED");
          END T;

     BEGIN

          T.SYNCH;  -- SUSPEND MAIN TASK BEFORE READING CLOCK.

     END;

     RESULT;

END C97120A;
