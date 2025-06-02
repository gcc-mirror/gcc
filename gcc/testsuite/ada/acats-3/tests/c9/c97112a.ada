-- C97112A.ADA

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
-- CHECK THAT A DELAY STATEMENT IS ALLOWED IN THE SEQUENCE OF STATEMENTS
-- OF A SELECT ALTERNATIVE OF A SELECTIVE WAIT CONTAINING A TERMINATE
-- ALTERNATIVE OR AN ELSE PART.

-- WRG 7/9/86

with Impdef;
WITH REPORT;   USE REPORT;
WITH CALENDAR; USE CALENDAR;
PROCEDURE C97112A IS

     ACCEPT_ALTERNATIVE_TAKEN : BOOLEAN := FALSE;

BEGIN

     TEST ("C97112A", "CHECK THAT A DELAY STATEMENT IS ALLOWED IN " &
                      "THE SEQUENCE OF STATEMENTS OF A SELECT " &
                      "ALTERNATIVE OF A SELECTIVE WAIT CONTAINING A " &
                      "TERMINATE ALTERNATIVE OR AN ELSE PART");

     --------------------------------------------------

  A: DECLARE

          TASK T IS
               ENTRY E;
          END T;

          TASK BODY T IS
               BEFORE, AFTER : TIME;
          BEGIN
               SELECT
                    ACCEPT E;
                    ACCEPT_ALTERNATIVE_TAKEN := TRUE;
                    BEFORE := CLOCK;
                    DELAY 10.0 * Impdef.One_Second;
                    AFTER  := CLOCK;
                    IF AFTER - BEFORE < 10.0 * Impdef.One_Second THEN
                         FAILED ("INSUFFICIENT DELAY (A)");
                    END IF;
               OR
                    TERMINATE;
               END SELECT;
          END T;

     BEGIN

          T.E;

     END A;

     IF NOT ACCEPT_ALTERNATIVE_TAKEN THEN
          FAILED ("ACCEPT ALTERNATIVE NOT TAKEN");
     END IF;

     --------------------------------------------------

  B: DECLARE

          TASK T IS
               ENTRY E;
          END T;

          TASK BODY T IS
               BEFORE, AFTER : TIME;
          BEGIN
               --ENSURE THAT E HAS BEEN CALLED BEFORE PROCEEDING:
               WHILE E'COUNT = 0 LOOP
                    DELAY 1.0 * Impdef.One_Second;
               END LOOP;

               SELECT
                    ACCEPT E;
                    BEFORE := CLOCK;
                    DELAY 10.0 * Impdef.One_Second;
                    AFTER  := CLOCK;
                    IF AFTER - BEFORE < 10.0 * Impdef.One_Second THEN
                         FAILED ("INSUFFICIENT DELAY (B-1)");
                    END IF;
               ELSE
                    FAILED ("ELSE PART EXECUTED (B-1)");
               END SELECT;

               SELECT
                    ACCEPT E;
                    FAILED ("ACCEPT STATEMENT EXECUTED (B-2)");
               ELSE
                    BEFORE := CLOCK;
                    DELAY 10.0 * Impdef.One_Second;
                    AFTER  := CLOCK;
                    IF AFTER - BEFORE < 10.0 * Impdef.One_Second THEN
                         FAILED ("INSUFFICIENT DELAY (B-2)");
                    END IF;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED");
          END T;

     BEGIN

          T.E;

     END B;

     --------------------------------------------------

     RESULT;

END C97112A;
