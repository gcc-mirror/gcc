-- C97305C.ADA

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
-- CHECK THAT IF THE RENDEZVOUS IS NOT IMMEDIATELY POSSIBLE BUT BECOMES
-- POSSIBLE BEFORE THE DELAY EXPIRES, THE TIMED ENTRY CALL IS ACCEPTED.

-- CASE A: SINGLE ENTRY; THE CALLED TASK IS EXECUTING AN ACCEPT
--         STATEMENT.

-- WRG 7/13/86
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C97305C IS

     RENDEZVOUS_OCCURRED            : BOOLEAN  := FALSE;
     STATEMENTS_AFTER_CALL_EXECUTED : BOOLEAN  := FALSE;
     DELAY_IN_MINUTES               : CONSTANT POSITIVE := 30;


BEGIN

     TEST ("C97305C", "CHECK THAT IF THE RENDEZVOUS IS NOT " &
                      "IMMEDIATELY POSSIBLE BUT BECOMES POSSIBLE " &
                      "BEFORE THE DELAY EXPIRES, THE TIMED ENTRY " &
                      "CALL IS ACCEPTED");

     DECLARE

          TASK T IS
               ENTRY E (B : IN OUT BOOLEAN);
          END T;

          TASK BODY T IS
          BEGIN
               DELAY 10.0 * Impdef.One_Long_Second;
               ACCEPT E (B : IN OUT BOOLEAN) DO
                    B := IDENT_BOOL (TRUE);
               END E;
          END T;

     BEGIN

          SELECT
               T.E (RENDEZVOUS_OCCURRED);
               STATEMENTS_AFTER_CALL_EXECUTED := IDENT_BOOL (TRUE);
          OR
               DELAY DELAY_IN_MINUTES * 60.0 * Impdef.One_Long_Second;
               FAILED ("TIMED ENTRY CALL NOT ACCEPTED AFTER" &
                       POSITIVE'IMAGE(DELAY_IN_MINUTES) &
                       " MINUTES ELAPSED");

          END SELECT;

     END;

     IF NOT RENDEZVOUS_OCCURRED THEN
          FAILED ("RENDEZVOUS DID NOT OCCUR");
     END IF;

     IF NOT STATEMENTS_AFTER_CALL_EXECUTED THEN
          FAILED ("STATEMENTS AFTER ENTRY CALL NOT EXECUTED");
     END IF;

     RESULT;

END C97305C;
