-- C97205A.ADA

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
-- CHECK THAT IF THE RENDEZVOUS IS IMMEDIATELY POSSIBLE (FOR A
-- CONDITIONAL ENTRY CALL), IT IS PERFORMED.

-- CASE A: SINGLE ENTRY; THE CALLED TASK IS EXECUTING AN ACCEPT
--         STATEMENT.

-- WRG 7/13/86
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C97205A IS

     RENDEZVOUS_OCCURRED            : BOOLEAN  := FALSE;
     STATEMENTS_AFTER_CALL_EXECUTED : BOOLEAN  := FALSE;
     COUNT                          : POSITIVE := 1;


BEGIN

     TEST ("C97205A", "CHECK THAT IF THE RENDEZVOUS IS IMMEDIATELY " &
                      "POSSIBLE (FOR A CONDITIONAL ENTRY CALL), IT " &
                      "IS PERFORMED");

     DECLARE

          TASK T IS
               ENTRY E (B : IN OUT BOOLEAN);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT E (B : IN OUT BOOLEAN) DO
                    B := IDENT_BOOL (TRUE);
               END E;
          END T;

     BEGIN

          WHILE NOT STATEMENTS_AFTER_CALL_EXECUTED LOOP
               DELAY 1.0 * Impdef.One_Second;

               SELECT
                    T.E (RENDEZVOUS_OCCURRED);
                    STATEMENTS_AFTER_CALL_EXECUTED := IDENT_BOOL (TRUE);
               ELSE
                    IF COUNT < 60 * 60 THEN
                         COUNT := COUNT + 1;
                    ELSE
                         FAILED ("NO RENDEZVOUS AFTER AT LEAST ONE " &
                                 "HOUR ELAPSED");
                         EXIT;
                    END IF;
               END SELECT;
          END LOOP;

     END;

     IF NOT RENDEZVOUS_OCCURRED THEN
          FAILED ("RENDEZVOUS DID NOT OCCUR");
     END IF;

     IF COUNT > 1 THEN
          COMMENT ("DELAYED" & POSITIVE'IMAGE(COUNT) & " SECONDS");
     END IF;

     RESULT;

END C97205A;
