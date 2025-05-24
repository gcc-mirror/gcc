-- C97201B.ADA

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
-- CHECK THAT A CONDITIONAL ENTRY CALL IS NOT ACCEPTED IF THERE IS
-- ANOTHER TASK QUEUED FOR THE ENTRY.

-- WRG 7/11/86
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C97201B IS


BEGIN

     TEST ("C97201B", "CHECK THAT A CONDITIONAL ENTRY CALL IS NOT " &
                      "ACCEPTED IF THERE IS ANOTHER TASK QUEUED " &
                      "FOR THE ENTRY");

     DECLARE

          TASK T IS
               ENTRY E;
               ENTRY SYNCH;
               ENTRY DONE;
          END T;

          TASK BODY T IS
          BEGIN
               -- ENSURE THAT E HAS BEEN CALLED BEFORE PROCEEDING:
               WHILE E'COUNT = 0 LOOP
                    DELAY 1.0 * Impdef.One_Second;
               END LOOP;

               ACCEPT SYNCH;

               SELECT
                    WHEN IDENT_BOOL (FALSE) =>
                         ACCEPT E;
                         FAILED ("CLOSED ALTERNATIVE TAKEN");
               OR
                    ACCEPT DONE DO
                         IF E'COUNT /= 1 THEN
                              FAILED (NATURAL'IMAGE(E'COUNT) &
                                      " CALLS WERE QUEUED FOR ENTRY " &
                                      "E OF TASK T");
                         END IF;
                    END DONE;
               OR
                    DELAY 1000.0 * Impdef.One_Second;
                    FAILED ("DELAY EXPIRED; E'COUNT =" &
                            NATURAL'IMAGE(E'COUNT) );
               END SELECT;

               WHILE E'COUNT > 0 LOOP
                    ACCEPT E;
               END LOOP;
          END T;

          TASK AGENT;

          TASK BODY AGENT IS
          BEGIN
               T.E;
          END AGENT;

     BEGIN

          T.SYNCH;

          DELAY 10.0 * Impdef.One_Second;

          SELECT
               T.E;
               FAILED ("CONDITIONAL ENTRY CALL ACCEPTED" );
          ELSE
               COMMENT ("ELSE PART EXECUTED");
               T.DONE;
          END SELECT;

     END;

     RESULT;

END C97201B;
