-- C97117C.ADA

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
-- CHECK THAT AN ELSE PART IS NOT EXECUTED IF A TASK IS QUEUED AT AN
-- OPEN ALTERNATIVE.

-- WRG 7/10/86

with Impdef;
WITH REPORT; USE REPORT;
PROCEDURE C97117C IS

BEGIN

     TEST ("C97117C", "CHECK THAT AN ELSE PART IS NOT EXECUTED IF A " &
                      "TASK IS QUEUED AT AN OPEN ALTERNATIVE");

     DECLARE

          TASK T IS
               ENTRY E;
               ENTRY NO_GO;
          END T;

          TASK BODY T IS
          BEGIN
               --ENSURE THAT E HAS BEEN CALLED BEFORE PROCEEDING:
               WHILE E'COUNT = 0 LOOP
                    DELAY 1.0 * Impdef.One_Second;
               END LOOP;

               SELECT
                    ACCEPT NO_GO;
                    FAILED ("ACCEPTED NONEXISTENT ENTRY CALL");
               OR WHEN IDENT_BOOL (TRUE)  =>
                    ACCEPT E;
               OR WHEN IDENT_BOOL (FALSE) =>
                    ACCEPT E;
                    FAILED ("CLOSED ALTERNATIVE TAKEN");
               ELSE
                    FAILED ("ELSE PART EXECUTED");
               END SELECT;
          END T;

     BEGIN

          T.E;

     END;

     RESULT;

END C97117C;
