-- C97120B.ADA

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
-- CHECK THAT IF A SPECIFIED DELAY IS ZERO OR NEGATIVE AND AN ENTRY CALL
-- IS WAITING AT AN OPEN ALTERNATIVE WHEN THE SELECTIVE WAIT IS
-- EXECUTED, THE CALL IS ACCEPTED.

-- WRG 7/11/86

with Impdef;
WITH REPORT; USE REPORT;
PROCEDURE C97120B IS

     ZERO, NEG : DURATION := 1.0;

BEGIN

     TEST ("C97120B", "CHECK THAT IF A SPECIFIED DELAY IS ZERO OR " &
                      "NEGATIVE AND AN ENTRY CALL IS WAITING AT AN " &
                      "OPEN ALTERNATIVE WHEN THE SELECTIVE WAIT IS " &
                      "EXECUTED, THE CALL IS ACCEPTED");

     IF EQUAL (3, 3) THEN
          ZERO :=  0.0;
          NEG  := -1.0;
     END IF;

     DECLARE

          TASK T IS
               ENTRY E;
          END T;

          TASK BODY T IS
          BEGIN
               WHILE E'COUNT = 0 LOOP
                    DELAY 1.0 * Impdef.One_Second;
               END LOOP;

            A: BEGIN
                    SELECT
                         WHEN IDENT_BOOL (TRUE) =>
                              ACCEPT E;
                    OR
                         DELAY ZERO * Impdef.One_Second;
                         FAILED ("ZERO DELAY ALTERNATIVE TAKEN");
                         ACCEPT E;
                    END SELECT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED (A)");
               END A;

               WHILE E'COUNT = 0 LOOP
                    DELAY 1.0 * Impdef.One_Second;
               END LOOP;

            B: BEGIN
                    SELECT
                         ACCEPT E;
                    OR
                         DELAY NEG;
                         FAILED ("NEGATIVE DELAY ALTERNATIVE TAKEN");
                         ACCEPT E;
                    END SELECT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED (B)");
               END B;

          END T;

     BEGIN

          T.E;
          T.E;

     END;

     RESULT;

END C97120B;
