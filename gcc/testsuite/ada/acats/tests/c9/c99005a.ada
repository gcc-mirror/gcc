-- C99005A.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE ATTRIBUTE 'COUNT RETURNS THE CORRECT VALUE.

-- HISTORY:
--     DHH 03/24/88 CREATED ORIGINAL TEST.

with Impdef;
WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE C99005A IS

BEGIN

     TEST("C99005A", "CHECK THAT THE ATTRIBUTE 'COUNT RETURNS THE " &
                     "CORRECT VALUE");

     DECLARE
          TASK A IS
          END A;

          TASK B IS
          END B;

          TASK C IS
          END C;

          TASK D IS
          END D;

          TASK E IS
          END E;

          TASK F IS
          END F;

          TASK G IS
          END G;

          TASK H IS
          END H;

          TASK I IS
          END I;

          TASK J IS
          END J;

          TASK T IS
               ENTRY WAIT;
          END T;

          TASK CHOICE IS
               ENTRY RETURN_CALL;
               ENTRY E2;
               ENTRY E1;
          END CHOICE;

          TASK BODY A IS
          BEGIN
               CHOICE.E1;
          END A;

          TASK BODY B IS
          BEGIN
               CHOICE.E1;
          END B;

          TASK BODY C IS
          BEGIN
               CHOICE.E1;
          END C;

          TASK BODY D IS
          BEGIN
               CHOICE.E1;
          END D;

          TASK BODY E IS
          BEGIN
               CHOICE.E1;
          END E;

          TASK BODY F IS
          BEGIN
               CHOICE.E2;
          END F;

          TASK BODY G IS
          BEGIN
               CHOICE.E2;
          END G;

          TASK BODY H IS
          BEGIN
               CHOICE.E2;
          END H;

          TASK BODY I IS
          BEGIN
               CHOICE.E2;
          END I;

          TASK BODY J IS
          BEGIN
               CHOICE.E2;
          END J;

          TASK BODY T IS
          BEGIN
               LOOP
                    SELECT
                         ACCEPT WAIT DO
                              DELAY 1.0 * Impdef.One_Second;
                         END WAIT;
                         CHOICE.RETURN_CALL;
                    OR
                         TERMINATE;
                    END SELECT;
               END LOOP;
          END T;

          TASK BODY CHOICE IS
          BEGIN
               WHILE E1'COUNT + E2'COUNT < 10 LOOP
                    T.WAIT;
                    ACCEPT RETURN_CALL;
               END LOOP;

               FOR I IN REVERSE 1 ..10 LOOP
                    SELECT
                         ACCEPT E2 DO
                              IF (E2'COUNT + E1'COUNT + 1) /= I THEN
                                   FAILED("'COUNT NOT RETURNING " &
                                          "CORRECT VALUE FOR LOOP" &
                                           INTEGER'IMAGE(I) & "VALUE " &
                                           INTEGER'IMAGE((E2'COUNT
                                           + E1'COUNT + 1)));
                              END IF;
                         END E2;
                    OR
                         ACCEPT E1 DO
                              IF (E2'COUNT + E1'COUNT + 1) /= I THEN
                                   FAILED("'COUNT NOT RETURNING " &
                                          "CORRECT VALUE FOR LOOP" &
                                           INTEGER'IMAGE(I) & "VALUE " &
                                           INTEGER'IMAGE((E2'COUNT
                                           + E1'COUNT + 1)));
                              END IF;
                         END E1;
                    END SELECT;
               END LOOP;
           END CHOICE;

     BEGIN
          NULL;
     END;

     RESULT;
END C99005A;
