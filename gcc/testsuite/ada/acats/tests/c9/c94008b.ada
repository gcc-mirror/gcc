-- C94008B.ADA

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
-- CHECK THAT A TASK WAITING AT AN OPEN TERMINATE ALTERNATIVE
-- DOES  N O T  TERMINATE UNTIL ALL OTHER TASKS DEPENDING ON THE SAME
-- UNIT EITHER ARE TERMINATED OR ARE WAITING AT AN OPEN TERMINATE.

-- WEI  3/ 4/82
-- TBN 11/25/85     RENAMED FROM C940BBA-B.ADA.

with Impdef;
WITH REPORT;
 USE REPORT;
PROCEDURE C94008B IS
BEGIN
     TEST ("C94008B", "TERMINATION WHILE WAITING AT AN OPEN TERMINATE");

BLOCK1 :
     DECLARE

          TASK TYPE TT1 IS
               ENTRY E1;
          END TT1;

          NUMB_TT1 : CONSTANT NATURAL := 3;
          DELAY_TIME : DURATION := 0.0;
          ARRAY_TT1 : ARRAY (1 .. NUMB_TT1) OF TT1;

          TASK BODY TT1 IS
          BEGIN
               DELAY_TIME := DELAY_TIME + 1.0 * Impdef.One_Second;
               DELAY DELAY_TIME;
               FOR I IN 1 .. NUMB_TT1
               LOOP
                    IF ARRAY_TT1 (I)'TERMINATED THEN
                         FAILED ("TOO EARLY TERMINATION OF " &
                                 "TASK TT1 INDEX" & INTEGER'IMAGE(I));
                    END IF;
               END LOOP;

               SELECT
                    WHEN TRUE => TERMINATE;
                 OR WHEN FALSE => ACCEPT E1;
               END SELECT;
          END TT1;

     BEGIN  -- BLOCK1.
          FOR I IN 1 .. NUMB_TT1
          LOOP
               IF ARRAY_TT1 (I)'TERMINATED THEN
                    FAILED ("TERMINATION BEFORE OUTER " &
                            "UNIT HAS BEEN LEFT OF TASK TT1 INDEX " &
                            INTEGER'IMAGE(I));
               END IF;
          END LOOP;
     END BLOCK1;

     RESULT;

END C94008B;
