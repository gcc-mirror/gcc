-- C94001G.ADA

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
-- CHECK THAT A COMPLETED TASK WITH DEPENDENT TASKS TERMINATES WHEN
-- A L L  DEPENDENT TASKS HAVE TERMINATED.

-- WEI  3/ 4/82
-- JBG 4/2/84
-- JWC 6/28/85   RENAMED FROM C940AIA-B.ADA

with Impdef;
WITH REPORT;
 USE REPORT;
PROCEDURE C94001G IS

     PACKAGE SPY IS      -- PROVIDE PROTECTED ACCESS TO SPYNUMB
          SUBTYPE ARG IS NATURAL RANGE 0..9;
          FUNCTION SPYNUMB RETURN NATURAL;                      -- READ
          FUNCTION FINIT_POS (DIGT : IN ARG) RETURN NATURAL;    -- WRITE
          PROCEDURE PSPY_NUMB (DIGT : IN ARG);                  -- WRITE
     END SPY;

     USE SPY;

     PACKAGE BODY SPY IS

          TASK GUARD IS
               ENTRY READ  (NUMB : OUT NATURAL);
               ENTRY WRITE (NUMB : IN NATURAL);
          END GUARD;

          TASK BODY GUARD IS
               SPYNUMB : NATURAL := 0;
          BEGIN
               LOOP
                    SELECT
                         ACCEPT READ (NUMB : OUT NATURAL) DO
                              NUMB := SPYNUMB;
                         END READ;
                    OR   ACCEPT WRITE (NUMB : IN NATURAL) DO
                              SPYNUMB := 10*SPYNUMB+NUMB;
                         END WRITE;
                    OR   TERMINATE;
                    END SELECT;
               END LOOP;
          END GUARD;

          FUNCTION SPYNUMB RETURN NATURAL IS
               TEMP : NATURAL;
          BEGIN
               GUARD.READ (TEMP);
               RETURN TEMP;
          END SPYNUMB;

          FUNCTION FINIT_POS (DIGT: IN ARG) RETURN NATURAL IS
          BEGIN
               GUARD.WRITE (DIGT);
               RETURN DIGT;
          END FINIT_POS;

          PROCEDURE PSPY_NUMB (DIGT : IN ARG) IS
          BEGIN
               GUARD.WRITE (DIGT);
          END PSPY_NUMB;
     END SPY;

BEGIN
     TEST ("C94001G", "TERMINATION WHEN ALL DEPENDENT TASKS " &
                      "HAVE TERMINATED");

BLOCK:
     DECLARE

          TASK TYPE TT1;

          TASK BODY TT1 IS
          BEGIN
               DELAY 1.0 * Impdef.One_Second;
               PSPY_NUMB (1);
          END TT1;

          TASK T1 IS
          END T1;

          TASK BODY T1 IS
               OBJ_TT1_1, OBJ_TT1_2, OBJ_TT1_3 : TT1;
          BEGIN
               NULL;
          END T1;

     BEGIN
          NULL;
     END BLOCK;               -- WAIT HERE FOR TERMINATION.

     IF SPYNUMB /= 111 THEN
          FAILED ("TASK T1 TERMINATED BEFORE " &
                  "ALL DEPENDENT TASKS HAVE TERMINATED");
          COMMENT ("ACTUAL ORDER WAS:" & INTEGER'IMAGE(SPYNUMB));
     END IF;

     RESULT;

END C94001G;
