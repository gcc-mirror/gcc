-- C94002B.ADA

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
-- CHECK THAT A MASTER UNIT, WHICH ALLOCATES TASKS OF A GLOBAL ACCESS
--   TYPE MAY TERMINATE WITHOUT WAITING FOR THE ALLOCATED TASKS TO
--   TERMINATE.

-- SUBTESTS ARE:
--   (A)  A SIMPLE TASK ALLOCATOR, IN A BLOCK.
--   (B)  A RECORD OF TASK ALLOCATOR, IN A SUBPROGRAM.
--   (C)  A RECORD OF ARRAY OF TASK ALLOCATOR, IN A TASK BODY.

-- JRK 10/8/81
-- SPS 11/2/82
-- SPS 11/21/82
-- JRK 11/29/82
-- TBN 1/20/86     REPLACED WITH C94006A-B.ADA AFTER LOWERING THE DELAY
--                 VALUES, AND MODIFYING THE COMMENTS.
-- PWN 09/11/94    REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C94002B IS

     TASK TYPE TT IS
          ENTRY E;
     END TT;

     TASK BODY TT IS
     BEGIN
          ACCEPT E;
          ACCEPT E;
     END TT;


BEGIN
     TEST ("C94002B", "CHECK THAT A MASTER UNIT, WHICH ALLOCATES " &
                      "TASKS OF A GLOBAL ACCESS TYPE MAY TERMINATE " &
                      "WITHOUT WAITING FOR THE ALLOCATED TASKS TO " &
                      "TERMINATE");

     --------------------------------------------------

     DECLARE -- (A)

          TYPE A_T IS ACCESS TT;
          A1 : A_T;

     BEGIN -- (A)

          DECLARE
               A2 : A_T;
          BEGIN
               A2 := NEW TT;
               A2.ALL.E;
               A1 := A2;
          END;

          IF A1.ALL'TERMINATED THEN
               FAILED ("ALLOCATED TASK PREMATURELY TERMINATED - (A)");
          END IF;

          A1.ALL.E;

     END; -- (A)

     --------------------------------------------------

     DECLARE -- (B)

          I : INTEGER;

          FUNCTION F RETURN INTEGER IS

               TYPE RT IS
                    RECORD
                         T : TT;
                    END RECORD;
               TYPE ART IS ACCESS RT;
               AR1 : ART;

               PROCEDURE P (AR : OUT ART) IS
                    AR2 : ART;
               BEGIN
                    AR2 := NEW RT;
                    AR2.T.E;
                    AR := AR2;
               END P;

          BEGIN
               P (AR1);

               IF AR1.T'TERMINATED THEN
                    FAILED ("ALLOCATED TASK PREMATURELY TERMINATED " &
                            "- (B)");
               END IF;

               AR1.T.E;
               RETURN 0;
          END F;

     BEGIN -- (B)

          I := F;

     END; -- (B)

     --------------------------------------------------

     DECLARE -- (C)

          LOOP_COUNT : INTEGER := 0;
          CUT_OFF : CONSTANT := 60;                -- DELAY.

          TASK TSK IS
               ENTRY ENT;
          END TSK;

          TASK BODY TSK IS

               LOOP_COUNT1 : INTEGER := 0;
               CUT_OFF1 : CONSTANT := 60;          -- DELAY.

               TYPE RAT;
               TYPE ARAT IS ACCESS RAT;
               TYPE ARR IS ARRAY (1..1) OF TT;
               TYPE RAT IS
                    RECORD
                         A : ARAT;
                         T : ARR;
                    END RECORD;
               ARA1 : ARAT;

               TASK TSK1 IS
                    ENTRY ENT1 (ARA : OUT ARAT);
               END TSK1;

               TASK BODY TSK1 IS
                    ARA2 : ARAT;
               BEGIN
                    ARA2 := NEW RAT;
                    ARA2.T(1).E;
                    ACCEPT ENT1 (ARA : OUT ARAT) DO
                         ARA := ARA2;
                    END ENT1;
               END TSK1;

          BEGIN
               TSK1.ENT1 (ARA1);

               WHILE NOT TSK1'TERMINATED AND LOOP_COUNT1 < CUT_OFF1 LOOP
                    DELAY 1.0 * Impdef.One_Second;
                    LOOP_COUNT1 := LOOP_COUNT1 + 1;
               END LOOP;

               IF LOOP_COUNT1 >= CUT_OFF1 THEN
                    FAILED ("DEPENDENT TASK TSK1 NOT TERMINATED " &
                            "WITHIN ONE MINUTE - (C)");
               END IF;

               IF ARA1.T(1)'TERMINATED THEN
                    FAILED ("ALLOCATED TASK PREMATURELY TERMINATED " &
                            "- (C)");
               END IF;

               ARA1.T(1).E;
          END TSK;

     BEGIN -- (C)

          WHILE NOT TSK'TERMINATED AND LOOP_COUNT < CUT_OFF LOOP
               DELAY 2.0 * Impdef.One_Second;
               LOOP_COUNT := LOOP_COUNT + 1;
          END LOOP;

          IF LOOP_COUNT >= CUT_OFF THEN
               FAILED ("DEPENDENT TASK TSK NOT TERMINATED WITHIN " &
                       "TWO MINUTES - (C)");
          END IF;

     END; -- (C)

     --------------------------------------------------

     RESULT;
END C94002B;
