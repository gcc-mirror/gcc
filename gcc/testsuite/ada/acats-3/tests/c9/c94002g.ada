-- C94002G.ADA

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
--     CHECK THAT A NON-MASTER UNIT, WHICH ALLOCATES TASKS OF A GLOBAL
--     ACCESS TYPE, MUST TERMINATE WITHOUT WAITING FOR THE ALLOCATED
--     TASKS TO TERMINATE IF AN EXCEPTION IS RAISED BUT NOT HANDLED IN
--     THE NON-MASTER UNIT.

--     SUBTESTS ARE:
--        (A)  A SIMPLE TASK ALLOCATOR, IN A BLOCK.
--        (B)  A RECORD OF TASK ALLOCATOR, IN A SUBPROGRAM.
--        (C)  A RECORD OF ARRAY OF TASK ALLOCATOR, IN A TASK BODY, NOT
--             DURING RENDEZVOUS.
--        (D)  A LIMITED PRIVATE TASK ALLOCATOR, IN A TASK BODY, DURING
--             RENDEZVOUS.

-- HISTORY:
--     TBN 01/20/86  CREATED ORIGINAL TEST.
--      JRK 05/01/86  IMPROVED ERROR RECOVERY.  FIXED EXCEPTION
--                    HANDLING.  ADDED CASE (D).
--      BCB 09/24/87  ADDED A RETURN STATEMENT TO THE HANDLER FOR OTHERS
--                    IN FUNCTION F, CASE B.
--      PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C94002G IS

     MY_EXCEPTION : EXCEPTION;

     TASK TYPE TT IS
          ENTRY E;
     END TT;

     TASK BODY TT IS
     BEGIN
          ACCEPT E;
          ACCEPT E;
     END TT;


BEGIN
     TEST ("C94002G", "CHECK THAT A NON-MASTER UNIT, WHICH ALLOCATES " &
                      "TASKS OF A GLOBAL ACCESS TYPE, MUST TERMINATE " &
                      "WITHOUT WAITING FOR THE ALLOCATED TASKS TO " &
                      "TERMINATE IF AN EXCEPTION IS RAISED BUT NOT " &
                      "HANDLED IN THE NON-MASTER UNIT");

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
               RAISE MY_EXCEPTION;
               FAILED ("MY_EXCEPTION WAS NOT RAISED IN (A)");
          END;

          ABORT A1.ALL;

     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF A1.ALL'TERMINATED THEN
                    FAILED ("ALLOCATED TASK PREMATURELY TERMINATED - " &
                            "(A)");
               ELSE A1.ALL.E;
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION IN (A)");
               IF A1 /= NULL THEN
                    ABORT A1.ALL;
               END IF;
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

               PROCEDURE P IS
                    AR2 : ART;
               BEGIN
                    AR2 := NEW RT;
                    AR2.T.E;
                    AR1 := AR2;
                    RAISE MY_EXCEPTION;
                    FAILED ("MY_EXCEPTION WAS NOT RAISED IN (B)");
               END P;

          BEGIN
               P;
               ABORT AR1.T;
               RETURN 0;
          EXCEPTION
               WHEN MY_EXCEPTION =>
                    IF AR1.T'TERMINATED THEN
                         FAILED ("ALLOCATED TASK PREMATURELY " &
                                 "TERMINATED - (B)");
                    ELSE AR1.T.E;
                    END IF;
                    RETURN 0;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION IN (B)");
                    IF AR1 /= NULL THEN
                         ABORT AR1.T;
                    END IF;
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
                    ARA2 := NEW RAT;         -- INITIATE TASK ARA2.T(1).
                    ARA2.T(1).E;
                    ACCEPT ENT1 (ARA : OUT ARAT) DO
                         ARA := ARA2;
                    END ENT1;
                    RAISE MY_EXCEPTION;       -- NOT PROPOGATED.
                    FAILED ("MY_EXCEPTION WAS NOT RAISED IN (C)");
               END TSK1;

          BEGIN
               TSK1.ENT1 (ARA1);     -- ARA1.T BECOMES ALIAS FOR ARA2.T.

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
               ELSE ARA1.T(1).E;
               END IF;
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

     DECLARE -- (D)

          LOOP_COUNT : INTEGER := 0;
          CUT_OFF : CONSTANT := 60;                -- DELAY.

          TASK TSK IS
               ENTRY ENT;
          END TSK;

          TASK BODY TSK IS

               LOOP_COUNT1 : INTEGER := 0;
               CUT_OFF1 : CONSTANT := 60;          -- DELAY.

               PACKAGE PKG IS
                    TYPE LPT IS LIMITED PRIVATE;
                    PROCEDURE CALL (X : LPT);
                    PROCEDURE KILL (X : LPT);
                    FUNCTION TERMINATED (X : LPT) RETURN BOOLEAN;
               PRIVATE
                    TYPE LPT IS NEW TT;
               END PKG;

               USE PKG;

               TYPE ALPT IS ACCESS LPT;
               ALP1 : ALPT;

               PACKAGE BODY PKG IS
                    PROCEDURE CALL (X : LPT) IS
                    BEGIN
                         X.E;
                    END CALL;

                    PROCEDURE KILL (X : LPT) IS
                    BEGIN
                         ABORT X;
                    END KILL;

                    FUNCTION TERMINATED (X : LPT) RETURN BOOLEAN IS
                    BEGIN
                         RETURN X'TERMINATED;
                    END TERMINATED;
               END PKG;

               TASK TSK1 IS
                    ENTRY ENT1 (ALP : OUT ALPT);
                    ENTRY DIE;
               END TSK1;

               TASK BODY TSK1 IS
                    ALP2 : ALPT;
               BEGIN
                    ALP2 := NEW LPT;         -- INITIATE TASK ALP2.ALL.
                    CALL (ALP2.ALL);
                    ACCEPT ENT1 (ALP : OUT ALPT) DO
                         ALP := ALP2;
                    END ENT1;
                    ACCEPT DIE DO
                         RAISE MY_EXCEPTION;       -- PROPOGATED.
                         FAILED ("MY_EXCEPTION WAS NOT RAISED IN (D)");
                    END DIE;
               END TSK1;

          BEGIN
               TSK1.ENT1 (ALP1); -- ALP1.ALL BECOMES ALIAS FOR ALP2.ALL.
               TSK1.DIE;
               FAILED ("MY_EXCEPTION WAS NOT PROPOGATED TO CALLING " &
                       "TASK - (D)");
               KILL (ALP1.ALL);
               ABORT TSK1;
          EXCEPTION
               WHEN MY_EXCEPTION =>
                    WHILE NOT TSK1'TERMINATED AND
                          LOOP_COUNT1 < CUT_OFF1 LOOP
                         DELAY 1.0 * Impdef.One_Second;
                         LOOP_COUNT1 := LOOP_COUNT1 + 1;
                    END LOOP;

                    IF LOOP_COUNT1 >= CUT_OFF1 THEN
                         FAILED ("DEPENDENT TASK TSK1 NOT TERMINATED " &
                                 "WITHIN ONE MINUTE - (D)");
                    END IF;

                    IF TERMINATED (ALP1.ALL) THEN
                         FAILED ("ALLOCATED TASK PREMATURELY " &
                                 "TERMINATED - (D)");
                    ELSE CALL (ALP1.ALL);
                    END IF;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION IN (D)");
                    IF ALP1 /= NULL THEN
                         KILL (ALP1.ALL);
                    END IF;
                    ABORT TSK1;
          END TSK;

     BEGIN -- (D)

          WHILE NOT TSK'TERMINATED AND LOOP_COUNT < CUT_OFF LOOP
               DELAY 2.0 * Impdef.One_Second;
               LOOP_COUNT := LOOP_COUNT + 1;
          END LOOP;

          IF LOOP_COUNT >= CUT_OFF THEN
               FAILED ("DEPENDENT TASK TSK NOT TERMINATED WITHIN " &
                       "TWO MINUTES - (D)");
          END IF;

     END; -- (D)

     --------------------------------------------------

     RESULT;
END C94002G;
