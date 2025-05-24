-- CB1010A.ADA

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
-- CHECK THAT STORAGE_ERROR IS RAISED WHEN STORAGE ALLOCATED TO A TASK
-- IS EXCEEDED.

-- PNH 8/26/85
-- JRK 8/30/85

WITH REPORT; USE REPORT;

PROCEDURE CB1010A IS

     N : INTEGER := IDENT_INT (1);
     M : INTEGER := IDENT_INT (0);

     PROCEDURE OVERFLOW_STACK IS
          A : ARRAY (1 .. 1000) OF INTEGER;
     BEGIN
          N := N + M;
          A (N) := M;
          IF N > M THEN  -- ALWAYS TRUE.
               OVERFLOW_STACK;
          END IF;
          M := A (N);    -- TO PREVENT TAIL RECURSION OPTIMIZATION.
     END OVERFLOW_STACK;

BEGIN
     TEST ("CB1010A", "CHECK THAT STORAGE_ERROR IS RAISED WHEN " &
                      "STORAGE ALLOCATED TO A TASK IS EXCEEDED");

     --------------------------------------------------

     COMMENT ("CHECK TASKS THAT DO NOT HANDLE STORAGE_ERROR " &
              "PRIOR TO RENDEZVOUS");

     DECLARE

          TASK T1 IS
               ENTRY E1;
          END T1;

          TASK BODY T1 IS
          BEGIN
               OVERFLOW_STACK;
               FAILED ("TASK T1 NOT TERMINATED BY STACK OVERFLOW");
          END T1;

     BEGIN

          T1.E1;
          FAILED ("NO EXCEPTION RAISED BY ENTRY CALL T1.E1");

     EXCEPTION
          WHEN TASKING_ERROR =>
               IF N /= 1 OR M /= 0 THEN
                    FAILED ("VALUES OF VARIABLES N OR M ALTERED - 1");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED BY CALL OF ENTRY E1 " &
                       "OF TERMINATED TASK T1");
     END;

     --------------------------------------------------

     COMMENT ("CHECK TASKS THAT DO HANDLE STORAGE_ERROR PRIOR TO " &
              "RENDEZVOUS");

     N := IDENT_INT (1);
     M := IDENT_INT (0);

     DECLARE

          TASK T2 IS
               ENTRY E2;
          END T2;

          TASK BODY T2 IS
          BEGIN
               OVERFLOW_STACK;
               FAILED ("EXCEPTION NOT RAISED BY STACK OVERFLOW IN " &
                       "TASK T2");
          EXCEPTION
               WHEN STORAGE_ERROR =>
                    ACCEPT E2;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED IN TASK T2 BY " &
                            "STACK OVERFLOW");
          END T2;

     BEGIN

          T2.E2;
          IF N /= 1 OR M /= 0 THEN
               FAILED ("VALUES OF VARIABLES N OR M ALTERED - 2");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY ENTRY CALL T2.E2");
               ABORT T2;
     END;

     --------------------------------------------------

     COMMENT ("CHECK TASKS THAT DO NOT HANDLE STORAGE_ERROR " &
              "DURING RENDEZVOUS");

     N := IDENT_INT (1);
     M := IDENT_INT (0);

     DECLARE

          TASK T3 IS
               ENTRY E3A;
               ENTRY E3B;
          END T3;

          TASK BODY T3 IS
          BEGIN
               ACCEPT E3A DO
                    OVERFLOW_STACK;
                    FAILED ("EXCEPTION NOT RAISED IN ACCEPT E3A BY " &
                            "STACK OVERFLOW");
               END E3A;
               FAILED ("EXCEPTION NOT PROPOGATED CORRECTLY IN TASK T3");
          EXCEPTION
               WHEN STORAGE_ERROR =>
                    ACCEPT E3B;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED IN TASK T3 BY " &
                            "STACK OVERFLOW");
          END T3;

     BEGIN

          T3.E3A;
          FAILED ("NO EXCEPTION RAISED BY ENTRY CALL T3.E3A");

     EXCEPTION
          WHEN STORAGE_ERROR =>
               T3.E3B;
               IF N /= 1 OR M /= 0 THEN
                    FAILED ("VALUES OF VARIABLES N OR M ALTERED - 3");
               END IF;
          WHEN TASKING_ERROR =>
               FAILED ("TASKING_ERROR RAISED BY ENTRY CALL T3.E3A " &
                       "INSTEAD OF STORAGE_ERROR");
               ABORT T3;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED BY ENTRY CALL T3.E3A");
               ABORT T3;
     END;

     --------------------------------------------------

     RESULT;
END CB1010A;
