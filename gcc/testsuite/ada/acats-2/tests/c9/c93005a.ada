-- C93005A.ADA

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
-- CHECK THAT WHEN AN EXCEPTION IS RAISED IN A DECLARATIVE PART, A TASK
-- DECLARED IN THE SAME DECLARATIVE PART BECOMES TERMINATED.

-- CHECK THAT A TASK WAITING ON ENTRIES OF SUCH A
-- TERMINATED-BEFORE-ACTIVATION TASK RECEIVES TASKING_ERROR.

-- JEAN-PIERRE ROSEN 3/9/84
-- JBG 06/01/84
-- JBG 05/23/85
-- EG  10/29/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C93005A IS

BEGIN
     TEST("C93005A", "EXCEPTIONS RAISED IN A DECLARATIVE PART " &
                     "CONTAINING TASKS");

     BEGIN

          DECLARE
               TASK TYPE T1 IS     -- CHECKS THAT T2 TERMINATES.
               END T1;

               TYPE AT1 IS ACCESS T1;

               TASK T2 IS          -- WILL NEVER BE ACTIVATED.
                    ENTRY E;
               END T2;

               PACKAGE RAISE_IT IS
               END RAISE_IT;

               TASK BODY T2 IS
               BEGIN
                    FAILED ("T2 ACTIVATED");
                    -- IN CASE OF FAILURE
                    LOOP
                         SELECT
                              ACCEPT E;
                         OR
                              TERMINATE;
                         END SELECT;
                    END LOOP;
               END T2;

               TASK BODY T1 IS
               BEGIN
                    DECLARE  -- THIS BLOCK TO CHECK THAT T3 TERMINATES.
                         TASK T3 IS
                         END T3;

                         TASK BODY T3 IS
                         BEGIN
                              T2.E;
                              FAILED ("RENDEZVOUS COMPLETED WITHOUT " &
                                      "ERROR - T3");
                         EXCEPTION
                              WHEN TASKING_ERROR =>
                                   NULL;
                              WHEN OTHERS =>
                                   FAILED("ABNORMAL EXCEPTION - T3");
                         END T3;
                    BEGIN
                         NULL;
                    END;

                    T2.E;    --T2 IS NOW TERMINATED

                    FAILED ("RENDEZVOUS COMPLETED WITHOUT ERROR - T1");

               EXCEPTION
                    WHEN TASKING_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("ABNORMAL EXCEPTION - T1");
               END;

               PACKAGE BODY RAISE_IT IS
                    PT1 : AT1 := NEW T1;
                    I   : POSITIVE := IDENT_INT(0); -- RAISE
                                                    -- CONSTRAINT_ERROR.
               BEGIN
                    IF I /= IDENT_INT(2) OR I = IDENT_INT(1) + 1 THEN
                         FAILED ("PACKAGE DIDN'T RAISE EXCEPTION");
                    END IF;
               END RAISE_IT;

          BEGIN     -- CAN'T LEAVE BLOCK UNTIL T1, T2, AND T3 ARE TERM.
               FAILED ("EXCEPTION NOT RAISED");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN TASKING_ERROR =>
               FAILED ("TASKING_ERROR IN MAIN PROGRAM");
          WHEN OTHERS =>
               FAILED ("ABNORMAL EXCEPTION IN MAIN-1");
     END;

     RESULT;

END C93005A;
