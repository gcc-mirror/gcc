-- C93004D.ADA

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
-- CHECK THAT WHEN AN EXCEPTION IS RAISED DURING THE ACTIVATION OF A
-- TASK, OTHER TASKS ARE UNAFFECTED.

-- THIS TEST CHECKS THE CASE IN WHICH SOME OF THE OTHER TASKS ARE 
-- PERHAPS ACTIVATED BEFORE THE EXCEPTION OCCURS AND SOME TASKS ARE 
-- PERHAPS ACTIVATED AFTER.

-- THE ENCLOSING BLOCK RECEIVES TASKING_ERROR.

-- CHECK THAT TASKS WAITING FOR ENTRIES OF SUCH TASKS RECEIVE
-- TASKING_ERROR.

-- R. WILLIAMS 8/6/86
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE C93004D IS

          
BEGIN
     TEST ( "C93004D", "CHECK THAT WHEN AN EXCEPTION IS RAISED " &
                       "DURING ACTIVATION OF A TASK, OTHER TASKS " &
                       "ARE NOT AFFECTED. IN THIS TEST, SOME OF THE " &
                       "TASKS ARE PERHAPS ACTIVATED BEFORE THE " &
                       "EXCEPTION OCCURS AND SOME PERHAPS AFTER" );


     DECLARE

          TASK T0 IS
               ENTRY E;
          END T0;

          TASK TYPE T1 IS
          END T1;

          TASK TYPE T2 IS
               ENTRY E;
          END T2;     

          ARR_T2: ARRAY(INTEGER RANGE 1..4) OF T2;

          TYPE AT1 IS ACCESS T1;

          TASK BODY T0 IS
          BEGIN
               ACCEPT E;
          END T0;

          PACKAGE START_T1 IS      -- THIS PACKAGE TO AVOID ACCESS
          END START_T1;            -- BEFORE ELABORATION ON T1.

          TASK BODY T1 IS
          BEGIN
               DECLARE   -- THIS BLOCK TO CHECK THAT T1BIS TERMINATES.
                    TASK T1BIS IS
                    END T1BIS;

                    TASK BODY T1BIS IS
                    BEGIN
                         ARR_T2(IDENT_INT(2)).E;
                         FAILED ("RENDEZVOUS COMPLETED - T3");
                    EXCEPTION
                         WHEN TASKING_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED("ABNORMAL EXCEPTION - T3");
                    END T1BIS;
               BEGIN
                    NULL;
               END;

               ARR_T2(IDENT_INT(2)).E;   -- ARR_T2(2) IS NOW 
                                         -- TERMINATED.

               FAILED ("RENDEZVOUS COMPLETED WITHOUT ERROR - T1");

          EXCEPTION
               WHEN TASKING_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("ABNORMAL EXCEPTION - T1");
          END;

          PACKAGE BODY START_T1 IS
               V_AT1 : AT1 := NEW T1;
          END START_T1;

          TASK BODY T2 IS
               I : POSITIVE := IDENT_INT(0); -- RAISE CONSTRAINT_ERROR.
          BEGIN
               IF I /= IDENT_INT(2) OR I = IDENT_INT(1) + 1 THEN
                    FAILED("T2 ACTIVATED OK");
               END IF;
          END T2;

          TASK T3 IS
               ENTRY E;
          END T3;

          TASK BODY T3 IS
          BEGIN     -- T3 MUST BE ACTIVATED OK.
               ACCEPT E;
          END T3;

     BEGIN -- T0, ARR_T2 (1 .. 4), T3 ACTIVATED HERE.

          FAILED ("TASKING_ERROR NOT RAISED IN MAIN");
          T3.E;          -- CLEAN UP.
          T0.E;
     EXCEPTION
          WHEN TASKING_ERROR =>
               BEGIN
                    T3.E;
                    T0.E;
               EXCEPTION
                    WHEN TASKING_ERROR =>
                         FAILED ("T0 OR T3 NOT ACTIVATED");
               END;
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED IN MAIN");
          WHEN OTHERS =>
               FAILED ("ABNORMAL EXCEPTION IN MAIN-2");
     END;

     RESULT;
END C93004D;
