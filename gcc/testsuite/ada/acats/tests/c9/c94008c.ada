-- C94008C.ADA

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
-- CHECK THAT SELECT WITH TERMINATE ALTERNATIVE WORKS CORRECTLY WITH
-- NESTED TASKS.

-- THIS TEST CONTAINS RACE CONDITIONS AND USES A GENERIC INSTANCE THAT
-- CONTAINS TASKS.

-- JEAN-PIERRE ROSEN 24 FEBRUARY 1984
-- JRK 4/7/86
-- JBG 8/29/86 ELIMINATED SHARED VARIABLES; ADDED GENERIC UNIT
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C94008C IS


-- GENERIC UNIT FOR DOING UPDATES OF SHARED VARIABLES
     GENERIC
          TYPE HOLDER_TYPE IS PRIVATE;
          TYPE VALUE_TYPE IS PRIVATE;
          INITIAL_VALUE : HOLDER_TYPE;
          WITH PROCEDURE SET (HOLDER : OUT HOLDER_TYPE;
                              VALUE  : IN  HOLDER_TYPE) IS <>;
          WITH PROCEDURE UPDATE (HOLDER : IN OUT HOLDER_TYPE;
                                 VALUE  : IN  VALUE_TYPE) IS <>;
     PACKAGE SHARED IS
          PROCEDURE SET (VALUE : IN HOLDER_TYPE);
          PROCEDURE UPDATE (VALUE : IN VALUE_TYPE);
          FUNCTION GET RETURN HOLDER_TYPE;
     END SHARED;

     PACKAGE BODY SHARED IS
          TASK SHARE IS
               ENTRY SET    (VALUE : IN HOLDER_TYPE);
               ENTRY UPDATE (VALUE : IN VALUE_TYPE);
               ENTRY READ   (VALUE : OUT HOLDER_TYPE);
          END SHARE;

          TASK BODY SHARE IS
               VARIABLE : HOLDER_TYPE;
          BEGIN
               LOOP
                    SELECT
                         ACCEPT SET (VALUE : IN HOLDER_TYPE) DO
                              SHARED.SET (VARIABLE, VALUE);
                         END SET;
                    OR
                         ACCEPT UPDATE (VALUE : IN VALUE_TYPE) DO
                              SHARED.UPDATE (VARIABLE, VALUE);
                         END UPDATE;
                    OR
                         ACCEPT READ (VALUE : OUT HOLDER_TYPE) DO
                              VALUE := VARIABLE;
                         END READ;
                    OR
                         TERMINATE;
                    END SELECT;
               END LOOP;
          END SHARE;

          PROCEDURE SET (VALUE : IN HOLDER_TYPE) IS
          BEGIN
               SHARE.SET (VALUE);
          END SET;

          PROCEDURE UPDATE (VALUE : IN VALUE_TYPE) IS
          BEGIN
               SHARE.UPDATE (VALUE);
          END UPDATE;

          FUNCTION GET RETURN HOLDER_TYPE IS
               VALUE : HOLDER_TYPE;
          BEGIN
               SHARE.READ (VALUE);
               RETURN VALUE;
          END GET;

     BEGIN
          SHARE.SET (INITIAL_VALUE);    -- SET INITIAL VALUE
     END SHARED;

     PACKAGE EVENTS IS

          TYPE EVENT_TYPE IS
               RECORD
                    TRACE  : STRING (1..4) := "....";
                    LENGTH : NATURAL := 0;
               END RECORD;

          PROCEDURE UPDATE (VAR : IN OUT EVENT_TYPE; VAL : CHARACTER);
          PROCEDURE SET (VAR : OUT EVENT_TYPE; VAL : EVENT_TYPE);
     END EVENTS;

     PACKAGE COUNTER IS
          PROCEDURE UPDATE (VAR : IN OUT INTEGER; VAL : INTEGER);
          PROCEDURE SET (VAR : OUT INTEGER; VAL : INTEGER);
     END COUNTER;

     PACKAGE BODY COUNTER IS
          PROCEDURE UPDATE (VAR : IN OUT INTEGER; VAL : INTEGER) IS
          BEGIN
               VAR := VAR + VAL;
          END UPDATE;

          PROCEDURE SET (VAR : OUT INTEGER; VAL : INTEGER) IS
          BEGIN
               VAR := VAL;
          END SET;
     END COUNTER;

     PACKAGE BODY EVENTS IS
          PROCEDURE UPDATE (VAR : IN OUT EVENT_TYPE; VAL : CHARACTER) IS
          BEGIN
               VAR.LENGTH := VAR.LENGTH + 1;
               VAR.TRACE(VAR.LENGTH) := VAL;
          END UPDATE;

          PROCEDURE SET (VAR : OUT EVENT_TYPE; VAL : EVENT_TYPE) IS
          BEGIN
               VAR := VAL;
          END SET;

     END EVENTS;

     USE EVENTS, COUNTER;

     PACKAGE TRACE IS NEW SHARED (EVENT_TYPE, CHARACTER, ("....", 0));
     PACKAGE TERMINATE_COUNT IS NEW SHARED (INTEGER, INTEGER, 0);

     FUNCTION ENTER_TERMINATE RETURN BOOLEAN IS
     BEGIN
          TERMINATE_COUNT.UPDATE (1);
          RETURN TRUE;
     END ENTER_TERMINATE;

BEGIN -- C94008C

     TEST ("C94008C", "CHECK CORRECT OPERATION OF SELECT WITH " &
                      "TERMINATE ALTERNATIVE");

     DECLARE

          PROCEDURE EVENT (VAR : CHARACTER) RENAMES TRACE.UPDATE;

          TASK T1 IS
               ENTRY E1;
          END T1;

          TASK BODY T1 IS

               TASK T2 IS
                    ENTRY E2;
               END T2;

               TASK BODY T2 IS

                    TASK T3 IS
                         ENTRY E3;
                    END T3;

                    TASK BODY T3 IS
                    BEGIN
                         SELECT
                              ACCEPT E3;
                         OR WHEN ENTER_TERMINATE => TERMINATE;
                         END SELECT;
                         EVENT ('D');
                    END T3;

               BEGIN -- T2

                    SELECT
                         ACCEPT E2;
                    OR WHEN ENTER_TERMINATE => TERMINATE;
                    END SELECT;

                    DELAY 10.0 * Impdef.One_Second;

                    IF TERMINATE_COUNT.GET /= 1 THEN
                         DELAY 20.0 * Impdef.One_Long_Second;
                    END IF;

                    IF TERMINATE_COUNT.GET /= 1 THEN
                         FAILED ("30 SECOND DELAY NOT ENOUGH - 1 ");
                    END IF;

                    EVENT ('C');
                    T1.E1;
                    T3.E3;
               END T2;

          BEGIN -- T1;

               SELECT
                    ACCEPT E1;
               OR WHEN ENTER_TERMINATE => TERMINATE;
               END SELECT;

               EVENT ('B');
               TERMINATE_COUNT.SET (0);
               T2.E2;

               SELECT
                    ACCEPT E1;
               OR WHEN ENTER_TERMINATE => TERMINATE;
               END SELECT;

               SELECT
                    ACCEPT E1;
               OR TERMINATE;  -- ONLY THIS ONE EVER CHOSEN.
               END SELECT;

               FAILED ("TERMINATE NOT SELECTED IN T1");
          END T1;

     BEGIN

          DELAY 10.0 * Impdef.One_Second; -- WAIT FOR T1, T2, AND T3 TO GET TO SELECT STMTS.

           IF TERMINATE_COUNT.GET /= 3 THEN
                DELAY 20.0 * Impdef.One_Long_Second;
           END IF;

           IF TERMINATE_COUNT.GET /= 3 THEN
                FAILED ("30 SECOND DELAY NOT ENOUGH - 2");
           END IF;

          EVENT ('A');
          T1.E1;

     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION IN MAIN BLOCK");
     END;

     IF TRACE.GET.TRACE /= "ABCD" THEN
          FAILED ("INCORRECT ORDER OF EVENTS: " & TRACE.GET.TRACE);
     END IF;

     RESULT;
END C94008C;
