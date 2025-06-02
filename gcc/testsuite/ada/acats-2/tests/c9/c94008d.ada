-- C94008D.ADA

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
-- CHECK CORRECT OPERATION OF SELECT WITH TERMINATE ALTERNATIVE WHEN
-- EXECUTED FROM AN INNER BLOCK WITH OUTER DEPENDING TASKS.

-- JEAN-PIERRE ROSEN 03-MAR-84
-- JRK 4/7/86
-- JBG 9/4/86 ELIMINATED SHARED VARIABLES; ADDED GENERIC UNIT/SUBUNIT
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

-- GENERIC UNIT FOR DOING UPDATES OF SHARED VARIABLES
GENERIC
     TYPE HOLDER_TYPE IS PRIVATE;
     TYPE VALUE_TYPE IS PRIVATE;
     INITIAL_VALUE : HOLDER_TYPE;
     WITH PROCEDURE SET (HOLDER : OUT HOLDER_TYPE;
                         VALUE  : IN  HOLDER_TYPE) IS <>;
     WITH PROCEDURE UPDATE (HOLDER : IN OUT HOLDER_TYPE;
                            VALUE  : IN  VALUE_TYPE) IS <>;
PACKAGE SHARED_C94008D IS
     PROCEDURE SET (VALUE : IN HOLDER_TYPE);
     PROCEDURE UPDATE (VALUE : IN VALUE_TYPE);
     FUNCTION GET RETURN HOLDER_TYPE;
END SHARED_C94008D;

PACKAGE BODY SHARED_C94008D IS
     TASK SHARE IS
          ENTRY SET    (VALUE : IN HOLDER_TYPE);
          ENTRY UPDATE (VALUE : IN VALUE_TYPE);
          ENTRY READ   (VALUE : OUT HOLDER_TYPE);
     END SHARE;

     TASK BODY SHARE IS SEPARATE;

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
END SHARED_C94008D;

PACKAGE EVENTS_C94008D IS

     TYPE EVENT_TYPE IS
          RECORD
               TRACE  : STRING (1..4) := "....";
               LENGTH : NATURAL := 0;
          END RECORD;

     PROCEDURE UPDATE (VAR : IN OUT EVENT_TYPE; VAL : CHARACTER);
     PROCEDURE SET (VAR : OUT EVENT_TYPE; VAL : EVENT_TYPE);
END EVENTS_C94008D;

PACKAGE COUNTER_C94008D IS
     PROCEDURE UPDATE (VAR : IN OUT INTEGER; VAL : INTEGER);
     PROCEDURE SET (VAR : OUT INTEGER; VAL : INTEGER);
END COUNTER_C94008D;

PACKAGE BODY COUNTER_C94008D IS
     PROCEDURE UPDATE (VAR : IN OUT INTEGER; VAL : INTEGER) IS
     BEGIN
          VAR := VAR + VAL;
     END UPDATE;

     PROCEDURE SET (VAR : OUT INTEGER; VAL : INTEGER) IS
     BEGIN
          VAR := VAL;
     END SET;
END COUNTER_C94008D;

PACKAGE BODY EVENTS_C94008D IS
     PROCEDURE UPDATE (VAR : IN OUT EVENT_TYPE; VAL : CHARACTER) IS
     BEGIN
          VAR.LENGTH := VAR.LENGTH + 1;
          VAR.TRACE(VAR.LENGTH) := VAL;
     END UPDATE;

     PROCEDURE SET (VAR : OUT EVENT_TYPE; VAL : EVENT_TYPE) IS
     BEGIN
          VAR := VAL;
     END SET;

END EVENTS_C94008D;

SEPARATE (SHARED_C94008D)
TASK BODY SHARE IS
     VARIABLE : HOLDER_TYPE;
BEGIN
     LOOP
          SELECT
               ACCEPT SET (VALUE : IN HOLDER_TYPE) DO
                    SHARED_C94008D.SET (VARIABLE, VALUE);
               END SET;
          OR
               ACCEPT UPDATE (VALUE : IN VALUE_TYPE) DO
                    SHARED_C94008D.UPDATE (VARIABLE, VALUE);
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

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
WITH SHARED_C94008D, COUNTER_C94008D, EVENTS_C94008D;
USE  COUNTER_C94008D, EVENTS_C94008D;
PROCEDURE C94008D IS

     PACKAGE TRACE IS
          NEW SHARED_C94008D (EVENT_TYPE, CHARACTER, ("....", 0));
     PACKAGE TERMINATE_COUNT IS
          NEW SHARED_C94008D (INTEGER, INTEGER, 0);

     PROCEDURE EVENT (VAR : CHARACTER) RENAMES TRACE.UPDATE;

     FUNCTION ENTER_TERMINATE RETURN BOOLEAN IS
     BEGIN
          TERMINATE_COUNT.UPDATE (1);
          RETURN TRUE;
     END ENTER_TERMINATE;

BEGIN
     TEST ("C94008D", "CHECK CORRECT OPERATION OF SELECT WITH " &
                      "TERMINATE ALTERNATIVE FROM AN INNER BLOCK");

     DECLARE

          TASK T1 IS
               ENTRY E1;
          END T1;

          TASK BODY T1 IS
          BEGIN
               DECLARE

                    TASK T2 IS
                         ENTRY E2;
                    END T2;

                    TASK BODY T2 IS
                    BEGIN
                         DELAY 10.0 * Impdef.One_Second;

                         IF TERMINATE_COUNT.GET /= 1 THEN
                              DELAY 20.0 * Impdef.One_Second;
                         END IF;

                         IF TERMINATE_COUNT.GET /= 1 THEN
                              FAILED ("30 SECOND DELAY NOT ENOUGH");
                         END IF;

                         IF T1'TERMINATED OR NOT T1'CALLABLE THEN
                              FAILED ("T1 PREMATURELY TERMINATED");
                         END IF;

                         EVENT ('A');

                         SELECT
                              ACCEPT E2;
                         OR TERMINATE;
                         END SELECT;

                         FAILED ("TERMINATE NOT SELECTED IN T2");
                    END T2;

               BEGIN
                    BEGIN
                         EVENT ('B');

                         SELECT
                              ACCEPT E1;
                         OR WHEN ENTER_TERMINATE => TERMINATE;
                         END SELECT;

                         FAILED ("TERMINATE NOT SELECTED IN T1");
                    END;
               END;
          END T1;

     BEGIN
          EVENT ('C');
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RECEIVED IN MAIN");
     END;

     IF TRACE.GET.TRACE(3) = '.' OR TRACE.GET.TRACE(4) /= '.' THEN
          FAILED ("ALL EVENTS NOT PROCESSED CORRECTLY");
     END IF;

     COMMENT ("EXECUTION ORDER WAS " & TRACE.GET.TRACE);

     RESULT;
END C94008D;
