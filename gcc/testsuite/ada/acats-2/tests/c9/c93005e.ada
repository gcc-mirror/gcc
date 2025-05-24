-- C93005E.ADA

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
-- CHECK THAT IF AN EXCEPTION IS RAISED IN A DECLARATIVE PART OR PACKAGE
-- SPECIFICATION, A TASK DECLARED IN THE SAME DECLARATIVE PART BECOMES
-- COMPLETED BEFORE IT HAS BEEN ACTIVATED; ANY TASKS AWAITING A
-- RENDEZVOUS WITH THE COMPLETED RECEIVE TASKING_ERROR.

-- CASE 3: TASKS IN PACKAGE SPECIFICATION.
--         THE TASKS DON'T DEPEND ON THE PACKAGE SPECIFICATION.

-- RAC 19-MAR-1985
-- JBG 06/03/85
-- EG  10/30/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PRAGMA ELABORATE (REPORT);
PACKAGE C93005E_PK1 IS

     -- THIS TYPE OF TASK IS ALWAYS UNACTIVATED.
     TASK TYPE UNACTIVATED IS
          ENTRY E;
     END UNACTIVATED;

     TYPE ACC_UNACTIVATED IS ACCESS UNACTIVATED;

     TYPE BAD_REC IS
          RECORD
               T : UNACTIVATED;
               I : POSITIVE := IDENT_INT(0); -- RAISE CONSTRAINT_ERROR.
          END RECORD;

     TYPE ACC_BAD_REC IS ACCESS BAD_REC;


     -- *******************************************
     -- DEFINITIONS FOR MUST NOT BE TERMINATED TASKS
     -- *******************************************
     --
     -- THIS SET OF DECLARATIONS DEFINES A RECORD TYPE MNT (MUST NOT
     -- TERMINATE).  WHENEVER SUCH A RECORD IS DECLARED, A COUNT IS
     -- INCREMENTED AND A TASK IS CREATED.   THE TASK WILL DECREMENT THE
     -- COUNT UNLESS IT IS INCORRECTLY AND PREMATURELY TERMINATED.
     -- THE ROUTINE CHECK IS CALLED TO VERIFY WHETHER THE COUNT
     -- HAS RETURNED TO 0 (ALL MNT TASKS GOT A CHANCE TO DO THEIR
     -- DECREMENT).

     -- AN MNT TASK.   SUCH TASKS MUST NOT BE TERMINATED
     -- BY ANYONE BUT THEMSELVES.
     --
     TASK TYPE MNT_TASK IS
     END MNT_TASK;

     FUNCTION F RETURN INTEGER;

     -- THE RECORD THAT IS DECLARED TO HOLD AN MNT TASK
     -- AND FORCE CALLING F BEFORE CREATING THE TASK.
     -- F INCREMENTS THE COUNT, THE TASK DECREMENTS THE
     -- COUNT.
     --
     TYPE MNT IS
          RECORD
               DUMMY : INTEGER :=  F;
               T     : MNT_TASK;
          END RECORD;

     PROCEDURE CHECK;


     -- *******************************************
     -- END OF DEFINITIONS FOR MUST NOT BE TERMINATED TASKS
     -- *******************************************

END C93005E_PK1;

with Impdef;
PACKAGE BODY C93005E_PK1 IS

-- THIS TASK IS CALLED IF AN UNACTIVATED TASK
-- IS EVER INCORRECTLY ACTIVATED.  IT REPORTS FAILURE.

     TASK T IS
          ENTRY E;
     END;

     -- ***********************************************
     -- START OF DEFINITIONS FOR MUST NOT TERMINATE TASKS
     -- ***********************************************

-- COUNT OF TASKS THAT MUST NOT BE TERMINATED AND
-- ARE STILL ACTIVE.

     MNT_COUNT : INTEGER := 0;

-- TASK TO SYNCHRONIZE THE MNT_COUNT VARIABLE

     TASK MNT_COUNTER IS
          ENTRY INCR;
          ENTRY DECR;
     END MNT_COUNTER;

-- SYNCHRONIZING TASK

     TASK BODY MNT_COUNTER IS
     BEGIN
          LOOP
               SELECT
                    ACCEPT INCR DO
                         MNT_COUNT := MNT_COUNT +1;
                    END INCR;

               OR  ACCEPT DECR DO
                         MNT_COUNT := MNT_COUNT -1;
                    END DECR;

               OR  TERMINATE;

               END SELECT;
          END LOOP;
     END MNT_COUNTER;

-- INCREMENT THE MNT_COUNT WHEN A TASK IS CREATED
--
     FUNCTION F RETURN INTEGER IS
     BEGIN
          MNT_COUNTER.INCR;
          RETURN 0;
     END F;

-- CHECK THAT THE MUST NOT BE TERMINATED TASKS ARE
-- NOT YET TERMINATED AND THAT THE SYNCRHONIZING TASK
-- ITSELF IS NOT TERMINATED.
--
     PROCEDURE CHECK IS
     BEGIN
          IF MNT_COUNT /= 0 OR MNT_COUNTER'TERMINATED THEN
               FAILED ("SOME MUST-NOT-TERMINATE TASK WAS PREMATURELY " &
                       "TERMINATED");
          END IF;
-- RESET THE COUNT FOR THE NEXT SUBTEST:
          MNT_COUNT := 0;
     END CHECK;

-- A MUST NOT BE TERMINATED TASK.  DELAY LONG ENOUGH
-- TO BE THE LAST TASK OF A SCOPE TO TERMINATE.   THEN
-- DECREMENT THE COUNTER.
--
     TASK BODY MNT_TASK IS
     BEGIN
          DELAY 5.0 * Impdef.One_Second;
          MNT_COUNTER.DECR;
     END MNT_TASK;

     -- ***********************************************
     -- END OF DEFINITIONS FOR MUST NOT TERMINATE TASKS
     -- ***********************************************

     TASK BODY T IS
     BEGIN
          LOOP
               SELECT
                    ACCEPT E DO
                         FAILED ("SOME TYPE U TASK WAS ACTIVATED");
                    END E;

               OR   TERMINATE;
               END SELECT;
          END LOOP;
     END T;

     -- TASKS OF TYPE UNACTIVATED MUST NEVER BE ACTIVATED.
     --
     TASK BODY UNACTIVATED IS
     BEGIN
          T.E;
     END UNACTIVATED;
END C93005E_PK1;

WITH REPORT, C93005E_PK1;
USE  REPORT, C93005E_PK1;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C93005E IS


BEGIN

     TEST("C93005E", "TEST EXCEPTIONS TERMINATE NOT YET ACTIVATED " &
                     "TASKS");

     COMMENT("SUBTEST 3: TASK IN DECL PART OF PACKAGE SPEC");
     COMMENT("  THE TASKS DON'T DEPEND ON THE DECLARATIVE PART");
B31: DECLARE
          X : MNT;
     BEGIN
B32:      BEGIN
B33:           DECLARE
                    PACKAGE RAISES_EXCEPTION IS
                         TYPE ACC_MNT IS ACCESS MNT;
                         Y   : ACC_MNT := NEW MNT;
                         PTR : ACC_BAD_REC := NEW BAD_REC;
                    END RAISES_EXCEPTION;
               BEGIN  -- WOULD HAVE BEEN ACTIVATED HERE
                    FAILED("EXCEPTION NOT RAISED");
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN WRONG SCOPE");
               END B33;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    COMMENT("SUBTEST 3 COMPLETED");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED IN B32");
          END B32;
     END B31;

     CHECK;

     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED ("EXCEPTION NOT ABSORBED");
          RESULT;
END C93005E;
