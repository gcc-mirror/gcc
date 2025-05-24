-- C94010A.ADA

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
-- CHECK THAT IF A GENERIC UNIT HAS A FORMAL LIMITED PRIVATE TYPE AND
-- DECLARES AN OBJECT OF THAT TYPE (OR HAS A SUBCOMPONENT OF THAT TYPE),
-- AND IF THE UNIT IS INSTANTIATED WITH A TASK TYPE OR AN OBJECT HAVING
-- A SUBCOMPONENT OF A TASK TYPE, THEN THE USUAL RULES APPLY TO THE
-- INSTANTIATED UNIT, NAMELY:
--     A) IF THE GENERIC UNIT IS A SUBPROGRAM, CONTROL CANNOT LEAVE THE
--        SUBPROGRAM UNTIL THE TASK CREATED BY THE OBJECT DECLARATION IS
--        TERMINATED.

-- THIS TEST CONTAINS RACE CONDITIONS AND SHARED VARIABLES.

-- TBN  9/22/86
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C94010A IS

     GLOBAL_INT : INTEGER := 0;
     MY_EXCEPTION : EXCEPTION;

     PACKAGE P IS
          TYPE LIM_PRI_TASK IS LIMITED PRIVATE;
     PRIVATE
          TASK TYPE LIM_PRI_TASK IS
          END LIM_PRI_TASK;
     END P;

     USE P;

     TASK TYPE TT IS
     END TT;

     TYPE REC IS
          RECORD
               A : INTEGER := 1;
               B : TT;
          END RECORD;

     TYPE LIM_REC IS
          RECORD
               A : INTEGER := 1;
               B : LIM_PRI_TASK;
          END RECORD;

     PACKAGE BODY P IS
          TASK BODY LIM_PRI_TASK IS
          BEGIN
               DELAY 30.0 * Impdef.One_Second;
               GLOBAL_INT := IDENT_INT (2);
          END LIM_PRI_TASK;
     END P;

     TASK BODY TT IS
     BEGIN
          DELAY 30.0 * Impdef.One_Second;
          GLOBAL_INT := IDENT_INT (1);
     END TT;

     GENERIC
          TYPE T IS LIMITED PRIVATE;
     PROCEDURE PROC (A : INTEGER);

     PROCEDURE PROC (A : INTEGER) IS
          OBJ_T : T;
     BEGIN
          IF A = IDENT_INT (1) THEN
               RAISE MY_EXCEPTION;
          END IF;
     END PROC;

     GENERIC
          TYPE T IS LIMITED PRIVATE;
     FUNCTION FUNC (A : INTEGER) RETURN INTEGER;

     FUNCTION FUNC (A : INTEGER) RETURN INTEGER IS
          OBJ_T : T;
     BEGIN
          IF A = IDENT_INT (1) THEN
               RAISE MY_EXCEPTION;
          END IF;
          RETURN 1;
     END FUNC;


BEGIN
     TEST ("C94010A", "CHECK TERMINATION RULES FOR INSTANTIATIONS OF " &
                      "GENERIC SUBPROGRAM UNITS WHICH CREATE TASKS");

     -------------------------------------------------------------------
     DECLARE
          PROCEDURE PROC1 IS NEW PROC (TT);
     BEGIN
          PROC1 (0);
          IF GLOBAL_INT = IDENT_INT (0) THEN
               FAILED ("TASK NOT DEPENDENT ON MASTER - 1");
               DELAY 35.0;
          END IF;
     END;

     -------------------------------------------------------------------
     GLOBAL_INT := IDENT_INT (0);

     DECLARE
          PROCEDURE PROC2 IS NEW PROC (REC);
     BEGIN
          PROC2 (1);
          FAILED ("EXCEPTION WAS NOT RAISED - 2");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL_INT = IDENT_INT (0) THEN
                    FAILED ("TASK NOT DEPENDENT ON MASTER - 2");
                    DELAY 35.0 * Impdef.One_Second;
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
     END;

     -------------------------------------------------------------------
     GLOBAL_INT := IDENT_INT (0);

     DECLARE
          PROCEDURE PROC3 IS NEW PROC (LIM_PRI_TASK);
     BEGIN
          PROC3 (1);
          FAILED ("EXCEPTION WAS NOT RAISED - 3");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL_INT = IDENT_INT (0) THEN
                    FAILED ("TASK NOT DEPENDENT ON MASTER - 3");
                    DELAY 35.0 * Impdef.One_Second;
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 3");
     END;

     -------------------------------------------------------------------
     GLOBAL_INT := IDENT_INT (0);

     DECLARE
          PROCEDURE PROC4 IS NEW PROC (LIM_REC);
     BEGIN
          PROC4 (0);
          IF GLOBAL_INT = IDENT_INT (0) THEN
               FAILED ("TASK NOT DEPENDENT ON MASTER - 4");
               DELAY 35.0 * Impdef.One_Second;
          END IF;
     END;

     -------------------------------------------------------------------
     GLOBAL_INT := IDENT_INT (0);

     DECLARE
          A : INTEGER;
          FUNCTION FUNC1 IS NEW FUNC (TT);
     BEGIN
          A := FUNC1 (1);
          FAILED ("EXCEPTION NOT RAISED - 5");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL_INT = IDENT_INT (0) THEN
                    FAILED ("TASK NOT DEPENDENT ON MASTER - 5");
                    DELAY 35.0 * Impdef.One_Second;
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 5");
     END;

     -------------------------------------------------------------------
     GLOBAL_INT := IDENT_INT (0);

     DECLARE
          A : INTEGER;
          FUNCTION FUNC2 IS NEW FUNC (REC);
     BEGIN
          A := FUNC2 (0);
          IF GLOBAL_INT = IDENT_INT (0) THEN
               FAILED ("TASK NOT DEPENDENT ON MASTER - 6");
               DELAY 35.0 * Impdef.One_Second;
          END IF;
     END;

     -------------------------------------------------------------------
     GLOBAL_INT := IDENT_INT (0);

     DECLARE
          A : INTEGER;
          FUNCTION FUNC3 IS NEW FUNC (LIM_PRI_TASK);
     BEGIN
          A := FUNC3 (0);
          IF GLOBAL_INT = IDENT_INT (0) THEN
               FAILED ("TASK NOT DEPENDENT ON MASTER - 7");
               DELAY 35.0 * Impdef.One_Second;
          END IF;
     END;

     -------------------------------------------------------------------
     GLOBAL_INT := IDENT_INT (0);

     DECLARE
          A : INTEGER;
          FUNCTION FUNC4 IS NEW FUNC (LIM_REC);
     BEGIN
          A := FUNC4 (1);
          FAILED ("EXCEPTION NOT RAISED - 8");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL_INT = IDENT_INT (0) THEN
                    FAILED ("TASK NOT DEPENDENT ON MASTER - 8");
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 8");
     END;

     -------------------------------------------------------------------
          
     RESULT;
END C94010A;
