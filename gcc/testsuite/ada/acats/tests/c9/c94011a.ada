-- C94011A.ADA

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
-- CHECK THAT IF A FORMAL ACCESS TYPE OF A GENERIC UNIT DESIGNATES A
-- FORMAL LIMITED PRIVATE TYPE, THEN WHEN THE UNIT IS INSTANTIATED WITH
-- A TASK TYPE OR A TYPE HAVING A SUBCOMPONENT OF A TASK TYPE, THE
-- MASTER FOR ANY TASKS ALLOCATED WITHIN THE INSTANTIATED UNIT IS
-- DETERMINED BY THE ACTUAL PARAMETER.

-- TBN  9/22/86

WITH REPORT; USE REPORT;
PROCEDURE C94011A IS

     GLOBAL_INT : INTEGER := 0;
     MY_EXCEPTION : EXCEPTION;

     PACKAGE P IS
          TYPE LIM_PRI_TASK IS LIMITED PRIVATE;
          PROCEDURE E (T : LIM_PRI_TASK);
     PRIVATE
          TASK TYPE LIM_PRI_TASK IS
               ENTRY E;
          END LIM_PRI_TASK;
     END P;

     USE P;

     TASK TYPE TT IS
          ENTRY E;
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
               ACCEPT E;
               GLOBAL_INT := IDENT_INT (2);
          END LIM_PRI_TASK;

          PROCEDURE E (T : LIM_PRI_TASK) IS
          BEGIN
               T.E;
          END E;
     END P;

     TASK BODY TT IS
     BEGIN
          ACCEPT E;
          GLOBAL_INT := IDENT_INT (1);
     END TT;

     GENERIC
          TYPE T IS LIMITED PRIVATE;
          TYPE ACC_T IS ACCESS T;
     PROCEDURE PROC (A : OUT ACC_T);

     PROCEDURE PROC (A : OUT ACC_T) IS
     BEGIN
          A := NEW T;
     END PROC;

     GENERIC
          TYPE T IS LIMITED PRIVATE;
          TYPE ACC_T IS ACCESS T;
     FUNCTION FUNC RETURN ACC_T;

     FUNCTION FUNC RETURN ACC_T IS
     BEGIN
          RETURN NEW T;
     END FUNC;

     GENERIC
          TYPE T IS LIMITED PRIVATE;
          TYPE ACC_T IS ACCESS T;
     PACKAGE PAC IS
          PTR_T : ACC_T := NEW T;
     END PAC;

BEGIN
     TEST ("C94011A", "CHECK THAT IF A FORMAL ACCESS TYPE OF A " &
                      "GENERIC UNIT DESIGNATES A FORMAL LIMITED " &
                      "PRIVATE TYPE, THEN WHEN THE UNIT IS " &
                      "INSTANTIATED, THE MASTER FOR ANY TASKS " &
                      "ALLOCATED WITHIN THE INSTANTIATED UNIT IS " &
                      "DETERMINED BY THE ACTUAL PARAMETER");

     -------------------------------------------------------------------
     DECLARE
          TYPE ACC_TT IS ACCESS TT;
          ACC1 : ACC_TT;
          PROCEDURE PROC1 IS NEW PROC (TT, ACC_TT);
     BEGIN
          PROC1 (ACC1);
          ACC1.E;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("TASK DEPENDENT ON WRONG MASTER - 1");
     END;
     IF GLOBAL_INT = IDENT_INT (0) THEN
          FAILED ("TASK NOT DEPENDENT ON MASTER - 1");
     END IF;
     
     -------------------------------------------------------------------
     BEGIN
          GLOBAL_INT := IDENT_INT (0);
          DECLARE
               TYPE ACC_REC IS ACCESS REC;
               A : ACC_REC;
               FUNCTION FUNC1 IS NEW FUNC (REC, ACC_REC);
          BEGIN
               A := FUNC1;
               A.B.E;
               RAISE MY_EXCEPTION;
          EXCEPTION
               WHEN MY_EXCEPTION =>
                    RAISE MY_EXCEPTION;
               WHEN OTHERS =>
                    FAILED ("TASK DEPENDENT ON WRONG MASTER - 2");
          END;
          FAILED ("MY_EXCEPTION NOT RAISED - 2");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL_INT = IDENT_INT (0) THEN
                    FAILED ("TASK NOT DEPENDENT ON MASTER - 2");
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
     END;
     
     -------------------------------------------------------------------
     GLOBAL_INT := IDENT_INT (0);

     BEGIN
          DECLARE
               TYPE ACC_LIM_TT IS ACCESS LIM_PRI_TASK;
          BEGIN
               DECLARE
                    A : ACC_LIM_TT;
                    FUNCTION FUNC2 IS NEW FUNC (LIM_PRI_TASK,
                                                            ACC_LIM_TT);
               BEGIN
                    A := FUNC2;
                    E (A.ALL);
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("TASK DEPENDENT ON WRONG MASTER - 3");
          END;
          IF GLOBAL_INT = IDENT_INT (0) THEN
               FAILED ("TASK NOT DEPENDENT ON MASTER - 3");
          END IF;
     END;

     -------------------------------------------------------------------
     GLOBAL_INT := IDENT_INT (0);

     BEGIN
          DECLARE
               TYPE ACC_LIM_REC IS ACCESS LIM_REC;
          BEGIN
               DECLARE
                    ACC2 : ACC_LIM_REC;
                    PROCEDURE PROC2 IS NEW PROC (LIM_REC, ACC_LIM_REC);
               BEGIN
                    PROC2 (ACC2);
                    E (ACC2.B);
               END;
               RAISE MY_EXCEPTION;
          EXCEPTION
               WHEN MY_EXCEPTION =>
                    RAISE MY_EXCEPTION;
               WHEN OTHERS =>
                    FAILED ("TASK DEPENDENT ON WRONG MASTER - 4");
          END;
          FAILED ("MY_EXCEPTION NOT RAISED - 4");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL_INT = IDENT_INT (0) THEN
                    FAILED ("TASK NOT DEPENDENT ON MASTER - 4");
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 4");
     END;

     -------------------------------------------------------------------
     BEGIN
           GLOBAL_INT := IDENT_INT (0);

          DECLARE
               TYPE ACC_TT IS ACCESS TT;
               PACKAGE PAC1 IS NEW PAC (TT, ACC_TT);
               USE PAC1; 
          BEGIN
               PTR_T.E;
               RAISE MY_EXCEPTION;
          EXCEPTION
               WHEN MY_EXCEPTION =>
                    RAISE MY_EXCEPTION;
               WHEN OTHERS =>
                    FAILED ("TASK DEPENDENT ON WRONG MASTER - 5");
          END;
          FAILED ("MY_EXCEPTION NOT RAISED - 5");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL_INT = IDENT_INT (0) THEN
                    FAILED ("TASK NOT DEPENDENT ON MASTER - 5");
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 5");
     END;
     
     -------------------------------------------------------------------
     GLOBAL_INT := IDENT_INT (0);

     DECLARE
          TYPE ACC_LIM_REC IS ACCESS LIM_REC;
     BEGIN
          DECLARE
               PACKAGE PAC2 IS NEW PAC (LIM_REC, ACC_LIM_REC);
               USE PAC2;
          BEGIN
               E (PTR_T.B);
          END;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("TASK DEPENDENT ON WRONG MASTER - 6");
     END;
     IF GLOBAL_INT = IDENT_INT (0) THEN
          FAILED ("TASK NOT DEPENDENT ON MASTER - 6");
     END IF;

     -------------------------------------------------------------------

     RESULT;
END C94011A;
