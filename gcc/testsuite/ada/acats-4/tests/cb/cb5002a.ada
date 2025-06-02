-- CB5002A.ADA

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
--     CHECK THAT WHEN "TASKING_ERROR" IS RAISED EXPLICITLY OR BY
--     PROPAGATION WITHIN AN ACCEPT STATEMENT, THEN "TASKING_ERROR"
--     IS RAISED IN BOTH THE CALLING AND THE CALLED TASK.

-- HISTORY:
--     DHH 03/31/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE CB5002A IS

BEGIN
     TEST("CB5002A", "CHECK THAT WHEN ""TASKING_ERROR"" IS RAISED " &
                     "EXPLICITLY OR BY PROPAGATION WITHIN AN ACCEPT " &
                     "STATEMENT, THEN ""TASKING_ERROR"" IS RAISED " &
                     "IN BOTH THE CALLING AND THE CALLED TASK");

     DECLARE
          TASK CALLING_EXP IS
               ENTRY A;
          END CALLING_EXP;

          TASK CALLED_EXP IS
               ENTRY B;
               ENTRY STOP;
          END CALLED_EXP;

          TASK CALLING_PROP IS
               ENTRY C;
          END CALLING_PROP;

          TASK CALLED_PROP IS
               ENTRY D;
               ENTRY STOP;
          END CALLED_PROP;

          TASK PROP IS
               ENTRY E;
               ENTRY STOP;
          END PROP;
-----------------------------------------------------------------------
          TASK BODY CALLING_EXP IS
          BEGIN
               ACCEPT A DO
                    BEGIN
                         CALLED_EXP.B;
                         FAILED("EXCEPTION NOT RAISED IN CALLING " &
                                "TASK - EXPLICIT RAISE");
                    EXCEPTION
                         WHEN TASKING_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED("WRONG EXCEPTION RAISED IN " &
                                     "CALLING TASK - EXPLICIT RAISE");
                    END; -- EXCEPTION
               END A;
          END CALLING_EXP;

          TASK BODY CALLED_EXP IS
          BEGIN
               BEGIN
                    ACCEPT B DO
                         RAISE TASKING_ERROR;
                         FAILED("EXCEPTION NOT RAISED IN CALLED " &
                                "TASK - EXPLICIT RAISE");
                    END B;
               EXCEPTION
                    WHEN TASKING_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("WRONG EXCEPTION RAISED IN CALLED " &
                                "TASK - EXPLICIT RAISE");
               END;  -- EXCEPTION BLOCK

               ACCEPT STOP;
          END CALLED_EXP;

-----------------------------------------------------------------------
          TASK BODY CALLING_PROP IS
          BEGIN
               ACCEPT C DO
                    BEGIN
                         CALLED_PROP.D;
                         FAILED("EXCEPTION NOT RAISED IN CALLING " &
                                "TASK - PROPAGATED RAISE");
                    EXCEPTION
                         WHEN TASKING_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED("WRONG EXCEPTION RAISED IN " &
                                     "CALLING TASK - PROPAGATED RAISE");
                    END;  -- EXCEPTION
               END C;
          END CALLING_PROP;

          TASK BODY CALLED_PROP IS
          BEGIN
               BEGIN
                    ACCEPT D DO
                         PROP.E;
                         FAILED("EXCEPTION NOT RAISED IN CALLED " &
                                "TASK - PROPAGATED RAISE");
                    END D;
               EXCEPTION
                    WHEN TASKING_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("WRONG EXCEPTION RAISED IN CALLED " &
                                "TASK - PROPAGATED RAISE");
               END;  -- EXCEPTION BLOCK;

               ACCEPT STOP;
          END CALLED_PROP;

          TASK BODY PROP IS
          BEGIN
               BEGIN
                    ACCEPT E DO
                         RAISE TASKING_ERROR;
                         FAILED("EXCEPTION NOT RAISED IN PROPAGATE " &
                                "TASK - ACCEPT E");
                    END E;
               EXCEPTION
                    WHEN TASKING_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("WRONG EXCEPTION RAISED IN PROP. TASK");
              END;    -- EXCEPTION BLOCK

              ACCEPT STOP;

          END PROP;
-----------------------------------------------------------------------
     BEGIN
          CALLING_EXP.A;
          CALLING_PROP.C;
          CALLED_EXP.STOP;
          CALLED_PROP.STOP;
          PROP.STOP;

     END;    -- DECLARE

     RESULT;
END CB5002A;
