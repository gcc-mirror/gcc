-- C94020A.ADA

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
-- CHECK THAT THE CONDITIONS FOR TERMINATION ARE RECOGNIZED WHEN THE
-- LAST MISSING TASK TERMINATES DUE TO AN ABORT

-- JEAN-PIERRE ROSEN 08-MAR-1984
-- JBG 6/1/84
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE C94020A IS

     TASK TYPE T2 IS
     END T2;
     
     TASK TYPE T3 IS
          ENTRY E;
     END T3;
     
     TASK BODY T2 IS
     BEGIN
          COMMENT("T2");
     END;
     
     TASK BODY T3 IS
     BEGIN
          COMMENT("T3");
          SELECT
               ACCEPT E;
          OR TERMINATE;
          END SELECT;
          FAILED("T3 EXITED SELECT OR TERMINATE");
     END;

BEGIN

     TEST ("C94020A", "TEST OF TASK DEPENDENCES, TERMINATE, ABORT");

     DECLARE
          TASK TYPE T1 IS
          END T1;
          
          V1 : T1;
          TYPE A_T1 IS ACCESS T1;

          TASK BODY T1 IS
          BEGIN
               ABORT T1;
               DELAY 0.0;          --SYNCHRONIZATION POINT
               FAILED("T1 NOT ABORTED");
          END;
          
     BEGIN
          DECLARE
               V2 : T2;
               A1 : A_T1;
          BEGIN
               DECLARE
                    V3 : T3;
                    TASK T4 IS
                    END T4;
                    TASK BODY T4 IS
                         TASK T41 IS
                         END T41;
                         TASK BODY T41 IS
                         BEGIN
                              COMMENT("T41");
                              ABORT T4;
                              DELAY 0.0;       --SYNCHRONIZATION POINT
                              FAILED("T41 NOT ABORTED");
                         END;
                    BEGIN  --T4
                         COMMENT("T4");
                    END;
               BEGIN
                    COMMENT("BLOC 3");
               END;
               COMMENT("BLOC 2");
               A1 := NEW T1;
          END;
          COMMENT("BLOC 1");
     EXCEPTION
          WHEN OTHERS => FAILED("SOME EXCEPTION RAISED");
     END;

     RESULT;

END C94020A;
