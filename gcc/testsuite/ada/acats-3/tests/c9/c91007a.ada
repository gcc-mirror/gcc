-- C91007A.ADA

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
--     IF THE ELABORATION OF AN ENTRY DECLARATION RAISES
--     "CONSTRAINT_ERROR", THEN NO TASKS ARE ACTIVATED, AND
--     "TASKING_ERROR" IS NOT RAISED.

-- HISTORY:
--     LDC 06/17/88  CREATED ORGINAL TEST

WITH REPORT;
USE REPORT;

PROCEDURE C91007A IS

     TYPE ENUM IS (TERESA, BRIAN, PHIL, JOLEEN, LYNN, DOUG, JODIE,
                   VINCE, TOM, DAVE, JOHN, ROSA);
     SUBTYPE ENUM_SUB IS ENUM RANGE BRIAN..LYNN;

BEGIN
     TEST("C91007A","IF THE ELABORATION OF AN ENTRY DECLARATION " &
                    "RAISES 'CONSTRAINT_ERROR', THEN NO TASKS ARE " &
                    "ACTIVATED, AND 'TASKING_ERROR' IS NOT RAISED");

     BEGIN
          DECLARE
               TASK TYPE TSK1;
               T1 : TSK1;
               TASK BODY TSK1 IS
               BEGIN
                    FAILED("TSK1 WAS ACTIVATED");
               END TSK1;


               TASK TSK2 IS
                    ENTRY ENT(ENUM_SUB RANGE TERESA..LYNN);
               END TSK2;

               TASK BODY TSK2 IS
               BEGIN
                    FAILED("TASK BODY WAS ACTIVATED");
               END TSK2;

               TASK TSK3;
               TASK BODY TSK3 IS
               BEGIN
                    FAILED("TSK3 WAS ACTIVATED");
               END TSK3;

          BEGIN
               NULL;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED("CONSTRAINT_ERROR WAS RAISED IN THE " &
                           "BEGIN BLOCK");
               WHEN TASKING_ERROR =>
                    FAILED("TASKING_ERROR WAS RAISED INSTEAD OF " &
                           "CONSTRAINT_ERROR IN THE BEGIN BLOCK");
               WHEN OTHERS =>
                    FAILED("OTHER EXCEPTION WAS RAISED IN " &
                           "THE BEGIN BLOCK");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN TASKING_ERROR =>
               FAILED("TASKING_ERROR WAS RAISED INSTEAD OF " &
                      "CONSTRAINT_ERROR");
          WHEN OTHERS =>
               FAILED("WRONG EXCEPTION WAS RAISED");
     END;

     RESULT;

END C91007A;
