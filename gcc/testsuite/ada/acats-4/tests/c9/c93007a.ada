-- C93007A.ADA

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
--     CHECK THAT IF AN ATTEMPT IS MADE TO ACTIVATE A TASK BEFORE ITS
--     BODY HAS BEEN ELABORATED, THE TASK IS COMPLETED AND "PROGRAM_
--     ERROR" (RATHER THAN "TASKING_ERROR") IS RAISED.

-- HISTORY:
--     DHH 03/16/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C93007A IS

BEGIN

     TEST("C93007A", "CHECK THAT IF AN ATTEMPT IS MADE TO ACTIVATE " &
                     "A TASK BEFORE ITS BODY HAS BEEN ELABORATED, " &
                     "THE TASK IS COMPLETED AND ""PROGRAM_ERROR"" " &
                     "(RATHER THAN ""TASKING_ERROR"") IS RAISED");

     DECLARE
          TASK TYPE PROG_ERR IS
               ENTRY START;
          END PROG_ERR;

          TYPE REC IS
               RECORD
                    B : PROG_ERR;
               END RECORD;

          TYPE ACC IS ACCESS PROG_ERR;

          PACKAGE P IS
               OBJ : REC;
          END P;

          PACKAGE BODY P IS
          BEGIN
               FAILED("EXCEPTION NOT RAISED - 1");
               OBJ.B.START;
          EXCEPTION
               WHEN PROGRAM_ERROR =>
                    NULL;
               WHEN TASKING_ERROR =>
                    FAILED("TASKING ERROR RAISED INCORRECTLY");
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED");
          END P;

          PACKAGE Q IS
               OBJ : ACC;
          END Q;

          PACKAGE BODY Q IS
          BEGIN
               OBJ := NEW PROG_ERR;
               FAILED("EXCEPTION NOT RAISED - 2");
               OBJ.START;
          EXCEPTION
               WHEN PROGRAM_ERROR =>
                    NULL;
               WHEN TASKING_ERROR =>
                    FAILED("ACCESS TASKING ERROR RAISED INCORRECTLY");
               WHEN OTHERS =>
                    FAILED("ACCESS UNEXPECTED EXCEPTION RAISED");
          END;

          TASK BODY PROG_ERR IS
          BEGIN
               ACCEPT START DO
                    IF TRUE THEN
                         COMMENT("IRRELEVANT");
                    END IF;
               END START;
          END PROG_ERR;
     BEGIN
          NULL;
     END; -- DECLARE

     RESULT;

EXCEPTION
     WHEN PROGRAM_ERROR =>
          FAILED("PROGRAM_ERROR RAISED AT INCORRECT POSITION");
          RESULT;

     WHEN OTHERS =>
          FAILED("UNEXPECTED EXCEPTION RAISED");
          RESULT;

END C93007A;
