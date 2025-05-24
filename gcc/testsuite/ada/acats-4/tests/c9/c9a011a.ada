-- C9A011A.ADA

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
--     CHECK THAT IF A CALLED TASK IS ABORTED WHILE IN RENDEZVOUS, THEN
--     "TASKING_ERROR" IS RAISED IN THE CALLING TASK.

-- HISTORY:
--     DHH 03/28/88 CREATED ORIGINAL TEST.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE C9A011A IS

     TASK TYPE CHOICE IS
          ENTRY E1;
     END CHOICE;

     T : CHOICE;

     TASK BODY CHOICE IS
          X : INTEGER;
     BEGIN
          ACCEPT E1 DO
               X := IDENT_INT(3);
               IF EQUAL(X,X) THEN
                    ABORT CHOICE;
               END IF;
          END E1;
     END CHOICE;

BEGIN

     TEST("C9A011A", "CHECK THAT IF A CALLED TASK IS ABORTED WHILE " &
                     "IN RENDEZVOUS, THEN ""TASKING_ERROR"" IS " &
                     "RAISED IN THE CALLING TASK");

     T.E1;
     FAILED("EXCEPTION NOT RAISED ON ABORT");

     RESULT;

EXCEPTION
     WHEN TASKING_ERROR =>
          RESULT;

     WHEN OTHERS =>
          FAILED("UNEXPECTED EXCEPTION RAISED ON ABORT");
          RESULT;
END C9A011A;
