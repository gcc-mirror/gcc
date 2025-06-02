-- C97204B.ADA

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
-- CHECK THAT TASKING_ERROR IS RAISED IF THE CALLED TASK IS ABORTED
-- BEFORE THE CONDITIONAL ENTRY CALL IS EXECUTED.

-- WRG 7/13/86

WITH REPORT; USE REPORT;
PROCEDURE C97204B IS

BEGIN

     TEST ("C97204B", "CHECK THAT TASKING_ERROR IS RAISED IF THE " &
                      "CALLED TASK IS ABORTED BEFORE THE CONDITIONAL " &
                      "ENTRY CALL IS EXECUTED");

     DECLARE

          TASK T IS
               ENTRY E (I : INTEGER);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT E (I : INTEGER);
               FAILED ("ENTRY CALL ACCEPTED");
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED");
          END T;

          FUNCTION F RETURN INTEGER IS
          BEGIN
               ABORT T;
               RETURN 1;
          END F;

     BEGIN

          SELECT
               T.E (F);
               FAILED ("CONDITIONAL ENTRY CALL MADE");
          ELSE
               FAILED ("ELSE PART EXECUTED");
          END SELECT;

          FAILED ("EXCEPTION NOT RAISED");

     EXCEPTION

          WHEN TASKING_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED");

     END;

     RESULT;

END C97204B;
