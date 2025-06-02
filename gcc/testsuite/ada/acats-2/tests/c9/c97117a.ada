-- C97117A.ADA

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
-- CHECK THAT PROGRAM_ERROR IS RAISED IF ALL ALTERNATIVES ARE CLOSED AND
-- NO ELSE PART IS PRESENT.

-- WRG 7/10/86

WITH REPORT; USE REPORT;
PROCEDURE C97117A IS

BEGIN

     TEST ("C97117A", "CHECK THAT PROGRAM_ERROR IS RAISED IF ALL " &
                      "ALTERNATIVES ARE CLOSED AND NO ELSE PART IS " &
                      "PRESENT");

     DECLARE

          TASK T IS
               ENTRY E;
          END T;

          TASK BODY T IS
          BEGIN
               SELECT
                    WHEN IDENT_BOOL (FALSE) =>
                         ACCEPT E;
                         FAILED ("CLOSED ACCEPT ALTERNATIVE TAKEN " &
                                 "FOR NONEXISTENT ENTRY CALL");
               OR   WHEN IDENT_BOOL (FALSE) =>
                         DELAY 0.0;
                         FAILED ("CLOSED ALTERNATIVE TAKEN");
               END SELECT;
               FAILED ("PROGRAM_ERROR NOT RAISED");
          EXCEPTION
               WHEN PROGRAM_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED");
          END T;

     BEGIN

          NULL;

     END;

     RESULT;

END C97117A;
