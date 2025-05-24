-- CE2102W.ADA

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
--     CHECK THAT USE_ERROR IS RAISED WHEN RESETTING A FILE OF MODE
--     OUT_FILE, WHEN OUT_FILE MODE IS NOT SUPPORTED FOR RESET BY
--     THE IMPLEMENTATION FOR DIRECT FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH DO NOT
--     SUPPORT RESET WITH OUT_FILE MODE FOR DIRECT FILES.

-- HISTORY:
--     TBN 07/23/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2102W IS
BEGIN

     TEST ("CE2102W", "CHECK THAT USE_ERROR IS RAISED WHEN RESETTING " &
                      "A FILE OF MODE OUT_FILE, WHEN OUT_FILE " &
                      "MODE IS NOT SUPPORTED FOR RESET BY THE " &
                      "IMPLEMENTATION FOR DIRECT FILES");

     DECLARE
          PACKAGE DIR IS NEW DIRECT_IO (BOOLEAN);
          USE DIR;
          FILE1 : FILE_TYPE;
          INCOMPLETE : EXCEPTION;
          VAR1 : BOOLEAN := FALSE;
     BEGIN
          BEGIN
               CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE FOR " &
                                    "OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE FOR " &
                                    "OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          WRITE (FILE1, VAR1);

          BEGIN
               RESET (FILE1);
               NOT_APPLICABLE ("RESET FOR OUT_FILE MODE IS " &
                               "SUPPORTED");
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON RESET");
          END;

          BEGIN
               DELETE (FILE1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE2102W;
