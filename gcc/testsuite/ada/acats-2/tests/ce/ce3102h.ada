-- CE3102H.ADA

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
--     CHECK THAT MODE_ERROR IS RAISED WHEN ATTEMPTING TO CHANGE
--     THE MODE OF A FILE SERVING AS THE CURRENT DEFAULT INPUT
--     OR DEFAULT OUTPUT FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 08/12/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3102H IS

     FILE1 : FILE_TYPE;
     INCOMPLETE : EXCEPTION;
     ITEM : CHARACTER := 'A';

BEGIN

     TEST ("CE3102H", "CHECK THAT MODE_ERROR IS RAISED WHEN " &
                      "ATTEMPTING TO CHANGE THE MODE OF A FILE " &
                      "SERVING AS THE CURRENT DEFAULT INPUT OR " &
                      "DEFAULT OUTPUT FILE");

     BEGIN
          CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE");
               RAISE INCOMPLETE;
     END;

     SET_OUTPUT (FILE1);

     BEGIN
          RESET (FILE1, IN_FILE);
          FAILED ("MODE_ERROR NOT RAISED FOR RESET");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR RESET");
     END;

     SET_OUTPUT (STANDARD_OUTPUT);

     PUT (FILE1, ITEM);
     CLOSE (FILE1);

     BEGIN
          OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON OPEN");
               RAISE INCOMPLETE;
     END;

     SET_INPUT (FILE1);

     BEGIN
          RESET (FILE1, OUT_FILE);
          FAILED ("MODE_ERROR NOT RAISED FOR RESET");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR RESET");
     END;

     SET_INPUT (STANDARD_INPUT);

     BEGIN
          DELETE (FILE1);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3102H;
