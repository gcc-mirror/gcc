-- CE2408B.ADA

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
--     CHECK THAT WRITE DOES NOT CAUSE AN EXCEPTION WHEN THE TO
--     PARAMETER IS GREATER THAN THE END POSITION.

--          2) FILE MODE IS INOUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF DIRECT FILES WITH MODE INOUT_FILE.

-- HISTORY:
--     GMT 08/05/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2408B IS

     PACKAGE DIR_IO IS NEW DIRECT_IO (INTEGER);
     USE DIR_IO;

     DIR_FILE   : FILE_TYPE;
     INCOMPLETE : EXCEPTION;

BEGIN
     TEST ("CE2408B", "FOR FILES OF MODE INOUT_FILE, CHECK THAT " &
                      "WRITE DOES NOT CAUSE AN EXCEPTION WHEN THE " &
                      """TO"" PARAMETER IS GREATER THAN THE END " &
                      "POSITION");
     BEGIN
          CREATE (DIR_FILE, INOUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE WITH " &
                               "MODE INOUT_FILE FOR DIR_IO - 1");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE WITH " &
                               "MODE INOUT_FILE FOR DIR_IO - 2");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE WITH " &
                       "MODE INOUT_FILE FOR DIR_IO - 3");
               RAISE INCOMPLETE;
     END;

     -- FILL UP FILE

     WRITE (DIR_FILE, 3);
     WRITE (DIR_FILE, 4);
     WRITE (DIR_FILE, 5);
     WRITE (DIR_FILE, 6);

     -- WRITE WHERE TO IS LARGER THAN END OF FILE

     BEGIN
          WRITE (DIR_FILE, 9, 7);
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("WRITE RAISED EXCEPTION WHEN TO " &
                       "PARAMETER WAS BEYOND END - 4");
     END;

     BEGIN
          SET_INDEX (DIR_FILE, 11);
          WRITE (DIR_FILE, 10);
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("SET_INDEX/WRITE RAISED EXCEPTION WHEN TO " &
                       "PARAMETER EXCEEDS THE END POSITION - 5");
     END;

     -- DELETE TEST FILE

     BEGIN
          DELETE (DIR_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2408B;
