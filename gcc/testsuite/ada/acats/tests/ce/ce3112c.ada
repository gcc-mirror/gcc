-- CE3112C.ADA

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
--     CHECK THAT AN EXTERNAL TEXT FILE SPECIFIED BY A NON-NULL
--     STRING NAME IS ACCESSIBLE AFTER THE COMPLETION OF THE MAIN
--     PROGRAM.

--     THIS TEST CREATES A TEXT FILE WHICH CE3112D.ADA WILL READ.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF AN EXTERNAL TEXT FILE WITH OUT_FILE MODE.

-- HISTORY:
--     GMT 08/13/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO;

PROCEDURE CE3112C IS

     INCOMPLETE         : EXCEPTION;
     FILE_NAME          : TEXT_IO.FILE_TYPE;
     PREVENT_EMPTY_FILE : STRING (1..5) := "HELLO";

BEGIN
     TEST ("CE3112C" , "CHECK THAT AN EXTERNAL TEXT FILE SPECIFIED " &
                       "BY A NON-NULL STRING NAME IS ACCESSIBLE " &
                       "AFTER THE COMPLETION OF THE MAIN PROGRAM");
     BEGIN
          BEGIN
               TEXT_IO.CREATE (FILE_NAME, TEXT_IO.OUT_FILE,
                               LEGAL_FILE_NAME);
          EXCEPTION
               WHEN TEXT_IO.USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                                    "WITH OUT_FILE MODE - 1");
                    RAISE INCOMPLETE;
               WHEN TEXT_IO.NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT " &
                                    "CREATE WITH OUT_FILE MODE - 2");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON " &
                            "TEXT CREATE - 3");
                    RAISE INCOMPLETE;
          END;

          TEXT_IO.PUT (FILE_NAME, PREVENT_EMPTY_FILE);
          TEXT_IO.CLOSE (FILE_NAME);

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3112C;
