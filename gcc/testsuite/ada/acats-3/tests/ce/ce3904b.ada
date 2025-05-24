-- CE3904B.ADA

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
--     CHECK THAT END_ERROR IS RAISED BY GET WITH AN ENUMERATION TYPE
--     WHEN THE ONLY REMAINING CHARACTERS IN THE FILE ARE SPACES,
--     HORIZONTAL TABULATION CHARACTERS, LINE TERMINATORS, AND PAGE
--     TERMINATORS.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS THAT SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 07/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3904B IS

     TYPE COLOR IS (RED, BLUE, GREEN);
     PACKAGE COLOR_IO IS NEW ENUMERATION_IO (COLOR);
     USE COLOR_IO;

     FILE : FILE_TYPE;
     ITEM : COLOR;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3904B", "CHECK THAT END_ERROR IS RAISED BY GET WITH " &
                      "AN ENUMERATION TYPE WHEN THE ONLY REMAINING " &
                      "CHARACTERS IN THE FILE ARE SPACES, HORIZONTAL " &
                      "TABULATION CHARACTERS, LINE TERMINATORS, AND " &
                      "PAGE TERMINATORS");

     BEGIN

          BEGIN
               CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                                    "WITH MODE OUT_FILE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE " &
                                    "WITH MODE OUT_FILE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          PUT (FILE, RED);
          NEW_LINE (FILE);
          NEW_LINE (FILE);
          NEW_PAGE (FILE);
          PUT (FILE, ASCII.HT);
          PUT (FILE, GREEN);
          NEW_LINE (FILE);
          NEW_LINE (FILE);
          NEW_PAGE (FILE);
          PUT (FILE, ' ');
          PUT (FILE, ASCII.HT);
          PUT (FILE, ' ');

          CLOSE (FILE);

          BEGIN
               OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON OPEN WITH " &
                                    "MODE IN_FILE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON OPEN");
                    RAISE INCOMPLETE;
          END;

          GET (FILE, ITEM);
          IF ITEM /= RED THEN
               FAILED ("INCORRECT VALUE READ - 1");
          END IF;

          GET (FILE, ITEM);
          IF ITEM /= GREEN THEN
               FAILED ("INCORRECT VALUE READ - 2");
          END IF;

          BEGIN
               GET (FILE, ITEM);
               FAILED ("END_ERROR NOT RAISED FOR GET");
          EXCEPTION
               WHEN END_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON GET");
          END;

          IF NOT END_OF_FILE (FILE) THEN
               FAILED ("END_OF_FILE NOT TRUE AFTER RAISING EXCEPTION");
          END IF;

          BEGIN
               DELETE (FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3904B;
