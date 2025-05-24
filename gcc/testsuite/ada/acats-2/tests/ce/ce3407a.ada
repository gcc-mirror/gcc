-- CE3407A.ADA

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
--     CHECK THAT END_OF_PAGE RETURNS THE CORRECT VALUE WHEN POSITIONED
--     AT THE BEGINNING AND AT THE END OF THE PAGE, AND BEFORE A FILE
--     TERMINATOR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/22/82
--     JBG 01/26/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/28/87  REMOVED UNNECESSARY CODE, REMOVED DEPENDENCE
--                   ON RESET AND CHECKED FOR USE_ERROR ON DELETE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3407A IS

     INCOMPLETE : EXCEPTION;
     FILE1 : FILE_TYPE;
     CHAR  : CHARACTER := ('C');
     ITEM_CHAR : CHARACTER;

BEGIN

     TEST ("CE3407A", "CHECK THAT END_OF_PAGE RETURNS " &
                      "THE CORRECT VALUE");

-- CREATE & INITIALIZE OUTPUT FILE

     BEGIN
          CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE WITH " &
                               "OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     FOR I IN 1..6 LOOP
          PUT (FILE1, CHAR);
     END LOOP;

     CLOSE (FILE1);

     BEGIN
          OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN WITH " &
                               "IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     IF END_OF_PAGE (FILE1) THEN
          FAILED ("INCORRECT VALUE AT FIRST POSITION - 1");
     END IF;

     IF END_OF_PAGE (FILE1) THEN
          FAILED ("INCORRECT VALUE AT FIRST POSITION - 2");
     END IF;

-- TEST WHEN POSITIONED BEFORE LAST CHARACTER IN FILE

     FOR I IN 1..5 LOOP
          GET (FILE1, ITEM_CHAR);
     END LOOP;

     IF END_OF_PAGE (FILE1) THEN
          FAILED ("INCORRECT VALUE BEFORE LAST CHARACTER");
     END IF;

-- TEST WHEN AT END OF FILE

     GET (FILE1, ITEM_CHAR);
     IF NOT END_OF_PAGE (FILE1) THEN
          FAILED ("INCORRECT VALUE AT LAST POSITION");
     END IF;

     SKIP_PAGE (FILE1);

     IF NOT END_OF_PAGE (FILE1) THEN
          FAILED ("INCORRECT VALUE BEFORE FILE TERMINATOR - 1");
     END IF;

     IF NOT END_OF_PAGE (FILE1) THEN
          FAILED ("INCORRECT VALUE BEFORE FILE TERMINATOR - 2");
     END IF;

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

END CE3407A;
