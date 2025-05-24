-- CE3406C.ADA

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
--     CHECK THAT SKIP_PAGE RAISES END_ERROR WHEN THE FILE IS POSITIONED
--     BEFORE THE FILE TERMINATOR BUT NOT WHEN THE FILE IS POSITIONED
--     BEFORE THE FINAL PAGE TERMINATOR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/17/82
--     JBG 01/24/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/24/87  CREATED NON-TEMPORARY FILE, REMOVED DEPENDENCE
--                   ON RESET, AND CHECKED CHARACTER READ IN.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3406C IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     CHAR : CHARACTER := ('C');
     ITEM_CHAR : CHARACTER;
     TWO : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(2));

BEGIN

     TEST ("CE3406C", "CHECK THAT SKIP_PAGE RAISES END_ERROR WHEN " &
                      "THE FILE IS POSITIONED BEFORE THE FILE " &
                      "TERMINATOR BUT NOT WHEN THE FILE IS " &
                      "POSITIONED BEFORE THE FINAL PAGE TERMINATOR");

-- CREATE AND INITIALIZE FILE

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     FOR I IN 1..2 LOOP
          FOR I IN 1..3 LOOP
               PUT (FILE,CHAR);
          END LOOP;
          NEW_LINE (FILE);
     END LOOP;

     CLOSE (FILE);

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN WITH " &
                               "IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

-- START TEST

-- TEST SKIP_PAGE BEFORE FINAL PAGE TERMINATOR

     WHILE NOT END_OF_PAGE (FILE) LOOP
          GET (FILE, ITEM_CHAR);
          IF ITEM_CHAR /= 'C' THEN
               FAILED ("INCORRECT VALUE READ FROM FILE");
          END IF;
     END LOOP;

     BEGIN
          SKIP_PAGE (FILE);
     EXCEPTION
          WHEN END_ERROR =>
               FAILED ("RAISED END_ERROR BEFORE FINAL PAGE " &
                       "TERMINATOR - 1");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED - 1");
     END;

     IF PAGE (FILE) /= TWO THEN
          FAILED ("PAGE NOT SET TO TWO");
     END IF;

-- TEST SKIP_PAGE BEFORE FILE TERMINATOR
     BEGIN
          SKIP_PAGE (FILE);
          FAILED ("END_ERROR NOT RAISED");
     EXCEPTION
          WHEN END_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED - 2");
     END;

     BEGIN
          DELETE (FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3406C;
