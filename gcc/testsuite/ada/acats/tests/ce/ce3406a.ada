-- CE3406A.ADA

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
--     CHECK THAT SKIP_PAGE READS AND DISCARDS CHARACTERS AND LINE
--     TERMINATORS UNTIL A PAGE TERMINATOR IS READ, ADDS ONE TO THE
--     CURRENT PAGE NUMBER, AND SETS THE CURRENT COLUMN NUMBER AND LINE
--     NUMBER TO ONE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/17/82
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/24/87  CREATED NON-TEMPORARY FILE, REMOVED DEPENDENCE
--                   ON RESET, AND CHECKED FOR USE_ERROR ON DELETE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3406A IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     CHAR_X : CHARACTER := ('X');
     ITEM_CHAR : CHARACTER;
     ONE : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(1));
     TWO : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(2));
     THREE : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(3));

BEGIN

     TEST ("CE3406A", "CHECK THAT SKIP_LINE READS AND " &
                      "SETS PAGE AND COLUMN CORRECTLY");

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

     PUT (FILE, "CDE");
     NEW_LINE (FILE);
     PUT (FILE, "FGHI");
     NEW_LINE (FILE);
     PUT (FILE, "JK");
     NEW_PAGE (FILE);
     NEW_PAGE (FILE);
     PUT (FILE,CHAR_X);

     CLOSE (FILE);

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN WITH " &
                               "IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     IF (LINE (FILE) /= ONE) THEN
          FAILED ("LINE NUMBER NOT EQUAL TO ONE");
     END IF;

     IF (PAGE (FILE) /= ONE) THEN
          FAILED ("PAGE NUMBER NOT EQUAL TO ONE");
     END IF;

     GET (FILE, ITEM_CHAR);

     IF ITEM_CHAR /= 'C' THEN
          FAILED ("INCORRECT VALUE READ FROM FILE - 1");
     END IF;

     SKIP_PAGE (FILE);

     IF COL (FILE) /= ONE THEN
          FAILED ("COLUMN NOT SET TO ONE - 1");
     END IF;

     IF LINE (FILE) /= ONE THEN
          FAILED ("LINE NOT SET TO ONE - 1");
     END IF;

     IF PAGE (FILE) /= TWO THEN
          FAILED ("PAGE NOT SET TO TWO");
     END IF;

     SKIP_PAGE (FILE);

     IF COL (FILE) /= ONE THEN
          FAILED ("COLUMN NOT SET TO ONE - 2");
     END IF;

     IF LINE (FILE) /= ONE THEN
          FAILED ("LINE NOT SET TO ONE - 2");
     END IF;

     IF PAGE (FILE) /= THREE THEN
          FAILED ("PAGE NOT SET TO THREE");
     END IF;

     GET (FILE, ITEM_CHAR);
     IF ITEM_CHAR /= 'X' THEN
          FAILED ("INCORRECT VALUE READ FROM FILE - 2");
     END IF;

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

END CE3406A;
