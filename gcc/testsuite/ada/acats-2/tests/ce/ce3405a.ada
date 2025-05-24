-- CE3405A.ADA

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
--     CHECK THAT NEW_PAGE OUTPUTS A LINE TERMINATOR FOLLOWED BY A PAGE
--     TERMINATOR IF THE CURRENT LINE IS NOT AT COLUMN 1 OR IF THE
--     CURRENT PAGE IS AT LINE 1;  IF THE CURRENT LINE IS AT COLUMN 1,
--     OUTPUTS A PAGE TERMINATOR ONLY.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 09/02/82
--     JBG 01/18/83
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/23/87  ADDED A CASE WHICH CALLS NEW_LINE AND NEW_PAGE
--                   CONSECUTIVELY AND SEPARATED CASES INTO DIFFERENT
--                   IF STATEMENTS.  ADDED CHECK FOR USE_ERROR ON
--                   DELETE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;
WITH CHECK_FILE;

PROCEDURE CE3405A IS

     INCOMPLETE : EXCEPTION;
     FILE  : FILE_TYPE;
     ONE   : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(1));
     TWO   : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(2));
     THREE : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(3));
     FOUR  : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(4));
     CHAR  : CHARACTER := ('C');

BEGIN

     TEST ("CE3405A", "CHECK THAT NEW_PAGE OUTPUTS A LINE TERMINATOR " &
                      "FOLLOWED BY A PAGE TERMINATOR IF THE CURRENT " &
                      "LINE IS NOT AT COLUMN 1 OR IF THE CURRENT " &
                      "PAGE IS AT LINE 1;  IF THE CURRENT LINE IS AT " &
                      "COLUMN 1, OUTPUTS A PAGE TERMINATOR ONLY");

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
     END;

     NEW_PAGE (FILE);
     NEW_PAGE (FILE);                   -- CURRENT PAGE TERMINATED
     IF PAGE (FILE) /= THREE THEN
          FAILED ("INITIAL PAGE COUNT INCORRECT");
     END IF;

     SET_LINE_LENGTH (FILE,THREE);
     PUT (FILE,CHAR);
     NEW_LINE (FILE);

     IF LINE (FILE) /= TWO THEN
          FAILED ("INCORRECT LINE NUMBER - 1");
     END IF;

     IF PAGE (FILE) /= THREE THEN
          FAILED ("INCORRECT PAGE NUMBER - 2");
     END IF;

     NEW_PAGE (FILE);             -- CURRENT LINE TERMINATED (B)
     IF LINE (FILE) /= ONE THEN
          FAILED ("LINE NUMBER NOT INCREMENTED");
     END IF;
     IF PAGE (FILE) /= FOUR THEN
          FAILED ("PAGE NUMBER NOT INCREMENTED");
     END IF;
     PUT (FILE, IDENT_CHAR('E'));  -- CURRENT LINE NOT TERM (C)
     NEW_PAGE (FILE);
     NEW_LINE (FILE);
     NEW_PAGE (FILE);

     CHECK_FILE (FILE, "#@#@C#@E#@#@%");

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

END CE3405A;
