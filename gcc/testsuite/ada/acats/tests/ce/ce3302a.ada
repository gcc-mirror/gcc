-- CE3302A.ADA

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
--     CHECK THAT SET_LINE_LENGTH, SET_PAGE_LENGTH, LINE_LENGTH, AND
--     PAGE_LENGTH RAISE MODE_ERROR WHEN APPLIED TO A FILE OF MODE
--     IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/16/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/19/87  CREATED AN EXTERNAL FILE WITH A NAME, REMOVED
--                   DEPENDENCE ON RESET, AND ADDED CODE TO DELETE
--                   EXTERNAL FILE.
WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3302A IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     FIVE : COUNT := COUNT(IDENT_INT(5));
     VAR1 : COUNT;
     ITEM : CHARACTER := 'A';

BEGIN
     TEST ("CE3302A", "CHECK THAT SET_LINE_LENGTH, SET_PAGE_LENGTH, " &
                      "LINE_LENGTH, AND PAGE_LENGTH RAISE MODE_ERROR " &
                      "WHEN APPLIED TO A FILE OF MODE IN_FILE");

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT FILE CREATE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT FILE CREATE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     PUT (FILE, ITEM);
     CLOSE (FILE);

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT FILE OPEN");
               RAISE INCOMPLETE;
     END;

     BEGIN
          SET_LINE_LENGTH (FILE, FIVE);
          FAILED ("MODE_ERROR NOT RAISED - SET_LINE_LENGTH");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - SET_LINE_LENGTH");
     END;

     BEGIN
          SET_PAGE_LENGTH (FILE, FIVE);
          FAILED ("MODE_ERROR NOT RAISED - SET_PAGE_LENGTH");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - SET_PAGE_LENGTH");
     END;

     BEGIN
          VAR1 := LINE_LENGTH (FILE);
          FAILED ("MODE_ERROR NOT RAISED - LINE_LENGTH");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - LINE_LENGTH");
     END;

     BEGIN
          VAR1 := PAGE_LENGTH (FILE);
          FAILED ("MODE_ERROR NOT RAISED - PAGE_LENGTH");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - PAGE_LENGTH");
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

END CE3302A;
