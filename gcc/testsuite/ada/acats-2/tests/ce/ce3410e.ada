-- CE3410E.ADA

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
--     CHECK THAT SET_LINE RAISES END_ERROR IF NO PAGE BEFORE THE END
--     OF THE FILE IS LONG ENOUGH.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/20/82
--     JBG 01/27/83
--     JBG 08/30/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/02/87  REMOVED DEPENDENCE ON RESET, ADDED NEW CASES FOR
--                   OBJECTIVE, AND CHECKED FOR USE_ERROR ON DELETE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3410E IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     CHAR : CHARACTER := ('C');
     ITEM_CHAR : CHARACTER;
     FIVE : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(5));

BEGIN

     TEST ("CE3410E", "CHECK THAT SET_LINE RAISES END_ERROR " &
                      "WHEN IT ATTEMPTS TO READ THE FILE TERMINATOR");

-- CREATE & INITIALIZE FILE

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE WITH " &
                               "MODE OUT_FILE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT CREATE " &
                               "WITH MODE OUT_FILE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     PUT (FILE, "ABCD");
     NEW_LINE (FILE);
     PUT (FILE, "DEF");
     NEW_LINE (FILE, 3);
     NEW_PAGE (FILE);
     PUT_LINE (FILE, "HELLO");
     NEW_PAGE (FILE);
     PUT_LINE (FILE, "GH");
     PUT_LINE (FILE, "IJK");
     PUT_LINE (FILE, "HI");
     PUT_LINE (FILE, "TESTING");

     CLOSE (FILE);

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN WITH " &
                               "MODE IN_FILE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          SET_LINE (FILE,FIVE);
          FAILED ("END ERROR NOT RAISED ON SET_LINE");
     EXCEPTION
          WHEN END_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON SET_LINE");
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

END CE3410E;
