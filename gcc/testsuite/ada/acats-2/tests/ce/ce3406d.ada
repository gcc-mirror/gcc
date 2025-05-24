-- CE3406D.ADA

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
--     CHECK THAT SKIP_PAGE OPERATES ON THE CURRENT DEFAULT INPUT
--     FILE WHEN NO FILE IS SPECIFIED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     JBG 01/26/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/24/87  CREATED NON-TEMPORARY FILE, REMOVED DEPENDENCE
--                   ON RESET, AND CHECKED CHARACTER READ IN.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3406D IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     ITEM_CHAR : CHARACTER;
     TWO : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(2));
     THREE : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(3));

BEGIN

     TEST ("CE3406D", "CHECK THAT SKIP_PAGE OPERATES ON THE CURRENT " &
                      "DEFAULT INPUT FILE WHEN NO FILE IS SPECIFIED");

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
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

     PUT (FILE, "ABC");
     NEW_PAGE (FILE);
     PUT (FILE, "DEF");

     CLOSE (FILE);

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN WITH " &
                               "IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     SET_INPUT (FILE);

     SKIP_PAGE;

     GET (FILE, ITEM_CHAR);
     IF ITEM_CHAR /= 'D' THEN
          FAILED ("INCORRECT VALUE READ FROM FILE");
     END IF;

     IF PAGE (CURRENT_INPUT) /= TWO THEN
          FAILED ("SKIP_PAGE NOT APPLIED TO CURRENT_INPUT");
     END IF;

     SKIP_PAGE (FILE);

     IF PAGE (CURRENT_INPUT) /= THREE THEN
          FAILED ("SKIP_PAGE NOT APPLIED TO CURRENT_INPUT");
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

END CE3406D;
