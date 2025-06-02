-- CE3108A.ADA

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
--     CHECK THAT A FILE CAN BE CLOSED AND THEN RE-OPENED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     DLD 08/11/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/16/85
--     GMT 08/17/87  REMOVED UNNECESSARY CODE AND ADDED A CHECK FOR
--                   USE_ERROR ON DELETE.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3108A IS

     TXT_FILE   : FILE_TYPE;
     VAR        : STRING (1..2);
     LAST       : INTEGER;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3108A", "CHECK THAT A FILE CAN BE CLOSED " &
                      "AND THEN RE-OPENED");

     -- INITIALIZE TEST FILES

     BEGIN

          BEGIN
               CREATE (TXT_FILE, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE");
                    RAISE INCOMPLETE;
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               PUT (TXT_FILE, "17");
               CLOSE (TXT_FILE);

               -- RE-OPEN TEXT TEST FILE

               BEGIN
                    OPEN (TXT_FILE, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR RAISED ON OPEN");
                         RAISE INCOMPLETE;
               END;

               GET (TXT_FILE, VAR);
               IF VAR /= "17" THEN
                    FAILED ("WRONG DATA RETURNED FROM READ -TEXT");
               END IF;

               -- DELETE TEST FILES

               BEGIN
                    DELETE (TXT_FILE);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NULL;
               END;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3108A;
