-- CE3108B.ADA

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
--     CHECK THAT THE NAME RETURNED BY THE NAME FUNCTION CAN BE USED
--     IN A SUBSEQUENT OPEN.

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

WITH TEXT_IO; USE TEXT_IO;
WITH REPORT; USE REPORT;

PROCEDURE CE3108B IS

     TYPE ACC_STR IS ACCESS STRING;

     TXT_FILE      : FILE_TYPE;
     TXT_FILE_NAME : ACC_STR;
     DIR_FILE_NAME : ACC_STR;
     VAR           : STRING(1..2);
     LAST          : INTEGER;
     INCOMPLETE    : EXCEPTION;

BEGIN

     TEST ("CE3108B", "CHECK THAT THE NAME RETURNED BY THE NAME-" &
                      "FUNCTION CAN BE USED IN A SUBSEQUENT OPEN");

     -- CREATE TEST FILES

     BEGIN
          BEGIN
               CREATE (TXT_FILE, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE - 1");
                    RAISE INCOMPLETE;
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE - 2");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               PUT (TXT_FILE, "14");
               TXT_FILE_NAME := NEW STRING'(NAME (TXT_FILE));
               CLOSE (TXT_FILE);

               -- ATTEMPT TO RE-OPEN TEXT TEST FILE USING RETURNED NAME
               -- VALUE

               BEGIN
                    OPEN (TXT_FILE, IN_FILE, TXT_FILE_NAME.ALL);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR ON RE-OPEN - 3");
                         RAISE INCOMPLETE;
               END;

               GET (TXT_FILE, VAR);
               IF VAR /= "14" THEN
                    FAILED ("WRONG DATA RETURNED FROM READ - 4");
               END IF;

               -- CLOSE AND DELETE TEST FILES

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

END CE3108B;
