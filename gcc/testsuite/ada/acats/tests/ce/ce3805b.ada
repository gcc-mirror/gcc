-- CE3805B.ADA

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
--     CHECK THAT FIXED_IO GET MAY READ THE LAST CHARACTER IN THE FILE
--     WITHOUT RAISING END_ERROR AND THAT SUBSEQUENT READING WILL RAISE
--     END_ERROR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/08/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/15/87  REMOVED UNNECESSARY CODE AND CORRECTED EXCEPTION
--                   HANDLING.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3805B IS

BEGIN

     TEST ("CE3805B", "CHECK THAT FIXED_IO GET MAY READ THE LAST "&
                      "CHARACTER IN THE FILE WITHOUT RAISING " &
                      "END_ERROR AND THAT SUBSEQUENT READING WILL " &
                      "RAISE END_ERROR");

     DECLARE
          FT1, FT2 : FILE_TYPE;
          TYPE FIXED IS DELTA 0.02 RANGE 0.0 .. 50.0;
          PACKAGE FX_IO IS NEW FIXED_IO (FIXED);
          X : FIXED;
          USE FX_IO;
          INCOMPLETE : EXCEPTION;

     BEGIN

-- CREATE AND INITIALIZE TEST FILES

          BEGIN
               CREATE (FT1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT " &
                                    "CREATE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          CREATE (FT2, OUT_FILE, LEGAL_FILE_NAME(2));

          PUT (FT1, "2.25");
          CLOSE (FT1);

          PUT (FT2, "2.50");
          NEW_LINE (FT2, 3);
          NEW_PAGE (FT2);
          NEW_LINE (FT2, 3);
          CLOSE (FT2);

-- BEGIN TEST

          BEGIN
               OPEN (FT1, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN " &
                                    "FOR IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          OPEN (FT2, IN_FILE, LEGAL_FILE_NAME(2));

          BEGIN
               GET (FT1, X);
               IF X /= 2.25 THEN
                    FAILED ("INCORRECT VALUE READ");
               END IF;
               BEGIN
                    GET (FT1, X);
                    FAILED ("END_ERROR NOT RAISED - 1");
               EXCEPTION
                    WHEN END_ERROR =>
                         NULL;
                    WHEN  OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 1");
               END;
          EXCEPTION
               WHEN END_ERROR =>
                    FAILED ("END_ERROR RAISED PREMATURELY - 1");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED - 1");
          END;

          BEGIN
               GET (FT2, X);
               IF X /= 2.50 THEN
                    FAILED ("INCORRECT VALUE READ");
               END IF;
               BEGIN
                    GET (FT2, X);
                    FAILED ("END_ERROR NOT RAISED - 2");
               EXCEPTION
                    WHEN END_ERROR =>
                         NULL;
                    WHEN  OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 2");
               END;
          EXCEPTION
               WHEN END_ERROR =>
                    FAILED ("END_ERROR RAISED PREMATURELY - 2");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED - 2");
          END;

          BEGIN
               DELETE (FT1);
               DELETE (FT2);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;

     END;

     RESULT;

END CE3805B;
