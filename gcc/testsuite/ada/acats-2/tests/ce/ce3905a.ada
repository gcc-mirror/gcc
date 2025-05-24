-- CE3905A.ADA

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
--     CHECK THAT GET FOR ENUMERATION TYPES OPERATES ON FILE OF MODE
--     IN_FILE AND THAT WHEN NO FILE IS SPECIFIED IT OPERATES ON THE
--     CURRENT DEFAULT INPUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/07/82
--     SPS 12/22/82
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/16/87  REMOVED DEPENDENCE ON RESET AND CORRECTED
--                   EXCEPTION HANDLING.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3905A IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3905A", "CHECK THAT GET FOR ENUMERATION TYPES " &
                      "OPERATES ON FILE OF MODE IN_FILE AND THAT " &
                      "WHEN NO FILE IS SPECIFIED IT OPERATES ON " &
                      "THE CURRENT DEFAULT INPUT_FILE");

     DECLARE
          TYPE DAY IS (MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY);
          PACKAGE DAY_IO IS NEW ENUMERATION_IO (DAY);
          FT : FILE_TYPE;
          FILE : FILE_TYPE;
          USE DAY_IO;
          X : DAY;
     BEGIN

-- CREATE AND INITIALIZE DATA FILES.

          BEGIN
               CREATE (FT, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                                    "WITH OUT_FILE MODE - 1");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT CREATE " &
                                    "WITH OUT_FILE MODE - 1");
                    RAISE INCOMPLETE;
          END;

          PUT (FT, "WEDNESDAY");
          NEW_LINE (FT);
          PUT (FT, "FRIDAY");

          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME(2));

          PUT (FILE, "TUESDAY");
          NEW_LINE (FILE);
          PUT (FILE, "THURSDAY");

          CLOSE (FT);

          BEGIN
               OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN " &
                                    "FOR IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          CLOSE (FILE);
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME(2));

          SET_INPUT (FILE);

-- BEGIN TEST

          GET (FT, X);
          IF X /= WEDNESDAY THEN
               FAILED ("VALUE FROM FILE INCORRECT");
          END IF;

          GET (X);
          IF X /= TUESDAY THEN
               FAILED ("VALUE FROM DEFAULT INCORRECT");
          END IF;

          GET (FT, X);
          IF X /= FRIDAY THEN
               FAILED ("VALUE FROM FILE INCORRECT");
          END IF;

          GET (FILE, X);
          IF X /= THURSDAY THEN
               FAILED ("VALUE FROM DEFAULT INCORRECT");
          END IF;

          BEGIN
               DELETE (FT);
               DELETE (FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3905A;
