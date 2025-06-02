-- CE3104A.ADA

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
--     CHECK THAT THE CURRENT COLUMN, LINE, AND PAGE NUMBERS OF
--     TEXT FILES ARE SET TO ONE AFTER A CREATE, OPEN, OR RESET.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/24/82
--     SPS 09/16/82
--     SPS 11/09/82
--     JBG 03/16/83
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/13/87  CHANGED FAILED MESSAGES AND ADDED SUBTEST
--                   EXCEPTION.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3104A IS

     INCOMPLETE, SUBTEST : EXCEPTION;
     FILE, FT : FILE_TYPE;
     ONE      : CONSTANT POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(1));
     CHAR     : CHARACTER;

BEGIN

     TEST ("CE3104A" , "CHECK THAT COLUMN, LINE, AND " &
                       "PAGE NUMBERS ARE ONE AFTER A " &
                       "CREATE, OPEN, OR RESET");

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
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE " &
                       "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
     END;

     IF COL (FILE) /= ONE THEN
          FAILED ("INCORRECT RESULTS FROM COLUMN AFTER CREATE");
     END IF;
     IF LINE (FILE) /= ONE THEN
          FAILED ("INCORRECT RESULTS FROM LINE AFTER CREATE");
     END IF;
     IF PAGE (FILE) /= ONE THEN
          FAILED ("INCORRECT RESULTS FROM PAGE AFTER CREATE");
     END IF;

     NEW_PAGE (FILE);
     NEW_LINE (FILE);
     PUT (FILE, "STRING");

     CLOSE (FILE);

     BEGIN
          BEGIN
               OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    RAISE SUBTEST;
          END;

          IF COL (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM COLUMN AFTER " &
                       "OPEN - IN_FILE");
          END IF;
          IF LINE (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM LINE AFTER " &
                       "OPEN - IN_FILE");
          END IF;
          IF PAGE (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM PAGE AFTER " &
                       "OPEN - IN_FILE");
          END IF;

          GET (FILE, CHAR);   -- SETS PAGE, LINE, AND COL /= 1

          BEGIN
               RESET (FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    CLOSE (FILE);
                    RAISE SUBTEST;
          END;

          IF COL (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM COLUMN AFTER RESET");
          END IF;
          IF LINE (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM LINE AFTER RESET");
          END IF;
          IF PAGE (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM PAGE AFTER RESET");
          END IF;

          GET (FILE, CHAR);   -- CHANGES LINE, PAGE, COL; STILL IN_FILE

          BEGIN
               RESET (FILE,OUT_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    CLOSE (FILE);
                    RAISE SUBTEST;
          END;

          IF COL (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM COLUMN AFTER RESET " &
                       "TO OUT_FILE");
          END IF;
          IF LINE (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM LINE AFTER RESET " &
                       "TO OUT_FILE");
          END IF;
          IF PAGE (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM PAGE AFTER RESET " &
                       "TO OUT_FILE");
          END IF;

          CLOSE (FILE);

     EXCEPTION
          WHEN SUBTEST =>
               NULL;
     END;

     BEGIN
          BEGIN
               OPEN (FILE, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    RAISE SUBTEST;
          END;

          IF COL (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM COLUMN AFTER OPEN " &
                       "TO OUT_FILE");
          END IF;
          IF LINE (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM LINE AFTER OPEN " &
                       "TO OUT_FILE");
          END IF;
          IF PAGE (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM PAGE AFTER OPEN " &
                       "TO OUT_FILE");
          END IF;

     EXCEPTION
          WHEN SUBTEST =>
               NULL;
     END;

     BEGIN
          BEGIN
               CREATE (FT, IN_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    RAISE SUBTEST;
          END;

          IF COL (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM COLUMN AFTER CREATE " &
                       "IN IN_FILE");
          END IF;
          IF LINE (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM LINE AFTER CREATE " &
                       "IN IN_FILE");
          END IF;
          IF PAGE (FILE) /= ONE THEN
               FAILED ("INCORRECT RESULTS FROM PAGE AFTER CREATE " &
                       "IN IN_FILE");
          END IF;

          CLOSE (FT);

     EXCEPTION
          WHEN SUBTEST =>
               NULL;
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
END CE3104A;
