-- CE3403F.ADA

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
--     CHECK THAT SKIP_LINE RAISES END_ERROR IF AN ATTEMPT IS
--     MADE TO SKIP A FILE TERMINATOR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 11/11/82
--     SPS 12/14/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  REVISED TEST TO USE A FILE NAME, REMOVED
--                   DEPENDENCE ON RESET, AND ADDED ATTEMPT TO
--                   DELETE THE FILE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3403F IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     CHAR : CHARACTER := ('C');
     ONE : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT (1));
     TWO : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT (2));

BEGIN
     TEST ("CE3403F" , "CHECK THAT SKIP_LINE RAISES END_ERROR " &
                       "IF AN ATTEMPT IS MADE TO SKIP A FILE " &
                       "TERMINATOR");

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
               FAILED ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     FOR I IN 1..3
     LOOP
          PUT (FILE,CHAR);
     END LOOP;

      CLOSE (FILE);

     BEGIN
           OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN " &
                               "FOR IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          GET (FILE, CHAR);
          IF CHAR /= 'C' THEN
               FAILED ("INCORRECT VALUE READ");
          END IF;

          SKIP_LINE (FILE);
          SKIP_LINE (FILE);
          FAILED ("END_ERROR NOT RAISED - 1");
     EXCEPTION
          WHEN END_ERROR =>

               IF COL (FILE) /= ONE THEN
                    FAILED ("COL NOT RESET CORRECTLY");
               END IF;

               IF NOT END_OF_FILE (FILE) THEN
                    FAILED ("NOT POSITIONED AT END OF FILE");
               END IF;

               IF PAGE (FILE) /= TWO THEN
                    FAILED ("PAGE NOT INCREMENTED");
               END IF;

               IF LINE (FILE) /= ONE THEN
                    FAILED ("LINE NOT RESET CORRECTLY");
               END IF;

               IF NOT END_OF_LINE (FILE) THEN
                    FAILED ("EOL FALSE AT FILE TERMINATOR");
               END IF;

               IF NOT END_OF_PAGE (FILE) THEN
                    FAILED ("EOP FALSE AT FILE TERMINATOR");
               END IF;

               BEGIN
                    SKIP_LINE (FILE);
                    FAILED ("END_ERROR NOT RAISED - 2");
               EXCEPTION
                    WHEN END_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 1");
               END;

          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
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

END CE3403F;
