-- CE3403E.ADA

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
--     CHECK THAT SKIP_LINE INCREMENTS THE CURRENT LINE NUMBER BY ONE
--     AND SETS THE CURRENT COLUMN NUMBER TO ONE IF THE LINE TERMINATOR
--     IS NOT FOLLOWED BY A PAGE TERMINATOR, AND THAT IT SETS BOTH THE
--     LINE AND COLUMN NUMBERS TO ONE AND INCREMENTS THE CURRENT PAGE
--     NUMBER BY ONE IF THE LINE TERMINATOR IS FOLLOWED BY A PAGE
--     TERMINATOR.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/20/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  REVISED TEST TO USE A FILE NAME, REMOVED
--                   DEPENDENCE ON RESET, AND ATTEMPTED TO
--                   DELETE THE FILE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3403E IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     ONE  : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(1));
     TWO  : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(2));
     CHAR : CHARACTER := ('C');

BEGIN

     TEST ("CE3403E" , "CHECK THAT SKIP_LINE SETS COLUMN, " &
                       "LINE, AND PAGE NUMBERS CORRECTLY");

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

     PUT (FILE, CHAR);
     NEW_LINE (FILE);
     PUT (FILE, CHAR);
     NEW_PAGE (FILE);
     PUT (FILE, CHAR);

      CLOSE (FILE);

     BEGIN
           OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN " &
                               "WITH IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     IF (LINE (FILE) /= ONE) OR (PAGE (FILE) /= ONE) THEN
          FAILED ("INCORRECT LINE AND PAGE NUMBERS");
     ELSE

-- LINE TERMINATOR NOT FOLLOWED BY PAGE TERMINATOR

          GET (FILE, CHAR);

          IF CHAR /= 'C' THEN
               FAILED ("INCORRECT VALUE READ - 1");
          END IF;

          SKIP_LINE (FILE);
          IF LINE (FILE) /= TWO THEN
               FAILED ("FIRST SUBTEST - LINE NOT INCREMENTED");
          END IF;
          IF COL (FILE) /= ONE THEN
               FAILED ("FIRST SUBTEST - COLUMN NOT SET TO ONE");
          END IF;

-- LINE TERMINATOR FOLLOWED BY PAGE TERMINATOR

          GET (FILE, CHAR);

          IF CHAR /= 'C' THEN
               FAILED ("INCORRECT VALUE READ - 2");
          END IF;

          SKIP_LINE (FILE);
          IF LINE (FILE) /= ONE THEN
               FAILED ("SECOND SUBTEST - LINE NOT SET TO ONE");
          END IF;
          IF COL (FILE) /= ONE THEN
               FAILED ("SECOND SUBTEST - COLUMN NOT SET TO ONE");
          END IF;
          IF PAGE (FILE) /= TWO THEN
               FAILED ("SECOND SUBTEST - PAGE NOT INCREMENTED");
          END IF;
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

END CE3403E;
