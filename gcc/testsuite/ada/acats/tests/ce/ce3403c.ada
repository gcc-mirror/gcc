-- CE3403C.ADA

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
--     CHECK THAT SKIP_LINE SETS THE CURRENT COLUMN NUMBER TO ONE,
--     AND THAT IT IS PERFORMED SPACING TIMES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/08/87  REVISED EXCEPTION HANDLING, REMOVED
--                   DEPENDENCE ON RESET, AND ADDED NEW CASES.
--     GJD 11/15/95  FIXED ADA 95 INCOMPATIBLE USE OF CHARACTER LITERALS.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3403C IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     ONE : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(1));
     SPAC3 : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(3));
     FOUR : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(4));
     CH: CHARACTER;

BEGIN

     TEST ("CE3403C" , "CHECK THAT SKIP_LINE SETS THE CURRENT " &
                       "COLUMN NUMBER TO ONE, AND THAT IT IS " &
                       "PERFORMED SPACING TIMES");

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

     FOR I IN CHARACTER RANGE 'A' .. 'E' LOOP
          FOR J IN 1 .. 3 LOOP
               PUT (FILE, I);
          END LOOP;
          NEW_LINE (FILE);
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

     IF COL (FILE) /= ONE THEN
          FAILED ("COLUMN NOT SET TO ONE");
     END IF;

     GET (FILE, CH);

     IF CH /= 'A' THEN
          FAILED ("INCORRECT VALUE READ - 1");
     END IF;

     SKIP_LINE (FILE,SPAC3);
     GET (FILE, CH);

     IF CH /= 'D' THEN
          FAILED ("INCORRECT VALUE READ - 2");
     END IF;

     IF LINE (FILE) /= FOUR THEN
          FAILED ("NOT PERFORMED SPACING TIMES");
     END IF;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3403C;
