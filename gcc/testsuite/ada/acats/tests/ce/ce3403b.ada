-- CE3403B.ADA

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
--     CHECK THAT THE SPACING PARAMETER OF SKIP_LINE IS OPTIONAL,
--     AND THAT THE DEFAULT VALUE IS ONE.
--     CHECK THAT THE FILE PARAMETER IS ALSO OPTIONAL, AND THAT THE
--     FUNCTION IS THEN APPLIED TO THE CURRENT DEFAULT INPUT FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 12/14/82
--     JBG 1/17/83
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/04/87  REVISED EXCEPTION HANDLERS, REMOVED
--                   DEPENDENCIES ON RESET, AND ADDED AN ATTEMPT
--                   TO DELETE FILE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3403B IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     SPAC, TWO : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(2));
     A    : INTEGER := CHARACTER'POS('A');
     CH   : CHARACTER;

BEGIN

     TEST ("CE3403B" , "CHECK DEFAULT SPACING AND FILE " &
                       "OF SKIP_LINE");

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

     FOR I IN 1 .. 3 LOOP          -- CREATES "BBB#CC#D##F#@%"
          FOR J IN 1 .. 4-I LOOP
               PUT (FILE, CHARACTER'VAL(A + I));
          END LOOP;
          NEW_LINE (FILE);
     END LOOP;
     NEW_LINE (FILE);
     PUT (FILE, 'F');

     CLOSE (FILE);

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN " &
                               "FOR IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     GET (FILE, CH);
     IF CH /= CHARACTER'VAL (A + 1) THEN
          FAILED ("LINE CONTENT WRONG - 1");
     END IF;

     SKIP_LINE (FILE);

     IF LINE (FILE) /= TWO THEN
          FAILED ("SPACING DEFAULT NOT ONE");
     END IF;

     GET (FILE, CH);
     IF CH /= CHARACTER'VAL (A + 2) THEN
          FAILED ("LINE CONTENT WRONG - 2");
     END IF;

     SET_INPUT (FILE);
     SKIP_LINE (FILE);

     IF LINE (FILE) /= 3 THEN
          FAILED ("SKIP_LINE DOES NOT OPERATE CORRECTLY ON " &
                  "DEFAULT FILE");
     END IF;

     GET (FILE, CH);
     IF CH /= CHARACTER'VAL (A + 3) THEN
          FAILED ("LINE CONTENT WRONG - 3");
     END IF;

     SKIP_LINE;

     IF LINE (FILE) /= 4 THEN
          FAILED ("LINE COUNT NOT 4; WAS " & COUNT'IMAGE(LINE(FILE)));
     END IF;

     GET (FILE, CH);
     IF CH /= 'F' THEN
          FAILED ("NOT RIGHT LINE");
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

END CE3403B;
