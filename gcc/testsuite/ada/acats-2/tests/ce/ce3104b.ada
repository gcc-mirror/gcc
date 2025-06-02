-- CE3104B.ADA

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
--     CHECK THAT THE FILE REMAINS OPEN AFTER A RESET.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     DWC 08/13/87  CREATED ORIGINAL TEST.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3104B IS

     INCOMPLETE : EXCEPTION;
     FILE   : FILE_TYPE;
     ITEM1  : STRING (1..5) := "STUFF";

BEGIN

     TEST ("CE3104B", "CHECK THAT THE FILE REMAINS OPEN AFTER " &
                      "A RESET");

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
          PUT_LINE (FILE, ITEM1);
          CLOSE (FILE);
     EXCEPTION
          WHEN USE_ERROR | NAME_ERROR =>
               NOT_APPLICABLE ("CREATE WITH OUT_FILE MODE " &
                               "NOT SUPPORTED");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED DURING " &
                       "FILE I/O");
               RAISE INCOMPLETE;
     END;

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("OPEN WITH IN_FILE MODE NOT " &
                               "SUPPORTED");
               RAISE INCOMPLETE;
     END;

     BEGIN
          RESET (FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     IF IS_OPEN (FILE) THEN
          CLOSE (FILE);
     ELSE
          FAILED ("RESET FOR IN_FILE, CLOSED FILE");
     END IF;

     BEGIN
          OPEN (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("OPEN WITH OUT_FILE MODE NOT " &
                               "SUPPORTED");
               RAISE INCOMPLETE;
     END;

     BEGIN
          RESET (FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     IF IS_OPEN (FILE) THEN
          BEGIN
               DELETE (FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;
     ELSE
          FAILED ("RESET FOR OUT_FILE CLOSED FILE");
     END IF;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;
END CE3104B;
