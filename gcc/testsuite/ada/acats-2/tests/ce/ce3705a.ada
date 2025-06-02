-- CE3705A.ADA

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
--     FOR GET FROM A FILE, CHECK THAT IF ONLY THE FILE TERMINATOR
--     REMAINS TO BE READ, THEN ANY CALL TO GET FOR AN INTEGER (EVEN
--     WITH WIDTH = 0) RAISES END_ERROR.

-- HISTORY:
--     BCB 10/28/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3705A IS

     FILE : FILE_TYPE;

     INCOMPLETE : EXCEPTION;

     I : INTEGER;

     PACKAGE INT_IO IS NEW INTEGER_IO(INTEGER); USE INT_IO;

BEGIN
     TEST ("CE3705A", "FOR GET FROM A FILE, CHECK THAT IF ONLY THE " &
                      "FILE TERMINATOR REMAINS TO BE READ, THEN ANY " &
                      "CALL TO GET FOR AN INTEGER (EVEN WITH WIDTH = " &
                      "0) RAISES END_ERROR");

     BEGIN
          BEGIN
               CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          PUT (FILE, 3);

          CLOSE (FILE);

          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);

          GET (FILE, I);

          BEGIN
               GET (FILE, I);
               FAILED ("END_ERROR NOT RAISED - 1");
          EXCEPTION
               WHEN END_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 1");
          END;

          BEGIN
               GET (FILE, I, WIDTH => 0);
               FAILED ("END_ERROR NOT RAISED - 2");
          EXCEPTION
               WHEN END_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 2");
          END;

          BEGIN
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
END CE3705A;
