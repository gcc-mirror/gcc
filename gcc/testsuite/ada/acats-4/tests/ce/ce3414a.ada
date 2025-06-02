-- CE3414A.ADA

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
--     CHECK THAT STATUS_ERROR IS RAISED WHEN NEW_LINE, SKIP_LINE,
--     END_OF_LINE, NEW_PAGE, SKIP_PAGE, END_OF_PAGE, END_OF_FILE,
--     SET_COL, SET_LINE, COL, LINE, AND PAGE ARE CALLED AND THE FILE
--     IS NOT OPEN.

-- HISTORY:
--     BCB 10/27/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3414A IS

     FILE : FILE_TYPE;

     INCOMPLETE : EXCEPTION;

     X : POSITIVE_COUNT;

BEGIN
     TEST ("CE3414A", "CHECK THAT STATUS_ERROR IS RAISED WHEN " &
                      "NEW_LINE, SKIP_LINE, END_OF_LINE, NEW_PAGE, " &
                      "SKIP_PAGE, END_OF_PAGE, END_OF_FILE, SET_COL, " &
                      "SET_LINE, COL, LINE, AND PAGE ARE CALLED AND " &
                      "THE FILE IS NOT OPEN");

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

          PUT (FILE, 'A');

          CLOSE (FILE);

          BEGIN
               NEW_LINE (FILE);
               FAILED ("STATUS_ERROR WAS NOT RAISED - 1");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 1");
          END;

          BEGIN
               SKIP_LINE (FILE);
               FAILED ("STATUS_ERROR WAS NOT RAISED - 2");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 2");
          END;

          BEGIN
               IF NOT END_OF_LINE (FILE) THEN
                    NULL;
               END IF;
               FAILED ("STATUS_ERROR WAS NOT RAISED - 3");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 3");
          END;

          BEGIN
               NEW_PAGE (FILE);
               FAILED ("STATUS_ERROR WAS NOT RAISED - 4");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 4");
          END;

          BEGIN
               SKIP_PAGE (FILE);
               FAILED ("STATUS_ERROR WAS NOT RAISED - 5");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 5");
          END;

          BEGIN
               IF NOT END_OF_PAGE (FILE) THEN
                    NULL;
               END IF;
               FAILED ("STATUS_ERROR WAS NOT RAISED - 6");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 6");
          END;

          BEGIN
               IF NOT END_OF_FILE (FILE) THEN
                    NULL;
               END IF;
               FAILED ("STATUS_ERROR WAS NOT RAISED - 7");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 7");
          END;

          BEGIN
               SET_COL (FILE, 2);
               FAILED ("STATUS_ERROR WAS NOT RAISED - 8");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 8");
          END;

          BEGIN
               SET_LINE (FILE, 2);
               FAILED ("STATUS_ERROR WAS NOT RAISED - 9");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 9");
          END;

          BEGIN
               X := COL (FILE);
               FAILED ("STATUS_ERROR WAS NOT RAISED - 10");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 10");
          END;

          BEGIN
               X := LINE (FILE);
               FAILED ("STATUS_ERROR WAS NOT RAISED - 11");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 11");
          END;

          BEGIN
               X := PAGE (FILE);
               FAILED ("STATUS_ERROR WAS NOT RAISED - 12");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 12");
          END;
     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;
END CE3414A;
