-- CE3604B.ADA

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
--     CHECK THAT GET_LINE DOES NOT DO A SKIP_LINE AND NO CHARACTERS ARE
--     READ WHEN THE INPUT IS AT THEN END OF A LINE AND THE STRING
--     PARAMETER IS A NULL STRING.  ALSO CHECK THAT GET_LINE DOES NOT
--     SKIP THE LINE TERMINATOR AFTER READING ALL THE CHARACTERS INTO
--     A STRING WHICH IS EXACTLY EQUAL TO THE NUMBER OF CHARACTERS
--     REMAINING ON THAT LINE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 10/13/87  CREATED ORIGINAL TEST.


WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3604B IS

BEGIN

     TEST ("CE3604B", "CHECK THAT GET_LINE READS LINES APPROPRIATELY");

     DECLARE
          INCOMPLETE : EXCEPTION;
          FILE : FILE_TYPE;
          ITEM1 : STRING (1 .. 19);
          ITEM2 : STRING (1 .. 20);
          NULL_ITEM : STRING (2 .. 1);
          LAST : NATURAL;

     BEGIN
          BEGIN
               CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT " &
                                    "CREATE WITH OUT_FILE MODE");

                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT " &
                            "CREATE");
                    RAISE INCOMPLETE;
          END;

          PUT (FILE, "FIRST LINE OF INPUT");
          NEW_LINE (FILE);
          PUT (FILE, "SECOND LINE OF INPUT");
          NEW_LINE (FILE);
          PUT (FILE, "THIRD LINE OF INPUT");

          CLOSE (FILE);

          BEGIN
               OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          GET (FILE, ITEM1);
          IF ITEM1 /= "FIRST LINE OF INPUT" THEN
               FAILED ("INCORRECT VALUE FOR GET");
          END IF;

          GET_LINE (FILE, NULL_ITEM, LAST);

          IF LINE (FILE) /= 1 THEN
               FAILED ("INCORRECT LINE NUMBER AFTER GET_LINE - 1");
          END IF;

          IF COL (FILE) /= 20 THEN
               FAILED ("INCORRECT COLUMN NUMBER AFTER GET_LINE - 1");
          END IF;

          SKIP_LINE (FILE);
          GET_LINE (FILE, ITEM2, LAST);
          IF ITEM2 /= "SECOND LINE OF INPUT" THEN
               FAILED ("INCORRECT VALUE FOR GET_LINE");
          END IF;

          IF LINE (FILE) /= 2 THEN
               FAILED ("INCORRECT LINE NUMBER AFTER GET_LINE - 2");
          END IF;

          IF COL (FILE) /= 21 THEN
               FAILED ("INCORRECT COLUMN NUMBER AFTER GET_LINE - 2");
          END IF;

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

END CE3604B;
