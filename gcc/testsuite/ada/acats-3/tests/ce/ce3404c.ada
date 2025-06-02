-- CE3404C.ADA

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
--     CHECK THAT END_OF_LINE RETURNS THE CORRECT VALUE WHEN POSITIONED
--     AT THE BEGINNING AND THE END OF A LINE, AND WHEN POSITIONED JUST
--     BEFORE THE FILE TERMINATOR.

--          CASE 1)  BOUNDED LINE LENGTH

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/17/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 09/22/87  REMOVED DEPENDENCE ON RESET AND MOVED THE CHECK
--                   FOR UNBOUNDED LINE_LENGTH TO CE3404D.ADA.

WITH REPORT;  USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3404C IS
     INCOMPLETE     : EXCEPTION;
     MY_FILE        : FILE_TYPE;
     ITEM_CHAR      : CHARACTER;
     CHAR           : CHARACTER := ('C');
     TEN            : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(10));
     BLANK_COUNTER  : NATURAL := 0;

BEGIN

     TEST ("CE3404C", "CHECK THAT END_OF_LINE RETURNS THE CORRECT " &
                      "VALUE WHEN POSITIONED AT THE BEGINNING " &
                      "AND THE END OF A LINE, AND WHEN POSITIONED " &
                      "JUST BEFORE THE FILE TERMINATOR");

--   CREATE AND INITIALIZE TEST FILE WITH BOUNDED LINE LENGTH

     BEGIN
          CREATE (MY_FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE WITH " &
                               "OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     SET_LINE_LENGTH (MY_FILE,TEN);

     FOR I IN 1..5 LOOP
          PUT (MY_FILE, CHAR);
     END LOOP;
     NEW_LINE (MY_FILE);
     PUT (MY_FILE, 'B');

     CLOSE (MY_FILE);

     BEGIN
          OPEN (MY_FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN WITH " &
                               "IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

--   BEGIN THE TEST

     IF END_OF_LINE (MY_FILE) THEN
          FAILED ("END_OF_LINE: INCORRECT VALUE AT FIRST POSITION - 5");
     END IF;

     IF COL (MY_FILE) /= 1 THEN
          FAILED ("EOL MODIFIED COL NUMBER - 6");
     END IF;

     FOR I IN 1..4 LOOP
          GET (MY_FILE,ITEM_CHAR);
     END LOOP;

     IF END_OF_LINE (MY_FILE) THEN
          FAILED ("END_OF_LINE: INCORRECT VALUE AT FIFTH POSITION - 7");
     END IF;

     GET (MY_FILE,ITEM_CHAR);

     WHILE NOT END_OF_LINE (MY_FILE) LOOP
          GET (MY_FILE, ITEM_CHAR);
          IF ITEM_CHAR = ' ' THEN
               BLANK_COUNTER := BLANK_COUNTER + 1;
          ELSE
               FAILED ("STRING WAS PADDED WITH SOMETHING OTHER THAN " &
                       "BLANKS - 8");
          END IF;
     END LOOP;

     IF BLANK_COUNTER > 5 THEN
          FAILED ("TOO MANY BLANKS WERE USED FOR PADDING - 9");
     END IF;

     IF LINE (MY_FILE) /= 1 THEN
          FAILED ("EOL SKIPPED LINE TERMINATOR - 10");
     END IF;

     IF NOT END_OF_LINE (MY_FILE) THEN
          FAILED ("EOL SKIPPED LINE TERMINATOR - 11");
     END IF;

     SKIP_PAGE (MY_FILE);

     IF PAGE (MY_FILE) /= 2 THEN
          FAILED ("INCORRECT PAGE NUMBER");
     END IF;

     IF NOT END_OF_LINE (MY_FILE) THEN
          FAILED ("INCORRECT VALUE WHEN POSITIONED JUST BEFORE FILE " &
                  "TERMINATOR");
     END IF;

     BEGIN
          DELETE (MY_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3404C;
