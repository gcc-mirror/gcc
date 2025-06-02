-- CE3106B.ADA

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
--     CHECK THAT RESETTING AN OUT_FILE TO AN IN_FILE HAS THE FOLLOWING
--     EFFECT:
--          1) IF THERE IS NO LINE TERMINATOR, A LINE TERMINATOR, PAGE
--             TERMINATOR, AND FILE TERMINATOR ARE WRITTEN AT THE END
--             OF THE FILE.
--          2) IF THERE IS A LINE TERMINATOR BUT NO PAGE TERMINATOR, A
--             PAGE TERMINATOR AND A FILE TERMINATOR ARE WRITTEN.
--          3) IF THERE IS A PAGE TERMINATOR, A FILE TERMINATOR IS
--             WRITTEN.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 07/08/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3106B IS

     INCOMPLETE : EXCEPTION;
     FILE1, FILE2, FILE3 : FILE_TYPE;
     ITEM : CHARACTER;

BEGIN

     TEST ("CE3106B", "CHECK THAT RESETTING AN OUT_FILE TO AN " &
                      "IN_FILE HAS THE CORRECT EFFECT ON THE " &
                      "FILE CONCERNING LINE, PAGE, AND FILE " &
                      "TERMINATORS");

     BEGIN

          BEGIN
               CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                                    "WITH MODE OUT_FILE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE" &
                                    "WITH MODE OUT_FILE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          PUT (FILE1, 'A');
          NEW_LINE (FILE1);
          PUT (FILE1, 'B');

          BEGIN
               RESET (FILE1, IN_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON FILE RESET " &
                                    "FROM OUT_FILE TO IN_FILE");
                    RAISE INCOMPLETE;
          END;

          GET (FILE1, ITEM);

          IF LINE (FILE1) /= 1 THEN
               FAILED ("INCORRECT LINE NUMBER - 1");
          END IF;

          GET (FILE1, ITEM);
          IF ITEM /= 'B' THEN
               FAILED ("INCORRECT VALUE READ - 1");
          END IF;

          IF LINE (FILE1) /= 2 THEN
               FAILED ("INCORRECT LINE NUMBER - 2");
          END IF;

          IF NOT END_OF_LINE (FILE1) THEN
               FAILED ("LINE TERMINATOR NOT WRITTEN WHEN FILE " &
                       "IS RESET");
          END IF;

          IF NOT END_OF_PAGE (FILE1) THEN
               FAILED ("PAGE TERMINATOR NOT WRITTEN WHEN FILE " &
                       "IS RESET");
          END IF;

          IF NOT END_OF_FILE (FILE1) THEN
               FAILED ("FILE TERMINATOR NOT WRITTEN WHEN FILE " &
                       "IS RESET");
          END IF;

          BEGIN
               DELETE (FILE1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

          CREATE (FILE2, OUT_FILE, LEGAL_FILE_NAME(2));
          PUT (FILE2, 'A');
          NEW_LINE (FILE2);
          PUT (FILE2, 'B');
          NEW_PAGE (FILE2);
          PUT (FILE2, 'C');
          NEW_LINE (FILE2);

          RESET (FILE2, IN_FILE);

          GET (FILE2, ITEM);
          GET (FILE2, ITEM);

          IF LINE (FILE2) /= 2 THEN
               FAILED ("INCORRECT LINE NUMBER - 3");
          END IF;

          GET (FILE2, ITEM);
          IF ITEM /= 'C' THEN
               FAILED ("INCORRECT VALUE READ - 2");
          END IF;

          IF LINE(FILE2) /= 1 THEN
               FAILED ("INCORRECT LINE NUMBER - 4");
          END IF;

          IF PAGE(FILE2) /= 2 THEN
               FAILED ("INCORRECT PAGE NUMBER - 1");
          END IF;

          IF NOT END_OF_PAGE (FILE2) THEN
               FAILED ("PAGE TERMINATOR NOT WRITTEN WHEN FILE " &
                       "IS RESET - 2");
          END IF;

          IF NOT END_OF_FILE (FILE2) THEN
               FAILED ("FILE TERMINATOR NOT WRITTEN WHEN FILE " &
                       "IS RESET - 2");
          END IF;

          BEGIN
               DELETE (FILE2);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

          CREATE (FILE3, OUT_FILE, LEGAL_FILE_NAME(3));
          PUT (FILE3, 'A');
          NEW_PAGE (FILE3);
          PUT (FILE3, 'B');
          NEW_PAGE (FILE3);
          NEW_LINE (FILE3);
          PUT (FILE3, 'C');
          NEW_PAGE (FILE3);

          RESET (FILE3, IN_FILE);

          GET (FILE3, ITEM);
          IF ITEM /= 'A' THEN
               FAILED ("INCORRECT VALUE READ - 3");
          END IF;

          GET (FILE3, ITEM);
          GET (FILE3, ITEM);

          IF LINE(FILE3) /= 2 THEN
               FAILED ("INCORRECT LINE NUMBER - 5");
          END IF;

          IF PAGE(FILE3) /= 3 THEN
               FAILED ("INCORRECT PAGE NUMBER - 2");
          END IF;

          IF NOT END_OF_FILE (FILE3) THEN
               FAILED ("FILE TERMINATOR NOT WRITTEN WHEN FILE " &
                       "IS RESET - 3");
          END IF;

          BEGIN
               DELETE (FILE3);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3106B;
