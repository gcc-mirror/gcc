-- CE3413B.ADA

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
--     CHECK THAT PAGE RAISES LAYOUT_ERROR WHEN THE VALUE OF THE
--     PAGE NUMBER EXCEEDS COUNT'LAST.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- HISTORY:
--     JLH 07/27/88  CREATED ORIGINAL TEST.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;


PROCEDURE CE3413B IS

     FILE : FILE_TYPE;
     INCOMPLETE, INAPPLICABLE : EXCEPTION;
     ITEM : STRING(1..3) := "ABC";
     LST : NATURAL;

BEGIN

     TEST ("CE3413B", "CHECK THAT PAGE RAISES LAYOUT_ERROR WHEN THE " &
                      "VALUE OF THE PAGE NUMBER EXCEEDS COUNT'LAST");

     BEGIN

          IF COUNT'LAST > 150000 THEN
               RAISE INAPPLICABLE;
          END IF;

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

          FOR I IN 1 .. COUNT'LAST-1 LOOP
               NEW_PAGE (FILE);
          END LOOP;

          PUT (FILE, ITEM);

          NEW_PAGE (FILE);
          PUT (FILE, "DEF");

          BEGIN
               IF PAGE(FILE) <= POSITIVE_COUNT(COUNT'LAST) THEN
                    FAILED ("PAGE NUMBER INCORRECT AFTER PAGE SET - 1");
               END IF;
               FAILED ("LAYOUT_ERROR NOT RAISED FOR PAGE - 1");
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    NULL;
               WHEN CONSTRAINT_ERROR =>
                    FAILED ("CONSTRAINT_ERROR RAISED FOR PAGE - 1");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED FOR PAGE - 1");
          END;

          CLOSE (FILE);

          BEGIN
               OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          FOR I IN 1 .. COUNT'LAST-1 LOOP
               SKIP_PAGE (FILE);
          END LOOP;

          IF PAGE(FILE) /= COUNT'LAST THEN
               FAILED ("INCORRECT PAGE NUMBER");
          END IF;

          GET_LINE (FILE, ITEM, LST);
          IF ITEM /= "ABC" THEN
               FAILED ("INCORRECT VALUE READ");
          END IF;

          SKIP_PAGE (FILE);

          BEGIN
               IF PAGE(FILE) <= POSITIVE_COUNT(COUNT'LAST) THEN
                    FAILED ("PAGE NUMBER INCORRECT AFTER PAGE SET - 2");
               END IF;
               FAILED ("LAYOUT_ERROR NOT RAISED FOR PAGE - 2");
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    NULL;
               WHEN CONSTRAINT_ERROR =>
                    FAILED ("CONSTRAINT_ERROR RAISED FOR PAGE - 2");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED FOR PAGE - 2");
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
          WHEN INAPPLICABLE =>
               NOT_APPLICABLE ("THE VALUE OF COUNT'LAST IS GREATER " &
                               "THAN 150000.  THE CHECKING OF THIS " &
                               "OBJECTIVE IS IMPRACTICAL");

     END;

     RESULT;

END CE3413B;
