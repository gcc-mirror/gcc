-- CE3409D.ADA

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
--     CHECK THAT, FOR FILES OF MODE IN_FILE, SET_COL READS UNTIL A
--     LINE FOUND HAVING A CHARACTER AT THE SPECIFIED COLUMN, SKIPPING
--     LINE AND PAGE TERMINATORS AS NECESSARY.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JBG 01/27/83
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/31/87  REMOVED DEPENDENCE ON REST, REMOVED UNNECESSARY
--                   CODE, CHECKED FOR USE_ERROR ON DELETE, AND ADDED
--                   NEW CASES FOR SET_COL.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3409D IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     FOUR : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(4));
     ITEM_CHAR : CHARACTER;

BEGIN

     TEST ("CE3409D", "CHECK THAT SET_COL SKIPS LINE AND PAGE " &
                      "TERMINATORS WHEN NECESSARY");

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE WITH " &
                               "MODE OUT_FILE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT CREATE " &
                               "WITH MODE OUT_FILE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     PUT (FILE, "ABC");
     NEW_LINE (FILE);
     PUT (FILE, "DEFGHI");
     NEW_PAGE (FILE);
     PUT (FILE, "XYZ");
     NEW_PAGE (FILE);
     PUT (FILE, "IJKL");

     CLOSE (FILE);

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN WITH " &
                               "MODE IN_FILE");
               RAISE INCOMPLETE;
     END;

     SET_COL (FILE, FOUR);
     GET (FILE, ITEM_CHAR);

     IF ITEM_CHAR = ' ' THEN
          BEGIN
               COMMENT ("FILE PADS LINES WITH SPACES");

               SET_COL (FILE, FOUR);
               GET (FILE, ITEM_CHAR);
               IF ITEM_CHAR /= 'G' THEN
                    FAILED ("INCORRECT VALUE FROM SET_COL - 1");
               END IF;

               SET_COL (FILE, FOUR);
               GET (FILE, ITEM_CHAR);
               IF ITEM_CHAR /= ' ' THEN
                    FAILED ("LINES SHOULD STILL BE PADDED WITH BLANKS");
               END IF;
          END;

     ELSIF ITEM_CHAR /= 'G' THEN
          FAILED ("SET_COL DOESN'T SKIP LINE MARKS; " &
                  "ACTUALLY READ '" & ITEM_CHAR & "'");
     ELSE
          BEGIN
               SET_COL (FILE, FOUR);
               GET (FILE, ITEM_CHAR);

               IF ITEM_CHAR /= 'L' THEN
                    FAILED ("SET_COL DOESN'T SKIP PAGE MARKS; " &
                            "ACTUALLY READ '" & ITEM_CHAR & "'");
               END IF;
          END;
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

END CE3409D;
