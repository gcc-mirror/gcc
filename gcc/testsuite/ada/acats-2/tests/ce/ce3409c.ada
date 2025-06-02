-- CE3409C.ADA

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
--     CHECK THAT SET_COL SETS THE CURRENT COLUMN NUMBER TO THE VALUE
--     SPECIFIED BY TO FOR FILES OF MODES IN_FILE AND OUT_FILE.
--     CHECK THAT IT HAS NO EFFECT IF THE VALUE SPECIFIED BY TO IS
--     EQUAL TO THE CURRENT COLUMN NUMBER FOR BOTH IN_FILE AND OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/20/82
--     JBG 01/27/83
--     SPS 02/18/83
--     EG  05/22/85
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/31/87  REMOVED DEPENDENCE ON RESET, REMOVED UNNECESSARY
--                   CODE, AND CHECKED FOR USE_ERROR ON DELETE.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;
WITH CHECK_FILE;

PROCEDURE CE3409C IS

     INCOMPLETE : EXCEPTION;

BEGIN
     TEST ("CE3409C", "CHECK THAT SET_COL SETS THE CURRENT COLUMN " &
                      "NUMBER TO THE VALUE SPECIFIED BY TO FOR FILES " &
                      "OF MODES IN_FILE AND OUT_FILE.  CHECK THAT IT " &
                      "HAS NO EFFECT IF THE VALUE SPECIFIED BY TO IS " &
                      "EQUAL TO THE CURRENT COLUMN NUMBER FOR BOTH " &
                      "IN_FILE AND OUT_FILE");

     DECLARE
          FILE : FILE_TYPE;
          CHAR : CHARACTER := ('C');
          ITEM_CHAR : CHARACTER;
          ONE  : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(1));
          TWO  : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(2));
          FOUR : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(4));
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

          SET_PAGE_LENGTH (FILE, TWO);
          SET_COL (FILE, FOUR);
          IF COL (FILE) /= FOUR THEN
               FAILED ("FOR OUT_FILE COLUMN NOT FOUR");
          ELSE
               PUT (FILE, 'C');
               SET_COL (FILE, 5);
               IF COL (FILE) /= FOUR+1 OR LINE (FILE) /= ONE THEN
                    FAILED ("FOR OUT_FILE COLUMN UNNECESSARILY " &
                            "CHANGED FROM FOUR");
               ELSE
                    SET_COL (FILE, 8);
                    PUT (FILE, "DE");
                    SET_COL (FILE, TWO+1);
                    IF COL (FILE) /= TWO+ONE OR LINE (FILE) /= TWO THEN
                         FAILED ("FOR OUT_FILE COLUMN NOT TWO");
                    END IF;
                    PUT (FILE, 'B');
                    SET_COL (FILE, TWO);

                    IF PAGE (FILE) /= TWO THEN
                         FAILED ("PAGE TERMINATOR NOT OUTPUT");
                    END IF;

                    IF LINE (FILE) /= ONE THEN
                         FAILED ("LINE TERMINATOR NOT OUTPUT");
                    END IF;

                    IF COL (FILE) /= TWO THEN
                         FAILED ("COL NOT TWO; IS" &
                                 COUNT'IMAGE(COL(FILE)));
                    END IF;

                    PUT (FILE, 'X');
               END IF;
          END IF;

          CHECK_FILE (FILE, "   C   DE#  B#@ X#@%");

          CLOSE (FILE);

          BEGIN
               OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN " &
                                    "WITH MODE IN_FILE");
                    RAISE INCOMPLETE;
          END;

          SET_COL (FILE, FOUR);
          IF COL (FILE) /= FOUR THEN
               FAILED ("FOR IN_FILE COLUMN NOT FOUR");
          ELSE
               GET (FILE, ITEM_CHAR);
               IF ITEM_CHAR /= 'C' THEN
                    FAILED ("SET_COL FOR READ; ACTUALLY READ '" &
                            ITEM_CHAR & "'");
               END IF;

               SET_COL (FILE, 5);
               IF COL (FILE) /= FOUR+1 OR LINE (FILE) /= ONE THEN
                    FAILED ("FOR IN_FILE COLUMN UNNECESSARILY " &
                            "CHANGED FROM FOUR");
               ELSE
                    SET_COL (FILE, 9);
                    GET (FILE, ITEM_CHAR);
                    IF ITEM_CHAR /= 'E' THEN
                         FAILED ("SET_COL FOR READ 2; ACTUALLY READ '" &
                                 ITEM_CHAR & "'");
                    END IF;

                    SET_COL (FILE, 3);
                    GET (FILE, ITEM_CHAR);
                    IF ITEM_CHAR /= 'B' THEN
                         FAILED ("SET_COL FOR READ 3; ACTUALLY READ '" &
                                 ITEM_CHAR & "'");
                    END IF;

                    IF COL (FILE) /= 4 OR LINE (FILE) /= TWO THEN
                         FAILED ("FOR IN_FILE COLUMN NOT TWO");
                    END IF;
               END IF;
          END IF;

          BEGIN
               DELETE (FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3409C;
