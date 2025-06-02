-- CE3410C.ADA

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
--     CHECK THAT SET_LINE SETS THE CURRENT LINE NUMBER TO THE VALUE
--     SPECIFIED BY TO FOR FILES OF MODES IN_FILE AND OUT_FILE.
--     CHECK THAT IT HAS NO EFFECT IF THE VALUE SPECIFIED BY TO IS
--     EQUAL TO THE CURRENT LINE NUMBER FOR BOTH IN_FILE AND OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/20/82
--     JBG 01/27/83
--     EG  05/22/85
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/01/87  REMOVED DEPENDENCE ON RESET, ADDED MORE TEST
--                   CASES, AND CHECKED FOR USE_ERROR ON DELETE.
--     JRL 02/29/96  Added File parameter to call to Set_Page_Length.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;
WITH CHECK_FILE;

PROCEDURE CE3410C IS

     INCOMPLETE : EXCEPTION;

BEGIN
     TEST ("CE3410C", "CHECK THAT SET_LINE SETS LINE " &
                      "NUMBER CORRECTLY");

     DECLARE
          FILE : FILE_TYPE;
          CHAR : CHARACTER := ('C');
          ITEM_CHAR : CHARACTER;
          ONE   : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(1));
          TWO   : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(2));
          THREE : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(3));
          FOUR  : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(4));
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

          SET_LINE (FILE, FOUR);
          IF LINE (FILE) /= FOUR THEN
               FAILED ("FOR OUT_FILE LINE NOT FOUR");
          ELSE
               PUT (FILE, 'C');
               NEW_LINE (FILE);
               SET_LINE (FILE, 5);
               IF LINE (FILE) /= FOUR+1 THEN
                    FAILED ("FOR OUT_FILE LINE UNNECESSARILY " &
                            "CHANGED FROM FOUR");
               ELSE
                    SET_LINE (FILE, 8);
                    PUT (FILE, "DE");
                    SET_LINE (FILE, TWO+1);
                    IF LINE (FILE) /= TWO+ONE THEN
                         FAILED ("FOR OUT_FILE LINE NOT THREE");
                    END IF;

                    SET_LINE (FILE, TWO);

                    IF PAGE (FILE) /= ONE+TWO THEN
                         FAILED ("PAGE TERMINATOR NOT OUTPUT - 2");
                    END IF;

                    IF LINE (FILE) /= TWO THEN
                         FAILED ("LINE NOT TWO; IS" &
                                 COUNT'IMAGE(LINE(FILE)));
                    END IF;

                    SET_PAGE_LENGTH (FILE, TWO);
                    PUT (FILE, 'X');
                    SET_LINE (FILE, TWO);
                    PUT (FILE, 'Y');

                    IF LINE (FILE) /= TWO THEN
                         FAILED ("LINE NOT TWO; IS " &
                                 COUNT'IMAGE(LINE(FILE)));
                    END IF;

                    IF PAGE (FILE) /= THREE THEN
                         FAILED ("PAGE NOT THREE; IS " &
                                 COUNT'IMAGE(PAGE(FILE)));
                    END IF;

               END IF;
          END IF;

          CHECK_FILE (FILE, "###C####DE#@##@#XY#@%");

          CLOSE (FILE);

          BEGIN
               OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED FOR TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          SET_LINE (FILE, FOUR);
          IF LINE (FILE) /= FOUR THEN
               FAILED ("FOR IN_FILE LINE NOT FOUR");
          ELSE
               GET (FILE, ITEM_CHAR);
               IF ITEM_CHAR /= 'C' THEN
                    FAILED ("SET_LINE FOR READ; ACTUALLY READ '" &
                            ITEM_CHAR & "'");
               END IF;

               SKIP_LINE (FILE);
               SET_LINE (FILE, 5);
               IF LINE (FILE) /= FOUR+1 OR PAGE (FILE) /= ONE THEN
                    FAILED ("INCORRECT LINE OR PAGE");
               ELSE
                    SET_LINE (FILE, 8);
                    GET (FILE, ITEM_CHAR);
                    IF ITEM_CHAR /= 'D' THEN
                         FAILED ("SET_LINE FOR READ 2; ACTUALLY READ '"&
                                 ITEM_CHAR & "'");
                    END IF;

                    SET_LINE (FILE, TWO);
                    IF PAGE (FILE) /= TWO THEN
                         FAILED ("FOR IN_FILE PAGE NOT TWO");
                    END IF;

                    SET_LINE (FILE, TWO);
                    IF PAGE (FILE) /= TWO OR LINE (FILE) /= TWO THEN
                         FAILED ("FOR IN_FILE PAGE NOT 2");
                    END IF;

                    SKIP_LINE (FILE);
                    SET_LINE (FILE, TWO);

                    GET (FILE, ITEM_CHAR);

                    IF ITEM_CHAR /= 'X' THEN
                         FAILED ("SET_LINE FOR READ 3; ACTUALLY READ '"&
                                 ITEM_CHAR & "'");
                    END IF;

               END IF;
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

END CE3410C;
