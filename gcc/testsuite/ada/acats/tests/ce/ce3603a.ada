-- CE3603A.ADA

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
--     CHECK THAT END_ERROR IS NOT RAISED BY:
--       GET FOR CHARACTERS UNTIL ONLY LINE AND PAGE TERMINATORS REMAIN;
--       GET FROM STRING UNTIL FEWER CHARACTERS THAN NEEDED REMAIN;
--       GET_LINE UNTIL THE FINAL PAGE TERMINATOR HAS BEEN SKIPPED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 08/31/82
--     JBG 12/23/82
--     EG  05/22/85
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  CORRECTED EXCEPTION HANDLING AND REMOVED
--                   DEPENDENCE ON RESET.


WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3603A IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3603A", "CHECK THAT END_ERROR IS RAISED BY GET AFTER " &
                      "THE LAST CHARACTER IN THE FILE HAS BEEN READ");

     DECLARE
          FILE1 : FILE_TYPE;
          OLDCH, CH : CHARACTER;
          ST        : STRING (1..10) := (1..10 => '.');
          COUNT     : NATURAL;
     BEGIN

          BEGIN
               CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
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
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT" &
                            "CREATE");
                    RAISE INCOMPLETE;
          END;

          PUT (FILE1, "LINE ONE");
          NEW_LINE (FILE1);
          PUT (FILE1, "LINE TWO");
          NEW_LINE (FILE1, 3);
          NEW_PAGE (FILE1);
          NEW_PAGE (FILE1);
          CLOSE (FILE1);

          BEGIN

               BEGIN
                    OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT " &
                                         "OPEN WITH IN_FILE MODE");
                         RAISE INCOMPLETE;
               END;

               SKIP_LINE (FILE1);
               GET (FILE1, ST(1..7));
               IF ST(1..7) /= "LINE TW" THEN
                    FAILED ("NOT POSITIONED RIGHT - GET CHAR");
               END IF;

-- COUNT NUMBER OF CHARACTERS IN FIRST LINE (TO ALLOW FOR TRAILING
--     BLANKS)

               COUNT := 0;
               WHILE NOT END_OF_LINE(FILE1)
               LOOP
                    GET (FILE1, CH);
                    OLDCH := CH;
                    COUNT := COUNT + 1;
               END LOOP;

               BEGIN
                    GET (FILE1, CH);
                    FAILED ("END_ERROR NOT RAISED - GET " &
                            "CHARACTER");
               EXCEPTION
                    WHEN END_ERROR =>
                         IF CH /= OLDCH THEN
                              FAILED ("CH MODIFIED ON END_" &
                                      "ERROR");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED " &
                                 "- GET CHARACTER");
               END;

               CLOSE (FILE1);

               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);

               SKIP_LINE (FILE1);
               GET (FILE1, ST(1..7));
               IF ST(1..7) /= "LINE TW" THEN
                    FAILED ("WRONG LINE 2. ACTUALLY READ '" & ST(1..7) &
                            "'");
               END IF;

               BEGIN
                    GET (FILE1, ST(8..8+COUNT));
                    FAILED ("END_ERROR NOT RAISED - GET " &
                            "STRING");
               EXCEPTION
                    WHEN END_ERROR =>
                         IF ST(1..7) /= "LINE TW" THEN
                              FAILED ("ST MODIFIED ON END_ERROR");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED " &
                                 "- GET STRING");
               END;

               CLOSE (FILE1);

          END;

          DECLARE
               LAST : NATURAL;
          BEGIN

               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);

               SKIP_LINE (FILE1);
               GET_LINE (FILE1, ST, LAST);
               IF LAST < 8 THEN
                    FAILED ("LAST < 8.  LAST IS" & INTEGER'IMAGE(LAST));
               ELSIF ST(1..8) /= "LINE TWO" THEN
                    FAILED ("GET_LINE FAILED. ACTUALLY READ '" &
                            ST(1..8) & "'");
               END IF;

               SKIP_PAGE (FILE1);
               SKIP_PAGE (FILE1);

               BEGIN
                    GET_LINE (FILE1, ST(1..1), LAST);
                    FAILED ("END_ERROR NOT RAISED - GET_LINE - 1");
               EXCEPTION
                    WHEN END_ERROR =>
                         IF LAST /= 8 THEN
                              FAILED ("LAST MODIFIED BY GET_LINE " &
                                      "ON END_ERROR.  LAST IS" &
                                      INTEGER'IMAGE(LAST));
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION - GET_LINE - 1");
               END;

               BEGIN     -- NULL ITEM ARGUMENT
                    GET_LINE (FILE1, ST(1..0), LAST);
               EXCEPTION
                    WHEN END_ERROR =>
                         FAILED ("GET_LINE ATTEMPTED TO READ INTO A " &
                                 "NULL STRING");
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION - GET_LINE - 2");
               END;
          END;

          BEGIN
               DELETE (FILE1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3603A;
