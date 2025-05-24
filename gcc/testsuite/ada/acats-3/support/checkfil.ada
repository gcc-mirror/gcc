-- CHECK_FILE.ADA
--
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
--
-- THIS IS A PROCEDURE USED BY MANY OF THE CHAPTER 14 TESTS TO CHECK THE
-- CONTENTS OF A TEXT FILE.

-- THIS PROCEDURE ASSUMES THE FILE PARAMETER PASSED TO IT IS AN OPEN
-- TEXT FILE.

-- THE STRING PARAMETER CONTAINS THE CHARACTERS THAT ARE SUPPOSED TO BE
-- IN THE TEXT FILE.  A '#' CHARACTER IS USED IN THE STRING TO DENOTE 
-- THE END OF A LINE.  A '@' CHARACTER IS USED TO DENOTE THE END OF A 
-- PAGE. A '%' CHARACTER IS USED TO DENOTE THE END OF THE TEXT FILE.
-- THESE SYMBOLS SHOULD NOT BE USED AS TEXT OUTPUT.

-- SPS 11/30/82
-- JBG 2/3/83

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CHECK_FILE (FILE: IN OUT FILE_TYPE; CONTENTS : STRING) IS

     X : CHARACTER;
     COL_COUNT : POSITIVE_COUNT := 1;
     LINE_COUNT : POSITIVE_COUNT := 1;
     PAGE_COUNT : POSITIVE_COUNT := 1;
     TRAILING_BLANKS_MSG_WRITTEN : BOOLEAN := FALSE;
     STOP_PROCESSING : EXCEPTION;

     PROCEDURE CHECK_END_OF_LINE (EXPECT_END_OF_PAGE : BOOLEAN) IS
     BEGIN

-- SKIP OVER ANY TRAILING BLANKS.  AN IMPLEMENTATION CAN LEGALLY
-- APPEND BLANKS TO THE END OF ANY LINE.

          WHILE NOT END_OF_LINE (FILE) LOOP
               GET (FILE, X);
               IF X /= ' ' THEN
                    FAILED ("FROM CHECK_FILE: END OF LINE EXPECTED - " &
                            X & " ENCOUNTERED");
                    RAISE STOP_PROCESSING;
               ELSE
                    IF NOT TRAILING_BLANKS_MSG_WRITTEN THEN
                         COMMENT ("FROM CHECK_FILE: " &
                                  "THIS IMPLEMENTATION PADS " &
                                  "LINES WITH BLANKS");
                         TRAILING_BLANKS_MSG_WRITTEN := TRUE;
                    END IF;
               END IF;
          END LOOP;

          IF LINE_COUNT /= LINE (FILE) THEN
               FAILED ("FROM CHECK_FILE: " &
                       "LINE COUNT INCORRECT - EXPECTED " &
                       POSITIVE_COUNT'IMAGE(LINE_COUNT) &
                       " GOT FROM FILE " & 
                       POSITIVE_COUNT'IMAGE(LINE(FILE)));
          END IF;

-- NOTE:  DO NOT SKIP_LINE WHEN AT END OF PAGE BECAUSE SKIP_LINE WILL
--        ALSO SKIP THE PAGE TERMINATOR.  SEE RM 14.3.5 PARAGRAPH 1.

          IF NOT EXPECT_END_OF_PAGE THEN
               IF END_OF_PAGE (FILE) THEN
                    FAILED ("FROM CHECK_FILE: PREMATURE END OF PAGE");
                    RAISE STOP_PROCESSING;
               ELSE
                    SKIP_LINE (FILE);
                    LINE_COUNT := LINE_COUNT + 1;
               END IF;
          END IF;
          COL_COUNT := 1;
     END CHECK_END_OF_LINE;

     PROCEDURE CHECK_END_OF_PAGE IS
     BEGIN
          IF NOT END_OF_PAGE (FILE) THEN
               FAILED ("FROM CHECK_FILE: " &
                       "END_OF_PAGE NOT WHERE EXPECTED");
               RAISE STOP_PROCESSING;
          ELSE
               IF PAGE_COUNT /= PAGE (FILE) THEN
                    FAILED ("FROM CHECK_FILE: " &
                            "PAGE COUNT INCORRECT - EXPECTED " &
                            POSITIVE_COUNT'IMAGE (PAGE_COUNT) &
                            " GOT FROM FILE " &
                            POSITIVE_COUNT'IMAGE (PAGE(FILE)));
               END IF;

               SKIP_PAGE (FILE);
               PAGE_COUNT := PAGE_COUNT + 1;
               LINE_COUNT := 1;
          END IF;
     END CHECK_END_OF_PAGE;

BEGIN

     RESET (FILE, IN_FILE);
     SET_LINE_LENGTH (STANDARD_OUTPUT, 0);
     SET_PAGE_LENGTH (STANDARD_OUTPUT, 0);

     FOR I IN 1 .. CONTENTS'LENGTH LOOP

          BEGIN
               CASE CONTENTS (I) IS
                    WHEN '#' =>
                         CHECK_END_OF_LINE (CONTENTS (I + 1) = '@');
                    WHEN '@' =>
                         CHECK_END_OF_PAGE;
                    WHEN '%' =>
                         IF NOT END_OF_FILE (FILE) THEN
                              FAILED ("FROM CHECK_FILE: " &
                                      "END_OF_FILE NOT WHERE EXPECTED");
                              RAISE STOP_PROCESSING;
                         END IF;
                    WHEN OTHERS =>
                         IF COL_COUNT /= COL(FILE) THEN
                              FAILED ("FROM CHECK_FILE: " &
                                      "COL COUNT INCORRECT - " &
                                      "EXPECTED " & POSITIVE_COUNT'
                                      IMAGE(COL_COUNT) & " GOT FROM " &
                                      "FILE " & POSITIVE_COUNT'IMAGE
                                      (COL(FILE)));
                         END IF;
                         GET (FILE, X);
                         COL_COUNT := COL_COUNT + 1;
                         IF X /= CONTENTS (I) THEN
                              FAILED("FROM CHECK_FILE: " &
                                     "FILE DOES NOT CONTAIN CORRECT " &
                                     "OUTPUT - EXPECTED " & CONTENTS(I) 
                                     & " - GOT " & X);
                              RAISE STOP_PROCESSING;
                         END IF;
               END CASE;
          EXCEPTION
               WHEN STOP_PROCESSING =>
                    COMMENT ("FROM CHECK_FILE: " &
                             "LAST CHARACTER IN FOLLOWING STRING " &
                             "REVEALED ERROR: " & CONTENTS (1 .. I));
                    EXIT;
          END;
 
     END LOOP;

EXCEPTION
     WHEN STATUS_ERROR =>
          FAILED ("FROM CHECK_FILE: " &
                  "STATUS_ERROR RAISED - FILE CHECKING INCOMPLETE");
     WHEN MODE_ERROR =>
          FAILED ("FROM CHECK_FILE: " &
                  "MODE_ERROR RAISED - FILE CHECKING INCOMPLETE");
     WHEN NAME_ERROR =>
          FAILED ("FROM CHECK_FILE: " &
                  "NAME_ERROR RAISED - FILE CHECKING INCOMPLETE");
     WHEN USE_ERROR =>
          FAILED ("FROM CHECK_FILE: " &
                  "USE_ERROR RAISED - FILE CHECKING INCOMPLETE");
     WHEN DEVICE_ERROR =>
          FAILED ("FROM CHECK_FILE: " &
                  "DEVICE_ERROR RAISED - FILE CHECKING INCOMPLETE");
     WHEN END_ERROR =>
          FAILED ("FROM CHECK_FILE: " &
                  "END_ERROR RAISED - FILE CHECKING INCOMPLETE");
     WHEN DATA_ERROR =>
          FAILED ("FROM CHECK_FILE: " &
                  "DATA_ERROR RAISED - FILE CHECKING INCOMPLETE");
     WHEN LAYOUT_ERROR =>
          FAILED ("FROM CHECK_FILE: " &
                  "LAYOUT_ERROR RAISED - FILE CHECKING INCOMPLETE");
     WHEN OTHERS =>
          FAILED ("FROM CHECK_FILE: " &
                  "SOME EXCEPTION RAISED - FILE CHECKING INCOMPLETE");

END CHECK_FILE;
