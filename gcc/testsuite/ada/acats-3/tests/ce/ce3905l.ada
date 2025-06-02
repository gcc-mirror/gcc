-- CE3905L.ADA

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
--     CHECK THAT DATA_ERROR IS RAISED, BY GET, WHEN THE INPUT CONTAINS
--
--     1. EMBEDDED BLANKS.
--     2. SINGLY QUOTED CHARACTER LITERALS.
--     3. IDENTIFIERS BEGINNING WITH NON LETTERS.
--     4. IDENTIFIERS CONTAINING SPECIAL CHARACTERS.
--     5. CONSECUTIVE UNDERSCORES.
--     6. LEADING OR TRAILING UNDERSCORES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     VKG 02/14/83
--     SPS 03/16/83
--     CPP 07/30/84
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/16/87  REMOVED UNNECESSARY CODE AND CORRECTED
--                   EXCEPTION HANDLING.

WITH TEXT_IO; USE TEXT_IO;
WITH REPORT; USE REPORT;

PROCEDURE CE3905L IS

     INCOMPLETE : EXCEPTION;

BEGIN
     TEST ("CE3905L", "CHECK GET FOR ENUMERATION_IO " &
                      "WITH LEXICAL ERRORS");
     DECLARE
          FT : FILE_TYPE;
     BEGIN

          BEGIN
               CREATE (FT, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          PUT (FT, "RED ISH");
          NEW_LINE (FT);
          PUT (FT, "'A ");
          NEW_LINE (FT);
          PUT (FT, "2REDISH");
          NEW_LINE (FT);
          PUT (FT, "BLUE$%ISH");
          NEW_LINE (FT);
          PUT (FT, "RED__ISH");
          NEW_LINE (FT);
          PUT (FT, "_YELLOWISH");
          NEW_LINE (FT);
          PUT  (FT, "GREENISH_");
          NEW_LINE (FT);

          CLOSE (FT);

          DECLARE
               TYPE COLOUR IS
                    ( GREYISH,
                      REDISH ,
                      BLUEISH,
                      YELLOWISH,
                      GREENISH, 'A');
               PACKAGE COLOUR_IO IS NEW ENUMERATION_IO(COLOUR);
               USE COLOUR_IO;
               X : COLOUR := GREYISH;
               CH : CHARACTER;
          BEGIN

               BEGIN
                    OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR RAISED; TEXT " &
                                         "OPEN WITH IN_FILE MODE");
                         RAISE INCOMPLETE;
               END;

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - 1");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= GREYISH THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 1");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 1");
               END;

               IF END_OF_LINE (FT) THEN
                    FAILED ("GET STOPPED AT END OF LINE - 1");
               ELSE
                    GET (FT, CH);
                    IF CH /= ' ' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION " &
                                 "- 1: CHAR IS " & CH);
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - 2");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= GREYISH THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 2");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 2");
               END;

               IF END_OF_LINE (FT) THEN
                    FAILED ("GET STOPPED AT END OF LINE - 2");
               ELSE
                    GET (FT, CH);
                    IF CH /= ' ' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION " &
                                 "- 2: CHAR IS " & CH);
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - 3");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= GREYISH THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 3");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 3");
               END;

               IF END_OF_LINE (FT) THEN
                    FAILED ("GET STOPPED AT END OF LINE - 3");
               ELSE
                    GET (FT, CH);
                    IF CH /= '2' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION " &
                                 "- 3: CHAR IS " & CH);
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - 4");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= GREYISH THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 4");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 4");
               END;

               IF END_OF_LINE (FT) THEN
                    FAILED ("GET STOPPED AT END OF LINE - 4");
               ELSE
                    GET (FT, CH);
                    IF CH /= '$' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION " &
                                 "- 4: CHAR IS " & CH);
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - 5");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= GREYISH THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 5");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 5");
               END;

               IF END_OF_LINE (FT) THEN
                    FAILED ("GET STOPPED AT END OF LINE - 5");
               ELSE
                    GET (FT, CH);
                    IF CH /= '_' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION " &
                                 "- 5: CHAR IS " & CH);
                    ELSE
                         GET (FT, CH);
                         IF CH /= 'I' THEN
                              FAILED ("ERROR READING DATA - 5");
                         END IF;
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - 6");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= GREYISH THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 6");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 6");
               END;

               IF END_OF_LINE (FT) THEN
                    FAILED ("GET STOPPED AT END OF LINE - 6");
               ELSE
                    GET (FT, CH);
                    IF CH /= '_' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION " &
                                 "- 6: CHAR IS " & CH);
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - 7");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= GREYISH THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 7");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 7");
               END;

               IF NOT END_OF_LINE (FT) THEN
                    BEGIN
                         GET (FT, X);
                         FAILED ("GET STOPPED AT WRONG  POSITION " &
                                 "- 7");
                    EXCEPTION
                         WHEN END_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION RAISED FOR " &
                                      "EMPTY FILE - 7");
                    END;
               END IF;
          END;

          BEGIN
               DELETE (FT);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3905L;
