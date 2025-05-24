-- CE3704F.ADA

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
--     CHECK THAT INTEGER_IO GET DOES NOT ALLOW EMBEDDED BLANKS OR
--     CONSECUTIVE UNDERSCORES TO BE INPUT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/04/82
--     VKG 01/14/83
--     CPP 07/30/84
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/10/87  REMOVED UNNECESSARY CODE, CORRECTED EXCEPTION
--                   HANDLING, AND ADDED MORE CHECKS OF THE VALUES
--                   OF CHARACTERS READ.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3704F IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3704F", "INTEGER_IO GET DOES NOT ALLOW EMBEDDED " &
                      "BLANKS OR CONSECUTIVE UNDERSCORES");

     DECLARE
          FT : FILE_TYPE;
          X : INTEGER;
          PACKAGE IIO IS NEW INTEGER_IO (INTEGER);
          USE IIO;
          CH : CHARACTER;
          P : POSITIVE;
     BEGIN

-- CREATE AND INITIALIZE FILE

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

          PUT (FT, "12_345");
          NEW_LINE (FT);
          PUT (FT, "12 345");
          NEW_LINE (FT);
          PUT (FT, "1__345");
          NEW_LINE (FT);
          PUT (FT, "-56");
          NEW_LINE (FT);
          PUT (FT, "10E0");
          NEW_LINE (FT);
          PUT (FT, "10E-2X");
          NEW_LINE (FT);
          PUT (FT, "4E1__2");
          NEW_LINE (FT);
          PUT (FT, "1 0#99#");
          NEW_LINE (FT);
          PUT (FT, "1__0#99#");
          NEW_LINE (FT);
          PUT (FT, "10#9_9#");
          NEW_LINE (FT);
          PUT (FT, "10#9__9#");
          NEW_LINE (FT);
          PUT (FT, "10#9 9#");
          NEW_LINE (FT);
          PUT (FT, "16#E#E1");
          NEW_LINE (FT);
          PUT (FT, "2#110#E1_1");
          NEW_LINE (FT);
          PUT (FT, "2#110#E1__1");
          CLOSE(FT);

-- BEGIN TEST

          BEGIN
               OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; " &
                                    "TEXT OPEN WITH IN_FILE " &
                                    "MODE");
                    RAISE INCOMPLETE;
          END;

          GET (FT, X);
          IF X /= 12345 THEN
               FAILED ("GET WITH UNDERSCORE INCORRECT - (1)");
          END IF;

          SKIP_LINE (FT);

          BEGIN
               GET (FT, X, 6);
               FAILED ("DATA_ERROR NOT RAISED - (2)");
          EXCEPTION
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (2)");
          END;

          SKIP_LINE (FT);

          BEGIN
               GET (FT, X);
               FAILED ("DATA_ERROR NOT RAISED - (3)");
          EXCEPTION
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (3)");
          END;

          IF END_OF_LINE (FT) THEN
               FAILED ("GET STOPPED AT END OF LINE - (3)");
          ELSE
               GET (FT, CH);
               IF CH /= '_' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION - " &
                            "(3): CHAR IS " & CH);
               END IF;
               GET (FT, CH);
               IF CH /= '3' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION - " &
                            "(3.5): CHAR IS " & CH);
               END IF;
          END IF;

          SKIP_LINE (FT);
          GET (FT, X);
          IF X /= (-56) THEN
               FAILED ("GET WITH GOOD CASE INCORRECT - (4)");
          END IF;

          SKIP_LINE (FT);
          GET (FT, X, 4);
          IF X /= 10 THEN
               FAILED ("GET WITH ZERO EXPONENT INCORRECT - (5)");
          END IF;

          SKIP_LINE (FT);

          BEGIN
               GET (FT, X);
               FAILED ("DATA_ERROR NOT RAISED - (6)");
          EXCEPTION
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (6)");
          END;

          IF END_OF_LINE (FT) THEN
               FAILED ("GET STOPPED AT END OF LINE - (6)");
          ELSE
               GET (FT, CH);
               IF CH /= 'X' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION - " &
                            "(6): CHAR IS " & CH);
               END IF;
          END IF;

          SKIP_LINE (FT);

          BEGIN
               GET (FT, X);
               FAILED ("DATA_ERROR NOT RAISED - (7)");
          EXCEPTION
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (7)");
          END;

          IF END_OF_LINE (FT) THEN
               FAILED ("GET STOPPED AT END OF LINE - (7)");
          ELSE
               GET (FT, CH);
               IF CH /= '_' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION - " &
                            "(7): CHAR IS " & CH);
               END IF;
               GET (FT, CH);
               IF CH /= '2' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION - " &
                            "(7.5): CHAR IS " & CH);
               END IF;
          END IF;

          SKIP_LINE (FT);

          BEGIN
               GET (FT, X, 7);
               FAILED ("DATA_ERROR NOT RAISED - (8)");
          EXCEPTION
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (8)");
          END;

          SKIP_LINE (FT);

          BEGIN
               GET (FT, X);
               FAILED ("DATA_ERROR NOT RAISED - (9)");
          EXCEPTION
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (9)");
          END;

          IF END_OF_LINE (FT) THEN
               FAILED ("GET STOPPED AT END OF LINE - (9)");
          ELSE
               GET (FT, CH);
               IF CH /= '_' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION " &
                            "- (9): CHAR IS " & CH);
               END IF;
               GET (FT, CH);
               IF CH /= '0' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION " &
                            "- (9.5): CHAR IS " & CH);
               END IF;
          END IF;

          SKIP_LINE (FT);
          GET (FT, X);
          IF X /= 99 THEN
               FAILED ("GET WITH UNDERSCORE IN " &
                       "BASED LITERAL INCORRECT - (10)");
          END IF;

          SKIP_LINE (FT);

          BEGIN
               GET (FT, X);
               FAILED ("DATA_ERROR NOT RAISED - (11)");
          EXCEPTION
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (11)");
          END;

          IF END_OF_LINE (FT) THEN
               FAILED ("GET STOPPED AT END OF LINE - (11)");
          ELSE
               GET (FT, CH);
               IF CH /= '_' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION - " &
                            "(11): CHAR IS " & CH);
               END IF;
               GET (FT, CH);
               IF CH /= '9' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION - " &
                            "(11.5): CHAR IS " & CH);
               END IF;
          END IF;

          SKIP_LINE (FT);

          BEGIN
               GET (FT, X, 6);
               FAILED ("DATA_ERROR NOT RAISED - (12)");
          EXCEPTION
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (12)");
          END;

          SKIP_LINE (FT);
          GET (FT, X, 7);
          IF X /= 224 THEN
               FAILED ("GET WITH GOOD CASE OF " &
                       "BASED LITERAL INCORRECT - (13)");
          END IF;

          SKIP_LINE (FT);
          GET (FT, X, 10);
          IF X /= (6 * 2 ** 11) THEN
               FAILED ("GET WITH UNDERSCORE IN EXPONENT" &
                       "OF BASED LITERAL INCORRECT - (14)");
          END IF;

          SKIP_LINE (FT);

          BEGIN
               GET (FT, X);
               FAILED ("DATA_ERROR NOT RAISED - (15)");
          EXCEPTION
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - (15)");
          END;

          IF END_OF_LINE (FT) THEN
               FAILED ("GET STOPPED AT END OF LINE - (15)");
          ELSE
               GET (FT, CH);
               IF CH /= '_' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION - " &
                            "(15): CHAR IS " & CH);
               END IF;
               GET (FT, CH);
               IF CH /= '1' THEN
                    FAILED ("GET STOPPED AT WRONG POSITION - " &
                            "(15.5): CHAR IS " & CH);
               END IF;
          END IF;

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

END CE3704F;
