-- CE3704D.ADA

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
--     CHECK THAT INTEGER_IO GET READS AT MOST WIDTH CHARACTERS
--     OR UP TO THE NEXT TERMINATOR; INCLUDING LEADING BLANKS
--     AND HORIZONTAL TABULATION CHARACTERS, WHEN WIDTH IS
--     NONZERO.

--     CHECK THAT INPUT TERMINATES WHEN A LINE TERMINATOR IS
--     ENCOUNTERED AND THAT DATA_ERROR IS RAISED IF THE DATA
--     READ IS INVALID.

-- APPLICABILITY CRITERIA:

--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/04/82
--     VKG 01/12/83
--     SPS 02/08/83
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  ADDED CASES FOR TABS, REMOVED UNNECESSARY
--                   CODE, AND CHECKED FOR USE_ERROR ON DELETE.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3704D IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3704D", "CHECK THAT INTEGER_IO GET READS AT MOST " &
                      "WIDTH CHARACTERS OR UP TO THE NEXT " &
                      "TERMINATOR; INCLUDING LEADING BLANKS AND " &
                      "HORIZONTAL TABULATION CHARACTERS, WHEN WIDTH " &
                      "IS NONZERO");

     DECLARE
          FT : FILE_TYPE;
          X : INTEGER;
          PACKAGE IIO IS NEW INTEGER_IO (INTEGER);
          USE IIO;
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

          PUT (FT, "  123");
          NEW_LINE (FT);
          PUT (FT, "-5678");
          NEW_LINE (FT);
          PUT (FT, "  ");
          NEW_PAGE (FT);
          PUT (FT, ASCII.HT & "9");
          NEW_PAGE (FT);

          CLOSE (FT);

          BEGIN
               OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

-- BEGIN TEST

          GET (FT, X, 5);
          IF X /= IDENT_INT (123) THEN
               FAILED ("WIDTH CHARACTERS NOT READ - 1");
          ELSE
               BEGIN
                    GET (FT, X, 2);
                    FAILED ("DATA_ERROR NOT RAISED - 1");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED -1");
               END;
               SKIP_LINE (FT);
               GET (FT, X, 6);
               IF X /= IDENT_INT (-5678) THEN
                    FAILED ("GET WITH WIDTH " &
                            "INCORRECT - 2");
               ELSE
                    BEGIN
                         GET (FT, X, 2);
                         FAILED ("DATA_ERROR NOT RAISED - 2");
                    EXCEPTION
                         WHEN DATA_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION RAISED - 2");
                    END;
                    SKIP_LINE(FT);
                    BEGIN
                         GET (FT, X, 2);
                         FAILED ("DATA_ERROR NOT RAISED - 3");
                    EXCEPTION
                         WHEN DATA_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION RAISED - 3");
                    END;
                    SKIP_LINE(FT);
                    GET (FT, X, 2);
                    IF X /= IDENT_INT (9) THEN
                         FAILED ("GET WITH WIDTH " &
                                 "INCORRECT - 3");
                    END IF;
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

END CE3704D;
