-- CE3804H.ADA

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
--     CHECK THAT FIXED_IO GET WHEN SUPPLIED WITH A WIDTH PARAMETER
--     GREATER THAN ZERO READS ONLY THAT MANY CHARACTERS.  ALSO CHECK
--     THAT INPUT TERMINATES WHEN A LINE TERMINATOR IS ENCOUNTERED AND
--     THAT DATA_ERROR IS RAISED WHEN THE DATA IS INVALID.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     DWC 09/14/87  CREATED ORIGINAL TEST.
--     RJW 08/17/89  CHANGED THE VALUE '-3.525' TO '-3.625'.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3804H IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3804H", "CHECK THAT FIXED_IO GET WHEN SUPPLIED WITH " &
                      "A WIDTH PARAMETER GREATER THAN ZERO READS " &
                      "ONLY THAT MANY CHARACTERS.  ALSO CHECK THAT " &
                      "INPUT TERMINATES WHEN A LINE TERMINATOR IS " &
                      "ENCOUNTERED AND THAT DATA_ERROR IS RAISED " &
                      "WHEN THE DATA IS INVALID");

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

          PUT(FT, "3.259.5 8.52");
          NEW_LINE (FT);
          PUT (FT, "  ");
          NEW_LINE (FT);
          PUT (FT, ASCII.HT & "9.0");
          NEW_LINE (FT);
          PUT (FT, "-3.625");
          NEW_LINE (FT);

          CLOSE (FT);

-- BEGIN TEST

          DECLARE
               TYPE FIXED IS DELTA 0.001 RANGE -100.0 .. 100.0;
               PACKAGE FX_IO IS NEW FIXED_IO (FIXED);
               USE FX_IO;
               X : FIXED;

          BEGIN
               BEGIN
                    OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR RAISED; TEXT" &
                                         "OPEN WITH IN_FILE MODE");
                         RAISE INCOMPLETE;
               END;

               GET (FT, X, 4);
               IF X /= 3.25 THEN
                    FAILED ("WIDTH CHARACTERS NOT READ - FIXED - 1");
               ELSE
                    GET (FT, X, 3);
                    IF X /= 9.5 THEN
                         FAILED ("WIDTH CHARACTERS NOT READ - " &
                                 "FIXED 2");
                    ELSE
                         GET (FT, X, 4);
                         IF X /= 8.5 THEN
                              FAILED ("DIDN'T COUNT LEADING BLANKS " &
                                      "- FIXED");
                         ELSE
                              SKIP_LINE(FT);
                              BEGIN
                                   GET (FT, X, 2);
                                   FAILED ("DATA_ERROR NOT RAISED - " &
                                           "FIXED");
                              EXCEPTION
                                   WHEN DATA_ERROR =>
                                        NULL;
                                   WHEN OTHERS =>
                                        FAILED ("WRONG EXCEPTION RAISED"
                                             & " - FIXED");
                              END;

                              SKIP_LINE(FT);
                              GET (FT, X, 4);
                              IF X /= 9.0 THEN
                                   FAILED ("GET WITH WIDTH " &
                                           "INCORRECT");
                              END IF;

                              SKIP_LINE (FT);
                              GET (FT, X, 7);
                              IF X /= -3.625 THEN
                                   FAILED ("WIDTH CHARACTERS NOT " &
                                           "READ");
                              END IF;
                         END IF;
                    END IF;
               END IF;

               BEGIN
                   DELETE (FT);
               EXCEPTION
                   WHEN USE_ERROR =>
                        NULL;
               END;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3804H;
