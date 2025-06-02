-- CE3704M.ADA

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
--     CHECK THAT GET FOR INTEGER_IO RAISES DATA_ERROR WHEN
--     THE INPUT CONTAINS
--
--     (1)  INTEGER_IO DECIMAL POINT
--     (2)  INTEGER_IO LEADING OR TRAILING UNDERSCORES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     VKG 02/10/83
--     CPP 07/30/84
--     EG  05/22/85
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/11/87  REMOVED UNNECESSARY CODE, CORRECTED
--                   EXCEPTION HANDLING, AND ADDED CASES WHICH
--                   CHECK GET AT THE END_OF_FILE.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3704M IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3704M", "CHECK THAT DATA_ERROR IS RAISED FOR " &
                      "INTEGER_IO WHEN A DECIMAL POINT, OR " &
                      "LEADING OR TRAILING UNDERSCORES " &
                      "ARE DETECTED");

     DECLARE
          FT : FILE_TYPE;
          CH : CHARACTER;
     BEGIN

          BEGIN
               CREATE (FT, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          PUT (FT, "3.14152");
          NEW_LINE (FT);
          PUT (FT, "2.15");
          NEW_LINE (FT);
          PUT (FT, "_312");
          NEW_LINE (FT);
          PUT (FT, "-312_");

          CLOSE (FT);

          DECLARE
               PACKAGE INT_IO IS NEW INTEGER_IO(INTEGER);
               USE INT_IO;
               X : INTEGER := 402;
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
                    GET (FT, X, 3);
                    FAILED ("DATA_ERROR NOT RAISED - (1)");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - (1)");
               END;

               IF END_OF_LINE (FT) THEN
                    FAILED ("GET STOPPED AT END OF LINE - (1)");
               ELSE
                    GET (FT, CH);
                    IF CH /= '4' THEN
                         FAILED ("GET STOPPED AT WRONG " &
                                 "POSITION - (1): CHAR IS " & CH);
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    IF X /= 2 THEN
                         FAILED ("WRONG VALUE READ - (2)");
                    END IF;
               EXCEPTION
                    WHEN DATA_ERROR =>
                         FAILED ("DATA_ERROR RAISED - (2)");
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - (2)");
               END;

               IF END_OF_LINE (FT) THEN
                    FAILED ("GET STOPPED AT END OF LINE - (2)");
               ELSE
                    GET (FT, CH);
                    IF CH /= '.' THEN
                         FAILED ("GET STOPPED AT WRONG " &
                                 "POSITION - (2): CHAR IS " & CH);
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - (3)");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - (3)");
               END;

               IF END_OF_LINE (FT) THEN
                    FAILED ("GET STOPPED AT END OF LINE - (3)");
               ELSE
                    GET (FT, CH);
                    IF CH /= '_' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION " &
                                 "- (3): CHAR IS " & CH);
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - (4)");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - (4)");
               END;

               IF NOT END_OF_LINE (FT) THEN
                    FAILED ("END_OF_LINE NOT TRUE AFTER (4)");
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

END CE3704M;
