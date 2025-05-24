-- CE3705E.ADA

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
--     CHECK THAT DATA_ERROR, NOT END_ERROR, IS RAISED WHEN FEWER THAN
--     WIDTH CHARACTERS REMAIN IN THE FILE, AND THE REMAINING CHARACTERS
--     SATISFY THE SYNTAX FOR A REAL LITERAL.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 07/20/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3705E IS

     PACKAGE IIO IS NEW INTEGER_IO (INTEGER);
     USE IIO;

     FILE : FILE_TYPE;
     ITEM : INTEGER;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3705E", "CHECK THAT DATA_ERROR, NOT END_ERROR, IS " &
                      "RAISED WHEN FEWER THAN WIDTH CHARACTERS " &
                      "REMAIN IN THE FILE, AND THE REMAINING " &
                      "CHARACTERS SATISFY THE SYNTAX FOR A REAL " &
                      "LITERAL");

     BEGIN

          BEGIN
               CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                                    "WITH MODE OUT_FILE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE " &
                                    "WITH MODE OUT_FILE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          PUT (FILE, "16#FFF#");
          NEW_LINE (FILE);
          PUT (FILE, "3.14159_26");

          CLOSE (FILE);

          BEGIN
               OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON OPEN " &
                                    "WITH MODE IN_FILE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON OPEN");
                    RAISE INCOMPLETE;
          END;

          GET (FILE, ITEM);
          IF ITEM /= 4095 THEN
               FAILED ("INCORRECT VALUE READ");
          END IF;

          BEGIN
               GET (FILE, ITEM, WIDTH => 11);
               FAILED ("DATA_ERROR NOT RAISED");
          EXCEPTION
               WHEN END_ERROR =>
                    FAILED ("END_ERROR INSTEAD OF DATA_ERROR RAISED");
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON GET");
          END;

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

END CE3705E;
