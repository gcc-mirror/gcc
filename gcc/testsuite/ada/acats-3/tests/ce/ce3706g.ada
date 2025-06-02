-- CE3706G.ADA

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
--     CHECK THAT INTEGER_IO PUT USES THE MINIMUM FIELD REQUIRED IF
--     WIDTH IS TOO SMALL AND THE LINE LENGTH IS SUFFICIENTLY LARGE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 10/05/82
--     JLH 09/17/87  COMPLETELY REVISED TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3706G IS

BEGIN

     TEST ("CE3706G", "CHECK THAT INTEGER_IO PUT USES THE MINIMUM " &
                      "FIELD REQUIRED IF WIDTH IS TOO SMALL AND THE " &
                      "LINE LENGTH IS SUFFICIENTLY LARGE");

     DECLARE
          FILE : FILE_TYPE;
          PACKAGE IIO IS NEW INTEGER_IO (INTEGER);
          USE IIO;
          INCOMPLETE : EXCEPTION;
          NUM : INTEGER := 12345;
          CH : CHARACTER;

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
          END;

          PUT (FILE, NUM, WIDTH => 3);
          TEXT_IO.PUT (FILE, ' ');

          CLOSE (FILE);

          BEGIN
               OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          GET (FILE, NUM);
          GET (FILE, CH);
          IF CH /= ' ' AND COL(FILE) /= 7 THEN
               FAILED ("INTEGER_IO PUT DOES NOT USE MINIMUM FIELD " &
                       "REQUIRED WHEN WIDTH IS TOO SMALL");
          END IF;

          IF NUM /= 12345 THEN
               FAILED ("INCORREC VALUE READ");
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

END CE3706G;
