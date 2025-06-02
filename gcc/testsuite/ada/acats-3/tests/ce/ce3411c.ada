-- CE3411C.ADA

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
--     CHECK THAT COL OPERATES ON THE CURRENT DEFAULT OUTPUT FILE WHEN
--     NO FILE IS SPECIFIED.  CHECK THAT COL CAN OPERATE ON FILES OF
--     MODES IN_FILE AND OUT_FILE, INCLUDING THE CURRENT DEFAULT
--     INPUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/29/82
--     JBG 01/31/83
--     JBG 08/30/83
--     JLH 09/02/87  REMOVED DEPENDENCE ON RESET, REMOVED UNNECESSARY
--                   CODE, AND CHECKED FOR USE_ERROR ON DELETE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3411C IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3411C", "CHECK THAT COL OPERATES ON DEFAULT IN_FILE AND "&
                      "OUT_FILE FILES");

     DECLARE
          F1, F2 : FILE_TYPE;
          C : POSITIVE_COUNT;
          X : CHARACTER;
     BEGIN
          IF COL /= COL (STANDARD_OUTPUT) THEN
               FAILED ("COL DEFAULT NOT STANDARD_OUTPUT");
          END IF;

          IF COL /= COL (STANDARD_INPUT) THEN
               FAILED ("COL DEFAULT NOT STANDARD_INPUT");
          END IF;

          IF COL /= COL (CURRENT_INPUT) THEN
               FAILED ("COL DEFAULT NOT CURRENT_INPUT");
          END IF;

          IF COL /= COL (CURRENT_OUTPUT) THEN
               FAILED ("COL DEFAULT NOT CURRENT_OUTPUT");
          END IF;

          BEGIN
               CREATE (F1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE WITH " &
                                    "OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          CREATE (F2, OUT_FILE);

          SET_OUTPUT (F2);

          PUT (F1, "STRING");
          IF COL (F1) /= 7 THEN
               FAILED ("COL INCORRECT SUBTEST 1");
          END IF;

          PUT (F2, "OUTPUT STRING");
          IF COL /= COL(F2) AND COL(F2) /= 14 THEN
               FAILED ("COL INCORRECT SUBTEST 2; WAS " &
                       COUNT'IMAGE(COL) & " VS. " &
                       COUNT'IMAGE(COL(F2)));
          END IF;

          CLOSE (F1);

          BEGIN
               OPEN (F1, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          SET_INPUT (F1);

          GET (F1, X);
          GET (F1, X);
          GET (F1, X);

          IF X /= 'R' THEN
               FAILED ("INCORRECT VALUE READ");
          END IF;

          IF COL (CURRENT_INPUT) /= 4 AND COL /= 4 THEN
               FAILED ("COL INCORRECT SUBTEST 3");
          END IF;

          BEGIN
               DELETE (F1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

          CLOSE (F2);

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3411C;
