-- CE3413C.ADA

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
--     CHECK THAT PAGE OPERATES ON THE CURRENT DEFAULT OUTPUT FILE WHEN
--     NO FILE IS SPECIFIED.  CHECK THAT PAGE CAN OPERATE ON FILES OF
--     MODES IN_FILE AND OUT_FILE, INCLUDING THE CURRENT DEFAULT
--     INPUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/29/82
--     JBG 08/30/83
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/04/87  REMOVED DEPENDENCE ON RESET, CORRECTED EXCEPTION
--                   HANDLING, AND CHECKED FOR USE_ERROR ON DELETE.


WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3413C IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3413C", "CHECK THAT PAGE OPERATES ON DEFAULT IN_FILE " &
                      "AND OUT_FILE FILES");

     DECLARE
          F1, F2 : FILE_TYPE;
          C : POSITIVE_COUNT;
          X : CHARACTER;
     BEGIN

          BEGIN
               CREATE (F1, OUT_FILE, LEGAL_FILE_NAME);
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
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON " &
                            "TEXT CREATE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               CREATE (F2, OUT_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                                    "FOR TEMPORARY FILES WITH " &
                                    "OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          SET_OUTPUT (F2);

          IF PAGE (F2) /= 1 AND PAGE (STANDARD_OUTPUT) /= 1 THEN
               FAILED ("PAGE INCORRECT SUBTEST - 1");
          END IF;

          FOR I IN 1 .. 3 LOOP
               PUT (F1, "STRING");
               NEW_PAGE (F1);
          END LOOP;

          IF PAGE (F1) /= 4 THEN
               FAILED ("PAGE INCORRECT SUBTEST - 2");
          END IF;

          SET_LINE_LENGTH (F2, 3);
          SET_PAGE_LENGTH (F2, 1);
          PUT ("OUTPUT STRING");
          IF PAGE /= PAGE(F2) THEN
               FAILED ("PAGE INCORRECT SUBTEST - 3");
          END IF;

          CLOSE (F1);

          BEGIN
               OPEN (F1, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN " &
                                    "IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          SET_INPUT (F1);

          IF PAGE (F1) /= 1 THEN
               FAILED ("PAGE INCORRECT SUBTEST - 4");
          END IF;

          SKIP_PAGE(F1);
          SKIP_PAGE(F1);
          IF PAGE (F1) /= PAGE (CURRENT_INPUT) THEN
               FAILED ("PAGE INCORRECT SUBTEST - 5");
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

END CE3413C;
