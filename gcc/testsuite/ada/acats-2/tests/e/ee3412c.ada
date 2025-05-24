-- EE3412C.ADA

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
--     CHECK THAT LINE OPERATES ON THE CURRENT DEFAULT OUTPUT FILE WHEN
--     NO FILE IS SPECIFIED.  CHECK THAT LINE CAN OPERATE ON FILES OF
--     MODE IN_FILE AND OUT_FILE, INCLUDING THE CURRENT DEFAULT
--     INPUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- PASS/FAIL CRITERIA:
--     THIS TEST IS PASSED IF IT EXECUTES, PRINTS TENTATIVELY PASSED,
--     AND THE CONTENTS OF THE STANDARD OUTPUT FILE ARE CORRECT.

-- HISTORY:
--     SPS 09/29/82
--     JBG 08/30/83
--     JLH 09/02/87  REMOVED DEPENDENCE ON RESET, REMOVED UNNECESSARY
--                   CODE, CHECKED FOR USE_ERROR ON DELETE, AND RENAMED
--                   FROM CE3412C.ADA.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE EE3412C IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("EE3412C", "CHECK THAT LINE OPERATES ON DEFAULT IN_FILE " &
                      "AND OUT_FILE FILES");

     DECLARE
          F1, F2 : FILE_TYPE;
          C : POSITIVE_COUNT;
          X : CHARACTER;
          ITEM : STRING (1..6);
     BEGIN
          C := LINE (STANDARD_OUTPUT);
          NEW_LINE (STANDARD_OUTPUT);
          SPECIAL_ACTION ("ONE BLANK LINE SHOULD PRECEDE THIS COMMENT");
          IF LINE /= C+2 THEN
               FAILED ("DEFAULT FOR LINE NOT STANDARD_OUTPUT");
          END IF;

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
          END;

          CREATE (F2, OUT_FILE);

          SET_OUTPUT (F2);

          FOR I IN 1 .. 6 LOOP
               PUT (F1, "STRING");
               NEW_LINE (F1);
          END LOOP;
          IF LINE (F1) /= 7 THEN
               FAILED ("LINE INCORRECT SUBTEST 1");
          END IF;

          SET_LINE_LENGTH (3);
          PUT ("OUTPUT STRING");
          IF LINE /= LINE(F2) THEN
               FAILED ("LINE INCORRECT SUBTEST 2");
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

          GET (F1, ITEM);
          IF ITEM /= "STRING" THEN
               FAILED ("INCORRECT VALUE READ");
          END IF;

          SKIP_LINE(F1);
          SKIP_LINE(F1);
          SKIP_LINE(F1);
          IF LINE (CURRENT_INPUT) /= 4 AND LINE (F1) /= 4 THEN
               FAILED ("LINE INCORRECT SUBTEST 3");
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

END EE3412C;
