-- CE3107B.ADA

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
--     CHECK THAT IS_OPEN RETURNS THE PROPER VALUES FOR FILES OF
--     TYPE TEXT_IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION WITH OUT_FILE MODE FOR TEXT FILES.

-- HISTORY:
--     DWC 08/17/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3107B IS

     TEST_FILE_ONE : FILE_TYPE;
     TEST_FILE_TWO : FILE_TYPE;
     VAL : BOOLEAN;

     INCOMPLETE : EXCEPTION;

BEGIN

     TEST("CE3107B", "CHECK THAT IS_OPEN RETURNS THE " &
                     "PROPER VALUES FOR FILES OF TYPE TEXT_IO");

-- FOLLOWING A CREATE

     BEGIN
          VAL := FALSE;
          CREATE(TEST_FILE_ONE, OUT_FILE, LEGAL_FILE_NAME);
          VAL := IS_OPEN(TEST_FILE_ONE);
          IF VAL = FALSE THEN
               FAILED("IS_OPEN RETURNS FALSE AFTER CREATE");
          END IF;
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE");
               RAISE INCOMPLETE;
     END;

-- FOLLOWING CLOSE

     VAL := TRUE;
     IF IS_OPEN(TEST_FILE_ONE) = TRUE THEN
          CLOSE(TEST_FILE_ONE);
     END IF;
     VAL := IS_OPEN(TEST_FILE_ONE);
     IF VAL = TRUE THEN
          FAILED("IS_OPEN RETURNS TRUE AFTER CLOSE");
     END IF;

-- FOLLOWING OPEN

     BEGIN
          VAL := FALSE;
          BEGIN
               OPEN (TEST_FILE_TWO, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    IF IS_OPEN (TEST_FILE_TWO) /= FALSE THEN
                         FAILED ("FILE OPEN AFTER USE_ERROR " &
                                 "DURING OPEN");
                    END IF;
                    RAISE INCOMPLETE;
          END;
          VAL := IS_OPEN(TEST_FILE_TWO);
          IF VAL = FALSE THEN
               FAILED("IS_OPEN RETURNS FALSE AFTER OPEN");
          END IF;

-- AFTER RESET

          BEGIN
               VAL := FALSE;
               RESET(TEST_FILE_TWO);
               VAL := IS_OPEN(TEST_FILE_TWO);
               IF VAL = FALSE THEN
                    FAILED("IS_OPEN RETURNS FALSE AFTER RESET");
               END IF;
          EXCEPTION
               WHEN USE_ERROR =>
                    COMMENT("IMPLEMENTATION DOES NOT SUPPORT RESET");
          END;
     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

-- AFTER DELETE

     BEGIN
          VAL := TRUE;
          DELETE(TEST_FILE_TWO);
          VAL := IS_OPEN(TEST_FILE_TWO);
          IF VAL = TRUE THEN
               FAILED("IS_OPEN RETURNS TRUE AFTER DELETE");
          END IF;
     EXCEPTION
          WHEN USE_ERROR =>
               IF IS_OPEN (TEST_FILE_TWO) /= FALSE THEN
                    FAILED ("FILE OPEN AFTER USE_ERROR " &
                            "DURING DELETE");
               END IF;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3107B;
