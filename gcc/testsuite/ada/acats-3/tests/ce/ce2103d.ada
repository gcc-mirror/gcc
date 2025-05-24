-- CE2103D.ADA

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
--     TYPE DIRECT_IO.

--          B) OPENED FILES

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTAIONS WHICH SUPPORT
--     CREATION OF EXTERNAL FILES FOR DIRECT FILES.

-- HISTORY:
--     SPW 08/13/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2103D IS

     PACKAGE DIR_IO IS NEW DIRECT_IO(CHARACTER);
     USE DIR_IO;
     INCOMPLETE : EXCEPTION;
     TEST_FILE_ONE : DIR_IO.FILE_TYPE;
     VAL : BOOLEAN;

BEGIN

     TEST ("CE2103D", "CHECK THAT IS_OPEN RETURNS THE PROPER " &
                      "VALUES FOR FILES OF TYPE DIRECT_IO");

-- FOLLOWING A CREATE

     VAL := FALSE;

     BEGIN
          CREATE (TEST_FILE_ONE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE WITH " &
                               "OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE WITH " &
                               "OUT_FILE MODE");
               RAISE INCOMPLETE;
     END;

     VAL := IS_OPEN (TEST_FILE_ONE);
     IF VAL = FALSE THEN
          FAILED ("IS_OPEN RETURNS FALSE AFTER CREATE");
     END IF;

-- FOLLOWING CLOSE

     VAL := TRUE;
     CLOSE (TEST_FILE_ONE);
     VAL := IS_OPEN (TEST_FILE_ONE);
     IF VAL = TRUE THEN
          FAILED ("IS_OPEN RETURNS TRUE AFTER CLOSE");
     END IF;

-- FOLLOWING OPEN

     VAL := FALSE;

     BEGIN
          OPEN (TEST_FILE_ONE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               IF IS_OPEN (TEST_FILE_ONE) /= FALSE THEN
                    FAILED ("IS_OPEN GIVES TRUE ON " &
                            "UNSUCCESSFUL OPEN");
               END IF;
               RAISE INCOMPLETE;
     END;

     VAL := IS_OPEN (TEST_FILE_ONE);
     IF VAL = FALSE THEN
          FAILED ("IS_OPEN RETURNS FALSE AFTER OPEN");
     END IF;

-- AFTER RESET

     VAL := FALSE;

     BEGIN
          RESET (TEST_FILE_ONE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     VAL := IS_OPEN (TEST_FILE_ONE);
     IF VAL = FALSE THEN
          FAILED ("IS_OPEN RETURNS FALSE AFTER RESET");
     END IF;

-- AFTER DELETE

     VAL := TRUE;

     BEGIN
          DELETE (TEST_FILE_ONE);
     EXCEPTION
          WHEN USE_ERROR =>
               IF IS_OPEN (TEST_FILE_ONE) /= FALSE THEN
                    FAILED ("IS_OPEN GIVES TRUE ON UNSUCCESSFUL " &
                            "DELETE");
               END IF;
               RAISE INCOMPLETE;
     END;

     VAL := IS_OPEN (TEST_FILE_ONE);
     IF VAL = TRUE THEN
          FAILED ("IS_OPEN RETURNS TRUE AFTER DELETE");
     END IF;

     RESULT;

EXCEPTION

     WHEN INCOMPLETE =>
          RESULT;

END CE2103D;
