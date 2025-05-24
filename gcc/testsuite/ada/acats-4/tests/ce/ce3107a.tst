-- CE3107A.TST

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

-- HISTORY:
--     DLD 08/10/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/29/85
--     DWC 08/17/87  SPLIT OUT CASES WHICH DEPEND ON A TEXT FILE
--                   BEING CREATED OR SUCCESSFULLY OPENED.  PLACED
--                   CASES INTO CE3107B.ADA.
--     PWB 03/07/97  ADDED CHECK FOR FILE SUPPORT.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3107A IS

     TEST_FILE_ZERO : FILE_TYPE;
     TEST_FILE_ONE : FILE_TYPE;
     TEST_FILE_TWO : FILE_TYPE;
     TEST_FILE_THREE : FILE_TYPE;
     VAL : BOOLEAN;

     INCOMPLETE : EXCEPTION;

BEGIN

     TEST("CE3107A", "CHECK THAT IS_OPEN RETURNS THE PROPER " &
                     "VALUES FOR UNOPENED FILES OF TYPE TEXT_IO");

-- FIRST TEST WHETHER IMPLEMENTATION SUPPORTS TEXT FILES AT ALL

     BEGIN
          TEXT_IO.CREATE ( TEST_FILE_ZERO,
                          TEXT_IO.OUT_FILE,
                          REPORT.LEGAL_FILE_NAME );
     EXCEPTION
          WHEN TEXT_IO.USE_ERROR | TEXT_IO.NAME_ERROR =>
               REPORT.NOT_APPLICABLE
                    ( "TEXT FILES NOT SUPPORTED -- CREATE OUT-FILE" );
               RAISE INCOMPLETE;
     END;
     TEXT_IO.DELETE ( TEST_FILE_ZERO );

-- WHEN FILE IS DECLARED BUT NOT OPEN

     VAL := TRUE;
     VAL := IS_OPEN(TEST_FILE_ONE);
     IF VAL = TRUE THEN
          FAILED("FILE NOT OPEN BUT IS_OPEN RETURNS TRUE");
     END IF;

-- FOLLOWING UNSUCCESSFUL CREATE

     BEGIN
          VAL := TRUE;
          CREATE(TEST_FILE_TWO, OUT_FILE,
                 "$ILLEGAL_EXTERNAL_FILE_NAME1");
          FAILED("NAME_ERROR NOT RAISED - UNSUCCESSFUL CREATE");
     EXCEPTION
          WHEN NAME_ERROR =>
               VAL := IS_OPEN(TEST_FILE_TWO);
               IF VAL = TRUE THEN
                    FAILED("IS_OPEN GIVES TRUE AFTER AN " &
                           "UNSUCCESSFUL CREATE");
               END IF;
     END;

-- FOLLOWING UNSUCCESSFUL OPEN

     BEGIN
          VAL := FALSE;
          OPEN(TEST_FILE_TWO, IN_FILE, LEGAL_FILE_NAME);
          FAILED("NAME_ERROR NOT RAISED - " &
                 "UNSUCCESSFUL OPEN");
     EXCEPTION
          WHEN NAME_ERROR =>
               VAL := IS_OPEN(TEST_FILE_TWO);
               IF VAL = TRUE THEN
                    FAILED("IS_OPEN GIVES TRUE - " &
                           "UNSUCCESSFUL OPEN");
               END IF;
     END;

-- CLOSE FILE WHILE NOT OPEN

     BEGIN
          VAL := TRUE;
          CLOSE(TEST_FILE_THREE);  -- STATUS ERROR
          FAILED("STATUS_ERROR NOT RAISED - UNSUCCESSFUL CLOSE");
     EXCEPTION
          WHEN OTHERS =>
               VAL := IS_OPEN(TEST_FILE_THREE);
               IF VAL = TRUE THEN
                    FAILED("IS_OPEN GIVES TRUE - UNSUCCESSFUL  " &
                           "CLOSE");
               END IF;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          NULL;
          REPORT.RESULT;
     WHEN OTHERS =>
          REPORT.FAILED ( "UNEXPECTED EXCEPTION" );
          REPORT.RESULT;
END CE3107A;
