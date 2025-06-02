-- CE2103A.TST

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
--     TYPE SEQUENTIAL_IO.

--          A) UNOPENED FILES

-- HISTORY:
--     DLD 08/10/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  06/03/85
--     SPW 08/10/87  SPLIT CASE FOR OPENED FILES INTO CE2103C.ADA.
--     PWB 03/07/97  ADDED CHECK FOR FILE SUPPORT.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2103A IS

     PACKAGE SEQ_IO IS NEW SEQUENTIAL_IO(CHARACTER);
          USE SEQ_IO;

     TEST_FILE_ZERO : SEQ_IO.FILE_TYPE;
     TEST_FILE_ONE : SEQ_IO.FILE_TYPE;
     TEST_FILE_TWO : SEQ_IO.FILE_TYPE;
     TEST_FILE_THREE : SEQ_IO.FILE_TYPE;
     TEST_FILE_FOUR : SEQ_IO.FILE_TYPE;
     VAL : BOOLEAN;

     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE2103A", "CHECK THAT IS_OPEN RETURNS THE PROPER " &
                      "VALUES FOR UNOPENED FILES OF TYPE " &
                      "SEQUENTIAL_IO");

-- FIRST TEST WHETHER IMPLEMENTATION SUPPORTS SEQUENTIAL FILES AT ALL

     BEGIN
          SEQ_IO.CREATE ( TEST_FILE_ZERO,
                          SEQ_IO.OUT_FILE,
                          REPORT.LEGAL_FILE_NAME );
     EXCEPTION
          WHEN SEQ_IO.USE_ERROR | SEQ_IO.NAME_ERROR =>
               REPORT.NOT_APPLICABLE
                    ( "SEQUENTIAL FILES NOT SUPPORTED -- CREATE OUT-FILE" );
               RAISE INCOMPLETE;
     END;
     SEQ_IO.DELETE ( TEST_FILE_ZERO );

-- WHEN FILE IS DECLARED BUT NOT OPEN

     BEGIN
          VAL := TRUE;
          VAL := IS_OPEN (TEST_FILE_ONE);
          IF VAL = TRUE THEN
               FAILED ("FILE NOT OPEN BUT IS_OPEN RETURNS TRUE");
          END IF;
     END;

-- FOLLOWING UNSUCCESSFUL CREATE

     BEGIN
          VAL := TRUE;
          CREATE (TEST_FILE_TWO, OUT_FILE,
                  "$ILLEGAL_EXTERNAL_FILE_NAME1");
          FAILED ("NAME_ERROR NOT RAISED - UNSUCCESSFUL CREATE");
     EXCEPTION
          WHEN NAME_ERROR =>
               VAL := IS_OPEN (TEST_FILE_TWO);
               IF VAL = TRUE THEN
                    FAILED ("IS_OPEN GIVES TRUE AFTER AN " &
                            "UNSUCCESSFUL CREATE");
               END IF;
     END;

-- FOLLOWING UNSUCCESSFUL OPEN

     BEGIN
          VAL := TRUE;
          OPEN (TEST_FILE_THREE, IN_FILE,
                "$ILLEGAL_EXTERNAL_FILE_NAME1");
          FAILED ("NAME_ERROR NOT RAISED - UNSUCCESSFUL OPEN");
     EXCEPTION
          WHEN NAME_ERROR =>
               VAL := IS_OPEN (TEST_FILE_THREE);
               IF VAL = TRUE THEN
                    FAILED ("IS_OPEN GIVES TRUE - UNSUCCESSFUL OPEN");
               END IF;
     END;

-- FOLLOWING CLOSING FILE THAT IS NOT OPEN

     BEGIN
          VAL := TRUE;
          CLOSE (TEST_FILE_FOUR);
          FAILED ("STATUS ERROR NOT RAISED WHEN " &
                  "ATTEMPTING TO CLOSE AN UNOPENED FILE");
     EXCEPTION
          WHEN STATUS_ERROR =>
               VAL := IS_OPEN (TEST_FILE_FOUR);
               IF VAL = TRUE THEN
                    FAILED ("IS_OPEN GIVES TRUE AFTER ATTEMPTING " &
                            "TO CLOSE AN UNOPENED FILE");
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
END CE2103A;
