-- CE2104D.ADA

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
--     CHECK THAT THE NAME RETURNED BY NAME CAN BE USED IN A
--     SUBSEQUENT OPEN.

--          B) DIRECT FILES

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHOSE
--     ENVIRONMENT SUPPORTS CREATE/OPEN FOR THE GIVEN MODE.

-- HISTORY:
--     DLD 08/11/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/31/85
--     TBN 11/04/86  ADDED A RAISE INCOMPLETE STATEMENT WHEN FAILED IS
--                   CALLED FOR OPEN OR CREATE.
--     SPW 08/07/87  REMOVED UNNECESSARY CODE AND CORRECTED EXCEPTION
--                   HANDLING.

WITH DIRECT_IO;
WITH REPORT; USE REPORT;

PROCEDURE CE2104D IS

     PACKAGE DIR_IO IS NEW DIRECT_IO(INTEGER);
     USE DIR_IO;
     TYPE ACC_STR IS ACCESS STRING;

     DIR_FILE_ONE : DIR_IO.FILE_TYPE;
     DIR_FILE_TWO : DIR_IO.FILE_TYPE;
     DIR_FILE_NAME : ACC_STR;
     VAR : INTEGER;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE2104D", "CHECK THAT THE NAME RETURNED BY NAME " &
                      "CAN BE USED IN A SUBSEQUENT OPEN");

-- CREATE TEST FILE

     BEGIN
          CREATE (DIR_FILE_ONE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE WITH " &
                               "OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE WITH " &
                               "OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
               RAISE INCOMPLETE;
     END;

     WRITE (DIR_FILE_ONE, 3);
     DIR_FILE_NAME := NEW STRING'(NAME(DIR_FILE_ONE));
     CLOSE (DIR_FILE_ONE);

-- ATTEMPT TO RE-OPEN DIRECT TEST FILE USING RETURNED NAME VALUE

     BEGIN
          OPEN (DIR_FILE_TWO, IN_FILE, DIR_FILE_NAME.ALL);
     EXCEPTION
          WHEN DIR_IO.USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON OPEN WITH " &
                               "IN_FILE MODE");
               RAISE INCOMPLETE;
          WHEN DIR_IO.NAME_ERROR =>
               FAILED ("STRING NOT ACCEPTED AS NAME FOR FILE - DIR");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("FILE NOT RE-OPENED - DIR");
               RAISE INCOMPLETE;

     END;

     READ (DIR_FILE_TWO, VAR);
     IF VAR /= 3 THEN
          FAILED ("WRONG DATA RETURNED FROM READ - DIR");
     END IF;

-- DELETE TEST FILE

     BEGIN
          DELETE (DIR_FILE_TWO);
     EXCEPTION
          WHEN USE_ERROR =>
               COMMENT ("DELETION OF EXTERNAL FILE IS NOT SUPPORTED");
     END;

     RESULT;

EXCEPTION

     WHEN INCOMPLETE =>
          RESULT;

END CE2104D;
