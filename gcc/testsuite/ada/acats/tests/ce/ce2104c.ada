-- CE2104C.ADA

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
--     CHECK THAT A FILE CAN BE CLOSED AND THEN RE-OPENED.

--          B) DIRECT FILES

-- APPLICABLILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHOSE
--     ENVIRONMENT SUPPORTS CREATE/OPEN FOR THE GIVEN MODE.

-- HISTORY:
--     DLD 08/11/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  06/03/85
--     PWB 02/10/86  CORRECTED REPORTED TEST NAME; CHANGED DATA FILE
--                   NAME TO "Y2104C" TO MATCH TEST NAME.
--     SPW 08/07/87  REMOVED UNNECESSARY CODE AND CORRECTED EXCEPTION
--                   HANDLING.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2104C IS

     PACKAGE DIR_IO IS NEW DIRECT_IO(INTEGER);
     USE DIR_IO;

     DIR_FILE : DIR_IO.FILE_TYPE;
     VAR : INTEGER;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE2104C", "CHECK THAT A FILE CAN BE CLOSED " &
                      "AND THEN RE-OPENED");

-- INITIALIZE TEST FILE

     BEGIN
          CREATE (DIR_FILE, OUT_FILE, LEGAL_FILE_NAME);
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

     WRITE (DIR_FILE, 28);
     CLOSE (DIR_FILE);

-- RE-OPEN DIRECT TEST FILE

     BEGIN
          OPEN (DIR_FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON OPEN WITH " &
                               "IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     READ (DIR_FILE, VAR);
     IF VAR /= 28 THEN
          FAILED ("WRONG DATA RETURNED FROM READ - DIRECT");
     END IF;

-- DELETE TEST FILE

     BEGIN
          DELETE (DIR_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION

     WHEN INCOMPLETE =>
          RESULT;

END CE2104C;
