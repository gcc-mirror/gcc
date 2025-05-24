-- CE2111E.ADA

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
--     CHECK THAT THE FILE REMAINS OPEN AFTER A RESET.

--     THIS OBJECTIVE IS BEING INTERPRETED AS : CHECK THAT A FILE
--     REMAINS OPEN AFTER AN ATTEMPT TO RESET.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT FILES.

-- HISTORY:
--     DLD 08/13/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/28/85
--     JLH 07/23/87  REWROTE TEST ALGORITHM.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2111E IS

     PACKAGE DIR_IO IS NEW DIRECT_IO (INTEGER);
          USE DIR_IO;

     DIR_FILE : DIR_IO.FILE_TYPE;
     VAR1 : INTEGER := 5;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE2111E", "CHECK THAT THE FILE REMAINS OPEN AFTER A RESET");

-- CREATE DIRECT TEST FILE

     BEGIN
          CREATE (DIR_FILE, OUT_FILE, LEGAL_FILE_NAME);
          WRITE (DIR_FILE, VAR1);
          CLOSE (DIR_FILE);
     EXCEPTION
          WHEN USE_ERROR | NAME_ERROR =>
               NOT_APPLICABLE ("DIRECT FILES NOT SUPPORTED");
               RAISE INCOMPLETE;
     END;

-- OPEN FILE

     BEGIN
          OPEN (DIR_FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("OPEN WITH IN_FILE MODE NOT SUPPORTED " &
                               "FOR DIR_IO");
               RAISE INCOMPLETE;
     END;

-- RESET FILE

     BEGIN
          RESET (DIR_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     IF IS_OPEN (DIR_FILE) THEN
          CLOSE (DIR_FILE);
     ELSE
          FAILED ("RESET FOR IN_FILE, CLOSED FILE");
     END IF;


-- RE-OPEN AS OUT_FILE AND REPEAT TEST

     BEGIN
          OPEN (DIR_FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("OPEN WITH OUT_FILE MODE NOT " &
                               "SUPPORTED FOR DIR_IO");
               RAISE INCOMPLETE;
     END;

     BEGIN
          RESET (DIR_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     IF IS_OPEN (DIR_FILE) THEN
          CLOSE (DIR_FILE);
     ELSE
          FAILED ("RESET FOR OUT_FILE, CLOSED FILE");
     END IF;

-- RE-OPEN AS IN_OUT FILE AND REPEAT TEST

     BEGIN
          OPEN (DIR_FILE, INOUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("OPEN WITH IN_OUT FILE MODE NOT " &
                               "SUPPORTED FOR DIR_IO");
               RAISE INCOMPLETE;
     END;

     BEGIN
          RESET (DIR_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     IF IS_OPEN (DIR_FILE) THEN
          BEGIN
               DELETE (DIR_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;
     ELSE
          FAILED ("RESET FOR INOUT_FILE, CLOSED FILE");
     END IF;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2111E;
