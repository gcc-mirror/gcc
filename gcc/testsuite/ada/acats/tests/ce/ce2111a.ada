-- CE2111A.ADA

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
--     SEQUENTIAL FILES.

-- HISTORY:
--     DLD 08/13/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/28/85
--     JLH 07/22/87  REWROTE TEST ALGORITHM.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2111A IS

     PACKAGE SEQ_IO IS NEW SEQUENTIAL_IO(INTEGER);
          USE SEQ_IO;

     SEQ_FILE : SEQ_IO.FILE_TYPE;
     VAR1 : INTEGER := 5;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE2111A", "CHECK THAT THE FILE REMAINS OPEN AFTER A RESET");

-- CREATE SEQUENTIAL TEST FILE

     BEGIN
          CREATE (SEQ_FILE, OUT_FILE, LEGAL_FILE_NAME);
          WRITE (SEQ_FILE, VAR1);
          CLOSE (SEQ_FILE);
     EXCEPTION
          WHEN USE_ERROR | NAME_ERROR =>
               NOT_APPLICABLE ("SEQUENTIAL FILES NOT SUPPORTED");
               RAISE INCOMPLETE;
     END;

-- OPEN FILE

     BEGIN
          OPEN (SEQ_FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("OPEN WITH IN_FILE MODE NOT SUPPORTED " &
                               "FOR SEQ_IO");
               RAISE INCOMPLETE;
     END;

-- RESET FILE

     BEGIN
          RESET(SEQ_FILE);
     EXCEPTION
          WHEN USE_ERROR  =>
               NULL;
     END;

     IF IS_OPEN (SEQ_FILE) THEN
          CLOSE (SEQ_FILE);
     ELSE
          FAILED ("RESET FOR IN_FILE, CLOSED FILE");
     END IF;

-- RE-OPEN AS OUT_FILE AND REPEAT TEST

     BEGIN
          OPEN (SEQ_FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("OPEN WITH OUT_FILE MODE NOT " &
                               "SUPPORTED FOR SEQ_IO");
               RAISE INCOMPLETE;
     END;

     BEGIN
          RESET (SEQ_FILE);
     EXCEPTION
          WHEN USE_ERROR  =>
               NULL;
     END;

     IF IS_OPEN (SEQ_FILE) THEN
          BEGIN
               DELETE (SEQ_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;
     ELSE
          FAILED ("RESET FOR OUT_FILE, CLOSED FILE");
     END IF;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2111A;
