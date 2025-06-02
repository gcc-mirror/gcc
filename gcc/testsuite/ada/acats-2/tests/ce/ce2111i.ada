-- CE2111I.ADA

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
--     CHECK THAT A SUPPLIED MODE PARAMETER IN A RESET CHANGES
--     THE MODE OF A GIVEN FILE.  IF NO PARAMETER IS SUPPLIED
--     THE MODE REMAINS THE SAME.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     RESET FOR SEQUENTIAL FILES.

-- HISTORY:
--     JLH 07/23/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2111I IS

     PACKAGE SEQ_IO IS NEW SEQUENTIAL_IO (INTEGER);
          USE SEQ_IO;
     SEQ_FILE : SEQ_IO.FILE_TYPE;
     SEQ_MODE : SEQ_IO.FILE_MODE;
     INCOMPLETE : EXCEPTION;
     VAR1 : INTEGER := 5;

BEGIN

     TEST("CE2111I", "CHECK THAT A SUPPLIED MODE PARAMETER SETS " &
                     "THE MODE OF THE GIVEN FILE APPROPRIATELY");

-- CREATE SEQUENTIAL TEST FILE

     BEGIN
          CREATE (SEQ_FILE, OUT_FILE, LEGAL_FILE_NAME);
          WRITE (SEQ_FILE, VAR1);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE");
               RAISE INCOMPLETE;
     END;

-- RESET TO DEFAULT

     BEGIN
          SEQ_MODE := IN_FILE;
          RESET (SEQ_FILE);
          SEQ_MODE := MODE (SEQ_FILE);
          IF SEQ_MODE /= OUT_FILE THEN
               FAILED ("DEFAULT RESET CHANGED MODE - SEQ");
          END IF;
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("RESET NOT SUPPORTED FOR SEQ OUT_FILE " &
                               "MODE");
               RAISE INCOMPLETE;
     END;

-- RESET TO IN_FILE

     BEGIN
          SEQ_MODE := OUT_FILE;
          RESET (SEQ_FILE, IN_FILE);
          SEQ_MODE := MODE (SEQ_FILE);
          IF SEQ_MODE /= IN_FILE THEN
               FAILED ("RESET TO IN_FILE FAILED - SEQ");
          END IF;
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("RESET FROM OUT_FILE TO IN_FILE MODE " &
                               "NOT SUPPORTED FOR SEQ FILES");
               RAISE INCOMPLETE;
     END;

     BEGIN
          DELETE (SEQ_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2111I;
