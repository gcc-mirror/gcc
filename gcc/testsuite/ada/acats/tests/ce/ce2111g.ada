-- CE2111G.ADA

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
--     RESET FOR DIRECT FILES.

-- HISTORY:
--     DLD 08/16/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/29/85
--     TBN 11/04/86  ADDED A RAISE INCOMPLETE STATEMENT WHEN FAILED
--                   IS CALLED FOR OPEN OR CREATE.
--     JLH 07/24/87  ADDED CHECKS FOR USE_ERR0R WHEN FILE IS RESET.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2111G IS

     PACKAGE DIR_IO IS NEW DIRECT_IO (INTEGER);
          USE DIR_IO;
     DIR_FILE : DIR_IO.FILE_TYPE;
     DIR_MODE : DIR_IO.FILE_MODE;
     INCOMPLETE : EXCEPTION;
     VAR1 : INTEGER := 5;

BEGIN

     TEST ("CE2111G", "CHECK THAT A SUPPLIED MODE PARAMETER SETS " &
                      "THE MODE OF THE GIVEN FILE APPROPRIATELY");

-- CREATE DIRECT TEST FILE

     BEGIN
          CREATE (DIR_FILE, INOUT_FILE, LEGAL_FILE_NAME);
          WRITE (DIR_FILE, VAR1);
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
          DIR_MODE := OUT_FILE;
          RESET (DIR_FILE);
          DIR_MODE := MODE (DIR_FILE);
          IF DIR_MODE /= INOUT_FILE THEN
               FAILED ("DEFAULT RESET CHANGED MODE - DIR");
          END IF;
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("RESET NOT SUPPORTED FOR DIR " &
                               "INOUT_FILES");
     END;

-- RESET TO OUT_FILE

     BEGIN
          DIR_MODE := IN_FILE;
          RESET (DIR_FILE, OUT_FILE);
          DIR_MODE := MODE (DIR_FILE);
          IF DIR_MODE /= OUT_FILE THEN
               FAILED ("RESET TO OUT_FILE FAILED - DIR");
          END IF;
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("RESET FROM INOUT_FILE TO OUT_FILE " &
                               "NOT SUPPORTED FOR DIR FILES");
     END;

-- RESET TO IN_FILE

     BEGIN
          DIR_MODE := OUT_FILE;
          RESET (DIR_FILE, IN_FILE);
          DIR_MODE := MODE (DIR_FILE);
          IF DIR_MODE /= IN_FILE THEN
               FAILED ("RESET TO IN_FILE FAILED - DIR");
          END IF;
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("RESET FROM OUT_FILE TO IN_FILE NOT " &
                               "SUPPORTED FOR DIR IN_FILE");
     END;

-- RESET TO INOUT_FILE

     BEGIN
          DIR_MODE := OUT_FILE;
          RESET (DIR_FILE, INOUT_FILE);
          DIR_MODE := MODE (DIR_FILE);
          IF DIR_MODE /= INOUT_FILE THEN
               FAILED ("RESET TO INOUT_FILE FAILED - DIR");
          END IF;
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("RESET FROM IN_FILE TO INOUT_FILE NOT " &
                               "SUPPORTED FOR DIR INOUT_FILES");
     END;

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

END CE2111G;
