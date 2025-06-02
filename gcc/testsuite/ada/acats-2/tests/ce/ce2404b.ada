-- CE2404B.ADA

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
--     CHECK THAT READ RAISES MODE_ERROR WHEN THE CURRENT MODE IS
--     OUT_FILE.

--          B)  CHECK TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF DIRECT FILES WITH MODE OUT_FILE.

-- HISTORY:
--     GMT 08/03/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2404B IS

     PACKAGE DIR_IO IS NEW DIRECT_IO(INTEGER);
     USE DIR_IO;
     DIR_FILE_2 : FILE_TYPE;
     I          : INTEGER;
     INCOMPLETE : EXCEPTION;

BEGIN
     TEST ("CE2404B", "CHECK THAT READ RAISES MODE_ERROR WHEN THE " &
                      "CURRENT MODE IS OUT_FILE AND THE FILE IS " &
                      "A TEMPORARY FILE");
     BEGIN
          CREATE (DIR_FILE_2, OUT_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE - 1");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE - 2");
               RAISE INCOMPLETE;
     END;

     BEGIN
          READ(DIR_FILE_2, I);
          FAILED("MODE_ERROR NOT RAISED ON READ - 3");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED("WRONG EXCEPTION RAISED ON READ - 4");
     END;

     CLOSE (DIR_FILE_2);

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2404B;
