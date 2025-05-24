-- CE2410B.ADA

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
--     CHECK THAT END_OF_FILE RAISES MODE_ERROR WHEN THE CURRENT
--     MODE IS OUT_FILE.

--          2) CHECK TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH OUT_FILE MODE FOR DIRECT FILES.

-- HISTORY:
--     GMT 08/05/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2410B IS

     PACKAGE DIR IS NEW DIRECT_IO (INTEGER);
     USE DIR;
     FILE1       : FILE_TYPE;
     INT         : INTEGER := IDENT_INT (18);
     BOOL        : BOOLEAN;
     INCOMPLETE  : EXCEPTION;

BEGIN

     TEST ("CE2410B", "CHECK THAT END_OF_FILE RAISES MODE_ERROR WHEN " &
                      "THE CURRENT MODE IS OUT_FILE AND THE FILE IS " &
                      "A TEMPORARY FILE.");

     BEGIN
          CREATE (FILE1, OUT_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("CREATE WITH OUT_FILE MODE NOT " &
                               "SUPPORTED FOR DIRECT FILES - 1");
               RAISE INCOMPLETE;
     END;

     BEGIN
          BOOL := END_OF_FILE (FILE1);
          FAILED ("MODE_ERROR NOT RAISED ON END_OF_FILE - 2");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON " &
                       "END_OF_FILE - 3");
     END;

     CLOSE (FILE1);

     RESULT ;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2410B ;
