-- CE2108H.ADA

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
--     CHECK THAT AN EXTERNAL DIRECT FILE SPECIFIED BY A NON-NULL
--     STRING NAME IS ACCESSIBLE AFTER THE COMPLETION OF THE MAIN
--     PROGRAM.

--     THIS TEST CHECKS THE CREATION OF A DIRECT FILE WHICH WAS
--     CREATED BY CE2108G.ADA.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT FILES.

-- HISTORY:
--     TBN 07/16/87  CREATED ORIGINAL TESTED.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2108H IS

     PACKAGE DIR IS NEW DIRECT_IO (INTEGER);
     USE DIR;
     INCOMPLETE : EXCEPTION;
     CHECK_SUPPORT, FILE_NAME : FILE_TYPE;
     PREVENT_EMPTY_FILE : NATURAL := 0;

BEGIN
     TEST ("CE2108H", "CHECK THAT AN EXTERNAL DIRECT FILE SPECIFIED " &
                      "BY A NON-NULL STRING NAME IS ACCESSIBLE AFTER " &
                      "THE COMPLETION OF THE MAIN PROGRAM");

     -- TEST FOR DIRECT FILE SUPPORT.

     BEGIN
          CREATE (CHECK_SUPPORT, OUT_FILE, LEGAL_FILE_NAME);
          BEGIN
               DELETE (CHECK_SUPPORT);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON DIRECT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON DIRECT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON DIRECT CREATE");
               RAISE INCOMPLETE;
     END;

     -- BEGIN TEST OBJECTIVE.

     BEGIN
          OPEN (FILE_NAME, IN_FILE, LEGAL_FILE_NAME(1, "CE2108G"));
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON OPEN WITH " &
                               "IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     READ (FILE_NAME, PREVENT_EMPTY_FILE);
     IF PREVENT_EMPTY_FILE /= 5 THEN
          FAILED ("OPENED WRONG FILE OR DATA ERROR");
     END IF;
     BEGIN
          DELETE (FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               COMMENT ("IMPLEMENTATION WOULD NOT ALLOW DELETION OF " &
                        "EXTERNAL FILE");
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;
END CE2108H;
