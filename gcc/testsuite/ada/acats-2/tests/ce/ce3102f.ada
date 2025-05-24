-- CE3102F.ADA

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
--     CHECK THAT USE_ERROR IS RAISED WHEN AN EXTERNAL FILE
--     CANNOT BE RESET.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES, BUT DO NOT SUPPORT RESET OF EXTERNAL FILES.

-- HISTORY:
--     JLH 08/12/87  CREATED ORIGINAL TEST.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3102F IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;

BEGIN

     TEST ("CE3102F", "CHECK THAT USE_ERROR IS RAISED WHEN AN " &
                      "EXTERNAL FILE CANNOT BE RESET");

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          RESET (FILE);
          NOT_APPLICABLE ("RESET FOR OUT_FILE MODE ALLOWED - 1");
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR RESET - 1");
     END;

     PUT (FILE, "HELLO");

     BEGIN
          RESET (FILE, IN_FILE);
          NOT_APPLICABLE ("RESET FROM OUT_FILE TO IN_FILE MODE " &
                          "ALLOWED - 1");
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RASIED FOR RESET - 2");
     END;

     CLOSE (FILE);

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("TEXT_IO NOT SUPPORTED FOR IN_FILE " &
                               "OPEN");
               RAISE INCOMPLETE;
     END;

     BEGIN
          RESET (FILE);
          NOT_APPLICABLE ("RESET FOR IN_FILE MODE ALLOWED - 2");
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR RESET - 3");
     END;

     BEGIN
          RESET (FILE, OUT_FILE);
          NOT_APPLICABLE ("RESET FROM IN_FILE TO OUT_FILE MODE " &
                          "ALLOWED - 2");
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR RESET - 4");
     END;

     BEGIN
          DELETE (FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3102F;
