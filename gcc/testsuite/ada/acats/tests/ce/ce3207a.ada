-- CE3207A.ADA

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
--     CHECK THAT MODE_ERROR IS RAISED IF THE PARAMETER TO SET_INPUT HAS
--     MODE OUT_FILE OR THE PARAMETER TO SET_OUTPUT HAS MODE IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 07/07/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3207A IS

     FILE1, FILE2 : FILE_TYPE;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3207A", "CHECK THAT MODE_ERROR IS RAISED IF THE " &
                      "PARAMETER TO SET_INPUT HAS MODE OUT_FILE " &
                      "OR THE PARAMETER TO SET_OUTPUT HAS MODE " &
                      "IN_FILE");

     BEGIN

          BEGIN
               CREATE (FILE1, OUT_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                                    "WITH MODE OUT_FILE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               SET_INPUT (FILE1);
               FAILED ("MODE_ERROR NOT RAISED FOR SET_INPUT WITH " &
                       "MODE OUT_FILE");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR SET_INPUT");
          END;

          CREATE (FILE2, OUT_FILE, LEGAL_FILE_NAME);

          PUT (FILE2, "OUTPUT STRING");
          CLOSE (FILE2);
          OPEN (FILE2, IN_FILE, LEGAL_FILE_NAME);

          BEGIN
               SET_OUTPUT (FILE2);
               FAILED ("MODE_ERROR NOT RAISED FOR SET_OUTPUT WITH " &
                       "MODE IN_FILE");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR SET_OUTPUT");
          END;

          BEGIN
               DELETE (FILE2);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3207A;
