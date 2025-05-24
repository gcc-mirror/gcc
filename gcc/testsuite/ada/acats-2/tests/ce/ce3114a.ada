-- CE3114A.ADA

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
--     CHECK THAT AN EXTERNAL TEXT FILE CEASES TO EXIST AFTER
--     A SUCCESSFUL DELETE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION AND DELETION OF TEXT FILES.

-- HISTORY:
--     SPS 08/25/82
--     SPS 11/09/82
--     JBG 04/01/83
--     EG  05/16/85
--     GMT 08/25/87  COMPLETELY REVISED.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3114A IS
BEGIN

     TEST ("CE3114A", "CHECK THAT AN EXTERNAL TEXT FILE CEASES TO " &
                      "EXIST AFTER A SUCCESSFUL DELETE");

     DECLARE
          FL1, FL2   : FILE_TYPE;
          VAR1       : CHARACTER := 'A';
          INCOMPLETE : EXCEPTION;
     BEGIN
          BEGIN
               CREATE (FL1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE - 1");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE - 2");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON " &
                            "CREATE - 3");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               PUT (FL1, VAR1);   -- THIS PUTS TO THE FILE IF
          EXCEPTION               -- IT CAN, NOT NECESSARY FOR
               WHEN OTHERS =>     -- THE OBJECTIVE.
                    NULL;
          END;

          BEGIN
               DELETE (FL1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("DELETION OF EXTERNAL TEXT FILES " &
                                    "IS NOT SUPPORTED - 4");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               OPEN (FL2, IN_FILE, LEGAL_FILE_NAME);
               FAILED ("EXTERNAL TEXT FILE STILL EXISTS AFTER " &
                       "A SUCCESSFUL DELETION - 5");
          EXCEPTION
               WHEN NAME_ERROR =>
                    NULL;
          END;
     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3114A;
