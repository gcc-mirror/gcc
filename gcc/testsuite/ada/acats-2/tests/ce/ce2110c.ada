-- CE2110C.ADA

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
--     CHECK THAT AN EXTERNAL FILE CEASES TO EXIST AFTER A SUCCESSFUL
--     DELETE.

-- APPLICABILITY CRITERIA:
--    THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--    CREATION AND DELETION OF DIRECT FILES.

-- HISTORY:
--     SPS 08/25/82
--     SPS 11/09/82
--     JBG 04/01/83
--     EG  05/31/85
--     JLH 07/21/87  ADDED A CALL TO NOT_APPLICABLE IF EXCEPTION
--                   USE_ERROR IS RAISED ON DELETE.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2110C IS
BEGIN

     TEST ("CE2110C", "CHECK THAT THE EXTERNAL FILE CEASES TO EXIST " &
                      "AFTER A SUCCESSFUL DELETE");

     DECLARE
          PACKAGE DIR IS NEW DIRECT_IO (INTEGER);
          USE DIR;
          FL1, FL2 : FILE_TYPE;
          VAR1 : INTEGER := 5;
          INCOMPLETE : EXCEPTION;
     BEGIN
          BEGIN
               CREATE (FL1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXCEPTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               WRITE (FL1, VAR1);     -- THIS WRITES TO THE FILE IF IT
          EXCEPTION                   -- CAN, NOT NECESSARY FOR THE
               WHEN OTHERS =>         -- OBJECTIVE.
                    NULL;
          END;

          BEGIN
               DELETE (FL1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("DELETION OF EXTERNAL FILE NOT " &
                                    "SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               OPEN (FL2, IN_FILE, LEGAL_FILE_NAME);
               FAILED ("EXTERNAL FILE STILL EXISTS AFTER " &
                       "A SUCCESSFUL DELETION - DIR");
          EXCEPTION
               WHEN NAME_ERROR =>
                    NULL;
          END;
     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE2110C;
