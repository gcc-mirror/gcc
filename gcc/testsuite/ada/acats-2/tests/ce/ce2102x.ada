-- CE2102X.ADA

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
--     CHECK THAT USE_ERROR IS RAISED IF AN IMPLEMENTATION DOES NOT
--     SUPPORT DELETION OF AN EXTERNAL SEQUENTIAL FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF A SEQUENTIAL FILE WITH OUT_FILE MODE.

-- HISTORY:
--     TBN 09/15/87  CREATED ORIGINAL TEST.

WITH SEQUENTIAL_IO;
WITH REPORT; USE REPORT;
PROCEDURE CE2102X IS
     INCOMPLETE : EXCEPTION;
BEGIN
     TEST ("CE2102X", "CHECK THAT USE_ERROR IS RAISED IF AN " &
                      "IMPLEMENTATION DOES NOT SUPPORT DELETION " &
                      "OF AN EXTERNAL SEQUENTIAL FILE");
     DECLARE
          PACKAGE SEQ IS NEW SEQUENTIAL_IO (INTEGER);
          USE SEQ;
          FILE1 : FILE_TYPE;
          INT1 : INTEGER := IDENT_INT(1);
     BEGIN
          BEGIN
               CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE OF " &
                                    "SEQUENTIAL FILE WITH OUT_FILE " &
                                    "MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE OF " &
                                    "SEQUENTIAL FILE WITH OUT_FILE " &
                                    "MODE");
                    RAISE INCOMPLETE;
          END;

          WRITE (FILE1, INT1);
          BEGIN
               DELETE (FILE1);
               COMMENT ("DELETION OF AN EXTERNAL SEQUENTIAL FILE IS " &
                        "ALLOWED");
          EXCEPTION
               WHEN USE_ERROR =>
                    COMMENT ("DELETION OF AN EXTERNAL SEQUENTIAL " &
                             "FILE IS NOT ALLOWED");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED WHILE " &
                            "DELETING AN EXTERNAL FILE");
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;
END CE2102X;
