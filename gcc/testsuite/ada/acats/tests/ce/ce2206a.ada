-- CE2206A.ADA

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
--     CHECK THAT READ FOR A SEQUENTIAL FILE RAISES END_ERROR WHEN
--     THERE ARE NO MORE ELEMENTS THAT CAN BE READ FROM THE GIVEN
--     FILE.  ALSO CHECK THAT END_OF_FILE CORRECTLY DETECTS THE END
--     OF A SEQUENTIAL FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     JLH 08/22/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2206A IS

     PACKAGE SEQ_IO IS NEW SEQUENTIAL_IO (CHARACTER);
     USE SEQ_IO;

     FILE : FILE_TYPE;
     ITEM : CHARACTER;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE2206A", "CHECK THAT READ FOR A SEQUENTIAL FILE RAISES " &
                      "END_ERROR WHEN THERE ARE NO MORE ELEMENTS " &
                      "THAT CAN BE READ FROM THE GIVEN FILE.  ALSO " &
                      "CHECK THAT END_OF_FILE CORRECTLY DETECTS THE " &
                      "END OF A SEQUENTIAL FILE");

     BEGIN

          BEGIN
               CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
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
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          WRITE (FILE, 'A');
          WRITE (FILE, 'B');

          CLOSE (FILE);

          BEGIN
               OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON OPEN WITH " &
                                    "MODE IN_FILE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON OPEN");
                    RAISE INCOMPLETE;
          END;

          READ (FILE, ITEM);
          IF ITEM /= 'A' THEN
               FAILED ("INCORRECT VALUE READ");
          END IF;

          IF END_OF_FILE (FILE) THEN
               FAILED ("END_OF_FILE NOT DETECTED CORRECTLY - 1");
          END IF;

          READ (FILE, ITEM);

          IF NOT END_OF_FILE (FILE) THEN
               FAILED ("END_OF_FILE NOT DETECTED CORRECTLY - 2");
          END IF;

          BEGIN
               READ (FILE, ITEM);
               FAILED ("END_ERROR NOT RAISED FOR READ");
          EXCEPTION
               WHEN END_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON READ");
          END;

          BEGIN
               DELETE (FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;

     END;

     RESULT;

END CE2206A;
