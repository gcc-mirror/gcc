-- CE2201K.ADA

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
--     CHECK THAT READ, WRITE, AND END_OF_FILE ARE SUPPORTED FOR
--     SEQUENTIAL FILES WITH ELEMENT TYPE ACCESS.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     JLH 07/28/87  CREATED ORIGINAL TEST.

WITH REPORT;
USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2201K IS

BEGIN

     TEST ("CE2201K", "CHECK THAT READ, WRITE, AND " &
                      "END_OF_FILE ARE SUPPORTED FOR " &
                      "SEQUENTIAL FILES - ACCESS TYPE");

     DECLARE
          TYPE ACC_INT IS ACCESS INTEGER;
          PACKAGE SEQ_ACC IS NEW SEQUENTIAL_IO (ACC_INT);
          USE SEQ_ACC;
          FILE_ACC : FILE_TYPE;
          INCOMPLETE : EXCEPTION;
          ACC : ACC_INT := NEW INTEGER'(33);
          ITEM_ACC : ACC_INT;
     BEGIN
          BEGIN
               CREATE (FILE_ACC, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("CREATE OF SEQUENTIAL FILE WITH " &
                                    "MODE OUT_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          WRITE (FILE_ACC, ACC);
          CLOSE (FILE_ACC);

          BEGIN
               OPEN (FILE_ACC, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("OPEN OF SEQUENTIAL FILE WITH " &
                                    "MODE IN_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          IF END_OF_FILE (FILE_ACC) THEN
               FAILED ("WRONG END_OF_FILE VALUE FOR TYPE ACCESS");
          END IF;

          READ (FILE_ACC, ITEM_ACC);

          IF NOT END_OF_FILE (FILE_ACC) THEN
               FAILED ("END OF FILE NOT TRUE - ACCESS");
          END IF;

          BEGIN
               DELETE (FILE_ACC);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE2201K;
