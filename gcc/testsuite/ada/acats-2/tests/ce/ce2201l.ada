-- CE2201L.ADA

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
--     SEQUENTIAL FILES WITH ELEMENT TYPE FIXED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     JLH 08/03/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2201L IS
BEGIN

     TEST ("CE2201L", "CHECK THAT READ, WRITE, AND END_OF_FILE " &
                      "ARE SUPPORTED FOR SEQUENTIAL FILES - FIXED");

     DECLARE
          TYPE FIX IS DELTA 0.5 RANGE -10.0 .. 255.0;
          PACKAGE SEQ_FIX IS NEW SEQUENTIAL_IO (FIX);
          USE SEQ_FIX;
          FILE_FIX : FILE_TYPE;
          INCOMPLETE : EXCEPTION;
          FX : FIX := -8.5;
          ITEM_FIX : FIX;
     BEGIN
          BEGIN
               CREATE (FILE_FIX, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("CREATE OF SEQUENTIAL FILE WITH " &
                                    "MODE OUT_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          WRITE (FILE_FIX, FX);
          CLOSE (FILE_FIX);

          BEGIN
               OPEN (FILE_FIX, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("OPEN OF SEQUENTIAL FILE WITH " &
                                    "MODE IN_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          IF END_OF_FILE (FILE_FIX) THEN
               FAILED ("WRONG END_OF_FILE VALUE FOR FIXED POINT");
          END IF;

          READ (FILE_FIX, ITEM_FIX);

          IF NOT END_OF_FILE (FILE_FIX) THEN
               FAILED ("END OF FILE NOT TRUE - FIXED");
          END IF;

          IF ITEM_FIX /= -8.5 THEN
               FAILED ("READ WRONG VALUE - STRING");
          END IF;

          BEGIN
               DELETE (FILE_FIX);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE2201L;
