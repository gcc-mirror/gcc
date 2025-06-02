-- CE2201B.ADA

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
--     SEQUENTIAL FILES WITH ELEMENT_TYPE CONSTRAINED ARRAY.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES WITH ELEMENT_TYPE CONSTRAINED ARRAY.

-- HISTORY:
--     ABW 08/17/82
--     SPS 09/15/82
--     SPS 11/09/82
--     JBG 05/02/83
--     EG  05/08/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 07/28/87  REMOVED THE DEPENDENCE OF RESET BEING SUPPORTED
--                   AND CREATED EXTERNAL FILES RATHER THAN TEMPORARY
--                   FILES.

WITH REPORT;
USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2201B IS

BEGIN

     TEST ("CE2201B", "CHECK THAT READ, WRITE, AND " &
                      "END_OF_FILE ARE SUPPORTED FOR " &
                      "SEQUENTIAL FILES - CONSTRAINED ARRAY");

     DECLARE
          TYPE ARR_CN IS ARRAY (1..5) OF BOOLEAN;
          PACKAGE SEQ_ARR_CN IS NEW SEQUENTIAL_IO (ARR_CN);
          USE SEQ_ARR_CN;
          FILE_ARR_CN : FILE_TYPE;
          INCOMPLETE : EXCEPTION;
          ARR1 : ARR_CN := (TRUE,TRUE,FALSE,TRUE,TRUE);
          ITEM_ARR1 : ARR_CN;
     BEGIN
          BEGIN
               CREATE (FILE_ARR_CN, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("CREATE OF SEQUENTIAL FILE WITH " &
                                    "MODE OUT_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          WRITE (FILE_ARR_CN, ARR1);
          CLOSE (FILE_ARR_CN);

          BEGIN
               OPEN (FILE_ARR_CN, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("OPEN OF SEQUENTIAL FILE WITH " &
                                    "MODE IN_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          IF END_OF_FILE (FILE_ARR_CN) THEN
               FAILED ("WRONG END_OF_FILE VALUE FOR " &
                       "CONSTRAINED ARRAY");
          END IF;

          READ (FILE_ARR_CN, ITEM_ARR1);

          IF ITEM_ARR1 /= ARR1 THEN
               FAILED ("READ WRONG VALUE - CONSTRAINED ARRAY");
          END IF;

          IF NOT END_OF_FILE (FILE_ARR_CN) THEN
               FAILED ("END OF FILE NOT TRUE - CONSTRAINED ARRAY");
          END IF;

          BEGIN
               DELETE  (FILE_ARR_CN);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE2201B;
