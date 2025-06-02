-- CE2201N.ADA

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
--     SEQUENTIAL FILES WITH ELEMENT_TYPE CONSTRAINED RECORD TYPES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES WITH ELEMENT_TYPE CONSTRAINED RECORD TYPES.

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

PROCEDURE CE2201N IS

BEGIN

     TEST ("CE2201N", "CHECK THAT READ, WRITE, AND " &
                      "END_OF_FILE ARE SUPPORTED FOR " &
                      "SEQUENTIAL FILES - CONSTRAINED RECORDS");

     DECLARE
          TYPE REC_DEF (DISCR : INTEGER := 18) IS
               RECORD
                    ONE : INTEGER := 1;
                    TWO : INTEGER := 2;
                    THREE : INTEGER := 17;
                    FOUR : INTEGER := 2;
               END RECORD;
          SUBTYPE REC_DEF_2 IS REC_DEF(2);
          PACKAGE SEQ_REC_DEF IS NEW SEQUENTIAL_IO (REC_DEF_2);
          USE SEQ_REC_DEF;
          FILE_REC_DEF : FILE_TYPE;
          INCOMPLETE : EXCEPTION;
          REC3 : REC_DEF(2);
          ITEM_REC3 : REC_DEF(2);
     BEGIN
          BEGIN
               CREATE (FILE_REC_DEF, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("CREATE OF SEQUENTIAL FILE WITH " &
                                    "MODE OUT_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          WRITE (FILE_REC_DEF, REC3);
          CLOSE (FILE_REC_DEF);

          BEGIN
               OPEN (FILE_REC_DEF, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("OPEN OF SEQUENTIAL FILE WITH " &
                                    "MODE IN_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          IF END_OF_FILE (FILE_REC_DEF) THEN
               FAILED ("WRONG END_OF_FILE VALUE FOR RECORD" &
                       "WITH DEFAULT");
          END IF;

          READ (FILE_REC_DEF, ITEM_REC3);

          IF ITEM_REC3 /= (2, IDENT_INT(1),2,17,2) THEN
               FAILED ("READ WRONG VALUE - RECORD WITH DEFAULT");
          END IF;

          IF NOT END_OF_FILE (FILE_REC_DEF) THEN
               FAILED ("END OF FILE NOT TRUE - RECORD WITH DEFAULT");
          END IF;

          BEGIN
               DELETE (FILE_REC_DEF);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE2201N;
