-- CE2201F.ADA

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
--      CHECK THAT READ, WRITE, AND END_OF_FILE ARE SUPPORTED FOR
--      SEQUENTIAL FILES WITH PRIVATE ELEMENT_TYPES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES WITH PRIVATE ELEMENT_TYPES.

-- HISTORY:
--     ABW 08/17/82
--     SPS 09/15/82
--     SPS 11/09/82
--     JBG 01/06/83
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/03/87  REMOVED DEPENDENCE OF RESET AND CREATED EXTERNAL
--                   FILES RATHER THAN TEMPORARY FILES.

WITH REPORT;
USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2201F IS

     PACKAGE PKG IS
          TYPE PRIV IS PRIVATE;
          FUNCTION MAKE_PRIV (X : INTEGER) RETURN PRIV;
     PRIVATE
          TYPE PRIV IS NEW INTEGER;
     END PKG;
     USE PKG;

     PACKAGE BODY PKG IS
          FUNCTION MAKE_PRIV (X : INTEGER) RETURN PRIV IS
          BEGIN
               RETURN PRIV(X);
          END;
     END PKG;

BEGIN

     TEST ("CE2201F", "CHECK THAT READ, WRITE, AND " &
                      "END_OF_FILE ARE SUPPORTED FOR " &
                      "SEQUENTIAL FILES FOR PRIVATE TYPES");

     DECLARE
          PACKAGE SEQ_PRV IS NEW SEQUENTIAL_IO (PRIV);
          USE SEQ_PRV;
          PRV, ITEM_PRV : PRIV;
          FILE_PRV : FILE_TYPE;
          INCOMPLETE : EXCEPTION;
     BEGIN
          BEGIN
               CREATE (FILE_PRV, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("CREATE OF SEQUENTIAL FILE WITH " &
                                    "MODE OUT_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          PRV := MAKE_PRIV(IDENT_INT(26));

          WRITE (FILE_PRV, PRV);
          CLOSE (FILE_PRV);

          BEGIN
               OPEN (FILE_PRV, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("OPEN OF SEQUENTIAL FILE WITH " &
                                    "MODE IN_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          IF END_OF_FILE (FILE_PRV) THEN
               FAILED ("WRONG END_OF_FILE VALUE FOR PRIVATE TYPE");
          END IF;

          READ (FILE_PRV, ITEM_PRV);

          IF ITEM_PRV /= MAKE_PRIV (26) THEN
               FAILED ("READ WRONG VALUE");
          END IF;

          IF NOT END_OF_FILE (FILE_PRV) THEN
               FAILED ("NOT AT END OF FILE");
          END IF;

          BEGIN
               DELETE (FILE_PRV);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE2201F;
