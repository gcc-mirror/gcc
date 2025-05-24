-- CE3405D.ADA

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
--     CHECK THAT NEW_PAGE INCREMENTS THE CURRENT PAGE NUMBER AND
--     SETS THE CURRENT COLUMN AND LINE NUMBERS TO ONE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 08/28/82
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/23/87  CORRECTED EXCEPTION HANDLING AND ADDED CASES FOR
--                   CONSECUTIVE NEW_LINE AND NEW_PAGE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;
WITH CHECK_FILE;

PROCEDURE CE3405D IS
     INCOMPLETE : EXCEPTION;
BEGIN

     TEST ("CE3405D", "CHECK THAT NEW_PAGE INCREMENTS PAGE COUNT " &
                      "AND SETS COLUMN AND LINE TO ONE");

     DECLARE
          FT : FILE_TYPE;
          CH : CHARACTER;
          PG_NUM : POSITIVE_COUNT;
     BEGIN

          BEGIN
               CREATE (FT, OUT_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                                    "FOR TEMP FILE WITH OUT_FILE " &
                                    "MODE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
                    RAISE INCOMPLETE;
          END;

          PUT (FT, "STRING");
          NEW_LINE (FT);
          PUT (FT, 'X');
          PG_NUM := PAGE (FT);

          NEW_PAGE (FT);

          IF COL(FT) /= 1 THEN
               FAILED ("COLUMN NUMBER NOT RESET - OUTPUT - 1");
          END IF;
          IF LINE (FT) /= 1 THEN
               FAILED ("LINE NUMBER NOT RESET - OUTPUT - 1");
          END IF;
          IF PAGE (FT) /= PG_NUM + 1 THEN
               FAILED ("PAGE NUMBER NOT INCREMENTED - OUTPUT - 1");
          END IF;

          PUT (FT, "MORE STUFF");
          NEW_LINE (FT);
          NEW_PAGE (FT);

          IF COL(FT) /= 1 THEN
               FAILED ("COLUMN NUMBER NOT RESET - OUTPUT - 2");
          END IF;
          IF LINE (FT) /= 1 THEN
               FAILED ("LINE NUMBER NOT RESET - OUTPUT - 2");
          END IF;
          IF PAGE (FT) /= PG_NUM + 2 THEN
               FAILED ("PAGE NUMBER NOT INCREMENTED - OUTPUT - 2");
          END IF;

          CHECK_FILE (FT, "STRING#X#@MORE STUFF#@%");

          CLOSE (FT);

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3405D;
