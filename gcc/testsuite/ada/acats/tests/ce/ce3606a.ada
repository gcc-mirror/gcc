-- CE3606A.ADA

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
--     CHECK THAT PUT_LINE WILL OUTPUT A LINE TERMINATOR WHEN THE
--     STRING PARAMETER IS NULL.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEMPORARY TEXT FILES.

-- HISTORY:
--     SPS 09/02/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  REMOVED UNNECESSARY CODE AND CORRECTED
--                   EXCEPTION HANDLING.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;
WITH CHECK_FILE;
PROCEDURE CE3606A IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3606A", "PUT_LINE PUTS LINE TERMINATOR WHEN STRING " &
                      "IS NULL");

     DECLARE
          FT : FILE_TYPE;
          NS1 : STRING (1 .. 0);
          NS2 : STRING (3 .. 1);
          LC : POSITIVE_COUNT := 1;
     BEGIN

          BEGIN
               CREATE (FT);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                                    "FOR TEMP FILES WITH OUT_FILE " &
                                    "MODE");
                    RAISE INCOMPLETE;
          END;

          PUT_LINE (FT, NS1);
          IF LINE (FT) /= LC + 1 THEN
               FAILED ("PUT_LINE OF NULL STRING 1; LINE " &
                       "COUNT WAS" & COUNT'IMAGE(LINE(FT)));
          END IF;

          PUT_LINE (FT, NS2);
          IF LINE (FT) /= LC + 2 THEN
               FAILED ("PUT_LINE OF NULL STRING 2; LINE " &
                       "COUNT WAS" & COUNT'IMAGE(LINE(FT)));
          END IF;

          CHECK_FILE (FT, "##@%");

          CLOSE (FT);
     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3606A;
