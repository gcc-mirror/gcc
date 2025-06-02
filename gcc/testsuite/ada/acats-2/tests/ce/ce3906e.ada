-- CE3906E.ADA

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
-- HISTORY:
--     CHECK THAT PUT FOR ENUMERATION TYPES RAISES LAYOUT_ERROR WHEN
--     THE NUMBER OF CHARACTERS TO BE OUTPUT EXCEEDS THE MAXIMUM LINE
--     LENGTH. CHECK THAT LAYOUT_ERROR IS NOT RAISED WHEN THE NUMBER
--     OF CHARACTERS TO BE OUTPUT DOES NOT EXCEED THE MAXIMUM LINE
--     LENGTH, BUT WHEN ADDED TO THE CURRENT COLUMN NUMBER, THE TOTAL
--     EXCEEDS THE MAXIMUM LINE LENGTH.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMETATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/11/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/18/87  CORRECTED EXCEPTION HANDLING.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;
WITH CHECK_FILE;

PROCEDURE CE3906E IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3906E", "CHECK THAT ENUMERATION_IO PUT RAISES " &
                      "LAYOUT_ERROR CORRECTLY");

     DECLARE
          FT : FILE_TYPE;
          TYPE COLOR IS (RED, BLU, YELLOW, ORANGE, RD);
          PACKAGE COLOR_IO IS NEW ENUMERATION_IO (COLOR);
          USE COLOR_IO;
          CRAYON : COLOR := ORANGE;
     BEGIN

          BEGIN
               CREATE (FT);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT " &
                                    "CREATE FOR TEMP FILES WITH " &
                                    "OUT_FILE MODE - 1");
                    RAISE INCOMPLETE;
          END;

          SET_LINE_LENGTH (FT, 5);

          BEGIN
               PUT (FT, CRAYON);
               FAILED("LAYOUT_ERROR NOT RAISED");
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED");
          END;

          PUT (FT, RED);

          PUT (FT, BLU);
          IF LINE (FT) /= 2 THEN
               FAILED ("PUT DID NOT CAUSE NEW_LINE EFFECT");
          END IF;

          PUT (FT, RD);

          CHECK_FILE (FT, "RED#" &
                          "BLURD#@%");

          CLOSE (FT);

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3906E;
