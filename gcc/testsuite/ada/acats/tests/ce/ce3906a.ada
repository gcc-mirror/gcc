-- CE3906A.ADA

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
--     CHECK THAT PUT FOR ENUMERATION TYPES CAN OPERATE ON FILES OF
--     MODE OUT_FILE AND THAT WHEN NO FILE PARAMETER IS SPECIFIED
--     THE CURRENT DEFAULT OUTPUT FILE IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEMPORARY TEXT FILES.

-- HISTORY:
--     SPS 10/08/82
--     SPS 01/03/83
--     SPS 02/18/83
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/17/87  REMOVED UNNECESSARY CODE AND CORRECTED EXCEPTION
--                   HANDLING.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;
WITH CHECK_FILE;

PROCEDURE CE3906A IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3906A", "CHECK THAT PUT FOR ENUMERATION TYPES CAN " &
                      "OPERATE ON FILES OF MODE OUT_FILE AND THAT " &
                      "WHEN NO FILE PARAMETER IS SPECIFIED THE " &
                      "CURRENT DEFAULT OUTPUT FILE IS USED. CHECK " &
                      "THAT ENUMERATION_IO PUT OPERATES ON OUT_FILE " &
                      "FILES");

     DECLARE
          FT1, FT2 : FILE_TYPE;
          TYPE COLOR IS (ROSE, VANILLA, CHARCOAL, CHOCOLATE);
          CRAYON : COLOR := ROSE;
          PACKAGE COLOR_IO IS NEW ENUMERATION_IO (COLOR);
          USE COLOR_IO;
     BEGIN

          BEGIN
               CREATE (FT1, OUT_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                                    "FOR TEMP FILES WITH OUT_FILE " &
                                    "MODE - 1");
                    RAISE INCOMPLETE;
          END;

          CREATE (FT2, OUT_FILE);

          SET_OUTPUT (FT2);

          PUT (FT1, CRAYON);
          NEW_LINE (FT1);
          PUT (FT1, CHOCOLATE);

          CRAYON := CHARCOAL;

          PUT (CRAYON);
          NEW_LINE;
          PUT (VANILLA);

-- CHECK OUTPUT

          SET_OUTPUT (STANDARD_OUTPUT);
          COMMENT ("CHECKING FT1");
          CHECK_FILE (FT1, "ROSE#CHOCOLATE#@%");

          COMMENT ("CHECKING FT2");
          CHECK_FILE (FT2, "CHARCOAL#VANILLA#@%");

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3906A;
