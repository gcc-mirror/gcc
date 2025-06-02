-- CE3906B.ADA

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
--     CHECK THAT PUT FOR ENUMERATION TYPES RAISES MODE_ERROR WHEN
--     APPLIED TO FILES OF MODE IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/08/82
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/17/87  REMOVED DEPENDENCY ON RESET AND CORRECTED
--                   EXCEPTION HANDLERS.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3906B IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3906B", "CHECK THAT PUT FOR ENUMERATION TYPES RAISES " &
                      "MODE_ERROR WHEN APPLIED TO FILES OF MODE " &
                      "IN_FILE");

     DECLARE
          FT : FILE_TYPE;
          TYPE FLOWER IS (ROSE, DAISY, SNAPDRAGON, VIOLET, CARNATION);
          PACKAGE FLOWER_IO IS NEW ENUMERATION_IO (FLOWER);
          USE FLOWER_IO;
          X : FLOWER := DAISY;
     BEGIN

          BEGIN
               CREATE (FT, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          PUT (FT, X);

          CLOSE (FT);

          BEGIN
               OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               PUT (FT, X);
               FAILED ("MODE_ERROR NOT RAISED - FILE");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FILE");
          END;

          BEGIN
               PUT (STANDARD_INPUT, X);
               FAILED ("MODE_ERROR NOT RAISED - STANDARD_INPUT");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - STANDARD_INPUT");
          END;

          BEGIN
               PUT (CURRENT_INPUT, X);
               FAILED ("MODE_ERROR NOT RAISED - CURRENT_INPUT");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CURRENT_INPUT");
          END;

          BEGIN
               DELETE (FT);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3906B;
