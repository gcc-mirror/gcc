-- CE3806A.ADA

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
--     CHECK THAT PUT FOR FLOAT_IO RAISES MODE_ERROR FOR FILES OF
--     MODE IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/10/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/11/87  REMOVED DEPENDENCE ON RESET AND CORRECTED
--                   EXCEPTION HANDLING.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3806A IS

BEGIN

     TEST ("CE3806A", "CHECK THAT PUT FOR FLOAT_IO RAISES MODE_ERROR " &
                      "FOR FILES OF MODE IN_FILE");

     DECLARE
          FT1 : FILE_TYPE;
          PACKAGE FL_IO IS NEW FLOAT_IO (FLOAT);
          USE FL_IO;
          INCOMPLETE : EXCEPTION;
          X : FLOAT := -34.267/19.2;

     BEGIN

          BEGIN
               CREATE (FT1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT " &
                                    "CREATE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          PUT (FT1, 'A');
          CLOSE (FT1);

          BEGIN
               OPEN (FT1, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               PUT (FT1, X);
               FAILED ("MODE_ERROR NOT RAISED - 1");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 1");
          END;

          BEGIN
               PUT (STANDARD_INPUT, X);
               FAILED ("MODE_ERROR NOT RAISED - 2");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 2");
          END;

          BEGIN
               PUT (CURRENT_INPUT, X);
               FAILED ("MODE_ERROR NOT RAISED - 3");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 3");
          END;

          BEGIN
               DELETE (FT1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;

     END;

     RESULT;

END CE3806A;
