-- CE3804O.ADA

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
--     CHECK THAT GET FOR FIXED_IO RAISES MODE_ERROR WHEN THE
--     MODE IS NOT IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     DWC 09/14/87  CREATED ORIGINAL TEST.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3804O IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3804O", "CHECK THAT GET FOR FIXED_IO RAISES " &
                      "MODE_ERROR WHEN THE MODE IS NOT IN_FILE");

     DECLARE
          FT: FILE_TYPE;
     BEGIN
          BEGIN
               CREATE (FT, OUT_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT " &
                                    "CREATE FOR TEMP FILES " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          DECLARE
               TYPE FIXED IS DELTA 0.25 RANGE 1.0 .. 3.0;
               PACKAGE FX_IO IS NEW FIXED_IO (FIXED);
               USE FX_IO;
               X : FIXED;
          BEGIN

               BEGIN
                    GET (FT, X);
                    FAILED ("MODE_ERROR NOT RAISED - FIXED " &
                            "UN-NAMED FILE");
               EXCEPTION
                    WHEN MODE_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - " &
                                 "FIXED UN-NAMED FILE");
               END;

               BEGIN
                    GET (STANDARD_OUTPUT, X);
                    FAILED ("MODE_ERROR NOT RAISED - FIXED " &
                            "STANDARD_OUTPUT");
               EXCEPTION
                    WHEN MODE_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - " &
                                 "FIXED STANDARD_OUTPUT");
               END;

               BEGIN
                    GET (CURRENT_OUTPUT, X);
                    FAILED ("MODE_ERROR NOT RAISED - FIXED " &
                            "CURRENT_OUTPUT");
               EXCEPTION
                    WHEN MODE_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - " &
                                 "FIXED CURRENT_OUTPUT");
               END;

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

END CE3804O;
