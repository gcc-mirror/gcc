-- CE3801A.ADA

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
--     CHECK THAT EACH FLOAT_IO OPERATION RAISES STATUS_ERROR WHEN
--     CALLED WITH A FILE PARAMETER DESIGNATING AN UN-OPEN FILE.

-- HISTORY:
--     SPS 09/07/82
--     SPS 12/22/82
--     DWC 09/11/87  CORRECTED EXCEPTION HANDLING AND REVISED IFS
--                   TO CHECK FOR CASE WHEN VALUE IS NEGATIVE OF
--                   WHAT IS EXPECTED.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3801A IS
BEGIN

     TEST ("CE3801A", "CHECK THAT EACH FLOAT_IO AND FIXED_IO " &
                      "OPERATION RAISES STATUS_ERROR WHEN CALLED " &
                      "WITH A FILE PARAMETER DESIGNATING AN " &
                      "UN-OPEN FILE");

     DECLARE
          TYPE FLT IS NEW FLOAT RANGE 1.0 .. 10.0;
          PACKAGE FLT_IO IS NEW FLOAT_IO (FLT);
          USE FLT_IO;
          X : FLT := FLT'FIRST;
          FT : FILE_TYPE;
     BEGIN

          BEGIN
               GET (FT, X);
               FAILED ("STATUS_ERROR NOT RAISED - GET FLOAT_IO - 1");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET " &
                            "FLOAT_IO - 1");
          END;

          BEGIN
               PUT (FT, X);
               FAILED ("STATUS_ERROR NOT RAISED - PUT FLOAT_IO - 1");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PUT " &
                            "FLOAT_IO - 1");
          END;

          BEGIN
               CREATE (FT, OUT_FILE);    -- THIS IS JUST AN ATTEMPT
               CLOSE (FT);               -- TO CREATE A FILE.
          EXCEPTION                      -- OBJECTIVE MET EITHER WAY.
               WHEN USE_ERROR =>
                    NULL;
          END;

          BEGIN
               GET (FT, X);
               FAILED ("STATUS_ERROR NOT RAISED - GET FLOAT_IO - 2");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET " &
                            "FLOAT_IO - 2");
          END;

          BEGIN
               PUT (FT, X);
               FAILED ("STATUS_ERROR NOT RAISED - PUT FLOAT_IO - 2");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PUT " &
                            "FLOAT_IO - 2");
          END;
     END;

     RESULT;

END CE3801A;
