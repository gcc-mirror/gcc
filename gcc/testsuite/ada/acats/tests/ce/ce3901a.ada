-- CE3901A.ADA

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
--     CHECK THAT GET AND PUT FOR ENUMERATED TYPES RAISE STATUS ERROR
--     IF THE FILE IS NOT OPEN.

-- HISTORY:
--     SPS 10/07/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     DWC 09/16/87  ADDED AN ATTEMPT TO CREATE A FILE AND THEN
--                   RETESTED OBJECTIVE.
--     BCB 10/03/90  ADDED NAME_ERROR AS A CHOICE TO THE EXCEPTION
--                   HANDLER FOR CREATE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3901A IS
BEGIN

     TEST ("CE3901A", "CHECK THAT GET AND PUT FOR ENUMERATED TYPES " &
                      "RAISE STATUS ERROR IF THE FILE IS NOT OPEN.");

     DECLARE
          TYPE COLOR IS (RED, BLUE, GREEN, ORANGE, YELLOW);
          FT : FILE_TYPE;
          PACKAGE COLOR_IO IS NEW ENUMERATION_IO (COLOR);
          USE COLOR_IO;
          X : COLOR;
     BEGIN
          BEGIN
               PUT (FT, RED);
               FAILED ("STATUS_ERROR NOT RAISED - PUT - 1");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PUT - 1");
          END;

          BEGIN
               GET (FT, X);
               FAILED ("STATUS_ERROR NOT RAISED - GET - 1");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET - 1");
          END;

          BEGIN
               CREATE (FT, OUT_FILE, LEGAL_FILE_NAME);  -- THIS IS JUST
               CLOSE (FT);                   -- AN ATTEMPT TO CREATE A
          EXCEPTION                          -- FILE.  OBJECTIVE IS MET
               WHEN USE_ERROR                -- EITHER WAY.
                  | NAME_ERROR =>  NULL;
          END;

          BEGIN
               PUT (FT, RED);
               FAILED ("STATUS_ERROR NOT RAISED - PUT - 2");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PUT - 2");
          END;

          BEGIN
               GET (FT, X);
               FAILED ("STATUS_ERROR NOT RAISED - GET - 2");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET - 2");
          END;
     END;

     RESULT;

END CE3901A;
