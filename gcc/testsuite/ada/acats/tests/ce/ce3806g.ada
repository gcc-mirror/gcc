-- CE3806G.ADA

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
--     CHECK THAT FIXED_IO PUT OPERATES ON FILES OF MODE OUT_FILE AND
--     IF NO FILE IS SPECIFIED THE CURRENT DEFAULT OUTPUT FILE IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     JLH 09/13/87  CREATED ORIGINAL TEST.
--     BCB 10/03/90  ADDED THE STATEMENT "RAISE INCOMPLETE;" TO
--                   NAME_ERROR EXCEPTION HANDLER.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3806G IS

BEGIN

     TEST ("CE3806G", "CHECK THAT FIXED_IO PUT OPERATES ON FILES " &
                      "OF MODE OUT_FILE AND IF NO FILE IS SPECIFIED " &
                      "THE CURRENT DEFAULT OUTPUT FILE IS USED");

     DECLARE
          FT1, FT2 : FILE_TYPE;
          TYPE FX IS DELTA 0.5 RANGE -10.0 .. 10.0;
          PACKAGE FXIO IS NEW FIXED_IO (FX);
          USE FXIO;
          INCOMPLETE : EXCEPTION;
          X : FX := -1.5;

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

          CREATE (FT2, OUT_FILE, LEGAL_FILE_NAME(2));

          SET_OUTPUT (FT2);

          BEGIN
               PUT (FT1, X);
               PUT (X + 1.0);

               CLOSE (FT1);

               BEGIN
                    OPEN (FT1, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT " &
                                         "OPEN WITH IN_FILE MODE");
                         RAISE INCOMPLETE;
               END;

               SET_OUTPUT (STANDARD_OUTPUT);

               CLOSE (FT2);

               OPEN (FT2, IN_FILE, LEGAL_FILE_NAME(2));

               X := 0.0;
               GET (FT1, X);
               IF X /= -1.5 THEN
                    FAILED ("VALUE INCORRECT - FIXED FROM FILE");
               END IF;
               X := 0.0;
               GET (FT2, X);
               IF X /= -0.5 THEN
                    FAILED ("VALUE INCORRECT - FIXED FROM DEFAULT");
               END IF;
          END;

          BEGIN
               DELETE (FT1);
               DELETE (FT2);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;

     END;

     RESULT;

END CE3806G;
