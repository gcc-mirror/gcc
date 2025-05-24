-- CE3804D.ADA

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
--     CHECK THAT FLOAT_IO GET RAISES DATA_ERROR WHEN THE DATA
--     READ IS OUT-OF-RANGE.  CHECK THAT ITEM IS LEFT UNAFFECTED
--     AND THAT READING MAY CONTINUE AFTER THE EXCEPTION HAS
--     BEEN HANDLED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 09/07/82
--     SPS 02/10/83
--     JBG 08/30/83
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/11/87  REMOVED UNNECESSARY CODE AND CORRECTED
--                   EXCEPTION HANDLING.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3804D IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3804D", "FLOAT_IO GET RAISES DATA_ERROR FOR " &
                      "OUT-OF-RANGE DATA");

     DECLARE
          FT : FILE_TYPE;
     BEGIN

-- CREATE AND INITIALIZE TEST FILE

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

          PUT (FT, "1.25");
          NEW_LINE (FT);
          PUT (FT, "-7.5");
          NEW_LINE (FT);
          PUT (FT, "3.5");
          NEW_LINE (FT);
          PUT (FT, "2.5");
          NEW_LINE (FT);
          CLOSE (FT);

-- BEGIN TEST

          DECLARE
               TYPE FL IS NEW FLOAT RANGE 1.0 .. 3.0;
               PACKAGE FL_IO IS NEW FLOAT_IO (FL);
               X : FL;
               USE FL_IO;
          BEGIN

               BEGIN
                    OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR RAISED; TEXT " &
                                         "OPEN WITH IN_FILE MODE");
                         RAISE INCOMPLETE;
               END;

               GET (FT, X);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - 1");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 1.25 THEN
                              FAILED ("ITEM ALTERED WHEN DATA_ERROR " &
                                      "IS RAISED - 1");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 1");
               END;

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - 2");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 1.25 THEN
                              FAILED ("ITEM ALTERED WHEN DATA_ERROR " &
                                      "IS RAISED - 2");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 2");
               END;

               GET (FT, X);
               IF X /= 2.5 THEN
                    FAILED ("READING NOT CONTINUED CORRECTLY " &
                            "AFTER DATA_ERROR");
               END IF;

               BEGIN
                    DELETE (FT);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NULL;
               END;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3804D;
