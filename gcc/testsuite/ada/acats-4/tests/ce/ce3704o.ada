-- CE3704O.ADA

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
--     CHECK THAT GET WILL RAISE DATA_ERROR IF THE USE OF # AND :
--     IN BASED LITERALS IS MIXED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     VKG 02/10/83
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/11/87  REMOVED UNNECESSARY CODE AND CORRECTED
--                   EXCEPTION HANDLING.

WITH TEXT_IO; USE TEXT_IO;
WITH REPORT; USE REPORT;

PROCEDURE CE3704O IS

     INCOMPLETE : EXCEPTION;

BEGIN
     TEST ("CE3704O", "CHECK THAT MIXED USE OF # AND : " &
                      "IN BASED LITERALS WILL RAISE DATA_ERROR");

     DECLARE
          FT : FILE_TYPE;
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


          PUT_LINE (FT, "8#77#E+1");
          PUT_LINE (FT, "2:110:");
          PUT (FT, "2#11:");
          NEW_LINE (FT);
          PUT (FT, "4:223#");
          NEW_LINE (FT);
          CLOSE (FT);


          DECLARE
               PACKAGE INT_IO IS NEW INTEGER_IO(INTEGER);
               USE INT_IO;
               X : INTEGER := 100;
               CH : CHARACTER;
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
               IF X /= 8#77#E+1 THEN
                    FAILED ("INCORRECT VALUE - 1");
               END IF;

               GET (FT, X);
               IF X /= 2#110# THEN
                    FAILED ("INCORRECT VALUE - 2");
               END IF;

               BEGIN
                    X := 100;
                    GET (FT,X);
                    FAILED ("DATA_ERROR NOT RAISED - 1");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 100 THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 1");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED");
               END;

               IF NOT END_OF_LINE (FT) THEN
                    GET (FT, CH);
                    IF CH /= ':' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION - 1");
                    END IF;
               END IF;

               BEGIN
                    X := 100;
                    GET (FT,X);
                    FAILED ("DATA_ERROR NOT RAISED - 2");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 100 THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 2");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 2");
               END;

               IF NOT END_OF_LINE (FT) THEN
                    GET (FT, CH);
                         IF CH /='#' THEN
                              FAILED ("GET STOPPED AT WRONG " &
                                      "POSITION - 1");
                         END IF;
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

END CE3704O;
