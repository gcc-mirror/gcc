-- CE3804M.ADA

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
--     VKG 02/07/83
--     JBG 03/30/84
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/14/87  SPLIT CASE FOR FIXED_IO INTO CE3804N.ADA AND
--                   CORRECTED EXCEPTION HANDLING.

WITH TEXT_IO; USE TEXT_IO;
WITH REPORT; USE REPORT;

PROCEDURE CE3804M IS

     INCOMPLETE : EXCEPTION;

BEGIN
     TEST ("CE3804M", "CHECK THAT FLOAT_IO GET WILL RAISE " &
                      "DATA_ERROR IF THE USE OF # AND : IN " &
                      "BASED LITERALS IS MIXED");

     DECLARE
          FT : FILE_TYPE;
     BEGIN

          BEGIN
               CREATE (FT, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT " &
                                    "CREATE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT " &
                                    "CREATE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;


          PUT_LINE (FT, "2#1.1#E+2");   -- 2#1.1#E+2
          PUT_LINE (FT, "8:1.1:E-2");   -- 8:1.1:E-2
          PUT (FT, "2#1.1:E+1");        -- 2#1.1:E+1
          NEW_LINE (FT);
          PUT (FT, "4:2.23#E+2");       -- 4:2.23#E+2
          NEW_LINE (FT);
          PUT (FT, "2#1.0#E+1");        -- 2#1.0#E+1
          NEW_LINE (FT);
          CLOSE (FT);

          DECLARE
               PACKAGE FL_IO IS NEW FLOAT_IO(FLOAT);
               USE FL_IO;
               X : FLOAT := 1.00E+10;
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
               IF X /= 2#1.1#E+2 THEN
                    FAILED ("DID NOT GET RIGHT VALUE - 1");
               END IF;

               GET (FT, X);
               IF X /= 8#1.1#E-2 THEN
                    FAILED ("DID NOT GET RIGHT VALUE - 2");
               END IF;

               BEGIN
                    X := 1.0E+10;
                    GET (FT,X);
                    FAILED ("DATA_ERROR NOT RAISED - 1");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 1.00E+10 THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 1");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 1");
               END;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT,X);
                    FAILED ("DATA_ERROR NOT RAISED - 2");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 1.00E+10 THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - 2");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 2");
               END;

               SKIP_LINE (FT);

               GET (FT, X);
               IF X /= 2#1.0#E+1 THEN
                    FAILED ("DID NOT GET RIGHT VALUE - 3");
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

END CE3804M;
