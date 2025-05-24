-- CE3804I.ADA

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
--     CHECK THAT FLOAT_IO GET OPERATES ON IN_FILE FILE AND WHEN
--     NO FILE IS SPECIFIED THE CURRENT DEFAULT INPUT FILE IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/06/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/14/87  SPLIT CASE FOR FIXED_IO INTO CE3804J.ADA AND
--                   CORRECTED EXCEPTION HANDLING.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3804I IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3804I", "CHECK THAT FLOAT_IO GET OPERATES ON " &
                      "IN_FILE FILE AND WHEN NO FILE IS " &
                      "SPECIFIED THE CURRENT DEFAULT INPUT " &
                      "FILE IS USED.");

     DECLARE
          FT1, FT2 : FILE_TYPE;
     BEGIN

-- CREATE AND INITIALIZE FILES

          BEGIN
               CREATE (FT1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT " &
                                    "CREATE WITH OUT_FILE MODE - 1");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT " &
                                    "CREATE WITH OUT_FILE MODE - 1");
                    RAISE INCOMPLETE;
          END;

          CREATE (FT2, OUT_FILE, LEGAL_FILE_NAME(2));

          PUT (FT1, "1.0");
          NEW_LINE (FT1);

          CLOSE (FT1);

          BEGIN
               OPEN (FT1, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN " &
                                    "FOR IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          PUT (FT2, "2.0");
          NEW_LINE (FT2);

          CLOSE (FT2);
          OPEN (FT2, IN_FILE, LEGAL_FILE_NAME(2));

          SET_INPUT (FT2);

          DECLARE
               TYPE FL IS NEW FLOAT;
               PACKAGE FLIO IS NEW FLOAT_IO (FL);
               USE FLIO;
               X : FL;
          BEGIN
               BEGIN
                    GET (FT1, X);
                    IF X /= 1.0 THEN
                         FAILED ("FLOAT FILE VALUE INCORRECT");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED - FILE FLOAT");
               END;

               BEGIN
                    GET (X);
                    IF X /= 2.0 THEN
                         FAILED ("FLOAT DEFAULT VALUE INCORRECT");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED - DEFAULT FLOAT");
               END;
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

END CE3804I;
