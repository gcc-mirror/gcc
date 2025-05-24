-- CE3804A.ADA

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
--     CHECK THAT GET FOR FLOAT_IO READS A PLUS OR MINUS SIGN
--     IF PRESENT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 09/07/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/11/87  CORRECTED EXCEPTION HANDLING AND REVISED IFS
--                   TO CHECK FOR CASE WHEN VALUE IS NEGATIVE OF WHAT
--                   IS EXPECTED.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3804A IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3804A", "CHECK THAT GET FOR FLOAT_IO READS A PLUS OR " &
                      "MINUS SIGN IF PRESENT");

     DECLARE
          FT : FILE_TYPE;
          TYPE FL IS NEW FLOAT RANGE -3.0 .. 3.0;
          X : FL;
          ST1 : CONSTANT STRING := IDENT_STR ("-3.0");
          ST2 : CONSTANT STRING := IDENT_STR ("+2.0");
          ST3 : CONSTANT STRING := IDENT_STR ("1.0");
     BEGIN

-- CREATE AND INITIALIZE DATA FILE

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

          PUT (FT, ST1);
          NEW_LINE(FT);
          PUT (FT, ST2);
          NEW_LINE(FT);
          PUT (FT, ST3);
          NEW_LINE(FT);
          CLOSE (FT);

-- BEGIN TEST

          DECLARE
               PACKAGE FL_IO IS NEW FLOAT_IO (FL);
               USE FL_IO;
               LST : POSITIVE;
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
               IF X = 3.0 THEN
                    FAILED ("MINUS SIGN NOT READ - 1");
               ELSIF X /= -3.0 THEN
                    FAILED ("INCORRECT VALUE READ - 1");
               END IF;

               GET (FT, X);
               IF X = -2.0 THEN
                    FAILED ("PLUS SIGN NOT READ - 2");
               ELSIF X /= +2.0 THEN
                    FAILED ("INCORRECT VALUE READ - 2");
               END IF;

               GET (FT, X);
               IF X /= 1.0 THEN
                    FAILED ("INCORRECT VALUE READ - 3");
               END IF;

               GET (ST1, X, LST);
               IF X = 3.0 THEN
                    FAILED ("MINUS SIGN NOT READ - 4");
               ELSIF X /= -3.0 THEN
                    FAILED ("INCORRECT VALUE READ - 4");
               END IF;

               GET (ST2, X, LST);
               IF X = -2.0 THEN
                    FAILED ("PLUS SIGN NOT READ - 5");
               ELSIF X /= +2.0 THEN
                    FAILED ("INCORRECT VALUE READ - 5");
               END IF;

               GET (ST3, X, LST);
               IF X /= 1.0 THEN
                    FAILED ("INCORRECT VALUE READ - 6");
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

END CE3804A;
