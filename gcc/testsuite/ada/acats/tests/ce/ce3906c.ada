-- CE3906C.ADA

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
--      CHECK THAT PUT FOR ENUMERATION TYPES OUTPUTS THE ENUMERATION
--      LITERAL WITH NO TRAILING OR PRECEDING BLANKS WHEN WIDTH IS
--      NOT SPECIFIED OR IS SPECIFIED TO BE LESS THAN OR EQUAL TO THE
--      LENGTH OF THE STRING.  CHECK THAT WHEN WIDTH IS SPECIFIED TO
--      BE GREATER THAN THE LENGTH OF THE STRING, TRAILING BLANKS ARE
--      OUTPUT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 10/08/82
--     SPS 01/03/83
--     VKG 01/07/83
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/18/87  REMOVED CALL TO CHECKFILE.  CLOSED AND REOPENED
--                   FILE AND CHECKED CONTENTS OF FILE USING
--                   ENUMERATION_IO GETS.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3906C IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3906C", "CHECK THAT ENUMERATION_IO PUT OUTPUTS " &
                      "ENUMERATION LITERALS CORRECTLY WITH AND " &
                      "WITHOUT WIDTH PARAMETERS");

     DECLARE
          FT : FILE_TYPE;
          TYPE MOOD IS (ANGRY, HAPPY, BORED, SAD);
          X : MOOD := BORED;
          PACKAGE MOOD_IO IS NEW ENUMERATION_IO (MOOD);
          CH : CHARACTER;
          USE MOOD_IO;
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

          DEFAULT_WIDTH := FIELD(IDENT_INT(5));

          IF DEFAULT_WIDTH /= FIELD(IDENT_INT(5)) THEN
               FAILED ("DEFAULT_WIDTH NOT SET CORRECTLY");
          END IF;

          PUT (FT, X, 3);                             -- BORED
          X := HAPPY;
          NEW_LINE(FT);
          PUT (FILE => FT, ITEM => X, WIDTH => 5);    -- HAPPY
          NEW_LINE (FT);
          PUT (FT, SAD, 5);                           -- SAD
          DEFAULT_WIDTH := FIELD(IDENT_INT(6));
          PUT (FT, X);                                -- HAPPY
          PUT (FT, SAD, 3);                           -- SAD
          NEW_LINE(FT);
          DEFAULT_WIDTH := FIELD(IDENT_INT(2));
          PUT (FT, SAD);                              -- SAD

          CLOSE (FT);

          BEGIN
               OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN FOR " &
                                    "IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          GET (FT, X);
          IF X /= BORED THEN
               FAILED ("BORED NOT READ CORRECTLY");
          END IF;

          GET (FT, X);
          IF X /= HAPPY THEN
               FAILED ("HAPPY NOT READ CORRECTLY - 1");
          END IF;

          SKIP_LINE (FT);

          GET (FT, X);
          IF X /= SAD THEN
               FAILED ("SAD NOT READ CORRECTLY - 1");
          END IF;

          GET (FT, CH);
          IF CH /= ' ' THEN
               FAILED ("BLANKS NOT POSITIONED CORRECTLY - 1");
          END IF;

          GET (FT, CH);
          IF CH /= ' ' THEN
               FAILED ("BLANKS NOT POSITIONED CORRECTLY - 2");
          END IF;

          GET (FT, X);
          IF X /= HAPPY THEN
               FAILED ("HAPPY NOT READ CORRECTLY - 2");
          END IF;

          GET (FT, CH);
          IF CH /= ' ' THEN
               FAILED ("BLANKS NOT POSITIONED CORRECTLY - 3");
          END IF;

          GET (FT, X);
          IF X /= SAD THEN
               FAILED ("SAD NOT READ CORRECTLY - 2");
          END IF;

          SKIP_LINE (FT);

          GET (FT, X);
          IF X /= SAD THEN
               FAILED ("SAD NOT READ CORRECTLY - 3");
          END IF;

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

END CE3906C;
