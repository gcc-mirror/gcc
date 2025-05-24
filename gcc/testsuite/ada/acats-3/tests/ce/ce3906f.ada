-- CE3906F.ADA

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
--     CHECK THAT THE SET PARAMETER AFFECTS THE CASE OF IDENTIFIERS,
--     BUT NOT CHARACTER LITERALS.  CHECK THAT CHARACTER LITERALS ARE
--     ENCLOSED IN APOSTROPHES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     JBG 12/30/82
--     VKG 01/12/83
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/18/87  CORRECTED EXCEPTION HANDLING.

WITH TEXT_IO; USE TEXT_IO;
WITH REPORT; USE REPORT;
WITH CHECK_FILE;

PROCEDURE CE3906F IS

     TYPE ENUM IS (REDISH,GREENISH,YELLOWISH);
     PACKAGE ENUM_IO IS NEW ENUMERATION_IO(ENUM);
     PACKAGE CHAR_IO IS NEW ENUMERATION_IO(CHARACTER);
     USE ENUM_IO; USE CHAR_IO;
     INCOMPLETE : EXCEPTION;
     FT : FILE_TYPE;

BEGIN

     TEST ("CE3906F", "CHECK THE CASE OF ENUMERATION IO OUTPUT");

     BEGIN
          CREATE (FT);
      EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED; TEXT " &
                               "CREATE FOR TEMP FILE WITH " &
                               "OUT_FILE MODE - 1");
               RAISE INCOMPLETE;
     END;

     IF ENUM_IO.DEFAULT_WIDTH /= 0 THEN
          FAILED ("INITIAL DEFAULT WIDTH INCORRECT");
     END IF;

     IF CHAR_IO.DEFAULT_SETTING /= UPPER_CASE THEN
          FAILED ("INITIAL DEFAULT_SETTING INCORRECT");
     END IF;

     PUT (FT, 'A', SET => LOWER_CASE);
     NEW_LINE (FT);
     PUT (FT, 'a', SET => LOWER_CASE);
     NEW_LINE (FT);
     PUT (FT, REDISH, SET => LOWER_CASE);
     NEW_LINE (FT);
     ENUM_IO.DEFAULT_SETTING := LOWER_CASE;
     CHAR_IO.PUT (FT, 'C');
     NEW_LINE  (FT);
     CHAR_IO.PUT (FT, 'b');
     NEW_LINE (FT);
     PUT (FT, REDISH);
     NEW_LINE (FT);
     PUT (FT, GREENISH, SET => LOWER_CASE);
     NEW_LINE (FT);
     PUT (FT, YELLOWISH, SET => UPPER_CASE);

     CHECK_FILE (FT, "'A'#'a'#redish#'C'#'b'#redish#greenish#"
                   & "YELLOWISH#@%");

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3906F;
