-- CE3602A.ADA

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
--     CHECK THAT GET FOR CHARACTERS AND STRINGS ALLOW A STRING TO SPAN
--     OVER MORE THAN ONE LINE, SKIPPING INTERVENING LINE AND PAGE
--     TERMINATORS.  ALSO CHECK THAT GET ACCEPTS A NULL STRING ACTUAL
--     PARAMETER AND A STRING SLICE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 08/30/82
--     VKG 01/26/83
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/04/87  REMOVED DEPENDENCE ON RESET, CORRECTED EXCEPTION
--                   HANDLING, AND ADDED NEW CASES FOR OBJECTIVE.


WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3602A IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3602A", "CHECK THAT GET FOR CHARACTERS AND STRINGS " &
                      "ALLOWS A STRING TO SPAN OVER MORE THAN ONE " &
                      "LINE, SKIPPING INTERVENING LINE AND PAGE " &
                      "TERMINATORS.  ALSO CHECK THAT GET ACCEPTS " &
                      "A NULL STRING ACTUAL PARAMETER AND A STRING " &
                      "SLICE");

     DECLARE
          FILE1 : FILE_TYPE;
          ST : STRING (1 .. 40);
          STR: STRING (1 .. 100);
          NST: STRING (1 .. 0);
          ORIGINAL_LINE_LENGTH : COUNT;

-- READ_CHARS RETURNS A STRING OF N CHARACTERS FROM A GIVEN FILE.

          FUNCTION READ_CHARS (FILE : FILE_TYPE;
                               N    : NATURAL )
                               RETURN STRING IS
          C: CHARACTER;
          BEGIN
               IF N = 0 THEN RETURN "";
               ELSE
                    GET (FILE,C);
                    RETURN C&READ_CHARS (FILE,N-1);
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("ERROR ON READ_CHARS");
          END READ_CHARS;


     BEGIN

-- CREATE AND INITIALIZE TEST DATA FILE

          BEGIN
               CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT " &
                                    "CREATE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON " &
                            "TEXT CREATE");
                    RAISE INCOMPLETE;
          END;

          ORIGINAL_LINE_LENGTH := LINE_LENGTH;

-- LINE_LENGTH SET IN CASE IMPLEMENTATION REQUIRES BOUNDED LENGTH LINES

          SET_LINE_LENGTH (16);
          PUT (FILE1, "THIS LINE SHALL ");
          SET_LINE_LENGTH (10);
          PUT (FILE1, "SPAN OVER ");
          SET_LINE_LENGTH (14);
          PUT (FILE1, "SEVERAL LINES.");
          CLOSE (FILE1);
          SET_LINE_LENGTH (ORIGINAL_LINE_LENGTH);


-- BEGIN TEST

          BEGIN

               BEGIN
                    OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT " &
                                         "OPEN WITH IN_FILE MODE - 1");
                         RAISE INCOMPLETE;
               END;

               STR(1..40) := READ_CHARS (FILE1, 40);
               CLOSE (FILE1);

               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);

               GET (FILE1, ST);
               IF STR(1..40) /= ST THEN
                    FAILED ("GET FOR STRING INCORRECT");
               END IF;

               IF STR(1..40) /= "THIS LINE SHALL SPAN OVER SEVERAL " &
                                "LINES." THEN
                    FAILED ("INCORRECT VALUE READ");
               END IF;

-- GET NULL STRING

               CLOSE (FILE1);

               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);

               BEGIN
                    GET (FILE1, NST);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED (" GET FAILED ON NULL STRING");
               END;

-- GET NULL SLICE

               BEGIN
                    GET (FILE1, STR (10 .. 1));
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("GET FAILED ON A NULL SLICE");
               END;

               BEGIN
                    DELETE (FILE1);
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

END CE3602A;
