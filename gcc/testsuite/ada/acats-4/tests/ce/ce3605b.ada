-- CE3605B.ADA

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
-- OBJECTIVE;
--     CHECK THAT PUT OUTPUTS A LINE TERMINATOR, RESETS THE COLUMN
--     NUMBER AND INCREMENTS THE LINE NUMBER WHEN THE LINE LENGTH IS
--     BOUNDED AND THE COLUMN NUMBER EQUALS THE LINE LENGTH WHEN PUT
--     IS CALLED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/02/82
--     JBG 12/28/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  GAVE FILE A NAME AND REMOVED CODE WHICH RESETS
--                   THE FILE.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;
PROCEDURE CE3605B IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3605B", "CHECK THAT PUT PROPERLY MAINTAINS THE " &
                      "LINE NUMBER AND COLUMN NUMBER WHEN THE " &
                      "LINE LENGTH IS BOUNDED");

     DECLARE
          FILE1 : FILE_TYPE;
          LN_CNT : POSITIVE_COUNT;
     BEGIN

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

          SET_LINE_LENGTH (FILE1, 5);
          LN_CNT := LINE (FILE1);

          FOR I IN 1 .. 5 LOOP
               PUT (FILE1, 'X');
          END LOOP;

          IF COL(FILE1) /= 6 THEN
               FAILED ("COLUMN NUMBER NOT INCREMENTED - PUT; " &
                       "VALUE WAS" & COUNT'IMAGE(COL(FILE1)));
          END IF;

          IF LINE(FILE1) /= LN_CNT THEN
               FAILED ("LINE COUNT MODIFIED - PUT CHARACTER; " &
                       "VALUE WAS" & COUNT'IMAGE(LINE(FILE1)));
          END IF;

          PUT (FILE1, 'X');
          IF COL(FILE1) /= 2 THEN
               FAILED ("COLUMN NUMBER NOT RESET - PUT CHARACTER; " &
                       "VALUE WAS" & COUNT'IMAGE(COL(FILE1)));
          END IF;

          IF LINE(FILE1) /= LN_CNT + 1 THEN
               FAILED("LINE NUMBER NOT INCREMENTED - PUT CHARACTER; " &
                       "VALUE WAS" & COUNT'IMAGE(LINE(FILE1)));
          END IF;

          NEW_LINE (FILE1);

          SET_LINE_LENGTH (FILE1, 4);
          LN_CNT := LINE (FILE1);

          PUT (FILE1, "XXXX");

          IF COL(FILE1) /= 5 THEN
               FAILED ("COLUMN NUMBER NOT INCREMENTED - PUT STRING; " &
                       "VALUE WAS" & COUNT'IMAGE(COL(FILE1)));
          END IF;

          IF LINE (FILE1) /= LN_CNT THEN
               FAILED ("LINE NUMBER INCREMENTED - PUT STRING; " &
                       "VALUE WAS" & COUNT'IMAGE(LINE (FILE1)));
          END IF;

          PUT (FILE1, "STR");

          IF COL(FILE1) /= 4 THEN
               FAILED ("COLUMN NUMBER NOT SET CORRECTLY - PUT" &
                       "STRING; VALUE WAS" & COUNT'IMAGE(COL(FILE1)));
          END IF;

          IF LINE (FILE1) /= LN_CNT + 1 THEN
               FAILED ("LINE NUMBER NOT INCREMENTED - PUT STRING; " &
                       "VALUE WAS" & COUNT'IMAGE(LINE (FILE1)));
          END IF;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;

     END;

     RESULT;

END CE3605B;
