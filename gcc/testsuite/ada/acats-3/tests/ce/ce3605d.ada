-- CE3605D.ADA

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
--     CHECK THAT PUT DOES NOT RAISE LAYOUT_ERROR WHEN THE NUMBER OF
--     CHARACTERS TO BE OUTPUT EXCEEDS THE LINE LENGTH.
--     CHECK THAT PUT HAS THE EFFECT OF NEW_LINE (AS WELL AS
--     OUTPUTTING THE ITEM) WHEN THE NUMBER OF CHARACTERS TO BE OUTPUT
--     OVERFLOWS A BOUNDED LINE LENGTH.
--     CHECK THAT PUT WITH A NULL STRING PERFORMS NO OPERATION.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/02/82
--     JBG 12/28/82
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  CORRECTED EXCEPTION HANDLING.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;
WITH CHECK_FILE;
PROCEDURE CE3605D IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3605D", "CHECK THAT LAYOUT_ERROR IS NOT RAISED BY PUT " &
                      "FOR STRING");

     DECLARE
          FT : FILE_TYPE;
          LC : POSITIVE_COUNT;
     BEGIN

          BEGIN
               CREATE (FT, OUT_FILE, LEGAL_FILE_NAME);
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

          SET_LINE_LENGTH (FT, 5);

          BEGIN
               PUT (FT, "STRING");

               IF LINE(FT) /= 2 THEN
                    FAILED ("LINE COUNT WAS" & COUNT'IMAGE(LINE(FT)) &
                            " INSTEAD OF 2");
               END IF;

               IF COL(FT) /= 2 THEN
                    FAILED ("COLUMN COUNT WAS" & COUNT'IMAGE(COL(FT)) &
                            " INSTEAD OF 2");
               END IF;

          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    FAILED ("LAYOUT_ERROR RAISED - 1");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 1");

          END;

          PUT (FT, "NEW");

          IF LINE(FT) /= 2 THEN
               FAILED ("LINE COUNT WRONG - 2; WAS" &
                       COUNT'IMAGE(LINE(FT)) &
                       " INSTEAD OF 2");
          END IF;

          IF COL(FT) /= 5 THEN
               FAILED ("COL COUNT WRONG - 2; WAS" &
                       COUNT'IMAGE(COL(FT)) &
                       " INSTEAD OF 5");
          END IF;

          BEGIN
               PUT (FT, "STR");
               IF LINE (FT) /= 3 THEN
                    FAILED ("PUT STRING WHEN IN MIDDLE OF " &
                            "LINE DOES NOT HAVE EFFECT OF " &
                            "NEW_LINE; LINE COUNT IS" &
                            COUNT'IMAGE(LINE(FT)));
               END IF;

               IF COL(FT) /= 3 THEN
                    FAILED ("COL COUNT WRONG - 3; WAS" &
                            COUNT'IMAGE(COL(FT)) &
                            " INSTEAD OF 3");
               END IF;

          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    FAILED ("LAYOUT_ERROR RAISED - 2");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
          END;

          PUT (FT, "ING");

          IF LINE(FT) /= 3 THEN
               FAILED ("LINE COUNT WRONG - 3; WAS" &
                       COUNT'IMAGE(LINE(FT)) &
                       " INSTEAD OF 3");
          END IF;

          IF COL(FT) /= 6 THEN
               FAILED ("COL COUNT WRONG - 3;  WAS" &
                       COUNT'IMAGE(COL(FT)) &
                       " INSTEAD OF 6");
          END IF;

          BEGIN
               PUT (FT, "");

               IF LINE(FT) /= 3 THEN
                    FAILED ("LINE COUNT WRONG - 3; WAS" &
                            COUNT'IMAGE(LINE(FT)) &
                            " INSTEAD OF 3");
               END IF;

               IF COL(FT) /= 6 THEN
                    FAILED ("COL COUNT WRONG - 3;  WAS" &
                            COUNT'IMAGE(COL(FT)) &
                            " INSTEAD OF 6");
               END IF;

          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    FAILED ("LAYOUT_ERROR RAISED - 3");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 3");
          END;

          CHECK_FILE (FT,
                      "STRIN#" &
                      "GNEWS#" &
                      "TRING#@%");

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

END CE3605D;
