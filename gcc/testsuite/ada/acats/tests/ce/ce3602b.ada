-- CE3602B.ADA

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
--     CHECK THAT GET (FOR CHARACTER AND STRINGS) PROPERLY SETS THE
--     PAGE, LINE, AND COLUMN NUMBERS AFTER EACH OPERATION.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 08/30/82
--     SPS 12/17/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/04/87  REMOVED DEPENDENCE ON UNBOUNDED LINE LENGTH AND
--                   CORRECTED EXCEPTION HANDLING.
--     BCB 11/13/87  GAVE SET_LINE_LENGTH PROCEDURE THE FILE VARIABLE
--                   AS A PARAMETER.  REMOVED LINE WHICH SAVED AND
--                   RESTORED THE LINE LENGTH.


WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;
WITH CHECK_FILE;

PROCEDURE CE3602B IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3602B", "CHECK THAT GET PROPERLY SETS PAGE, LINE, AND " &
                      "COLUMN NUMBERS");

     DECLARE
          FILE1 : FILE_TYPE;
          LINE1 : CONSTANT STRING := "LINE ONE OF TEST DATA FILE";
          LINE2 : CONSTANT STRING := "LINE TWO";
          LINE3 : CONSTANT STRING := "LINE THREE";
          CN, LN : POSITIVE_COUNT;
          CH : CHARACTER;
          ST: STRING (1 .. 5);
          ORIGINAL_LINE_LENGTH : COUNT;

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
          SET_LINE_LENGTH (FILE1, LINE1'LENGTH);

          PUT (FILE1, LINE1);
          SET_LINE_LENGTH (FILE1, LINE2'LENGTH);
          PUT (FILE1, LINE2);
          NEW_LINE (FILE1, 2);
          NEW_PAGE (FILE1);
          SET_LINE_LENGTH (FILE1, LINE3'LENGTH);
          PUT (FILE1, LINE3);
          CLOSE (FILE1);

-- BEGIN TEST

          BEGIN
               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          IF COL (FILE1) /= 1 THEN
               FAILED ("COLUMN NUMBER NOT INITIALLY ONE");
          END IF;

          IF LINE (FILE1) /= 1 THEN
               FAILED ("LINE NUMBER NOT INITIALLY ONE");
          END IF;

          IF PAGE (FILE1) /= 1 THEN
               FAILED ("PAGE NUMBER NOT INITIALLY ONE");
          END IF;

-- TEST COLUMN NUMBER FOR CHARACTER

          GET (FILE1, CH);
          IF CH /= 'L' THEN
               FAILED ("CHARACTER NOT EQUAL TO L - 1");
          END IF;
          CN := COL (FILE1);
          IF CN /= 2 THEN
               FAILED ("COLUMN NUMBER NOT SET CORRECTLY " &
                       "- GET CHARACTER.  COL NUMBER IS" &
                       COUNT'IMAGE(CN));
          END IF;

-- TEST COLUMN NUMBER FOR STRING

          GET (FILE1, ST);
          CN := COL (FILE1);
          IF CN /= 7 THEN
               FAILED ("COLUMN NUMBER NOT SET CORRECTLY " &
                       "- GET STRING.  COL NUMBER IS" &
                       COUNT'IMAGE(CN));
          END IF;

-- POSITION CURRENT INDEX TO END OF LINE

          WHILE NOT END_OF_LINE (FILE1) LOOP
               GET (FILE1, CH);
          END LOOP;

          IF CH /= 'E' THEN
               FAILED ("CHARACTER NOT EQUAL TO E");
          END IF;

-- TEST LINE NUMBER FOR CHARACTER

          GET(FILE1, CH);
          IF CH /= 'L' THEN
               FAILED ("CHARACTER NOT EQUAL TO L - 2");
          END IF;
          LN := LINE (FILE1);
          IF LN /= 2 THEN
               FAILED ("LINE NUMBER NOT SET CORRECTLY " &
                       "- GET CHARACTER.  LINE NUMBER IS" &
                       COUNT'IMAGE(LN));
          END IF;
          IF PAGE (FILE1) /= POSITIVE_COUNT(IDENT_INT(1)) THEN
               FAILED ("PAGE NUMBER NOT CORRECT - 1.  PAGE IS" &
                       COUNT'IMAGE(PAGE(FILE1)));
          END IF;

-- TEST LINE NUMBER FOR STRING

          WHILE NOT END_OF_LINE (FILE1) LOOP
               GET (FILE1, CH);
          END LOOP;
          GET (FILE1, ST);
          IF ST /= "LINE " THEN
               FAILED ("INCORRECT VALUE READ - ST");
          END IF;
          LN := LINE (FILE1);
          CN := COL (FILE1);
          IF CN /= 6 THEN
               FAILED ("COLUMN NUMBER NOT SET CORRECTLY " &
                       "- GET STRING.  COL NUMBER IS" &
                       COUNT'IMAGE(CN));
          END IF;
          IF LN /= 1 THEN
               FAILED ("LINE NUMBER NOT SET CORRECTLY " &
                       "- GET STRING.  LINE NUMBER IS" &
                       COUNT'IMAGE(LN));
          END IF;
          IF PAGE (FILE1) /= POSITIVE_COUNT(IDENT_INT(2)) THEN
               FAILED ("PAGE NUMBER NOT CORRECT - 2.  PAGE IS" &
                       COUNT'IMAGE(PAGE(FILE1)));
          END IF;

          BEGIN
               DELETE (FILE1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;

     END;

     RESULT;

END CE3602B;
