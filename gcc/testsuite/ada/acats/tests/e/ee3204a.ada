-- EE3204A.ADA

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
--     CHECK THAT AFTER THE DEFAULT FILES HAVE BEEN REDEFINED,
--     OUTPUT ON THE STANDARD FILES IS STILL PROPERLY HANDLED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- PASS/FAIL CRITERIA:
--     THIS TEST IS PASSED IF IT EXECUTES, PRINTS TENTATIVELY PASSED,
--     AND THE CONTENTS OF THE STANDARD OUTPUT FILE ARE CORRECT.

-- HISTORY:
--     JLH 07/08/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE EE3204A IS

     FILE1, FILE2 : FILE_TYPE;
     ITEM : CHARACTER := 'B';
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("EE3204A", "CHECK THAT AFTER THE DEFAULT FILES HAVE BEEN " &
                      "REDEFINED, OUTPUT ON THE STANDARD " &
                      "FILES IS STILL PROPERLY HANDLED");

     BEGIN

          BEGIN
               CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                                    "WITH MODE OUT_FILE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE " &
                                    "WITH MODE OUT_FILE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          CREATE (FILE2, OUT_FILE, LEGAL_FILE_NAME(2));
          PUT (FILE2, 'A');
          NEW_LINE (FILE2);
          PUT (FILE2, 'B');

          CLOSE (FILE2);

          BEGIN
               OPEN (FILE2, IN_FILE, LEGAL_FILE_NAME(2));
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON OPEN WITH " &
                                    "WITH MODE IN_FILE");
                    RAISE INCOMPLETE;
          END;

          SET_INPUT (FILE2);

          GET (ITEM);
          IF ITEM /= 'A' THEN
               FAILED ("INCORRECT VALUE READ FROM DEFAULT FILE");
          END IF;

          SET_OUTPUT (FILE1);

          PUT ("THIS TEST FAILS IF THIS APPEARS IN STANDARD OUTPUT");
          NEW_LINE;
          PUT ("THIS TEST FAILS IF THIS APPEARS IN STANDARD OUTPUT");

          PUT (STANDARD_OUTPUT, "FIRST LINE OF INPUT");
          NEW_LINE (STANDARD_OUTPUT);
          PUT (STANDARD_OUTPUT, "SECOND LINE OF INPUT");

          SPECIAL_ACTION ("CHECK THAT THE CONTENTS OF THE STANDARD " &
                          "OUTPUT FILE ARE CORRECT");
          SPECIAL_ACTION ("IT SHOULD CONTAIN:");
          SPECIAL_ACTION ("TEST HEADER LINES");
          SPECIAL_ACTION ("FIRST LINE OF INPUT");
          SPECIAL_ACTION ("SECOND LINE OF INPUT");

          BEGIN
               DELETE (FILE1);
               DELETE (FILE2);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END EE3204A;
