-- CE3404B.ADA

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
--     CHECK THAT END_OF_LINE OPERATES ON THE CURRENT DEFAULT INPUT FILE
--     IF NO FILE IS SPECIFIED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW  08/26/82
--     SPS  09/17/82
--     SPS  11/11/82
--     TBN  11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                    RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT  09/22/87  CREATED A NON-TEMP FILE, REMOVED DEPENDENCE ON
--                    RESET, AND CHECKED THE VALUE OF THE CHAR READ.

WITH REPORT;  USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3404B IS

     INCOMPLETE : EXCEPTION;
     MY_FILE    : FILE_TYPE;
     LOOP_COUNT : INTEGER := 0;
     BOOL       : BOOLEAN;
     CHAR       : CHARACTER := ('C');

BEGIN

     TEST ("CE3404B", "CHECK THAT END_OF_LINE OPERATES ON THE " &
                      "CURRENT DEFAULT INPUT FILE IF NO FILE " &
                      "IS SPECIFIED");

--   CREATE AND INITIALIZE THE FILE

     BEGIN
          CREATE (MY_FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                               "WITH OUT_FILE MODE - 1");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT CREATE " &
                               "WITH OUT_FILE MODE - 2");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE - 3");
               RAISE INCOMPLETE;
     END;

     FOR I IN 1..3 LOOP
          PUT (MY_FILE,CHAR);
     END LOOP;
     NEW_LINE (MY_FILE);
     PUT (MY_FILE,CHAR);

     CLOSE (MY_FILE);

     BEGIN
          OPEN (MY_FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE ERROR RAISED ON ATTEMPT TO " &
                               "RE-OPEN WITH MODE OF IN_FILE - 4");
               RAISE INCOMPLETE;
     END;

     SET_INPUT (MY_FILE);

--   START THE TEST

     LOOP
          GET (CHAR);
          IF CHAR /= 'C' THEN
               FAILED ("CHAR READ FROM FILE HAS WRONG VALUE - 5");
               RAISE INCOMPLETE;
          END IF;
     EXIT WHEN END_OF_LINE;
          LOOP_COUNT := LOOP_COUNT + 1;
          IF LOOP_COUNT > IDENT_INT (3) THEN
               FAILED ("END_OF_LINE ON DEFAULT INCORRECT - 6");
               EXIT;
          END IF;
     END LOOP;

     GET (CHAR);
     IF CHAR /= 'C' THEN
          FAILED ("FINAL CHAR READ FROM FILE HAS WRONG VALUE - 7");
     END IF;

     BEGIN
          DELETE (MY_FILE);
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3404B;
