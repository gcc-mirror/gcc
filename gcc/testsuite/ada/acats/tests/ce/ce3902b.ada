-- CE3902B.ADA

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
--     CHECK THAT THE OPERATIONS IN GENERIC PACKAGE ENUMERATION_IO
--     ALL HAVE THE CORRECT PARAMETER NAMES.

-- HISTORY:
--     JLH 08/25/88  CREATED ORIGINAL TEST.
--     RJW 02/28/90  ADDED CODE TO PREVENT MODE_ERROR FROM BEING RAISED.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3902B IS

     TYPE COLOR IS (RED, BLUE, GREEN);
     PACKAGE COLOR_IO IS NEW ENUMERATION_IO (COLOR);
     USE COLOR_IO;

     FILE1 : FILE_TYPE;
     CRAYON : COLOR := RED;
     INDEX : POSITIVE;
     NUM : FIELD := 5;
     COLOR_STRING : STRING (1..5);
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3902B", "CHECK THAT THE OPERATIONS IN GENERIC PACKAGE " &
                      "ENUMERATION_IO ALL HAVE THE CORRECT PARAMETER " &
                      "NAMES");

     BEGIN

          BEGIN
               CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          SET_OUTPUT (FILE1);

          PUT (FILE => FILE1, ITEM => CRAYON, WIDTH => NUM,
               SET => UPPER_CASE);

          PUT (ITEM => GREEN, WIDTH => 5, SET => LOWER_CASE);

          PUT (TO => COLOR_STRING, ITEM => BLUE, SET => UPPER_CASE);

          CLOSE (FILE1);

          SET_OUTPUT (STANDARD_OUTPUT);

          BEGIN
               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON OPEN WITH " &
                                    "MODE IN_FILE");
                    RAISE INCOMPLETE;
          END;

          SET_INPUT (FILE1);

          GET (FILE => FILE1, ITEM => CRAYON);

          GET (ITEM => CRAYON);

          GET (FROM => COLOR_STRING, ITEM => CRAYON, LAST => INDEX);

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

END CE3902B;
