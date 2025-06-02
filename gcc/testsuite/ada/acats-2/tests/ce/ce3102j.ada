-- CE3102J.ADA

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
--     CHECK THAT USE_ERROR IS RAISED WHEN OPENING A FILE OF MODE
--     IN_FILE, WHEN IN_FILE MODE IS NOT SUPPORTED FOR OPEN BY THE
--     IMPLEMENTATION FOR TEXT_IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH DO NOT
--     SUPPORT IN_FILE MODE FOR OPEN FOR TEXT_IO.

-- HISTORY:
--     JLH 08/12/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3102J IS

     FILE1 : FILE_TYPE;
     INCOMPLETE : EXCEPTION;
     RAISED_USE_ERROR : BOOLEAN := FALSE;
     VAR1 : CHARACTER := 'A';

BEGIN

     TEST ("CE3102J", "CHECK THAT USE_ERROR IS RAISED WHEN MODE " &
                      "IN_FILE IS NOT SUPPORTED FOR THE OPERATION " &
                      "OF OPEN FOR TEXT_IO");
     BEGIN
          BEGIN
               CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE FOR " &
                                    "OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE FOR " &
                                    "OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
                    RAISE INCOMPLETE;
          END;

          PUT (FILE1, VAR1);
          CLOSE (FILE1);

          BEGIN
               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
               NOT_APPLICABLE ("OPEN FOR IN_FILE MODE ALLOWED");
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON OPEN");
          END;

          IF IS_OPEN (FILE1) THEN
               BEGIN
                    DELETE (FILE1);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NULL;
               END;
          END IF;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3102J;
