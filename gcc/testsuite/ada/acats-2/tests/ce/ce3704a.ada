-- CE3704A.ADA

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
-- HISTORY:
--     CHECK THAT GET FOR INTEGER_IO CAN OPERATE ON ANY FILE OF MODE
--     IN_FILE AND THAT IF NO FILE IS SPECIFIED THE CURRENT DEFAULT
--     INPUT FILE IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/01/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  REMOVED UNNECESSARY CODE, CORRECTED EXCEPTION
--                   HANDLING, AND REMOVED DEPENDENCE ON RESET.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3704A IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3704A", "CHECK THAT GET FOR INTEGER_IO CAN OPERATE " &
                      "ON ANY FILE OF MODE IN_FILE AND THAT IF " &
                      "NO FILE IS SPECIFIED THE CURRENT DEFAULT " &
                      "INPUT FILE IS USED");

     DECLARE
          FT : FILE_TYPE;
          FT2: FILE_TYPE;
          TYPE NI IS NEW INTEGER RANGE 1 .. 700;
          X : NI;
          PACKAGE IIO IS NEW INTEGER_IO (NI);
          USE IIO;
     BEGIN

-- CREATE AND INITIALIZE DATA FILES

          BEGIN
               CREATE (FT, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT " &
                                    "CREATE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT " &
                                    "CREATE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          PUT (FT, '3');
          PUT (FT, '6');
          PUT (FT, '9');

          CLOSE (FT);

          BEGIN
               OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          CREATE (FT2, OUT_FILE, LEGAL_FILE_NAME(2));

          PUT (FT2, '6');
          PUT (FT2, '2');
          PUT (FT2, '4');

          CLOSE (FT2);
          OPEN (FT2, IN_FILE, LEGAL_FILE_NAME(2));

          SET_INPUT (FT2);

          GET (FT, X);

          IF X /= 369 THEN
               FAILED ("GET RETURNED WRONG VALUE; VALUE WAS" &
                       NI'IMAGE(X));
          END IF;

          GET (X);

          IF X /= 624 THEN
               FAILED ("GET FOR DEFAULT WAS WRONG; VALUE WAS" &
                       NI'IMAGE(X));
          END IF;

          BEGIN
               DELETE (FT);
               DELETE (FT2);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3704A;
