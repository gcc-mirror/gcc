-- CE3606B.ADA

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
--     CHECK THAT PUT_LINE WILL OUTPUT A LINE ON MORE THAN ONE LINE
--     WHEN THE LINE LENGTH IS BOUNDED, IF THE STRING IS GREATER
--     THAN THE LINE LENGTH.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEMPORARY TEXT FILES.

-- HISTORY:
--     SPS 09/02/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  REMOVED UNNECESSARY CODE AND CORRECTED
--                   EXCEPTION HANDLING.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;
WITH CHECK_FILE;
PROCEDURE CE3606B IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3606B", "CHECK THAT PUT_LINE WILL OUTPUT A LINE " &
                      "ON MORE THAN ONE LINE WHEN THE LINE " &
                      "LENGTH IS BOUNDED, IF THE STRING IS " &
                      "GREATER THAN THE LINE LENGTH");

     DECLARE
          FT : FILE_TYPE;
          LONG_LINE : CONSTANT STRING := "THIS LINE IS A LONG " &
               "LINE WHICH WHEN OUTPUT SHOULD SPAN OVER SEVERAL " &
               "LINES IN THE OUTPUT FILE";
     BEGIN

          BEGIN
               CREATE (FT);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                                    "FOR TEMP FILES WITH OUT_FILE " &
                                    "MODE");
                    RAISE INCOMPLETE;
          END;

          SET_LINE_LENGTH (FT, 10);

          PUT_LINE (FT, LONG_LINE);
          PUT_LINE (FT, "AA");

          CHECK_FILE (FT, "THIS LINE #" &
                          "IS A LONG #" &
                          "LINE WHICH#" &
                          " WHEN OUTP#" &
                          "UT SHOULD #" &
                          "SPAN OVER #" &
                          "SEVERAL LI#" &
                          "NES IN THE#" &
                          " OUTPUT FI#" &
                          "LE#" &
                          "AA#@%");

          CLOSE (FT);
     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE3606B;
