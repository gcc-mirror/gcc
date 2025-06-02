-- CE3602C.ADA

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
--     CHECK THAT GET RAISES MODE_ERROR FOR FILES OF MODE OUT_FILE.

-- APPLICABILITY CRITEIRA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 08/31/82
--     SPS 12/17/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  CORRECTED EXCEPTION HANDLING AND CHECKED FOR
--                   USE_ERROR ON DELETE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3602C IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3602C", "CHECK THAT MODE_ERROR IS RAISED BY GET FOR " &
                      "FILES OF MODE OUT_FILE");

     DECLARE
          FILE1, FILE2 : FILE_TYPE;
          CH : CHARACTER;
          ST : STRING (1 .. 5);
     BEGIN

          BEGIN
               CREATE (FILE1, OUT_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                                    "FOR TEMPORARY FILE WITH " &
                                    "OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON " &
                            "TEXT CREATE - 1");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               CREATE (FILE2, OUT_FILE, LEGAL_FILE_NAME);
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
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT " &
                            "CREATE - 2");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               GET (FILE1, CH);
               FAILED ("MODE_ERROR NOT RAISED - GET CHAR UN-NAMED " &
                       "FILE");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET CHAR " &
                            "UN-NAMED FILE");
          END;

          BEGIN
               GET (FILE2, CH);
               FAILED ("MODE_ERROR NOT RAISED - GET CHAR NAMED FILE");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET CHAR " &
                            "NAMED FILE");
          END;

          BEGIN
               GET (STANDARD_OUTPUT, CH);
               FAILED ("MODE_ERROR NOT RAISED - GET CHAR " &
                       "STANDARD_OUTPUT");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET CHAR " &
                            "STANDARD_OUTPUT");
          END;

          BEGIN
               GET (CURRENT_OUTPUT, CH);
               FAILED ("MODE_ERROR NOT RAISED - GET CHAR " &
                       "CURRENT_OUTPUT");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET CHAR " &
                            "CURRENT_OUTPUT");
          END;

          BEGIN
               GET (FILE1, ST);
               FAILED ("MODE_ERROR NOT RAISED - GET STRING UN-NAMED " &
                       "FILE");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET STRING " &
                            "UN-NAMED FILE");
          END;

          BEGIN
               GET (FILE2, ST);
               FAILED ("MODE_ERROR NOT RAISED - GET STRING NAMED FILE");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET STRING " &
                            "NAMED FILE");
          END;

          BEGIN
               GET (STANDARD_OUTPUT, ST);
               FAILED ("MODE_ERROR NOT RAISED - GET STRING " &
                       "STANDARD_OUTPUT");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET STRING " &
                            "STANDARD_OUTPUT");
          END;

          BEGIN
               GET (CURRENT_OUTPUT, ST);
               FAILED ("MODE_ERROR NOT RAISED - GET STRING " &
                       "CURRENT_OUTPUT");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET STRING " &
                            "CURRENT_OUTPUT");
          END;

          CLOSE (FILE1);

          BEGIN
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

END CE3602C;
