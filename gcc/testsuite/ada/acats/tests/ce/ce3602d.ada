-- CE3602D.ADA

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
--     CHECK THAT FILES ARE OF MODE IN_FILE AND THAT WHEN NO FILE IS
--     SPECIFIED THAT CURRENT DEFAULT INPUT FILE IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 10/06/82
--     SPS 12/17/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  REMOVED DEPENDENCE ON RESET AND CORRECTED
--                   EXCEPTION HANDLING.


WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3602D IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3602D", "CHECK THAT GET FOR STRINGS AND CHARACTERS " &
                      "OPERATES ON IN_FILE FILES");

     DECLARE
          FT , FILE : FILE_TYPE;
          X : CHARACTER;
          ST: STRING (1 .. 3);
     BEGIN

-- CREATE AND INITIALIZE FILES

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
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT " &
                            "CREATE");
                    RAISE INCOMPLETE;
          END;

          PUT (FT, "ABCE");
          NEW_LINE (FT);
          PUT (FT, "EFGHIJKLM");

          CLOSE (FT);

          BEGIN
               OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN " &
                                    "WITH IN_FILE MODE - 1");
                    RAISE INCOMPLETE;
          END;

          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME(2));

          PUT (FILE, "STRING");
          NEW_LINE (FILE);
          PUT (FILE, "END OF OUTPUT");

          CLOSE (FILE);

          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME(2));

          SET_INPUT (FILE);

-- BEGIN TEST

          GET (FT, X);
          IF X /= IDENT_CHAR ('A') THEN
               FAILED ("CHARACTER FROM FILE INCORRECT, WAS '" &
                       X & "'");
          END IF;

          GET (FT, ST);
          IF ST /= "BCE" THEN
               FAILED ("STRING FROM FILE INCORRECT; WAS """ &
                       ST & """");
          END IF;

          GET (X);
          IF X /= IDENT_CHAR ('S') THEN
               FAILED ("CHARACTER FROM DEFAULT INCORRECT; WAS '" &
                       X & "'");
          END IF;

          GET (ST);
          IF ST /= "TRI" THEN
               FAILED ("STRING FROM DEFAULT INCORRECT; WAS """ &
                       ST & """");
          END IF;

          BEGIN
               DELETE (FT);
               DELETE (FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;

     END;

     RESULT;

END CE3602D;
