-- CE3704E.ADA

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
--     CHECK THAT INTEGER_IO GET RAISES DATA_ERROR WHEN THE LEXICAL
--     ELEMENT IS NOT OF THE INTEGER TYPE EXPECTED.  CHECK THAT ITEM
--     IS UNAFFECTED AND READING CAN CONTINUE AFTER THE EXCEPTION
--     HAS BEEN HANDLED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/04/82
--     VKG 01/14/83
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/10/87  REMOVED UNNECCESSARY CODE, CORRECTED EXCEPTION
--                   HANDLING, AND CHECKED FOR USE_ERROR ON DELETE.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3704E IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3704E", "CHECK THAT INTEGER_IO GET RAISES DATA_ERROR " &
                      "WHEN THE LEXICAL ELEMENT IS NOT OF THE " &
                      "INTEGER TYPE EXPECTED.  CHECK THAT ITEM " &
                      "IS UNAFFECTED AND READING CAN CONTINUE AFTER " &
                      "THE EXCEPTION HAS BEEN HANDLED");

     DECLARE
          FT : FILE_TYPE;
          TYPE INT IS NEW INTEGER RANGE 10 .. 20;
          PACKAGE IIO IS NEW INTEGER_IO (INT);
          USE IIO;
          X : INT := 16;
     BEGIN

-- CREATE AND INITIALIZE FILE

          BEGIN
               CREATE (FT, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          PUT (FT, " 101 12");
          CLOSE(FT);

          BEGIN
               OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               GET (FT, X, 2);
               FAILED ("DATA_ERROR NOT RAISED - 1");
          EXCEPTION
               WHEN DATA_ERROR =>
                    IF X /= 16 THEN
                         FAILED ("ITEM AFFECTED BY GET WHEN DATA" &
                                 "_ERROR IS RAISED");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 1");
          END;

          BEGIN
               GET (FT, X, 3);
               FAILED ("DATA_ERROR NOT RAISED - 2");
          EXCEPTION
               WHEN DATA_ERROR =>
                    IF X /= 16 THEN
                         FAILED ("ITEM AFFECTED BY GET WHEN DATA" &
                                 "_ERROR IS RAISED");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 2");
          END;

          BEGIN
               GET (FT, X, 2);
               IF X /= 12 THEN
                    FAILED ("READING NOT CONTINUED CORRECTLY " &
                            "AFTER EXCEPTION");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("GET OF CORRECT DATA RAISED EXCEPTION");
          END;

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

END CE3704E;
