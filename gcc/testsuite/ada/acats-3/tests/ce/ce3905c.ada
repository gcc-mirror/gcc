-- CE3905C.ADA

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
--     CHECK THAT GET FOR ENUMERATION TYPES RAISES DATA_ERROR WHEN THE
--     ELEMENT RETRIEVED IS NOT OF THE TYPE EXPECTED OR IS OUT OF THE
--     RANGE OF A SUBTYPE.  ALSO CHECK THAT CONSTRAINT_ERROR IS RAISED
--     IF THE VALUE READ IS OUT OF RANGE OF THE ITEM PARAMETER, BUT
--     WITHIN THE RANGE OF THE INSTANTIATED TYPE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/08/82
--     SPS 12/14/82
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/16/87  REMOVED DEPENDENCE ON RESET AND CORRECTED
--                   EXCEPTION HANDLING.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3905C IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3905C", "CHECK THAT GET FOR ENUMERATION TYPES RAISES " &
                      "DATA_ERROR WHEN THE ELEMENT RETRIEVED IS NOT " &
                      "OF THE TYPE EXPECTED OR IS OUT OF THE RANGE " &
                      "OF A SUBTYPE.  ALSO CHECK THAT " &
                      "CONSTRAINT_ERROR IS RAISED IF THE VALUE READ " &
                      "IS OUT OF RANGE OF THE ITEM PARAMETER, BUT " &
                      "WITHIN THE RANGE OF THE INSTANTIATED TYPE");

     DECLARE
          FT : FILE_TYPE;
          TYPE COLOR IS (RED, BLUE, YELLOW, WHITE, ORANGE, GREEN,
                         PURPLE, BLACK);
          SUBTYPE P_COLOR IS COLOR RANGE RED .. YELLOW;
          CRAYON : COLOR := BLACK;
          PAINT : P_COLOR := BLUE;
          ST : STRING (1 .. 2);
          PACKAGE COLOR_IO IS NEW ENUMERATION_IO (COLOR);
          USE COLOR_IO;
     BEGIN

-- CREATE AND INITIALIZE DATA FILE

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

          PUT (FT, "BROWN");
          NEW_LINE (FT);
          PUT (FT, "ORANGE");
          NEW_LINE (FT);
          PUT (FT, "GREEN");
          NEW_LINE (FT);
          PUT (FT, "WHITE");
          NEW_LINE (FT);
          PUT (FT, "WHI");
          NEW_LINE (FT);
          PUT (FT, "TE");
          NEW_LINE (FT);
          PUT (FT, "RED");

          CLOSE (FT);

          BEGIN
               OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; OPEN WITH " &
                             "IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

-- START TEST

          BEGIN
               GET (FT, CRAYON);                  -- BROWN
               FAILED ("DATA_ERROR NOT RAISED - 1");
          EXCEPTION
               WHEN DATA_ERROR =>
                    IF CRAYON /= BLACK THEN
                         FAILED ("ITEM CRAYON AFFECTED - 1");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 1");
          END;

          BEGIN
               GET (FT, PAINT);                   -- ORANGE
               FAILED ("CONSTRAINT_ERROR NOT RAISED");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF PAINT /= BLUE THEN
                         FAILED ("ITEM PAINT AFFECTED - 2");
                    END IF;
               WHEN DATA_ERROR =>
                    FAILED ("DATA_ERROR RAISED FOR ITEM SUBTYPE");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 2");
          END;

          DECLARE
               PACKAGE P_COLOR_IO IS NEW ENUMERATION_IO (P_COLOR);
               USE P_COLOR_IO;
          BEGIN
               BEGIN
                    P_COLOR_IO.GET (FT, PAINT);   -- GREEN
                    FAILED ("DATA_ERROR NOT RAISED - 3");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF PAINT /= BLUE THEN
                              FAILED ("ITEM PAINT AFFECTED - 3");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 3");
               END;

               BEGIN
                    P_COLOR_IO.GET (FT, PAINT);   -- WHITE
                    FAILED ("DATA_ERROR NOT RAISED - 3A");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - 3A");
               END;
          END;

          BEGIN
               GET (FT, CRAYON);                  -- WHI
               FAILED ("DATA_ERROR NOT RAISED - 4");
          EXCEPTION
               WHEN DATA_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 4");
          END;

          GET (FT, ST);                 -- TE

          GET (FT, CRAYON);                  -- RED
          IF CRAYON /= RED THEN
               FAILED ("READING NOT CONTINUED CORRECTLY AFTER" &
                       "DATA_ERROR EXCEPTION");
          END IF;

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

END CE3905C;
