-- CE3704N.ADA

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
--     CHECK THAT GET FOR INTEGER_IO RAISES DATA_ERROR WHEN:
--     (A) BASE LESS THAN 2 OR GREATER THAN 16
--     (B) THE LETTERS IN BASE ARE OUT OF THE BASE RANGE
--     (C) THERE IS NO CLOSING '#' SIGN FOR A BASED LITERAL

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     VKG 02/10/83
--     SPS 03/16/83
--     CPP 07/30/84
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/11/87  REMOVED UNNECESSARY CODE, CORRECTED
--                   EXCEPTION HANDLING, AND CHECKED FOR
--                   USE_ERROR ON DELETE.

WITH TEXT_IO; USE TEXT_IO;
WITH REPORT ; USE REPORT ;

PROCEDURE CE3704N IS
     INCOMPLETE : EXCEPTION;

BEGIN
     TEST ("CE3704N" ,"CHECK THAT DATA_ERROR IS RAISED WHEN " &
                      "A BASED LITERAL DOES NOT HAVE ITS BASE " &
                      "IN THE RANGE 2 .. 16, DIGIT IS OUTSIDE " &
                      "THE BASE RANGE, OR THERE IS NO CLOSING " &
                      "'#' SIGN");

     DECLARE
          FT : FILE_TYPE;
     BEGIN
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

          PUT (FT, "1#0000#");
          NEW_LINE (FT);
          PUT (FT, "A#234567#");
          NEW_LINE (FT);
          PUT (FT, "17#123#1");
          NEW_LINE (FT);
          PUT (FT, "5#1253#2");
          NEW_LINE (FT);
          PUT (FT, "8#123");
          CLOSE (FT);

          DECLARE
               PACKAGE INT_IO IS NEW INTEGER_IO(INTEGER);
               USE INT_IO;
               X : INTEGER := 1003;
               CH : CHARACTER;
          BEGIN
               BEGIN
                    OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR RAISED; TEXT " &
                                         "OPEN WITH IN_FILE MODE");
                         RAISE INCOMPLETE;
               END;

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - (1)");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 1003 THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - (1)");
               END;

               IF NOT END_OF_LINE (FT) THEN
                    GET (FT, CH);
                    FAILED ("GET STOPPED AT WRONG POSITION - " &
                            "(1): CHAR IS " & CH);
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - (2)");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 1003 THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - (2)");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - (2)");
               END;

               IF END_OF_LINE (FT) THEN
                    FAILED ("GET STOPPED AT END OF LINE - (2)");
               ELSE
                    GET (FT, CH);
                    IF CH /= 'A' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION " &
                                 "- (2): CHAR IS " & CH);
                         END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - (2A)");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 1003 THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - (2A)");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - (2A)");
               END;

               IF NOT END_OF_LINE (FT) THEN
                    GET (FT, CH);
                    IF CH /= '1' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION " &
                                 "- (2A): CHAR IS " & CH);
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - (3)");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 1003 THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - (3)");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - (3)");
               END;

               IF NOT END_OF_LINE (FT) THEN
                    GET (FT, CH);
                    IF CH /= '2' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION - " &
                                 "(3): CHAR IS " & CH);
                    END IF;
               END IF;

               SKIP_LINE (FT);

               BEGIN
                    GET (FT, X);
                    FAILED ("DATA_ERROR NOT RAISED - (4)");
               EXCEPTION
                    WHEN DATA_ERROR =>
                         IF X /= 1003 THEN
                              FAILED ("ACTUAL PARAMETER TO GET " &
                                      "AFFECTED ON DATA_ERROR - (4)");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - (4)");
               END;

               IF NOT END_OF_LINE (FT) THEN
                    GET (FT, CH);
                    IF CH /= ' ' THEN
                         FAILED ("GET STOPPED AT WRONG POSITION " &
                                 "- (4): CHAR IS " & CH);
                    END IF;
               END IF;

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

END CE3704N;
