-- CE2401I.ADA

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
--     CHECK THAT READ (WITH AND WITHOUT PARAMETER FROM), WRITE (WITH
--     AND WITHOUT PARAMETER TO), SET_INDEX, INDEX, SIZE, AND
--     END_OF_FILE ARE SUPPORTED FOR DIRECT FILES WITH ELEMENT_TYPE
--     FIXED POINT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY FOR IMPLEMENTATIONS WHICH SUPPORT CREATION OF
--     DIRECT FILES WITH INOUT_FILE MODE AND OPENING OF DIRECT FILES
--     WITH IN_FILE MODE.

-- HISTORY:
--     DWC 08/10/87  CREATED ORIGINAL VERSION.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2401I IS

     END_SUBTEST : EXCEPTION;

BEGIN

     TEST ("CE2401I", "CHECK THAT READ, WRITE, SET_INDEX, " &
                       "INDEX, SIZE, AND END_OF_FILE ARE " &
                       "SUPPORTED FOR DIRECT FILES WITH " &
                       "ELEMENT_TYPE FIXED");

     DECLARE

          TYPE FIX_TYPE IS DELTA 0.5 RANGE 0.0 .. 255.0;
          PACKAGE DIR_FIX IS NEW DIRECT_IO (FIX_TYPE);
          USE DIR_FIX;
          FILE_FIX : FILE_TYPE;

     BEGIN
          BEGIN
               CREATE (FILE_FIX, INOUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - FIXED POINT");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON " &
                            "CREATE - FIXED POINT");
                    RAISE END_SUBTEST;
          END;

          DECLARE
               FIX : FIX_TYPE := 16.0;
               ITEM_FIX : FIX_TYPE;
               ONE_FIX : POSITIVE_COUNT := 1;
               TWO_FIX : POSITIVE_COUNT := 2;

          BEGIN
               BEGIN
                    WRITE (FILE_FIX, FIX);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "FIXED POINT - 1");
               END;

               BEGIN
                    WRITE (FILE_FIX, FIX, TWO_FIX);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "FIXED POINT - 2");
               END;

               BEGIN
                    IF SIZE (FILE_FIX) /= TWO_FIX THEN
                         FAILED ("SIZE FOR TYPE FIXED POINT");
                    END IF;

                    IF NOT END_OF_FILE (FILE_FIX) THEN
                         FAILED ("WRONG END_OF_FILE VALUE FOR " &
                                 "FIXED POINT");
                    END IF;

                    SET_INDEX (FILE_FIX, ONE_FIX);

                    IF INDEX (FILE_FIX) /= ONE_FIX THEN
                         FAILED ("WRONG INDEX VALUE FOR FIXED " &
                                 "POINT");
                    END IF;
               END;

               CLOSE (FILE_FIX);

               BEGIN
                    OPEN (FILE_FIX, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("OPEN FOR IN_FILE MODE " &
                                         "NOT SUPPORTED");
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE_FIX, ITEM_FIX);
                    IF ITEM_FIX /= FIX THEN
                        FAILED ("WRONG VALUE READ FOR FIXED POINT");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITHOUT FROM FOR FIXED " &
                                 "POINT");
               END;

               BEGIN
                    READ (FILE_FIX, ITEM_FIX, ONE_FIX);
                    IF ITEM_FIX /= FIX THEN
                        FAILED ("WRONG VALUE READ WITH INDEX " &
                                "FOR FIXED POINT");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITH FROM FOR FIXED POINT");
               END;

               BEGIN
                    DELETE (FILE_FIX);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NULL;
               END;
          END;

     EXCEPTION
          WHEN END_SUBTEST =>
               NULL;
     END;

     RESULT;

END CE2401I;
