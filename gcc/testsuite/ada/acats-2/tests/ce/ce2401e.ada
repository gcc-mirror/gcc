-- CE2401E.ADA

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
--     FLOATING POINT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY FOR IMPLEMENTATIONS WHICH SUPPORT CREATION OF
--     DIRECT FILES WITH INOUT_FILE MODE AND OPENING OF DIRECT FILES
--     WITH IN_FILE MODE.

-- HISTORY:
--     ABW 08/18/82
--     SPS 09/15/82
--     SPS 11/11/82
--     JBG 05/02/83
--     EG  11/19/85  HANDLE IMPLEMENTATIONS WITH
--                   POSITIVE_COUNT'LAST=1.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/10/87  ISOLATED EXCEPTIONS. SPLIT FIXED POINT TESTS
--                   INTO CE2401I.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2401E IS

     END_SUBTEST : EXCEPTION;

BEGIN

     TEST ("CE2401E", "CHECK THAT READ, WRITE, SET_INDEX, " &
                       "INDEX, SIZE, AND END_OF_FILE ARE " &
                       "SUPPORTED FOR DIRECT FILES WITH " &
                       "ELEMENT_TYPE FLOAT");

     DECLARE

          PACKAGE DIR_FLT IS NEW DIRECT_IO (FLOAT);
          USE DIR_FLT;
          FILE_FLT : FILE_TYPE;

     BEGIN
          BEGIN
               CREATE (FILE_FLT, INOUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - FLOAT");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON " &
                            "CREATE - FLOAT");
                    RAISE END_SUBTEST;
          END;

          DECLARE
               FLT : FLOAT := 65.0;
               ITEM_FLT : FLOAT;
               ONE_FLT : POSITIVE_COUNT := 1;
               TWO_FLT : POSITIVE_COUNT := 2;
          BEGIN
               BEGIN
                    WRITE (FILE_FLT, FLT);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "FLOATING POINT - 1");
               END;

               BEGIN
                    WRITE (FILE_FLT, FLT, TWO_FLT);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "FLOATING POINT - 2");
               END;

               BEGIN
                    IF SIZE (FILE_FLT) /= TWO_FLT THEN
                         FAILED ("SIZE FOR FLOATING POINT");
                    END IF;

                    IF NOT END_OF_FILE (FILE_FLT) THEN
                         FAILED ("WRONG END_OF_FILE VALUE FOR " &
                                 "FLOATING POINT");
                    END IF;

                    SET_INDEX (FILE_FLT, ONE_FLT);
                    IF INDEX (FILE_FLT) /= ONE_FLT THEN
                         FAILED ("WRONG INDEX VALUE FOR " &
                                 "FLOATING POINT");
                    END IF;
               END;

               CLOSE (FILE_FLT);

               BEGIN
                    OPEN (FILE_FLT, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("OPEN FOR IN_FILE " &
                                         "MODE NOT SUPPORTED");
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE_FLT, ITEM_FLT);
                    IF ITEM_FLT /= FLT THEN
                         FAILED ("WRONG VALUE READ FOR " &
                                 "FLOATING POINT");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITHOUT FROM FOR " &
                                 "TYPE FLOATING POINT");
               END;

               BEGIN
                    READ (FILE_FLT, ITEM_FLT, ONE_FLT);
                    IF ITEM_FLT /= FLT THEN
                         FAILED ("WRONG VALUE READ WITH INDEX FOR " &
                                 "FLOATING POINT");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITH FROM FOR " &
                                 "TYPE FLOATING POINT");
               END;

               BEGIN
                    DELETE (FILE_FLT);
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

END CE2401E;
