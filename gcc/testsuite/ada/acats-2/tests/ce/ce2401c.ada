-- CE2401C.ADA

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
--     CHECK READ (WITH AND WITHOUT PARAMETER FROM), WRITE (WITH
--     AND WITHOUT PARAMETER TO), SET_INDEX, INDEX, SIZE, AND
--     END_OF_FILE ARE IMPLEMENTED FOR DIRECT FILES WITH
--     ELEMENT_TYPE CONSTRAINED ARRAY, AND RECORD WITHOUT DISCRIMINANTS.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT FILES.

-- HISTORY:
--     ABW 08/18/82
--     SPS 09/20/82
--     SPS 11/09/82
--     JBG 05/02/83
--     JRK 03/26/84
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/10/87  ISOLATED EXCEPTIONS.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2401C IS
     END_SUBTEST: EXCEPTION;
BEGIN

     TEST ("CE2401C" , "CHECK READ, WRITE, SET_INDEX " &
                       "INDEX, SIZE, AND END_OF_FILE FOR " &
                       "DIRECT FILES FOR CONSTRAINED ARRAY TYPES, " &
                       "AND RECORD TYPES WITHOUT DISCRIMINANTS");

     DECLARE
          TYPE ARR_CN IS ARRAY (1..5) OF BOOLEAN;
          PACKAGE DIR_ARR_CN IS NEW DIRECT_IO (ARR_CN);
          USE DIR_ARR_CN;
          FILE : FILE_TYPE;
     BEGIN
          BEGIN
               CREATE (FILE, INOUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - CONSTRAINED ARRAY");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON " &
                            "CREATE - CONSTRAINED ARRAY");
                    RAISE END_SUBTEST;
          END;

          DECLARE
               ARR : ARR_CN := (TRUE,TRUE,FALSE,TRUE,TRUE);
               ITEM : ARR_CN;
               ONE : POSITIVE_COUNT := 1;
               TWO : POSITIVE_COUNT := 2;
          BEGIN
               BEGIN
                    WRITE (FILE,ARR);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "CONTRAINED ARRAY - 1");
               END;

               BEGIN
                    WRITE (FILE,ARR,TWO);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "CONSTRAINED ARRAY - 2");
               END;

               BEGIN
                    IF SIZE (FILE) /= TWO THEN
                         FAILED ("SIZE FOR TYPE CONSTRAINED ARRAY");
                    END IF;
                    IF NOT END_OF_FILE (FILE) THEN
                         FAILED ("WRONG END_OF_FILE VALUE FOR TYPE " &
                                 "CONSTRAINED ARRAY");
                    END IF;
                    SET_INDEX (FILE,ONE);
                    IF INDEX (FILE) /= ONE THEN
                         FAILED ("WRONG INDEX VALUE FOR TYPE " &
                                 "CONSTRAINED ARRAY");
                    END IF;
               END;

               CLOSE (FILE);

               BEGIN
                    OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("OPEN FOR IN_FILE MODE " &
                                         "NOT SUPPORTED - 1");
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE,ITEM);
                    IF ITEM /= ARR THEN
                         FAILED ("INCORRECT ARRAY VALUES READ " &
                                 "- 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITHOUT FROM FOR " &
                                 "TYPE CONSTRAINED ARRAY");
               END;

               BEGIN
                    READ (FILE,ITEM,ONE);
                    IF ITEM /= ARR THEN
                         FAILED ("INCORRECT ARRAY VALUES READ " &
                                 "- 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITH FROM FOR " &
                                 "TYPE CONSTRAINED ARRAY");
               END;
          END;

          BEGIN
               DELETE (FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN END_SUBTEST =>
               NULL;
     END;

     DECLARE
          TYPE REC IS
               RECORD
                    ONE : INTEGER;
                    TWO : INTEGER;
          END RECORD;
          PACKAGE DIR_REC IS NEW DIRECT_IO (REC);
          USE DIR_REC;
          FILE : FILE_TYPE;
     BEGIN
          BEGIN
               CREATE (FILE, INOUT_FILE, LEGAL_FILE_NAME(2));
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - RECORD");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON CREATE - " &
                            "RECORD");
          END;

          DECLARE
               REC1 : REC := REC'(ONE=>18,TWO=>36);
               ITEM : REC;
               ONE : POSITIVE_COUNT := 1;
               TWO : POSITIVE_COUNT := 2;
          BEGIN
               BEGIN
                    WRITE (FILE,REC1);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR - " &
                                 "RECORD - 1");
               END;

               BEGIN
                    WRITE (FILE,REC1,TWO);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR - " &
                                 "RECORD - 2");
               END;

               BEGIN
                    IF SIZE (FILE) /= TWO THEN
                         FAILED ("SIZE FOR TYPE RECORD");
                    END IF;
                    IF NOT END_OF_FILE (FILE) THEN
                         FAILED ("WRONG END_OF_FILE VALUE FOR RECORD");
                    END IF;
                    SET_INDEX (FILE,ONE);
                    IF INDEX (FILE) /= ONE THEN
                         FAILED ("WRONG INDEX VALUE FOR TYPE RECORD");
                    END IF;
               END;

               CLOSE (FILE);

               BEGIN
                    OPEN (FILE, IN_FILE, LEGAL_FILE_NAME(2));
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("OPEN FOR IN_FILE MODE " &
                                         "NOT SUPPORTED - 2");
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE,ITEM);
                    IF ITEM /= REC1 THEN
                         FAILED ("INCORRECT RECORD VALUES READ " &
                                 "- 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITHOUT FROM FOR RECORD");
               END;

               BEGIN
                    READ (FILE,ITEM,ONE);
                    IF ITEM /= REC1 THEN
                         FAILED ("INCORRECT RECORD VALUES READ " &
                                 "- 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITH FROM FOR " &
                                 "TYPE RECORD");
               END;
          END;

          BEGIN
               DELETE (FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN END_SUBTEST =>
               NULL;
     END;

     RESULT;

END CE2401C;
