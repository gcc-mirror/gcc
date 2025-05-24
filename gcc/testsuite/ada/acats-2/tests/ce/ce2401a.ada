-- CE2401A.ADA

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
--     AND WITHOUT PARAMETER TO), SET_INDEX, INDEX, SIZE AND
--     END_OF_FILE ARE SUPPORTED FOR DIRECT FILES WITH ELEMENT_TYPES
--     STRING, CHARACTER, AND INTEGER.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT DIRECT FILES.

-- HISTORY:
--     ABW 08/16/82
--     SPS 09/15/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGE TO .ADA TEST.
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 07/31/87  ISOLATED EXCEPTIONS.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2401A IS
     END_SUBTEST : EXCEPTION;
BEGIN

     TEST ("CE2401A" , "CHECK THAT READ, WRITE, SET_INDEX " &
                       "INDEX, SIZE AND END_OF_FILE ARE " &
                       "SUPPORTED FOR DIRECT FILES");

     DECLARE
          SUBTYPE STR_TYPE IS STRING (1..12);
          PACKAGE DIR_STR IS NEW DIRECT_IO (STR_TYPE);
          USE DIR_STR;
          FILE_STR : FILE_TYPE;
     BEGIN
          BEGIN
               CREATE (FILE_STR, INOUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - STRING");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON " &
                            "CREATE - STRING");
                    RAISE END_SUBTEST;
          END;

          DECLARE
               STR : STR_TYPE := "TEXT OF FILE";
               ITEM_STR : STR_TYPE;
               ONE_STR : POSITIVE_COUNT := 1;
               TWO_STR : POSITIVE_COUNT := 2;
          BEGIN
               BEGIN
                    WRITE (FILE_STR,STR);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "STRING - 1");
               END;

               BEGIN
                    WRITE (FILE_STR,STR,TWO_STR);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "STRING - 2");
               END;

               BEGIN
                    IF SIZE (FILE_STR) /= TWO_STR THEN
                         FAILED ("SIZE FOR TYPE STRING");
                    END IF;
                    IF NOT END_OF_FILE (FILE_STR) THEN
                         FAILED ("WRONG END_OF_FILE VALUE FOR STRING");
                    END IF;
                    SET_INDEX (FILE_STR,ONE_STR);
                    IF INDEX (FILE_STR) /= ONE_STR THEN
                         FAILED ("WRONG INDEX VALUE FOR STRING");
                    END IF;
               END;

               CLOSE (FILE_STR);

               BEGIN
                    OPEN (FILE_STR, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("OPEN FOR IN_FILE MODE " &
                                         "NOT SUPPORTED - 1");
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE_STR,ITEM_STR);
                    IF ITEM_STR /= STR THEN
                         FAILED ("INCORRECT STRING VALUE READ - 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITHOUT FROM FOR STRING");
               END;

               BEGIN
                    READ (FILE_STR,ITEM_STR,ONE_STR);
                    IF ITEM_STR /= STR THEN
                         FAILED ("INCORRECT STRING VALUE READ - 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITH FROM FOR STRING");
               END;
          END;

          BEGIN
               DELETE (FILE_STR);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN END_SUBTEST =>
               NULL;
     END;

     DECLARE
          PACKAGE DIR_CHR IS NEW DIRECT_IO (CHARACTER);
          USE DIR_CHR;
          FILE_CHR : FILE_TYPE;
     BEGIN
          BEGIN
               CREATE (FILE_CHR, INOUT_FILE, LEGAL_FILE_NAME(2));
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - CHARACTER");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON " &
                            "CREATE - CHARACTER");
                    RAISE END_SUBTEST;
          END;

          DECLARE
               CHR : CHARACTER := 'C';
               ITEM_CHR : CHARACTER;
               ONE_CHR : POSITIVE_COUNT := 1;
               TWO_CHR : POSITIVE_COUNT := 2;
          BEGIN
               BEGIN
                    WRITE (FILE_CHR,CHR);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "CHARACTER - 1");
               END;

               BEGIN
                    WRITE (FILE_CHR,CHR,TWO_CHR);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "CHARACTER - 2");
               END;

               BEGIN
                    IF SIZE (FILE_CHR) /= TWO_CHR THEN
                         FAILED ("SIZE FOR TYPE CHARACTER");
                    END IF;
                    IF NOT END_OF_FILE (FILE_CHR) THEN
                         FAILED ("WRONG END_OF_FILE VALUE FOR TYPE " &
                                 "CHARACTER");
                    END IF;
                    SET_INDEX (FILE_CHR,ONE_CHR);
                    IF INDEX (FILE_CHR) /= ONE_CHR THEN
                         FAILED ("WRONG INDEX VALUE FOR TYPE " &
                                 "CHARACTER");
                    END IF;
               END;

               CLOSE (FILE_CHR);

               BEGIN
                    OPEN (FILE_CHR, IN_FILE, LEGAL_FILE_NAME(2));
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("OPEN FOR IN_FILE MODE " &
                                         "NOT SUPPORTED - 2");
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE_CHR,ITEM_CHR);
                    IF ITEM_CHR /= CHR THEN
                         FAILED ("INCORRECT CHR VALUE READ - 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITHOUT FROM FOR " &
                                 "TYPE CHARACTER");
               END;

               BEGIN
                    READ (FILE_CHR,ITEM_CHR,ONE_CHR);
                    IF ITEM_CHR /= CHR THEN
                         FAILED ("INCORRECT CHR VALUE READ - 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITH FROM FOR " &
                                 "TYPE CHARACTER");
               END;
          END;

          BEGIN
               DELETE (FILE_CHR);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN END_SUBTEST =>
               NULL;
     END;

     DECLARE
          PACKAGE DIR_INT IS NEW DIRECT_IO (INTEGER);
          USE DIR_INT;
          FILE_INT : FILE_TYPE;
     BEGIN
          BEGIN
               CREATE (FILE_INT, INOUT_FILE, LEGAL_FILE_NAME(3));
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - INTEGER");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON " &
                            "CREATE - INTEGER");
                    RAISE END_SUBTEST;
          END;

          DECLARE
               INT : INTEGER := IDENT_INT (33);
               ITEM_INT : INTEGER;
               ONE_INT : POSITIVE_COUNT := 1;
               TWO_INT : POSITIVE_COUNT := 2;
          BEGIN
               BEGIN
                    WRITE (FILE_INT,INT);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "INTEGER - 1");
               END;

               BEGIN
                    WRITE (FILE_INT,INT,TWO_INT);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "INTEGER - 2");
               END;

               BEGIN
                    IF SIZE (FILE_INT) /= TWO_INT THEN
                         FAILED ("SIZE FOR TYPE INTEGER");
                    END IF;
                    IF NOT END_OF_FILE (FILE_INT) THEN
                         FAILED ("WRONG END_OF_FILE VALUE FOR TYPE " &
                                 "INTEGER");
                    END IF;
                    SET_INDEX (FILE_INT, ONE_INT);
                    IF INDEX (FILE_INT) /= ONE_INT THEN
                         FAILED ("WRONG INDEX VALUE FOR TYPE INTEGER");
                    END IF;
               END;

               CLOSE (FILE_INT);

               BEGIN
                    OPEN (FILE_INT, IN_FILE, LEGAL_FILE_NAME(3));
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("OPEN FOR IN_FILE MODE " &
                                         "NOT SUPPORTED - 3");
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE_INT,ITEM_INT);
                    IF ITEM_INT /= INT THEN
                         FAILED ("INCORRECT INT VALUE READ - 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITHOUT FROM FOR " &
                                 "TYPE INTEGER");
               END;

               BEGIN
                    READ (FILE_INT,ITEM_INT,ONE_INT);
                    IF ITEM_INT /= INT THEN
                         FAILED ("INCORRECT INT VALUE READ - 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITH FROM FOR " &
                                 "TYPE INTEGER");
               END;
          END;

          BEGIN
               DELETE (FILE_INT);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN END_SUBTEST =>
               NULL;
     END;

     RESULT;

END CE2401A;
