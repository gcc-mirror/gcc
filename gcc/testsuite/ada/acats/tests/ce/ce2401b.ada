-- CE2401B.ADA

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
--     END_OF_FILE FOR DIRECT FILES WITH ELEMENT_TYPES BOOLEAN,
--     ACCESS, AND ENUMERATED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT FILES.

-- HISTORY:
--     ABW 08/18/82
--     SPS 09/15/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGE TO .ADA TEST.
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/07/87  ISOLATED EXCEPTIONS.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2401B IS
     END_SUBTEST : EXCEPTION;
BEGIN

     TEST ("CE2401B", "CHECK READ, WRITE, SET_INDEX " &
                      "INDEX, SIZE, AND END_OF_FILE FOR " &
                      "DIRECT FILES FOR BOOLEAN, ACCESS " &
                      "AND ENUMERATION TYPES");
     DECLARE
          PACKAGE DIR_BOOL IS NEW DIRECT_IO (BOOLEAN);
          USE DIR_BOOL;
          FILE_BOOL : FILE_TYPE;
     BEGIN
          BEGIN
               CREATE (FILE_BOOL, INOUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - BOOLEAN");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON " &
                            "CREATE - BOOLEAN");
                    RAISE END_SUBTEST;
          END;

          DECLARE
               BOOL : BOOLEAN := IDENT_BOOL (TRUE);
               ITEM_BOOL : BOOLEAN;
               ONE_BOOL : POSITIVE_COUNT := 1;
               TWO_BOOL : POSITIVE_COUNT := 2;
          BEGIN
               BEGIN
                    WRITE (FILE_BOOL,BOOL);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "BOOLEAN - 1");
               END;

               BEGIN
                    WRITE (FILE_BOOL,BOOL,TWO_BOOL);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "BOOLEAN - 2");
               END;

               BEGIN
                    IF SIZE (FILE_BOOL) /= TWO_BOOL THEN
                         FAILED ("SIZE FOR TYPE BOOLEAN");
                    END IF;
                    IF NOT END_OF_FILE (FILE_BOOL) THEN
                         FAILED ("WRONG END_OF_FILE VALUE FOR " &
                                 "BOOLEAN");
                    END IF;
                    SET_INDEX (FILE_BOOL,ONE_BOOL);
                    IF INDEX (FILE_BOOL) /= ONE_BOOL THEN
                         FAILED ("WRONG INDEX VALUE FOR TYPE BOOLEAN");
                    END IF;
               END;

               CLOSE (FILE_BOOL);

               BEGIN
                    OPEN (FILE_BOOL, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("OPEN FOR IN_FILE MODE " &
                                         "NOT SUPPORTED - 1");
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE_BOOL,ITEM_BOOL);
                    IF ITEM_BOOL /= BOOL THEN
                         FAILED ("INCORRECT BOOLEAN VALUE READ - 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITHOUT FROM FOR " &
                                 "TYPE BOOLEAN");
               END;

               BEGIN
                    READ (FILE_BOOL,ITEM_BOOL,ONE_BOOL);
                    IF ITEM_BOOL /= BOOL THEN
                         FAILED ("INCORRECT BOOLEAN VALUE READ - 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITH FROM FOR BOOLEAN");
               END;
          END;

          BEGIN
               DELETE (FILE_BOOL);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN END_SUBTEST =>
               NULL;
     END;

     DECLARE
          TYPE ENUMERATED IS (ONE,TWO,THREE);
          PACKAGE DIR_ENUM IS NEW DIRECT_IO (ENUMERATED);
          USE DIR_ENUM;
          FILE_ENUM : FILE_TYPE;
     BEGIN
          BEGIN
               CREATE (FILE_ENUM, INOUT_FILE, LEGAL_FILE_NAME(2));
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - ENUMERATED");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON " &
                            "CREATE - ENUMERATED");
                    RAISE END_SUBTEST;
          END;

          DECLARE
               ENUM : ENUMERATED := (THREE);
               ITEM_ENUM : ENUMERATED;
               ONE_ENUM : POSITIVE_COUNT := 1;
               TWO_ENUM : POSITIVE_COUNT := 2;
          BEGIN
               BEGIN
                    WRITE (FILE_ENUM,ENUM);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "ENUMERATED - 1");
               END;

               BEGIN
                    WRITE (FILE_ENUM,ENUM,TWO_ENUM);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "ENUMERATED - 2");
               END;

               BEGIN
                    IF SIZE (FILE_ENUM) /= TWO_ENUM THEN
                         FAILED ("SIZE FOR TYPE ENUMERATED");
                    END IF;
                    IF NOT END_OF_FILE (FILE_ENUM) THEN
                         FAILED ("WRONG END_OF_FILE VALUE FOR TYPE " &
                                 "ENUMERATED");
                    END IF;
                    SET_INDEX (FILE_ENUM,ONE_ENUM);
                    IF INDEX (FILE_ENUM) /= ONE_ENUM THEN
                         FAILED ("WRONG INDEX VALUE FOR TYPE " &
                                 "ENUMERATED");
                    END IF;
               END;

               CLOSE (FILE_ENUM);

               BEGIN
                    OPEN (FILE_ENUM, IN_FILE, LEGAL_FILE_NAME(2));
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("OPEN FOR IN_FILE MODE " &
                                         "NOT SUPPORTED - 2");
                    RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE_ENUM,ITEM_ENUM);
                    IF ITEM_ENUM /= ENUM THEN
                         FAILED ("INCORRECT ENUM VALUE READ - 1");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITHOUT FROM FOR ENUMERATED");
               END;

               BEGIN
                    READ (FILE_ENUM,ITEM_ENUM,ONE_ENUM);
                    IF ITEM_ENUM /= ENUM THEN
                         FAILED ("INCORRECT ENUM VALUE READ - 2");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITH FROM FOR " &
                                 "TYPE ENUMERATED");
               END;
          END;

          BEGIN
               DELETE (FILE_ENUM);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN END_SUBTEST =>
               NULL;
     END;

     DECLARE
          TYPE ACC_INT IS ACCESS INTEGER;
          PACKAGE DIR_ACC IS NEW DIRECT_IO (ACC_INT);
          USE DIR_ACC;
          FILE_ACC : FILE_TYPE;
     BEGIN
          BEGIN
               CREATE (FILE_ACC, INOUT_FILE, LEGAL_FILE_NAME(3));
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - ACCESS");
                    RAISE END_SUBTEST;
          END;

          DECLARE
               ACC : ACC_INT := NEW INTEGER'(33);
               ITEM_ACC : ACC_INT;
               ONE_ACC : POSITIVE_COUNT := 1;
               TWO_ACC : POSITIVE_COUNT := 2;
          BEGIN
               BEGIN
                    WRITE (FILE_ACC,ACC);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "ACCESS - 1");
               END;

               BEGIN
                    WRITE (FILE_ACC,ACC,TWO_ACC);

               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                 "ACCESS - 2");
               END;

               BEGIN
                    IF SIZE (FILE_ACC) /= TWO_ACC THEN
                         FAILED ("SIZE FOR TYPE ACCESS");
                    END IF;
                    IF NOT END_OF_FILE (FILE_ACC) THEN
                         FAILED ("WRONG END_OF_FILE VALUE FOR ACCESS");
                    END IF;
                    SET_INDEX (FILE_ACC,ONE_ACC);
                    IF INDEX (FILE_ACC) /= ONE_ACC THEN
                         FAILED ("WRONG INDEX VALUE FOR TYPE ACCESS");
                    END IF;
               END;

               CLOSE (FILE_ACC);

               BEGIN
                    OPEN (FILE_ACC, IN_FILE, LEGAL_FILE_NAME(3));
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("OPEN FOR IN_FILE NOT " &
                                         "SUPPORTED - 3");
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE_ACC,ITEM_ACC);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITHOUT FROM FOR ACCESS");
               END;

               BEGIN
                    READ (FILE_ACC,ITEM_ACC,ONE_ACC);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ WITH FROM FOR ACCESS");
               END;
          END;

          BEGIN
               DELETE (FILE_ACC);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN END_SUBTEST =>
               NULL;
     END;

     RESULT;

END CE2401B;
