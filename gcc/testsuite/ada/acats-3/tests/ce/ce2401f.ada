-- CE2401F.ADA

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
--     PRIVATE.

-- APPLICABILITY CRITERIA:
--
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION WITH INOUT_FILE MODE AND OPENING WITH IN_FILE MODE FOR
--     DIRECT FILES.

-- HISTORY:
--     ABW 08/18/82
--     SPS 09/15/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGE TO .ADA TEST
--     EG  11/19/85  CORRECT SO TEST CAN HANDLE IMPLEMENTATION WITH
--                   POSITIVE_COUNT'LAST=1; COVER POSSIBILITY OF CREATE
--                   RAISING USE_ERROR; ENSURE RESET DOESN'T RAISE
--                   EXCEPTION IF CREATE FAILS; CHECK THAT WE CAN READ
--                   DATA THAT HAS BEEN WRITTEN.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/11/87  ISOLATED EXCEPTIONS.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2401F IS

     END_SUBTEST : EXCEPTION;

BEGIN

     TEST ("CE2401F", "CHECK THAT READ, WRITE, SET_INDEX, " &
                       "INDEX, SIZE, AND END_OF_FILE ARE " &
                       "SUPPORTED FOR DIRECT FILES WITH " &
                       "ELEMENT_TYPE PRIVATE");

     DECLARE

          PACKAGE PKG IS
               TYPE PRIV IS PRIVATE;
               FUNCTION ASSIGN RETURN PRIV;
          PRIVATE
               TYPE PRIV IS NEW INTEGER;
          END PKG;

          USE PKG;

          PACKAGE DIR_PRV IS NEW DIRECT_IO (PRIV);
          USE DIR_PRV;
          FILE_PRV : FILE_TYPE;

          PACKAGE BODY PKG IS
               FUNCTION ASSIGN RETURN PRIV IS
               BEGIN
                    RETURN (16);
               END;
          BEGIN
               NULL;
          END PKG;

     BEGIN
          BEGIN
               CREATE (FILE_PRV, INOUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR | NAME_ERROR RAISED " &
                                    "ON CREATE - PRIVATE");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON " &
                            "CREATE - PRIVATE");
                    RAISE END_SUBTEST;
          END;

          BEGIN

               DECLARE

                    PRV, ITEM_PRV : PRIV;
                    ONE_PRV : POSITIVE_COUNT := 1;
                    TWO_PRV : POSITIVE_COUNT := 2;

               BEGIN

                    PRV := ASSIGN;

                    BEGIN
                         WRITE (FILE_PRV, PRV);
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                      "PRIVATE - 1");
                    END;

                    BEGIN
                         WRITE (FILE_PRV, PRV, TWO_PRV);
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                                      "PRIVATE - 2");
                    END;

                    BEGIN
                         IF SIZE (FILE_PRV) /= TWO_PRV THEN
                              FAILED ("SIZE FOR TYPE PRIVATE");
                         END IF;
                         IF NOT END_OF_FILE (FILE_PRV) THEN
                              FAILED ("WRONG END_OF_FILE VALUE FOR " &
                                      "PRIVATE TYPE");
                         END IF;

                         SET_INDEX (FILE_PRV, ONE_PRV);

                         IF INDEX (FILE_PRV) /= ONE_PRV THEN
                              FAILED ("WRONG INDEX VALUE FOR PRIVATE " &
                                      "TYPE");
                         END IF;
                    END;

                    CLOSE (FILE_PRV);

                    BEGIN
                         OPEN (FILE_PRV, IN_FILE, LEGAL_FILE_NAME);
                    EXCEPTION
                         WHEN USE_ERROR =>
                              NOT_APPLICABLE ("OPEN FOR IN_FILE NOT " &
                                       "SUPPORTED");
                              RAISE END_SUBTEST;
                    END;

                    BEGIN
                         READ (FILE_PRV, ITEM_PRV);
                         IF ITEM_PRV /= PRV THEN
                              FAILED ("INCORRECT PRIVATE TYPE VALUE " &
                                      "READ - 1");
                         END IF;
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ("READ WITHOUT FROM FOR " &
                                      "PRIVATE TYPE");
                    END;

                    BEGIN
                         READ (FILE_PRV, ITEM_PRV, ONE_PRV);
                         IF ITEM_PRV /= PRV THEN
                              FAILED ("INCORRECT PRIVATE TYPE VALUE " &
                                      "READ - 2");
                         END IF;
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ("READ WITH FROM FOR " &
                                      "PRIVATE TYPE");
                    END;
               END;

               BEGIN
                    DELETE (FILE_PRV);
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

END CE2401F;
