-- CE2406A.ADA

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
--     FOR A DIRECT ACCESS FILE, CHECK THAT AFTER A READ, THE CURRENT
--     READ POSITION IS INCREMENTED BY ONE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT_IO FILES.

-- HISTORY:
--     ABW 08/20/82
--     SPS 09/16/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGE TO .ADA TEST.
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 08/05/87  REMOVED DEPENDENCE ON RESET AND ADDED CHECK FOR
--                   USE_ERROR ON DELETE.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2406A IS

     PACKAGE DIR IS NEW DIRECT_IO (INTEGER);
     USE DIR;
     FILE1                : FILE_TYPE;
     INT                  : INTEGER := IDENT_INT (18);
     BOOL                 : BOOLEAN := IDENT_BOOL (TRUE);
     INT_ITEM1, INT_ITEM2 : INTEGER;
     INCOMPLETE           : EXCEPTION;

BEGIN

     TEST ("CE2406A", "CHECK THAT READ POSITION IS INCREMENTED " &
                      "BY ONE AFTER A READ");

     -- CREATE AND INITIALIZE FILE1

     BEGIN

          BEGIN
               CREATE (FILE1, INOUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN NAME_ERROR | USE_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR | USE_ERROR RAISED " &
                                    "ON CREATE - 1");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED ON CREATE - 2");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               WRITE (FILE1, INT);
               WRITE (FILE1, 26);
               WRITE (FILE1, 12);
               WRITE (FILE1, 19);
               WRITE (FILE1, INT);
               WRITE (FILE1, 3);

               -- BEGIN TEST

               CLOSE (FILE1);
               BEGIN
                    OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR RAISED ON" &
                                         "OPEN - 3");
                         RAISE INCOMPLETE;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED ON " &
                                         "OPEN - 4");
                         RAISE INCOMPLETE;
               END;


               IF INDEX(FILE1) /= POSITIVE_COUNT (IDENT_INT(1)) THEN
                    FAILED ("INITIAL INDEX VALUE INCORRECT - 5");
               ELSE
                    READ (FILE1, INT_ITEM1);
                    IF INDEX(FILE1) /= POSITIVE_COUNT(IDENT_INT(2)) THEN
                         FAILED ("INDEX VALUE NOT INCREMENTED - 6");
                    ELSE
                         IF INT_ITEM1 /= IDENT_INT(18) THEN
                              FAILED ("READ INCORRECT VALUE - 7");
                         END IF;
                         READ (FILE1, INT_ITEM1, 4);
                         IF INDEX(FILE1) /=
                            POSITIVE_COUNT (IDENT_INT(5)) THEN
                             FAILED ("INDEX VALUE NOT INCREMENTED " &
                                     "WHEN TO IS SPECIFIED - 8");
                         ELSE
                              IF INT_ITEM1 /= IDENT_INT(19) THEN
                                   FAILED ("READ INCORRECT VALUE - 9");
                              END IF;
                              READ (FILE1, INT_ITEM1);
                              IF INDEX(FILE1) /=
                                 POSITIVE_COUNT(IDENT_INT(6)) THEN
                                   FAILED ("INDEX VALUE NOT " &
                                           "INCREMENTED WHEN " &
                                           "LAST - 10");
                              ELSIF INT_ITEM1 /= IDENT_INT(18) THEN
                                   FAILED ("READ INCORRECT " &
                                           "IN_FILE VALUE - 11");
                              END IF;
                         END IF;
                    END IF;
               END IF;

               CLOSE (FILE1);
               BEGIN
                    OPEN (FILE1, INOUT_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("USE_ERROR RAISED ON " &
                                         "OPEN - 12");
                         RAISE INCOMPLETE;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED ON " &
                                         "OPEN - 13");
                         RAISE INCOMPLETE;
               END;

               IF INDEX(FILE1) /= POSITIVE_COUNT(IDENT_INT(1)) THEN
                    FAILED ("INITIAL INDEX VALUE INCORRECT - 14");
               ELSE
                    READ (FILE1, INT_ITEM2);
                    IF INDEX(FILE1) /= POSITIVE_COUNT(IDENT_INT(2)) THEN
                         FAILED ("INDEX VALUE NOT INCREMENTED - 15");
                    ELSE
                         IF INT_ITEM2 /= IDENT_INT(18) THEN
                              FAILED ("READ INCORRECT VALUE - 16");
                         END IF;
                         READ (FILE1, INT_ITEM2, 4);
                         IF INDEX (FILE1) /=
                            POSITIVE_COUNT(IDENT_INT(5)) THEN
                             FAILED ("INDEX VALUE NOT INCREMENTED " &
                                     "WHEN TO IS SPECIFIED - 17");
                         ELSE
                              IF INT_ITEM2 /= IDENT_INT(19) THEN
                                   FAILED ("INCORRECT VALUE - 18");
                              END IF;
                              READ (FILE1, INT_ITEM2);
                              IF INDEX(FILE1) /=
                                 POSITIVE_COUNT(IDENT_INT(6)) THEN
                                   FAILED ("INDEX VALUE NOT " &
                                           "INCREMENTED WHEN " &
                                           "LAST - INOUT_FILE - 19");
                              ELSIF INT_ITEM2 /= IDENT_INT(18) THEN
                                   FAILED ("READ INCORRECT " &
                                           "INOUT_FILE VALUE - 20");
                              END IF;
                         END IF;
                    END IF;
               END IF;

               BEGIN
                    DELETE (FILE1);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NULL;
               END;

          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE2406A;
