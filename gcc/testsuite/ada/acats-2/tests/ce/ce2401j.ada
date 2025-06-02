-- CE2401J.ADA

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
--     CHECK THAT DATA WRITTEN INTO A DIRECT FILE CAN BE READ
--     CORRECTLY.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION WITH INOUT_FILE MODE AND OPENING WITH IN_FILE MODE FOR
--     DIRECT FILES.

-- HISTORY:
--     DWC 08/12/87  CREATE ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2401J IS
     END_SUBTEST: EXCEPTION;
BEGIN

     TEST ("CE2401J" , "CHECK THAT DATA WRITTEN INTO A DIRECT FILE " &
                       "CAN BE READ CORRECTLY");

     DECLARE
          PACKAGE DIR_IO IS NEW DIRECT_IO (INTEGER);
          USE DIR_IO;
          FILE : FILE_TYPE;
     BEGIN
          BEGIN
               CREATE (FILE, INOUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("CREATE WITH INOUT FILE NOT " &
                                    "SUPPORTED");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED ON " &
                            "CREATE");
                    RAISE END_SUBTEST;
          END;

          DECLARE
               OUT_ITEM1 : INTEGER := 10;
               OUT_ITEM2 : INTEGER := 21;
               OUT_ITEM3 : INTEGER := 32;
               IN_ITEM   : INTEGER;
               ONE   : POSITIVE_COUNT := 1;
               THREE : POSITIVE_COUNT := 3;
               FIVE  : POSITIVE_COUNT := 5;
          BEGIN
               BEGIN
                    WRITE (FILE, OUT_ITEM1, ONE);
                    WRITE (FILE, OUT_ITEM2, THREE);
                    BEGIN
                         READ (FILE, IN_ITEM, ONE);
                         IF OUT_ITEM1 /= IN_ITEM THEN
                              FAILED ("INCORRECT INTEGER VALUE " &
                                      "READ - 1");
                         END IF;
                    END;
                    WRITE (FILE, OUT_ITEM3, FIVE);
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED ON WRITE");
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE, IN_ITEM, THREE);
                    IF OUT_ITEM2 /= IN_ITEM THEN
                         FAILED ("INCORRECT INTEGER VALUE READ - 2");
                    END IF;
               END;

               BEGIN
                    RESET (FILE);
                    READ (FILE, IN_ITEM);
                    IF OUT_ITEM1 /= IN_ITEM THEN
                         FAILED ("INCORRECT INTEGER VALUE READ - 3");
                    END IF;
               EXCEPTION
                    WHEN USE_ERROR => NULL;
               END;

               CLOSE (FILE);

               BEGIN
                    OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
               EXCEPTION
                    WHEN USE_ERROR =>
                         RAISE END_SUBTEST;
               END;

               BEGIN
                    READ (FILE, IN_ITEM);
                    IF OUT_ITEM1 /= IN_ITEM THEN
                         FAILED ("INCORRECT INTEGER VALUE READ - 4");
                         RAISE END_SUBTEST;
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ IN IN_FILE MODE - 1");
               END;

               BEGIN
                    READ (FILE, IN_ITEM, ONE);
                    IF OUT_ITEM1 /= IN_ITEM THEN
                         FAILED ("INCORRECT INTEGER VALUE READ - 5");
                         RAISE END_SUBTEST;
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ IN IN_FILE MODE - 2");
               END;

               BEGIN
                    READ (FILE, IN_ITEM, FIVE);
                    IF OUT_ITEM3 /= IN_ITEM THEN
                         FAILED ("INCORRECT INTEGER VALUE READ - 6");
                         RAISE END_SUBTEST;
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ IN IN_FILE MODE - 3");
               END;

               BEGIN
                    READ (FILE, IN_ITEM, THREE);
                    IF OUT_ITEM2 /= IN_ITEM THEN
                         FAILED ("INCORRECT INTEGER VALUE READ - 7");
                         RAISE END_SUBTEST;
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("READ IN IN_FILE MODE - 4");
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

END CE2401J;
