-- CE3102B.TST

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
--     CHECK THAT FOR TEXT FILES NAME_ERROR IS RAISED BY CREATE AND
--     OPEN IF THE GIVEN NAME STRING DOES NOT ALLOW THE IDENTIFICATION
--     OF AN EXTERNAL FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE FOR TEXT_IO.

-- HISTORY:
--     ABW 08/24/82
--     JBG 03/16/83
--     EG  05/30/85
--     JLH 08/12/87  REMOVED UNNECESSARY CODE, ADDED NEW CASES FOR OPEN,
--                   AND REMOVED DEPENDENCE ON DELETE.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3102B IS

     FILE1, FILE2 : FILE_TYPE;
     FILE_NAME_OK : BOOLEAN := FALSE;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3102B", "CHECK THAT NAME_ERROR IS RAISED " &
                      "APPROPRIATELY");

     -- CHECK THAT A LEGAL FILE NAME IS OK SO TEST IS VALID

     BEGIN
          CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE " &
                               "OF ASSUMED VALID FILE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE " &
                               "OF ASSUMED VALID FILE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          DELETE (FILE1);

          BEGIN
               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
               FAILED ("FILE STILL EXISTS AFTER DELETE");
          EXCEPTION
               WHEN NAME_ERROR =>
                    NULL;
               WHEN USE_ERROR =>
                    FAILED ("USE_ERROR RAISED ON TEXT OPEN WITH " &
                            "IN_FILE MODE");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT OPEN");
          END;

     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     -- PERFORM VARIOUS CHECKS

     BEGIN
          OPEN (FILE2, IN_FILE, LEGAL_FILE_NAME(2));
          FAILED ("NO EXCEPTION FOR NON-EXISTENT FILE - IN_FILE");
     EXCEPTION
          WHEN NAME_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR OPEN OF " &
                       "NON-EXISTENT FILE - IN_FILE");
     END;

     BEGIN
          OPEN (FILE2, OUT_FILE, LEGAL_FILE_NAME(3));
          FAILED ("NO EXCEPTION FOR NON-EXISTENT FILE - OUT_FILE");
     EXCEPTION
          WHEN NAME_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR OPEN FOR " &
                       "NON-EXISTENT FILE - OUT_FILE");
     END;

     BEGIN
          CREATE (FILE1, NAME => "$ILLEGAL_EXTERNAL_FILE_NAME1");
          FAILED ("NO EXCEPTION RAISED FOR " &
                  "$ILLEGAL_EXTERNAL_FILE_NAME1 - CREATE");
     EXCEPTION
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED FOR " &
                       "$ILLEGAL_EXTERNAL_FILE_NAME1 - CREATE");
          WHEN NAME_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR " &
                       "$ILLEGAL_EXTERNAL_FILE_NAME1 - CREATE");
     END;

     BEGIN
          CREATE (FILE2, NAME => "$ILLEGAL_EXTERNAL_FILE_NAME2");
          FAILED ("NO EXCEPTION RAISED FOR " &
                  "$ILLEGAL_EXTERNAL_FILE_NAME2 - CREATE");
     EXCEPTION
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED FOR " &
                       "$ILLEGAL_EXTERNAL_FILE_NAME2 - CREATE");
          WHEN NAME_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR " &
                       "$ILLEGAL_EXTERNAL_FILE_NAME2 - CREATE");
     END;

     BEGIN
          OPEN (FILE2, IN_FILE,
                NAME => "$ILLEGAL_EXTERNAL_FILE_NAME1");
          FAILED ("NO EXCEPTION RAISED FOR " &
                  "$ILLEGAL_EXTERNAL_FILE_NAME1 - OPEN");
     EXCEPTION
          WHEN USE_ERROR =>
               FAILED ("USE ERROR RAISED FOR " &
                       "$ILLEGAL_EXTERNAL_FILE_NAME1 - OPEN");
          WHEN NAME_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR " &
                       "$ILLEGAL_EXTERNAL_FILE_NAME1 - OPEN");
     END;

     BEGIN
          OPEN (FILE1, IN_FILE,
                NAME => "$ILLEGAL_EXTERNAL_FILE_NAME2");
          FAILED ("NO EXCEPTION RAISED FOR " &
                  "$ILLEGAL_EXTERNAL_FILE_NAME2 - OPEN");
     EXCEPTION
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED FOR " &
                       "$ILLEGAL_EXTERNAL_FILE_NAME2 - OPEN");
          WHEN NAME_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR " &
                       "$ILLEGAL_EXTERNAL_FILE_NAME2 - OPEN");
     END;

     RESULT;

EXCEPTION

     WHEN INCOMPLETE =>
          RESULT;

END CE3102B;
