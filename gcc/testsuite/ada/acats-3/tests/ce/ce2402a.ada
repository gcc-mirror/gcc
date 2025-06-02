-- CE2402A.ADA

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
--     CHECK THAT READ, WRITE, INDEX, SET_INDEX, SIZE, AND
--     END_OF_FILE RAISE STATUS_ERROR WHEN APPLIED TO A NON-OPEN
--     DIRECT FILE.  USE_ERROR IS NOT PERMITTED.

-- HISTORY:
--     ABW 08/17/82
--     SPS 09/16/82
--     SPS 11/09/82
--     JBG 08/30/83
--     EG  11/26/84
--     EG  06/04/85
--     GMT 08/03/87  CLARIFIED SOME OF THE FAILED MESSAGES, AND
--                   REMOVED THE EXCEPTION FOR CONSTRAINT_ERROR.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2402A IS

     PACKAGE DIR IS NEW DIRECT_IO (INTEGER);
     USE DIR;
     FILE1   : FILE_TYPE;
     CNST    : CONSTANT INTEGER := 101;
     IVAL    : INTEGER;
     BOOL    : BOOLEAN;
     X_COUNT : COUNT;
     P_COUNT : POSITIVE_COUNT;

BEGIN
     TEST ("CE2402A","CHECK THAT READ, WRITE, INDEX, "   &
                     "SET_INDEX, SIZE, AND END_OF_FILE " &
                     "RAISE STATUS_ERROR WHEN APPLIED "  &
                     "A NON-OPEN DIRECT FILE");
     BEGIN
          WRITE (FILE1, CNST);
          FAILED ("STATUS_ERROR WAS NOT RAISED ON WRITE - 1");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED ON WRITE - 2");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON WRITE - 3");
     END;

     BEGIN
          X_COUNT := SIZE (FILE1);
          FAILED ("STATUS_ERROR NOT RAISED ON SIZE - 4");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED ON SIZE - 5");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON SIZE - 6");
     END;

     BEGIN
          BOOL := END_OF_FILE (FILE1);
          FAILED ("STATUS_ERROR WAS NOT RAISED ON END_OF_FILE - 7");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED ON END_OF_FILE - 8");
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON END_OF_FILE - 9");
     END;

     BEGIN
          P_COUNT := INDEX (FILE1);
          FAILED ("STATUS_ERROR WAS NOT RAISED ON INDEX - 10");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED ON INDEX - 11");
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON INDEX - 12");
     END;

     BEGIN
          READ (FILE1, IVAL);
          FAILED ("STATUS_ERROR WAS NOT RAISED ON READ - 13");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED ON READ - 14");
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON READ - 15");
     END;

     DECLARE
          ONE : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(1));
     BEGIN
          BEGIN
               WRITE (FILE1, CNST, ONE);
               FAILED ("STATUS_ERROR NOT RAISED ON WRITE - 16");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN USE_ERROR =>
                    FAILED ("USE_ERROR RAISED ON WRITE - 17");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED ON WRITE - 18");
          END;

          BEGIN
               SET_INDEX (FILE1,ONE);
               FAILED ("STATUS_ERROR NOT RAISED ON SET_INDEX - 19");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN USE_ERROR =>
                    FAILED ("USE_ERROR RAISED ON SET_INDEX - 20");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED ON SET_INDEX - 21");
          END;

          BEGIN
               READ (FILE1, IVAL, ONE);
               FAILED ("STATUS_ERROR WAS NOT RAISED ON READ - 22");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN USE_ERROR =>
                    FAILED ("USE_ERROR RAISED ON READ - 23");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED ON READ - 24");
          END;
     END;

     RESULT;

END CE2402A;
