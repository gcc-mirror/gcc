-- CE2102B.ADA

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
--     CHECK TO SEE THAT STATUS_ERROR IS RAISED WHEN PERFORMING ILLEGAL
--     OPERATIONS ON OPENED OR UNOPENED FILES OF TYPE DIRECT_IO.

--          A) OPENED FILES

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO THOSE  IMPLEMENTATIONS WHICH
--     SUPPORT CREATE WITH OUT_FILE MODE FOR DIRECT FILES.

-- HISTORY:
--     DLD 08/10/82
--     SPS 11/03/82
--     JBG 02/22/84
--     SPW 08/13/87  SPLIT CASE FOR UNOPENED FILES INTO CE2102M.ADA.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2102B IS

     PACKAGE DIR_IO IS NEW DIRECT_IO(INTEGER);
     USE DIR_IO;
     TEST_FILE_ONE : DIR_IO.FILE_TYPE;

BEGIN

     TEST ("CE2102B", "CHECK THAT STATUS_ERROR IS RAISED WHEN " &
                      "PERFORMING ILLEGAL OPERATIONS ON FILES " &
                      "OF TYPE DIRECT_IO");

     BEGIN
          CREATE (TEST_FILE_ONE, OUT_FILE, LEGAL_FILE_NAME);

-- CHECK THAT OPEN STATEMENT RAISES EXCEPTION WHEN FILE IS ALREADY OPEN

          BEGIN
               OPEN (TEST_FILE_ONE, OUT_FILE, LEGAL_FILE_NAME);
               FAILED ("STATUS_ERROR NOT RAISED WHEN FILE IS " &
                       "ALREADY OPEN - 1");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED ON OPEN - 1");
          END;

          BEGIN
               OPEN (TEST_FILE_ONE, IN_FILE, LEGAL_FILE_NAME);
               FAILED ("STATUS_ERROR NOT RAISED WHEN FILE IS " &
                       "ALREADY OPEN - 2");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED ON OPEN - 2");
          END;

          BEGIN
               OPEN (TEST_FILE_ONE, INOUT_FILE, LEGAL_FILE_NAME);
               FAILED ("STATUS_ERROR NOT RAISED WHEN FILE IS " &
                       "ALREADY OPEN - 3");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED ON OPEN - 3");
          END;

-- CHECK THAT CREATE STATEMENT RAISES EXCEPTION WHEN FILE IS ALREADY
-- OPEN

          BEGIN
               CREATE (TEST_FILE_ONE, IN_FILE, LEGAL_FILE_NAME);
               FAILED ("STATUS_ERROR NOT RAISED WHEN AN OPEN " &
                       "FILE IS USED IN A CREATE - 1");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED ON CREATE - 1");
          END;

          BEGIN
               CREATE (TEST_FILE_ONE, OUT_FILE, LEGAL_FILE_NAME);
               FAILED ("STATUS_ERROR NOT RAISED WHEN AN OPEN " &
                       "FILE IS USED IN A CREATE - 2");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED ON CREATE - 2");
          END;

          BEGIN
               CREATE (TEST_FILE_ONE, INOUT_FILE, LEGAL_FILE_NAME);
               FAILED ("STATUS_ERROR NOT RAISED WHEN AN OPEN " &
                       "FILE IS USED IN A CREATE - 3");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED ON CREATE - 3");
          END;

--DELETE TEST FILE

          BEGIN
               DELETE (TEST_FILE_ONE);
          EXCEPTION
               WHEN USE_ERROR =>
                    COMMENT ("DELETION OF EXTERNAL FILE APPEARS NOT " &
                             "TO BE SUPPORTED");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED FOR DELETE");
          END;

     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED FOR CREATE " &
                               "WITH OUT_FILE MODE");
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED FOR CREATE " &
                               "WITH OUT_FILE MODE");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR CREATE");
     END;

     RESULT;

END CE2102B;
