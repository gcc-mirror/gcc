-- CE2108E.ADA

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
--     CHECK THAT AN EXTERNAL SEQUENTIAL FILE SPECIFIED BY A NON-NULL
--     STRING NAME IS ACCESSIBLE AFTER THE COMPLETION OF THE MAIN
--     PROGRAM.

--     THIS TEST CREATES A SEQUENTIAL FILE;  CE2108F.ADA READS IT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF AN EXTERNAL SEQUENTIAL FILE WITH OUT_FILE MODE.

-- HISTORY:
--     TBN 07/16/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2108E IS

     PACKAGE SEQ IS NEW SEQUENTIAL_IO (INTEGER);
     INCOMPLETE : EXCEPTION;
     FILE_NAME : SEQ.FILE_TYPE;
     PREVENT_EMPTY_FILE : NATURAL := 5;

BEGIN

     TEST ("CE2108E" , "CHECK THAT AN EXTERNAL SEQUENTIAL FILE " &
                       "SPECIFIED BY A NON-NULL STRING NAME IS " &
                       "ACCESSIBLE AFTER THE COMPLETION OF THE MAIN " &
                       "PROGRAM");
     BEGIN
          BEGIN
               SEQ.CREATE (FILE_NAME, SEQ.OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN SEQ.USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON SEQUENTIAL " &
                                    "CREATE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN SEQ.NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON SEQUENTIAL " &
                                    "CREATE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON " &
                            "SEQUENTIAL CREATE");
                    RAISE INCOMPLETE;
          END;

          SEQ.WRITE (FILE_NAME, PREVENT_EMPTY_FILE);
          SEQ.CLOSE (FILE_NAME);

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;

END CE2108E;
