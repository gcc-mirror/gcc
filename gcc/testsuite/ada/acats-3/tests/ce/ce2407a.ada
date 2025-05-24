-- CE2407A.ADA

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
--     CHECK THAT WRITE RAISES MODE_ERROR WHEN THE CURRENT MODE
--     IS IN_FILE.

--          1) CHECK NON-TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH OUT_FILE MODE AND OPEN WITH IN_FILE MODE FOR DIRECT
--     FILES.

-- HISTORY:
--     ABW 08/20/82
--     SPS 09/16/82
--     SPS 11/09/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 08/06/86  REMOVED THE DEPENDENCE ON RESET AND MOVED THE CHECK
--                   FOR TEMPORARY FILES INTO CE2407B.ADA.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2407A IS

     PACKAGE DIR IS NEW DIRECT_IO (INTEGER);
     USE DIR;
     INCOMPLETE : EXCEPTION;
     FILE1      : FILE_TYPE;
     INT        : INTEGER := IDENT_INT (18);

BEGIN
     TEST ("CE2407A", "CHECK THAT WRITE RAISES MODE_ERROR WHEN THE " &
                      "CURRENT MODE IS IN_FILE AND THE FILE IS " &
                      "A NON-TEMPORARY FILE");
     BEGIN
          CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE - 1");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE - 2");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE - 3");
               RAISE INCOMPLETE;
     END;

     WRITE (FILE1, INT);
     CLOSE (FILE1);

     BEGIN
          OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE("USE_ERROR RAISED ON OPEN - 4");
               RAISE INCOMPLETE;
     END;



     BEGIN
          WRITE (FILE1,INT);
          FAILED ("MODE_ERROR NOT RAISED ON WRITE - 5");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED ON WRITE - 6");
     END;

     BEGIN
          DELETE (FILE1);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2407A;
