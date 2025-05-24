-- CE3402A.ADA

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
--     CHECK THAT NEW_LINE RAISES MODE_ERROR WHEN THE FILE MODE
--     IS IN_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/16/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/19/87  ADDED ATTEMPT TO DELETE THE FILE AND REPLACED
--                   RESET WITH CLOSE AND OPEN.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3402A IS

     INCOMPLETE : EXCEPTION;
     FILE1 : FILE_TYPE;
     SPAC : CONSTANT POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(1));

BEGIN

     TEST ("CE3402A" , "CHECK THAT NEW_LINE RAISES MODE_ERROR " &
                       "WHEN THE FILE MODE IS IN_FILE");

     BEGIN
          CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          PUT_LINE (FILE1, "STUFF");
          CLOSE (FILE1);
          OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED FOR OPEN " &
                               "WITH IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          NEW_LINE (FILE1,SPAC);
          FAILED ("MODE_ERROR NOT RAISED FOR IN_FILE");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR IN_FILE");
     END;

     BEGIN
          NEW_LINE (STANDARD_INPUT,SPAC);
          FAILED ("MODE_ERROR NOT RAISED FOR STANDARD_INPUT");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR STANDARD_INPUT");
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

END CE3402A;
