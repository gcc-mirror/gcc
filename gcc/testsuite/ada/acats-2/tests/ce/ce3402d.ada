-- CE3402D.ADA

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
--     CHECK THAT NEW_LINE SETS THE CURRENT COLUMN NUMBER TO ONE,
--     AND NEW_LINE OUTPUTS LINE TERMINATORS WHEN THE SPACING IS
--     GREATER THAN ONE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH OUT_FILE MODE FOR TEXT FILES.

-- HISTORY:
--     ABW 08/26/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/19/87  CHANGED FAILED MESSAGE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3402D IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     ONE : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(1));
     SPAC3 : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(3));
     FOUR : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(4));

BEGIN

     TEST ("CE3402D",  "CHECK THAT NEW_LINE SETS THE CURRENT " &
                       "COLUMN NUMBER TO ONE, AND NEW_LINE OUTPUTS " &
                       "TERMINATORS WHEN THE SPACING IS " &
                       "GREATER THAN ONE");

     BEGIN
          CREATE (FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     FOR I IN 1..5 LOOP
          PUT (FILE, 'X');
     END LOOP;

     NEW_LINE (FILE, SPAC3);
     IF LINE (FILE) /= FOUR THEN
          FAILED ("NEW_LINE DID NOT OUTPUT LINE TERMINATORS");
     END IF;

     IF COL (FILE) /= ONE THEN
          FAILED ("COLUMN NOT SET TO ONE");
     END IF;
     CLOSE (FILE);

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3402D;
