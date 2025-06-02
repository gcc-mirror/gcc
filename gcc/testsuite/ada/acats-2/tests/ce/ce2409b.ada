-- CE2409B.ADA

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
--     FOR DIRECT ACCESS FILES, CHECK THAT A WRITE TO A POSITION
--     GREATER THAN THE CURRENT END POSITION CAUSES THE WRITE
--     POSITION AND THE FILE SIZE TO BE INCREMENTED.

--          2) CHECK FILES OF MODE OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH MODE OUT_FILE FOR DIRECT FILES.

-- HISTORY:
--     GMT 08/05/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2409B IS

     PACKAGE DIR IS NEW DIRECT_IO (INTEGER);
     USE DIR;
     FILE1      : FILE_TYPE;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE2409B", "CHECK THAT WRITE POSITION AND " &
                      "SIZE ARE INCREMENTED APPROPRIATELY");
     BEGIN
          CREATE (FILE1, OUT_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("CREATE WITH MODE OUT_FILE NOT " &
                               "SUPPORTED FOR DIR FILES - 1");
               RAISE INCOMPLETE;
     END;

     DECLARE
          INT      : INTEGER := IDENT_INT (18);
          TWO_C    : COUNT := COUNT (IDENT_INT(2));
          THREE_C  : COUNT := COUNT (IDENT_INT(3));
          THREE_PC : POSITIVE_COUNT
                           := POSITIVE_COUNT (IDENT_INT(3));
          FOUR_PC  : POSITIVE_COUNT
                           := POSITIVE_COUNT (IDENT_INT(4));
     BEGIN
          WRITE (FILE1, INT);
          WRITE (FILE1, INT);
          IF INDEX (FILE1) /= THREE_PC THEN
               FAILED ("INCORRECT VALUE FOR INDEX - 2");
          END IF;
          IF SIZE (FILE1) /= TWO_C THEN
               FAILED ("INCORRECT VALUE FOR SIZE - 3");
          END IF;

          WRITE (FILE1, INT);
          IF INDEX (FILE1) /= FOUR_PC THEN
               FAILED ("INCORRECT VALUE FOR INDEX - 4");
          END IF;
          IF SIZE (FILE1) /= THREE_C THEN
               FAILED ("INCORRECT VALUE FOR SIZE - 5");
          END IF;

     END;

     CLOSE (FILE1);

     RESULT ;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2409B ;
