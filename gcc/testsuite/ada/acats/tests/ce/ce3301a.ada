-- CE3301A.ADA

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
--     CHECK THAT WHEN THE LINE AND PAGE LENGTH ARE NONZERO, LINE AND
--     PAGE TERMINATORS ARE OUTPUT AT THE APPROPRIATE POINTS.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/22/82
--     SPS 11/15/82
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/27/87  COMPLETELY REVISED TEST.
--     LDC 05/26/88  ADDED "FILE" PARAMETERS.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3301A IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     TWO : CONSTANT COUNT := COUNT(IDENT_INT(2));
     TEN : CONSTANT COUNT := COUNT(IDENT_INT(10));
     THREE : CONSTANT COUNT := COUNT(IDENT_INT(3));
     ITEM1 : STRING (1..10);
     ITEM2 : STRING (1..2);

BEGIN

     TEST ("CE3301A", "CHECK THAT WHEN THE LINE AND PAGE LENGTH ARE " &
                      "NONZERO, LINE AND PAGE TERMINATORS ARE " &
                      "OUTPUT AT THE APPROPRIATE POINTS");

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT CREATE " &
                               "WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     IF LINE_LENGTH (FILE) /= UNBOUNDED THEN
          FAILED ("LINE LENGTH NOT INITIALLY UNBOUNDED");
     END IF;

     IF PAGE_LENGTH (FILE) /= UNBOUNDED THEN
          FAILED ("PAGE LENGTH NOT INITIALLY UNBOUNDED");
     END IF;

     SET_LINE_LENGTH (FILE,TEN);
     SET_PAGE_LENGTH (FILE,TWO);

     FOR I IN 1 .. 30 LOOP
          PUT (FILE,'C');
     END LOOP;

     IF PAGE (FILE) /= 2 AND LINE (FILE) /= 1 THEN
          FAILED ("LINE AND PAGE LENGTHS WERE NOT BOUND " &
                  "CORRECTLY");
     END IF;

     SET_LINE_LENGTH (FILE, TWO);
     SET_PAGE_LENGTH (FILE, THREE);
     PUT (FILE, "DDDDDDD");

     CLOSE (FILE);

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN WITH " &
                               "IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     GET (FILE, ITEM1);

     IF NOT (END_OF_LINE (FILE)) THEN
          FAILED ("INCORRECT VALUE BEFORE LINE TERMINATOR");
     END IF;

     IF END_OF_PAGE (FILE) THEN
          FAILED ("PAGE TERMINATOR OUTPUT AT INAPPROPRIATE POINT");
     END IF;

     GET (FILE, ITEM1);

     IF ITEM1 /= "CCCCCCCCCC" THEN
          FAILED ("INCORRECT VALUE READ");
     END IF;

     IF NOT (END_OF_LINE(FILE)) THEN
          FAILED ("INCORRECT VALUE BEFORE LINE TERMINATOR");
     END IF;

     IF NOT (END_OF_PAGE(FILE)) THEN
          FAILED ("INCORRECT VALUE BEFORE PAGE TERMINATOR");
     END IF;

     GET (FILE, ITEM1);
     GET (FILE, ITEM2);

     IF ITEM2 /= "DD" THEN
          FAILED ("INCORRECT VALUE READ");
     END IF;

     IF NOT (END_OF_LINE(FILE)) THEN
          FAILED ("INCORRECT VALUE BEFORE LINE TERMINATOR");
     END IF;

     IF END_OF_PAGE (FILE) THEN
          FAILED ("PAGE TERMINATOR OUTPUT AT INAPPROPRIATE POINT");
     END IF;

     GET (FILE, ITEM2);

     IF ITEM2 /= "DD" THEN
          FAILED ("INCORRECT VALUE READ");
     END IF;

     IF NOT (END_OF_LINE(FILE)) THEN
          FAILED ("INCORRECT VALUE BEFORE LINE TERMINATOR");
     END IF;

     IF NOT (END_OF_PAGE(FILE)) THEN
          FAILED ("INCORRECT VALUE BEFORE PAGE TERMINATOR");
     END IF;

     BEGIN
          DELETE (FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3301A;
