-- CE2411A.ADA

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
--     CHECK THAT INDEX RETURNS THE CORRECT INDEX POSITION AND THAT
--     SET_INDEX CORRECTLY SETS THE INDEX POSITION IN A DIRECT FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT FILES.

-- HISTORY:
--     TBN 10/01/86
--     JLH 08/07/87  REVISED EXTERNAL FILE NAME, REMOVED CHECK FOR
--                   NAME_ERROR ON OPEN CALLS, AND REMOVED
--                   UNNECESSARY CODE.

WITH DIRECT_IO;
WITH REPORT; USE REPORT;
PROCEDURE CE2411A IS

     PACKAGE DIR_IO IS NEW DIRECT_IO (INTEGER);
     USE DIR_IO;

     FILE1 : FILE_TYPE;
     INCOMPLETE : EXCEPTION;

BEGIN
     TEST ("CE2411A", "CHECK THAT INDEX RETURNS THE CORRECT INDEX " &
                      "POSITION AND THAT SET_INDEX CORRECTLY SETS " &
                      "THE INDEX POSITION IN A DIRECT FILE");


     -- INITIALIZE TEST FILE

     BEGIN
          CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED DURING CREATE " &
                               "WITH OUT_FILE MODE FOR DIR_IO");
               RAISE INCOMPLETE;
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED DURING CREATE " &
                               "WITH OUT_FILE MODE FOR DIR_IO");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNKNOWN EXCEPTION RAISED DURING CREATE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          IF INDEX (FILE1) /= 1 THEN
               FAILED ("STARTING INDEX POSITION IS INCORRECT - 1");
               RAISE INCOMPLETE;
          END IF;
          FOR I IN 1 .. 10 LOOP
               WRITE (FILE1, I);
          END LOOP;
          IF INDEX (FILE1) /= 11 THEN
               FAILED ("INDEX DOES NOT RETURN CORRECT POSITION - 2");
          END IF;
          WRITE (FILE1, 20, 20);
          IF INDEX (FILE1) /= 21 THEN
               FAILED ("INDEX DOES NOT RETURN CORRECT POSITION - 3");
          END IF;
          SET_INDEX (FILE1, 11);
          IF INDEX (FILE1) /= 11 THEN
               FAILED ("SET_INDEX DOES NOT CORRECTLY SET POSITION - 4");
          END IF;
          WRITE (FILE1, 11);
          IF INDEX (FILE1) /= 12 THEN
               FAILED ("INDEX DOES NOT RETURN CORRECT POSITION - 5");
          END IF;
     END;

     CLOSE (FILE1);

     BEGIN
          OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED DURING OPEN INFILE " &
                               "FOR DIR_IO");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNKNOWN EXCEPTION RAISED DURING OPEN INFILE");
               RAISE INCOMPLETE;
     END;

     DECLARE
          NUM : INTEGER;
     BEGIN
          IF INDEX (FILE1) /= 1 THEN
               FAILED ("STARTING INDEX POSITION IS INCORRECT - 7");
               RAISE INCOMPLETE;
          END IF;
          FOR I IN 1 .. 10 LOOP
               READ (FILE1, NUM);
               IF NUM /= I THEN
                    FAILED ("FILE CONTAINS INCORRECT DATA - 8");
               END IF;
               IF INDEX (FILE1) /= POSITIVE_COUNT(I + 1) THEN
                    FAILED ("INDEX DOES NOT RETURN THE CORRECT " &
                            "POSITION - 9");
               END IF;
          END LOOP;
          SET_INDEX (FILE1, 20);
          IF INDEX (FILE1) /= 20 THEN
               FAILED ("SET_INDEX DOES NOT CORRECTLY SET POSITION - " &
                       "10");
          END IF;
          READ (FILE1, NUM, 20);
          IF NUM /= 20 THEN
               FAILED ("FILE CONTAINS INCORRECT DATA - 11");
          END IF;
          IF INDEX (FILE1) /= 21 THEN
               FAILED ("INDEX DOES NOT RETURN CORRECT POSITION - 12");
          END IF;
          SET_INDEX (FILE1, 1);
          IF INDEX (FILE1) /= 1 THEN
               FAILED ("SET_INDEX DOES NOT CORRECTLY SET POSITION - " &
                       "13");
          END IF;
     END;

     CLOSE (FILE1);

     BEGIN
          OPEN (FILE1, INOUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED DURING OPEN " &
                               "INOUT_FILE FOR DIR_IO");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNKNOWN EXCEPTION RAISED DURING OPEN INOUT");
               RAISE INCOMPLETE;
     END;

     DECLARE
          NUM : INTEGER;
     BEGIN
          IF INDEX (FILE1) /= 1 THEN
               FAILED ("STARTING INDEX POSITION IS INCORRECT - 15");
               RAISE INCOMPLETE;
          END IF;
          FOR I IN 1 .. 10 LOOP
               READ (FILE1, NUM);
               IF NUM /= I THEN
                    FAILED ("FILE CONTAINS INCORRECT DATA - 16");
               END IF;
               IF INDEX (FILE1) /= POSITIVE_COUNT(I + 1) THEN
                    FAILED ("INDEX DOES NOT RETURN THE CORRECT " &
                            "POSITION - 17");
               END IF;
          END LOOP;
          SET_INDEX (FILE1, 20);
          IF INDEX (FILE1) /= 20 THEN
               FAILED ("SET_INDEX DOES NOT CORRECTLY SET POSITION - " &
                       "18");
          END IF;
          WRITE (FILE1, 12, 12);
          IF INDEX (FILE1) /= 13 THEN
               FAILED ("INDEX DOES NOT RETURN CORRECT POSITION - 19");
          END IF;
          SET_INDEX (FILE1, 1);
          IF INDEX (FILE1) /= 1 THEN
               FAILED ("SET_INDEX DOES NOT CORRECTLY SET POSITION - " &
                       "20");
          END IF;
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
END CE2411A;
