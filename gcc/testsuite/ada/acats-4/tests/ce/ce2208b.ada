-- CE2208B.ADA

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
--     CHECK THAT DATA CAN BE OVERWRITTEN IN THE SEQUENTIAL FILE AND THE
--     CORRECT VALUES CAN LATER BE READ.  ALSO CHECK THAT OVERWRITING
--     TRUNCATES THE FILE TO THE LAST ELEMENT WRITTEN.

-- APPLICABILITY CRITERIA:
--      THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--      THE CREATING AND OPENING OF SEQUENTIAL FILES.

-- HISTORY:
--     TBN  09/30/86  CREATED ORIGINAL TEST.
--     GMT  07/24/87  ADDED CHECKS FOR USE_ERROR AND REMOVED SOME CODE.
--     BCB  10/03/90  CHANGED CODE TO CHECK THAT OVERWRITING TRUNCATES
--                    INSTEAD OF WHETHER IT TRUNCATES.

WITH SEQUENTIAL_IO;
WITH REPORT; USE REPORT;
PROCEDURE CE2208B IS

     PACKAGE SEQ_IO IS NEW SEQUENTIAL_IO (INTEGER);
     USE SEQ_IO;

     FILE1      : FILE_TYPE;
     INCOMPLETE : EXCEPTION;

BEGIN
     TEST ("CE2208B",
           "CHECK THAT DATA CAN BE OVERWRITTEN IN THE SEQUENTIAL " &
           "FILE AND THE CORRECT VALUES CAN LATER BE READ.  ALSO " &
           "CHECK THAT OVERWRITING TRUNCATES THE FILE." );

     -- INITIALIZE TEST FILE

     BEGIN
          CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED DURING CREATE");
               RAISE INCOMPLETE;
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED DURING CREATE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNKNOWN EXCEPTION RAISED DURING CREATE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          FOR I IN 1 .. 25 LOOP
               WRITE (FILE1, I);
          END LOOP;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED DURING WRITE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          CLOSE (FILE1);
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED DURING CLOSE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          OPEN (FILE1, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ( "OPEN WITH  OUT_FILE  MODE NOT "  &
                                "SUPPORTED FOR SEQUENTIAL FILES" );
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED DURING OPEN");
               RAISE INCOMPLETE;
     END;

     BEGIN
          FOR I IN 26 .. 36 LOOP
               WRITE (FILE1, I);
          END LOOP;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED DURING OVERWRITE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          CLOSE (FILE1);
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED DURING 2ND CLOSE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ( "OPEN WITH  IN_FILE  MODE NOT "   &
                                "SUPPORTED FOR SEQUENTIAL FILES" );
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED DURING SECOND OPEN");
               RAISE INCOMPLETE;
     END;

     DECLARE
          END_REACHED : BOOLEAN := FALSE;
          COUNT : INTEGER := 26;
          NUM : INTEGER;
     BEGIN
          WHILE COUNT <= 36 AND NOT END_REACHED LOOP
               BEGIN
                    READ (FILE1, NUM);
                    IF NUM /= COUNT THEN
                         FAILED ("INCORRECT RESULTS READ FROM FILE " &
                                 INTEGER'IMAGE (NUM));
                    END IF;
                    COUNT := COUNT + 1;
               EXCEPTION
                    WHEN END_ERROR =>
                         END_REACHED := TRUE;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED DURING " &
                                 "READING - 1");
                         RAISE INCOMPLETE;
               END;
          END LOOP;
          IF COUNT <= 36 THEN
               FAILED ("FILE WAS INCOMPLETE");
               RAISE INCOMPLETE;
          ELSE
               BEGIN
                    READ (FILE1, NUM);
                    FAILED ("END_ERROR NOT RAISED BY ATTEMPT TO READ");
               EXCEPTION
                    WHEN END_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED " &
                                 "DURING READING - 2");
                         RAISE INCOMPLETE;
               END;
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

END CE2208B;
